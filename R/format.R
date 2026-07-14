# Tier 1: pure formatting. No graphics, no device, no dependencies.
#
# This file is where Bug 1 (character cells rendered as "Unknown") and Bug 2
# (numbers rendered at 15 significant digits) die. Nothing below this file is
# allowed to inspect a value's type or decide how many digits it shows.

# ---------------------------------------------------------------------------
# numeric helpers
# ---------------------------------------------------------------------------

#' Digits to the left of the decimal point
#'
#' Returns the number of digits in the integer part of `x`, i.e.
#' `floor(log10(abs(x))) + 1`. Zero (and every non-finite value) is reported as
#' `1`, because `log10(0)` is `-Inf` and would poison the arithmetic downstream.
#'
#' @param x A double vector.
#'
#' @return An integer vector the same length as `x`.
#'
#' @keywords internal
#' @noRd
lhs_digits <- function(x) {
  out <- rep(1L, length(x))
  nz <- is.finite(x) & x != 0
  if (any(nz)) {
    ax <- abs(x[nz])
    e <- floor(log10(ax))
    # log10() is not exact; nudge back if it landed on the wrong side of a
    # power of ten.
    e <- e + (ax >= 10^(e + 1)) - (ax < 10^e)
    out[nz] <- as.integer(e) + 1L
  }
  out
}

#' Significant digits needed to represent a value
#'
#' The smallest `s` in `1:digits` for which `signif(x, s)` still equals `x` when
#' both are viewed at `digits` significant digits. The comparison is made at
#' `digits` (not against the raw double) so that floating point noise does not
#' inflate the answer: `0.1 + 0.2` needs one significant digit, not seventeen.
#'
#' @param x A finite double vector.
#' @param digits Maximum significant digits considered. Default 15, which is the
#'   precision a double actually carries.
#'
#' @return An integer vector the same length as `x`.
#'
#' @keywords internal
#' @noRd
sig_digits_needed <- function(x, digits = 15L) {
  n <- length(x)
  out <- rep(as.integer(digits), n)
  if (n == 0L) {
    return(out)
  }
  target <- signif(x, digits)
  done <- rep(FALSE, n)
  for (s in seq_len(digits)) {
    todo <- which(!done)
    if (length(todo) == 0L) {
      break
    }
    ok <- signif(x[todo], s) == target[todo]
    done[todo[ok]] <- TRUE
    out[todo[ok]] <- s
  }
  out
}

#' Decimal places needed to represent a value exactly
#'
#' How many digits after the decimal point `x` genuinely needs. This is the
#' `have` half of the `d <- pmin(want, have)` rule: it is what stops a value that
#' is already exact from being padded with meaningless zeros, and it is what
#' keeps `c(1, 1e10)` in fixed notation.
#'
#' @param x A double vector. Non-finite values report `0`.
#' @param max_dec Upper bound on the answer.
#'
#' @return An integer vector the same length as `x`.
#'
#' @keywords internal
#' @noRd
decs_to_exact <- function(x, max_dec = 350L) {
  out <- integer(length(x))
  fin <- which(is.finite(x))
  if (length(fin) == 0L) {
    return(out)
  }
  xf <- x[fin]
  out[fin] <- pmin(
    as.integer(max_dec),
    pmax(0L, sig_digits_needed(xf) - lhs_digits(xf))
  )
  out
}

#' Format one double with one decimal count
#'
#' `formatC()` does **not** vectorise over `digits` (it errors with "the
#' condition has length > 1"), so the per-element decimal counts have to be
#' walked one at a time.
#'
#' @param x A double vector.
#' @param d An integer vector of decimal counts, same length as `x`.
#'
#' @return A character vector the same length as `x`.
#'
#' @keywords internal
#' @noRd
formatC_each <- function(x, d) {
  if (length(x) == 0L) {
    return(character(0))
  }
  vapply(
    seq_along(x),
    function(i) formatC(x[i], format = "f", digits = d[i]),
    character(1)
  )
}

# ---------------------------------------------------------------------------
# splitting
# ---------------------------------------------------------------------------

#' Split a token into its significant and insignificant spans
#'
#' Walks the token left to right counting significant digits. Leading zeros do
#' not count (`0.0999` has three significant digits, not five); once counting has
#' started every digit counts, zero or not (`10000000000` reaches three at the
#' third character). Everything after the `sigfig`-th significant digit -- digits,
#' the trailing rounding marker, anything -- is the insignificant span.
#'
#' Tokens with fewer than `sigfig` significant digits, and tokens with no digits
#' at all (`"NA"`, `"Inf"`), come back whole with an empty insignificant span.
#'
#' Never call this on a scientific token: `"1.00e+15"` reaches three significant
#' digits inside the mantissa and would grey the exponent. Callers force
#' `insig = ""` in scientific mode.
#'
#' @param tok A character vector of rendered tokens.
#' @param sigfig Number of significant digits to keep in the black span.
#'
#' @return A list with character components `sig` and `insig`, each the same
#'   length as `tok`. `paste0(sig, insig)` is `tok`.
#'
#' @keywords internal
#' @noRd
split_sig <- function(tok, sigfig = 3L) {
  n <- length(tok)
  sig <- tok
  insig <- rep("", n)
  digits <- as.character(0:9)
  for (i in seq_len(n)) {
    ch <- strsplit(tok[i], "", fixed = TRUE)[[1]]
    isdig <- ch %in% digits
    seen <- 0L
    cut <- NA_integer_
    for (k in seq_along(ch)) {
      if (!isdig[k]) {
        next
      }
      if (seen == 0L && ch[k] == "0") {
        next
      }
      seen <- seen + 1L
      if (seen == sigfig) {
        cut <- k
        break
      }
    }
    if (!is.na(cut) && cut < length(ch)) {
      sig[i] <- paste(ch[seq_len(cut)], collapse = "")
      insig[i] <- paste(ch[(cut + 1L):length(ch)], collapse = "")
    }
  }
  list(sig = sig, insig = insig)
}

#' Split a token at its decimal point
#'
#' `head` is everything before the first `"."`, `tail` is the `"."` and
#' everything after it. These are the anchors the layout tier uses to line the
#' decimal points of a formatting unit up on one x coordinate, without padding
#' and without a monospace font.
#'
#' @param tok A character vector of rendered tokens.
#'
#' @return A list with character components `head` and `tail`.
#'
#' @keywords internal
#' @noRd
split_dec <- function(tok) {
  p <- regexpr(".", tok, fixed = TRUE)
  head <- ifelse(p > 0L, substr(tok, 1L, p - 1L), tok)
  tail <- ifelse(p > 0L, substr(tok, p, nchar(tok)), "")
  list(head = as.character(head), tail = as.character(tail))
}

#' Truncate strings to a maximum width
#'
#' Strings longer than `max_chars` are cut and given a trailing `ellipsis`, so
#' the result is never wider than `max_chars`. The ellipsis is ASCII `"..."` by
#' default and must stay ASCII by default: `pdf()`, the device `R CMD check`
#' uses, cannot encode U+2026.
#'
#' @param x A character vector.
#' @param max_chars Maximum width of the result, in characters.
#' @param ellipsis The marker appended to a truncated string.
#'
#' @return A character vector the same length as `x`.
#'
#' @keywords internal
#' @noRd
truncate_chr <- function(x, max_chars = 12L, ellipsis = "...") {
  if (length(x) == 0L) {
    return(character(0))
  }
  max_chars <- as.integer(max_chars)
  keep <- max_chars - nchar(ellipsis, type = "chars")
  if (keep < 1L) {
    stop("`max_chars` must be wider than `ellipsis`.")
  }
  long <- !is.na(x) & nchar(x, type = "chars") > max_chars
  if (any(long)) {
    x[long] <- paste0(substr(x[long], 1L, keep), ellipsis)
  }
  x
}

#' Type tag for a column header
#'
#' The abbreviation a data frame column is labelled with (`<dbl>`, `<chr>`, ...).
#'
#' @param x Any vector.
#'
#' @return A length-one character string.
#'
#' @keywords internal
#' @noRd
type_tag <- function(x) {
  if (is.data.frame(x)) {
    return("<df>")
  }
  if (is.factor(x)) {
    return("<fct>")
  }
  if (inherits(x, "POSIXct")) {
    return("<dttm>")
  }
  # A POSIXlt IS A DATETIME, and it must be asked BEFORE `is.list()` -- which is
  # TRUE of it, and which would otherwise tag it `<list>`. It never becomes eleven
  # ragged columns of sec/min/hour/...: `is_paint_list()` refuses it as a whole
  # structure, and `elem_expands()` refuses it as an element. It reaches here only as
  # a thing being NAMED -- an element summarised as `<dttm [1]>`, a data frame column
  # labelled `<dttm>` -- and the name it deserves is the one its POSIXct twin gets.
  if (inherits(x, "POSIXlt")) {
    return("<dttm>")
  }
  if (inherits(x, "Date")) {
    return("<date>")
  }
  if (inherits(x, "difftime")) {
    return("<drtn>")
  }
  if (is.list(x)) {
    return("<list>")
  }
  switch(
    typeof(x),
    double = "<dbl>",
    integer = "<int>",
    character = "<chr>",
    logical = "<lgl>",
    complex = "<cpl>",
    paste0("<", class(x)[1], ">")
  )
}

# ---------------------------------------------------------------------------
# the generic
# ---------------------------------------------------------------------------

#' Format a vector for painting
#'
#' Turns the values of one formatting unit into the spans a painter draws. A
#' formatting unit is a whole matrix or vector, or one column of a data frame;
#' `paint_format()` is called once per unit, which is exactly why the same value
#' looks identical in every cell of a matrix.
#'
#' Numbers are shown at `sigfig` significant digits. The digits past the
#' `sigfig`-th are not discarded -- they are returned separately in `insig`, so
#' the renderer can draw them in grey. `paste0(sig, insig)` is always exactly the
#' token that gets drawn.
#'
#' The whole unit renders in fixed notation, or the whole unit renders in
#' scientific notation. The decision is made on **width**, not magnitude: if the
#' widest fixed token would exceed `max_dec_width` characters, the unit flips.
#' In scientific mode `insig` is always `""`, because every digit of a
#' scientific token, exponent included, is significant.
#'
#' @param x A vector.
#' @param sigfig Significant digits to show in black. Must be in `1:15`.
#' @param max_chars Strings longer than this are truncated with `ellipsis`.
#' @param max_dec_width Widest fixed-notation token tolerated before the unit
#'   flips to scientific notation.
#' @param subtle_digits Which digits are returned in the grey `insig` span.
#'   `"insignificant"` greys everything past the `sigfig`-th significant digit,
#'   exact or not (so `100000` is black `100` plus grey `000`).
#'   `"rounded"` greys only values whose token actually lost precision (so
#'   `100000` is entirely black, but `123456.789` still greys `457.`).
#'   `"none"` returns an empty `insig` for every value.
#' @param ellipsis The truncation marker. Keep it ASCII unless you know your
#'   device can encode the alternative.
#' @param ... Passed to methods.
#'
#' @return A data frame with one row per element of `x` and columns:
#'   \describe{
#'     \item{`sig`}{the black span}
#'     \item{`insig`}{the grey span, `""` when nothing is insignificant}
#'     \item{`head`}{the token up to the decimal point}
#'     \item{`tail`}{the decimal point and everything after it}
#'     \item{`ink`}{`"black"` for a finite value, `"blue"` for `Inf`/`NaN`,
#'       `"red"` for `NA`}
#'     \item{`align`}{`"decimal"`, `"left"` or `"right"`}
#'   }
#'   with attributes `tag` (the type tag) and `sci` (did the unit flip to
#'   scientific notation?).
#'
#' @examples
#' paintr:::paint_format(c(1, 1 / 3))
#' paintr:::paint_format(123456.789)
#' paintr:::paint_format(letters[1:3])
#'
#' @keywords internal
paint_format <- function(x,
                         sigfig = 3L,
                         max_chars = 12L,
                         max_dec_width = 13L,
                         subtle_digits = c("insignificant", "rounded", "none"),
                         ellipsis = "...",
                         ...) {
  UseMethod("paint_format")
}

#' Validate the arguments shared by every `paint_format()` method
#'
#' @inheritParams paint_format
#'
#' @return The matched `subtle_digits` value.
#'
#' @keywords internal
#' @noRd
check_format_args <- function(sigfig, max_chars, max_dec_width, subtle_digits) {
  if (length(sigfig) != 1L || is.na(sigfig) || sigfig < 1L || sigfig > 15L) {
    stop("`sigfig` must be a single number between 1 and 15.")
  }
  if (length(max_chars) != 1L || is.na(max_chars) || max_chars < 1L) {
    stop("`max_chars` must be a single positive number.")
  }
  if (length(max_dec_width) != 1L || is.na(max_dec_width) || max_dec_width < 1L) {
    stop("`max_dec_width` must be a single positive number.")
  }
  match.arg(subtle_digits, c("insignificant", "rounded", "none"))
}

#' Assemble the data frame every method returns
#'
#' @keywords internal
#' @noRd
format_frame <- function(sig, insig, head, tail, ink, align, tag, sci) {
  out <- data.frame(
    sig = as.character(sig),
    insig = as.character(insig),
    head = as.character(head),
    tail = as.character(tail),
    ink = as.character(ink),
    align = as.character(align),
    stringsAsFactors = FALSE
  )
  attr(out, "tag") <- tag
  attr(out, "sci") <- sci
  out
}

# ---------------------------------------------------------------------------
# numeric core
# ---------------------------------------------------------------------------

#' Format a numeric vector into spans
#'
#' The load-bearing arithmetic, shared by `paint_format.double()` and
#' `paint_format.integer()`.
#'
#' The raw value is what gets formatted. Formatting `signif(x, sigfig)` instead
#' would render `123456.789` as `"123000"`, and the grey digits would then be
#' fabricated zeros -- worse than the bug this replaces.
#'
#' @inheritParams paint_format
#' @param x A double vector.
#'
#' @return A list of the pieces `format_frame()` needs.
#'
#' @keywords internal
#' @noRd
format_num <- function(x, sigfig, max_dec_width, subtle_digits) {
  n <- length(x)
  x <- as.double(x)

  # -0 formats as "-0" but is the same number as 0; pillar shows "0".
  neg_zero <- is.finite(x) & x == 0
  x[neg_zero] <- 0

  fin <- is.finite(x)

  lhs <- lhs_digits(x)
  want <- pmax(0L, as.integer(sigfig) - lhs)
  have <- decs_to_exact(x)
  # Per element. A unit-wide max here would give c(1, 1e10) two decimals, a
  # 14-character token, and a wrong flip to scientific notation.
  d <- pmin(want, have)
  d[!fin] <- 0L

  tok <- rep(NA_character_, n)
  if (any(fin)) {
    tok[fin] <- formatC_each(x[fin], d[fin])
  }

  # A value rounded away by the fixed rendering keeps pillar's trailing "."
  # marker, so 123456.789 is "123457." and not a fake-exact "123457".
  rounded <- fin & d < have
  mark <- rounded & d == 0L
  tok[mark] <- paste0(tok[mark], ".")

  # One fixed-vs-scientific decision for the whole unit, made on width.
  sci <- FALSE
  if (any(fin)) {
    sci <- max(nchar(tok[fin])) > as.integer(max_dec_width)
  }
  if (sci) {
    tok[fin] <- formatC(x[fin], format = "e", digits = as.integer(sigfig) - 1L)
  }

  tok[is.na(x) & !is.nan(x)] <- "NA"
  tok[is.nan(x)] <- "NaN"
  inf <- !is.na(x) & is.infinite(x)
  tok[inf] <- ifelse(x[inf] > 0, "Inf", "-Inf")

  parts <- split_sig(tok, sigfig)
  sig <- parts$sig
  insig <- parts$insig

  if (identical(subtle_digits, "none")) {
    # `sig` must be restored to the whole token, not just its black half, or
    # paste0(sig, insig) silently stops being the rendered token.
    sig <- tok
    insig <- rep("", n)
  } else if (identical(subtle_digits, "rounded")) {
    # Grey only what the rendering actually lost. An exact value is all black.
    keep <- rounded
    sig[!keep] <- tok[!keep]
    insig[!keep] <- ""
  }

  # Hard rule. Every digit of "1.00e+15" is significant, exponent included, and
  # the naive split would grey "e+15".
  if (sci) {
    sig <- tok
    insig <- rep("", n)
  }

  dec <- split_dec(tok)

  ink <- rep("black", n)
  ink[is.nan(x) | (!is.na(x) & is.infinite(x))] <- "blue"
  ink[is.na(x) & !is.nan(x)] <- "red"

  list(
    sig = sig,
    insig = insig,
    head = dec$head,
    tail = dec$tail,
    ink = ink,
    align = rep("decimal", n),
    sci = sci
  )
}

# ---------------------------------------------------------------------------
# methods
# ---------------------------------------------------------------------------
#
# There is deliberately no `paint_format.vector()`. `inherits(1:3, "vector")` is
# FALSE, so such a method would never fire for an atomic vector -- that is the
# exact trap that makes `highlight_data(letters)` error today. The methods are
# fanned out explicitly instead.

#' @rdname paint_format
#' @export
paint_format.double <- function(x,
                                sigfig = 3L,
                                max_chars = 12L,
                                max_dec_width = 13L,
                                subtle_digits = c("insignificant", "rounded", "none"),
                                ellipsis = "...",
                                ...) {
  subtle_digits <- check_format_args(sigfig, max_chars, max_dec_width, subtle_digits)
  p <- format_num(x, sigfig, max_dec_width, subtle_digits)
  format_frame(p$sig, p$insig, p$head, p$tail, p$ink, p$align, "<dbl>", p$sci)
}

#' @rdname paint_format
#' @export
paint_format.integer <- function(x,
                                 sigfig = 3L,
                                 max_chars = 12L,
                                 max_dec_width = 13L,
                                 subtle_digits = c("insignificant", "rounded", "none"),
                                 ellipsis = "...",
                                 ...) {
  subtle_digits <- check_format_args(sigfig, max_chars, max_dec_width, subtle_digits)
  # An integer is always exact, so it never rounds, never grows a "." marker and
  # never gets wide enough to flip to scientific notation.
  p <- format_num(x, sigfig, max_dec_width, subtle_digits)
  format_frame(p$sig, p$insig, p$head, p$tail, p$ink, p$align, "<int>", p$sci)
}

#' @rdname paint_format
#' @export
paint_format.character <- function(x,
                                   sigfig = 3L,
                                   max_chars = 12L,
                                   max_dec_width = 13L,
                                   subtle_digits = c("insignificant", "rounded", "none"),
                                   ellipsis = "...",
                                   ...) {
  check_format_args(sigfig, max_chars, max_dec_width, subtle_digits)
  n <- length(x)
  na <- is.na(x)
  tok <- truncate_chr(as.character(x), max_chars, ellipsis)
  tok[na] <- "NA"
  ink <- rep("black", n)
  ink[na] <- "red"
  format_frame(tok, rep("", n), tok, rep("", n), ink, rep("left", n), "<chr>", FALSE)
}

#' @rdname paint_format
#' @export
paint_format.logical <- function(x,
                                 sigfig = 3L,
                                 max_chars = 12L,
                                 max_dec_width = 13L,
                                 subtle_digits = c("insignificant", "rounded", "none"),
                                 ellipsis = "...",
                                 ...) {
  check_format_args(sigfig, max_chars, max_dec_width, subtle_digits)
  n <- length(x)
  na <- is.na(x)
  tok <- rep("NA", n)
  tok[!na & x] <- "TRUE"
  tok[!na & !x] <- "FALSE"
  ink <- rep("black", n)
  ink[na] <- "red"
  format_frame(tok, rep("", n), tok, rep("", n), ink, rep("right", n), "<lgl>", FALSE)
}

#' @rdname paint_format
#' @export
paint_format.factor <- function(x,
                                sigfig = 3L,
                                max_chars = 12L,
                                max_dec_width = 13L,
                                subtle_digits = c("insignificant", "rounded", "none"),
                                ellipsis = "...",
                                ...) {
  check_format_args(sigfig, max_chars, max_dec_width, subtle_digits)
  n <- length(x)
  na <- is.na(x)
  tok <- truncate_chr(as.character(x), max_chars, ellipsis)
  tok[na] <- "NA"
  ink <- rep("black", n)
  ink[na] <- "red"
  format_frame(tok, rep("", n), tok, rep("", n), ink, rep("left", n), "<fct>", FALSE)
}

#' What one element of a list IS: its type and its size
#'
#' The token an element gets when it is not drawn as cells: `<int [3]>`,
#' `<chr [1]>`, `<dbl [2 x 2]>`, `<df [5 x 3]>`, `<list [2]>`. It is a
#' DESCRIPTION, not a value, which is why it is drawn grey.
#'
#' It replaces a constant. `paint_format.list()` used to ignore its own `x` and
#' stamp the single string `"<list>"` over every element of a list column, so a
#' frame with a list column said `<list>` five times and the reader learned
#' nothing at all -- not the type of what was in there, not even that the five
#' entries differed. `print()` on a tibble has said `<int [3]>` for years.
#'
#' The separator is an ASCII `"x"`, and it must stay ASCII: `pdf()` -- the device
#' `R CMD check` draws on -- cannot encode U+00D7.
#'
#' @param x One element of a list. `NULL` is a legal element and gets `<NULL>`.
#'
#' @return A length-one character string.
#'
#' @keywords internal
#' @noRd
elem_sum <- function(x) {
  if (is.null(x)) {
    return("<NULL>")
  }
  tag <- elem_type(x)
  inner <- substr(tag, 2L, nchar(tag) - 1L)
  d <- if (is.data.frame(x)) c(nrow(x), ncol(x)) else dim(x)
  size <- if (is.null(d)) as.character(length(x)) else paste(d, collapse = " x ")
  paste0("<", inner, " [", size, "]>")
}

#' The type half of [elem_sum()], on its own
#'
#' The TYPE LANE OF A SUMMARISED ELEMENT CANNOT BE READ OFF `paint_format()`.
#' Summarising an element means calling `paint_format(data[k])` -- a length-one
#' LIST -- which dispatches to `paint_format.list()`, whose `tag` is `"<list>"`,
#' because the thing it was handed genuinely is a list. So the type lane would
#' read `<list>` under a cell reading `<int [2 x 2]>`, disagreeing with the cell
#' directly above it. The tag has to come from the ELEMENT, and this is where it
#' comes from.
#'
#' @param x One element of a list.
#'
#' @return A length-one character string.
#'
#' @keywords internal
#' @noRd
elem_type <- function(x) {
  if (is.null(x)) {
    return("<NULL>")
  }
  type_tag(x)
}

#' @rdname paint_format
#' @export
paint_format.list <- function(x,
                              sigfig = 3L,
                              max_chars = 12L,
                              max_dec_width = 13L,
                              subtle_digits = c("insignificant", "rounded", "none"),
                              ellipsis = "...",
                              ...) {
  check_format_args(sigfig, max_chars, max_dec_width, subtle_digits)
  n <- length(x)
  # format() renders a list as the deparsed contents of every element, which
  # overruns the cell and is unreadable. A description is the honest answer; it is
  # drawn grey because it describes the element rather than showing it.
  #
  # PER ELEMENT, not one constant for the whole unit. The token is deliberately
  # NOT truncated at `max_chars`: half of `<dbl [2 x 2]>` is not a description of
  # anything, and unlike a value this string's width is bounded by the structure
  # rather than by the data.
  tok <- vapply(x, elem_sum, character(1), USE.NAMES = FALSE)
  format_frame(tok, rep("", n), tok, rep("", n), rep("grey50", n), rep("left", n), "<list>", FALSE)
}

#' @rdname paint_format
#' @export
paint_format.default <- function(x,
                                 sigfig = 3L,
                                 max_chars = 12L,
                                 max_dec_width = 13L,
                                 subtle_digits = c("insignificant", "rounded", "none"),
                                 ellipsis = "...",
                                 ...) {
  check_format_args(sigfig, max_chars, max_dec_width, subtle_digits)
  n <- length(x)
  na <- is.na(x)
  # Date, POSIXct, difftime, complex: format() is already S3 and already right.
  # It pads to a common width, which we do not want, so trim.
  tok <- trimws(format(x))
  tok <- truncate_chr(tok, max_chars, ellipsis)
  tok[na] <- "NA"
  ink <- rep("black", n)
  ink[na] <- "red"
  format_frame(tok, rep("", n), tok, rep("", n), ink, rep("left", n), type_tag(x), FALSE)
}
