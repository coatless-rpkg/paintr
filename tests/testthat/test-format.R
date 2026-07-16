# Tier 1 tests: pure formatting. No graphics device is opened anywhere in this
# file, and no dependency beyond testthat is used.
#
# The expected tokens below were verified byte-for-byte against pillar 1.11.1
# with:
#   strip <- function(s) gsub("\033\\[[0-9;]*m", "", s)
#   as.character(trimws(strip(as.character(
#     format(pillar::pillar_shaft(x), width = 30)))))
# They are hard-coded here on purpose: paintr must not depend on pillar, not even
# in Suggests, so the parity is frozen as literals rather than recomputed.

tok <- function(x, ...) {
  f <- paint_format(x, ...)
  paste0(f$sig, f$insig)
}

# ---------------------------------------------------------------------------
# BUG 1 -- character data rendered as "Unknown"
# ---------------------------------------------------------------------------

test_that("bug 1: character values format as themselves, never as 'Unknown'", {
  # The old is.finite()/is.na()/is.nan() ifelse chain was a partial function
  # pretending to be total: a string is none of those, so every character cell
  # fell off the end of the chain and came out as a red "Unknown".
  f <- paint_format(c("alpha", "beta", "gamma"))

  expect_equal(f$sig, c("alpha", "beta", "gamma"))
  expect_false(any(grepl("Unknown", unlist(f), fixed = TRUE)))
  expect_equal(f$ink, rep("black", 3))
  expect_equal(f$align, rep("left", 3))
  expect_equal(attr(f, "tag"), "<chr>")
})

test_that("bug 1: every atomic type produces a token, and none says 'Unknown'", {
  # The root cause was a chain of type tests with no total fallback. Sweep the
  # whole type space and assert totality.
  inputs <- list(
    dbl = c(1.5, 2.5),
    int = 1:3,
    chr = letters[1:3],
    lgl = c(TRUE, FALSE, NA),
    fct = factor(c("a", "b")),
    date = as.Date(c("2024-01-01", "2024-06-30")),
    cpl = complex(real = 1, imaginary = 1),
    lst = list(1:3, letters),
    empty_chr = character(0),
    na_chr = c("a", NA)
  )

  for (nm in names(inputs)) {
    f <- paint_format(inputs[[nm]])
    expect_s3_class(f, "data.frame")
    expect_equal(nrow(f), length(inputs[[nm]]), info = nm)
    expect_false(any(grepl("Unknown", unlist(f), fixed = TRUE)), info = nm)
    expect_false(any(is.na(f$sig)), info = nm)
  }
})

test_that("bug 1: there is no paint_format.vector() method", {
  # inherits(1:3, "vector") is FALSE, so a .vector method would never fire for an
  # atomic vector. That trap is what makes highlight_data(letters) error today.
  # The methods must be fanned out explicitly instead.
  expect_false(inherits(1:3, "vector"))
  expect_false(inherits(letters, "vector"))
  expect_null(getS3method("paint_format", "vector", optional = TRUE))

  for (cls in c("double", "integer", "character", "logical", "factor", "default")) {
    expect_true(
      is.function(getS3method("paint_format", cls, optional = TRUE)),
      info = cls
    )
  }
})

# ---------------------------------------------------------------------------
# BUG 2 -- numbers rendered at 15 significant digits
# ---------------------------------------------------------------------------

test_that("bug 2: numbers do not render at 15 significant digits", {
  # as.character(1/3) is "0.333333333333333" and as.character(pi) is
  # "3.14159265358979". Both overran the cell and disagreed with print(x).
  expect_equal(tok(1 / 3), "0.333")
  expect_equal(tok(pi), "3.14")

  expect_false(identical(tok(1 / 3), as.character(1 / 3)))
  expect_false(identical(tok(pi), as.character(pi)))

  # Three significant digits by default, and sigfig is honoured when raised.
  expect_equal(tok(pi, sigfig = 5L), "3.1416")
  expect_equal(tok(1 / 3, sigfig = 1L), "0.3")
})

test_that("bug 2: no token overruns its budget", {
  # The old renderer emitted 15-digit strings into a fixed-size cell. Fixed
  # notation is bounded by max_dec_width, scientific by its own shape.
  x <- c(1 / 3, pi, 123456.789, 1e10, 0.0999)
  expect_true(all(nchar(tok(x)) <= 13L))

  # And the whole unit flips rather than emitting one monster token.
  expect_true(all(nchar(tok(c(1, 1e15))) <= 13L))
  expect_true(all(nchar(tok(.Machine$double.xmax)) <= 13L))
})

# ---------------------------------------------------------------------------
# pillar parity battery
# ---------------------------------------------------------------------------

test_that("numeric tokens are byte-identical to pillar 1.11.1 (fixed notation)", {
  expect_equal(tok(c(10, 200, -30)), c("10", "200", "-30"))
  expect_equal(tok(c(1, 1.5)), c("1", "1.5"))
  expect_equal(tok(c(1, 1 / 3)), c("1", "0.333"))
  expect_equal(tok(c(0.0999, 1)), c("0.0999", "1"))
  expect_equal(tok(123456.789), "123457.")
  expect_equal(tok(c(123456.789, 1)), c("123457.", "1"))
  expect_equal(tok(c(1, 1e10)), c("1", "10000000000"))
  expect_equal(tok(1.5e-8), "0.000000015")
  expect_equal(tok(200.5), "200.")
  expect_equal(tok(pi * 10^(0:3)), c("3.14", "31.4", "314.", "3142."))
  expect_equal(tok(c(NA_real_, NA_real_)), c("NA", "NA"))
  expect_equal(tok(c(NA, NaN, Inf, -Inf)), c("NA", "NaN", "Inf", "-Inf"))
  expect_equal(tok(0), "0")
  expect_equal(tok(-0.0), "0")
})

test_that("scientific notation is the one deliberate deviation from pillar", {
  # pillar renders these as "1e 0"/"1e15" and "1.80e308" -- a terminal
  # column-alignment hack with no decimal point and no sign. We render plain
  # ASCII scientific, because pdf() cannot encode the alternatives and because a
  # graphics device has no columns to align.
  expect_equal(tok(c(1, 1e15)), c("1.00e+00", "1.00e+15"))
  expect_equal(tok(.Machine$double.xmax), "1.80e+308")

  # ASCII only: no multiplication sign, no superscripts, no Unicode minus.
  all_tok <- c(tok(c(1, 1e15)), tok(.Machine$double.xmax), tok(1.5e-8))
  expect_false(any(grepl("[^ -~]", all_tok)))
})

test_that("more pillar parity, verified against pillar 1.11.1", {
  expect_equal(tok(9.999), "10.00")
  expect_equal(tok(0.9999), "1.000")
  expect_equal(tok(99.99), "100.0")
  expect_equal(tok(999.6), "1000.")
  expect_equal(tok(1e-5), "0.00001")
  expect_equal(tok(0.1 + 0.2), "0.3") # not "0.300": fp noise is not precision
  expect_equal(tok(1 / 7), "0.143")
  expect_equal(tok(2 / 3), "0.667")
  expect_equal(tok(1234567890123), "1234567890123")
  expect_equal(tok(0.00012345), "0.000123")
  expect_equal(tok(100000), "100000")
  expect_equal(tok(123456789L), "123456789")
  expect_equal(tok(letters[1:3]), c("a", "b", "c"))
  expect_equal(tok(factor(c("a", "b"))), c("a", "b"))
  expect_equal(tok(as.Date("2026-07-12")), "2026-07-12")
  expect_equal(tok(complex(real = 1, imaginary = 1)), "1+1i")
})

# ---------------------------------------------------------------------------
# the fixed-vs-scientific decision
# ---------------------------------------------------------------------------

test_that("fixed vs scientific is one decision per unit, made on width", {
  # 13 characters fixed: stays fixed. 14: the whole unit flips.
  expect_false(attr(paint_format(1234567890123), "sci")) # 13 chars
  expect_true(attr(paint_format(12345678901234), "sci")) # 14 chars

  expect_false(attr(paint_format(c(1, 1e10)), "sci")) # "10000000000", 11
  expect_true(attr(paint_format(c(1, 1e13)), "sci")) # "10000000000000", 14
  expect_true(attr(paint_format(c(1, 1e15)), "sci"))

  # The decision is unit-wide: one outlier takes every element with it.
  expect_equal(tok(c(1, 1e15)), c("1.00e+00", "1.00e+15"))

  # And it is a width threshold, so it moves when max_dec_width moves.
  expect_true(attr(paint_format(c(1, 1e10), max_dec_width = 10L), "sci"))
  expect_false(attr(paint_format(12345678901234, max_dec_width = 20L), "sci"))
})

test_that("pmin(want, have) keeps c(1, 1e10) out of scientific notation", {
  # want for 1 is 2 decimals; have is 0. Without the per-element pmin(), 1e10
  # would be formatted with 2 decimals -> "10000000000.00" -> 14 chars -> the
  # unit would wrongly flip to scientific.
  x <- c(1, 1e10)
  lhs <- c(1L, 11L)
  want <- pmax(0L, 3L - lhs)
  have <- decs_to_exact(x)

  expect_equal(want, c(2L, 0L))
  expect_equal(have, c(0L, 0L))
  expect_equal(pmin(want, have), c(0L, 0L))

  expect_false(attr(paint_format(x), "sci"))
  expect_equal(tok(x), c("1", "10000000000"))
})

test_that("the raw value is formatted, never signif(x, sigfig) first", {
  # signif(123456.789, 3) is 123000. Formatting that would render "123000" and
  # the grey "insignificant" digits would be fabricated zeros -- a lie, and worse
  # than the bug it replaces.
  f <- paint_format(123456.789)
  expect_equal(paste0(f$sig, f$insig), "123457.")
  expect_equal(f$insig, "457.")
  expect_false(grepl("000", paste0(f$sig, f$insig), fixed = TRUE))
})

test_that("all-NA and all-non-finite units do not flip and do not warn", {
  # max(nchar(tok[is.finite(x)])) is max(integer(0)) == -Inf plus a warning if
  # the finite subset is not guarded.
  expect_silent(f <- paint_format(c(NA_real_, NA_real_)))
  expect_false(attr(f, "sci"))
  expect_equal(f$ink, c("red", "red"))

  expect_silent(g <- paint_format(c(NA, NaN, Inf, -Inf)))
  expect_false(attr(g, "sci"))
  expect_equal(g$ink, c("red", "blue", "blue", "blue"))
})

# ---------------------------------------------------------------------------
# the sig / insig split
# ---------------------------------------------------------------------------

test_that("split_sig() cuts after the sigfig-th significant digit", {
  expect_equal(split_sig("123457.", 3L), list(sig = "123", insig = "457."))
  expect_equal(split_sig("10000000000", 3L), list(sig = "100", insig = "00000000"))
  expect_equal(split_sig("200.", 3L), list(sig = "200", insig = "."))
  expect_equal(split_sig("100000", 3L), list(sig = "100", insig = "000"))
  expect_equal(split_sig("3142.", 3L), list(sig = "314", insig = "2."))

  # Leading zeros are not significant.
  expect_equal(split_sig("0.0999", 3L), list(sig = "0.0999", insig = ""))
  expect_equal(split_sig("0.333", 3L), list(sig = "0.333", insig = ""))

  # A minus sign is not a significant digit.
  expect_equal(split_sig("-30", 3L), list(sig = "-30", insig = ""))
  expect_equal(split_sig("-123457.", 3L), list(sig = "-123", insig = "457."))

  # Fewer than sigfig significant digits: nothing is insignificant.
  expect_equal(split_sig("1.5", 3L), list(sig = "1.5", insig = ""))
  expect_equal(split_sig("1", 3L), list(sig = "1", insig = ""))

  # No digits at all.
  expect_equal(split_sig(c("NA", "NaN", "Inf", "-Inf"), 3L)$insig, rep("", 4))
})

test_that("paste0(sig, insig) is exactly the rendered token, always", {
  units <- list(
    c(10, 200, -30), c(1, 1.5), c(1, 1 / 3), c(0.0999, 1), 123456.789,
    c(1, 1e10), c(1, 1e15), 1.5e-8, 200.5, pi * 10^(0:3), 0, -0.0,
    .Machine$double.xmax, c(NA, NaN, Inf, -Inf), 1:5, 123456789L,
    letters, c(TRUE, FALSE, NA), factor(c("a", "b")), Sys.Date(),
    complex(real = 1, imaginary = 1), list(1, 2)
  )
  for (subtle in c("insignificant", "rounded", "none")) {
    for (u in units) {
      f <- paint_format(u, subtle_digits = subtle)
      joined <- paste0(f$sig, f$insig)
      expect_equal(length(joined), length(u))
      expect_false(any(is.na(joined)))
      # head/tail is the same token cut at the decimal point.
      expect_equal(paste0(f$head, f$tail), joined)
    }
  }
})

test_that("scientific mode implies insig == '' -- the hard rule", {
  # "1.00e+15" reaches three significant digits inside the mantissa, so a naive
  # split would grey the exponent. Every digit of a scientific token, exponent
  # included, is significant.
  for (subtle in c("insignificant", "rounded", "none")) {
    f <- paint_format(c(1, 1e15), subtle_digits = subtle)
    expect_true(attr(f, "sci"))
    expect_equal(f$insig, c("", ""))
    expect_equal(f$sig, c("1.00e+00", "1.00e+15"))

    g <- paint_format(.Machine$double.xmax, subtle_digits = subtle)
    expect_true(attr(g, "sci"))
    expect_equal(g$insig, "")
  }

  # The naive split really would have greyed the exponent -- proving the rule is
  # load-bearing and not decoration.
  expect_equal(split_sig("1.00e+15", 3L), list(sig = "1.00", insig = "e+15"))
})

test_that("subtle_digits = 'insignificant' is pillar's behaviour (the default)", {
  f <- paint_format(100000)
  expect_equal(f$sig, "100")
  expect_equal(f$insig, "000")

  f <- paint_format(1e10)
  expect_equal(f$sig, "100")
  expect_equal(f$insig, "00000000")

  f <- paint_format(123456.789)
  expect_equal(f$sig, "123")
  expect_equal(f$insig, "457.")

  # It is the default.
  expect_equal(paint_format(100000), paint_format(100000, subtle_digits = "insignificant"))
})

test_that("subtle_digits = 'rounded' greys only genuinely lost precision", {
  # 100000 is exact: it stays fully black.
  f <- paint_format(100000, subtle_digits = "rounded")
  expect_equal(f$sig, "100000")
  expect_equal(f$insig, "")

  f <- paint_format(1e10, subtle_digits = "rounded")
  expect_equal(f$sig, "10000000000")
  expect_equal(f$insig, "")

  # 123456.789 was rounded to "123457." -- it still greys "457.".
  f <- paint_format(123456.789, subtle_digits = "rounded")
  expect_equal(f$sig, "123")
  expect_equal(f$insig, "457.")

  # 200.5 was rounded to "200." -- the marker is grey.
  f <- paint_format(200.5, subtle_digits = "rounded")
  expect_equal(f$sig, "200")
  expect_equal(f$insig, ".")

  # Integers are always exact, so they are always all black in this mode.
  f <- paint_format(123456789L, subtle_digits = "rounded")
  expect_equal(f$sig, "123456789")
  expect_equal(f$insig, "")

  # Mixed unit: the exact element is black, the rounded one is not.
  f <- paint_format(c(100000, 123456.789), subtle_digits = "rounded")
  expect_equal(f$sig, c("100000", "123"))
  expect_equal(f$insig, c("", "457."))
})

test_that("subtle_digits = 'none' always yields an empty insig", {
  for (x in list(100000, 1e10, 123456.789, c(1, 1e15), 1:3, 123456789L)) {
    f <- paint_format(x, subtle_digits = "none")
    expect_equal(f$insig, rep("", length(x)))
    expect_equal(f$sig, paste0(
      paint_format(x)$sig, paint_format(x)$insig
    ))
  }
})

# ---------------------------------------------------------------------------
# head / tail, for decimal anchoring
# ---------------------------------------------------------------------------

test_that("head and tail split the token at the decimal point", {
  f <- paint_format(c(123456.789, 1, 1.5, -30))
  expect_equal(f$head, c("123457", "1", "1", "-30"))
  expect_equal(f$tail, c(".", "", ".5", ""))
  expect_equal(paste0(f$head, f$tail), c("123457.", "1", "1.5", "-30"))

  # The minus sign travels with the head, so decimals still line up.
  f <- paint_format(c(-1.25, 10.5))
  expect_equal(f$head, c("-1", "10"))
  expect_equal(f$tail, c(".25", ".5"))
})

# ---------------------------------------------------------------------------
# non-numeric types
# ---------------------------------------------------------------------------

test_that("character columns truncate with an ASCII ellipsis", {
  f <- paint_format("abcdefghijklmnopqrstuvwxyz", max_chars = 12L)
  expect_equal(f$sig, "abcdefghi...")
  expect_equal(nchar(f$sig), 12L)
  expect_equal(f$insig, "")

  # Not one character over the budget.
  x <- c("abcdefghijkl", "abcdefghijklm")
  f <- paint_format(x, max_chars = 12L)
  expect_equal(f$sig, c("abcdefghijkl", "abcdefghi..."))
  expect_true(all(nchar(f$sig) <= 12L))

  # ASCII, so pdf() can encode it. U+2026 is opt-in only.
  expect_false(any(grepl("[^ -~]", f$sig)))
  expect_equal(
    paint_format("abcdefghijklmnop", max_chars = 8L, ellipsis = "\u2026")$sig,
    "abcdefg\u2026"
  )

  expect_error(
    paint_format("abcdefghij", max_chars = 3L, ellipsis = "..."),
    "wider than"
  )
})

test_that("truncate_chr() leaves short strings and NA alone", {
  expect_equal(truncate_chr(c("ab", NA, "abcdefghijklmno"), 12L), c("ab", NA, "abcdefghi..."))
  expect_equal(truncate_chr(character(0), 12L), character(0))
  expect_equal(truncate_chr("", 12L), "")
})

test_that("a control character never survives into a drawn token", {
  # A newline, a tab and a carriage return each become a single space, so the
  # renderer is never handed a value that would spill onto a second line.
  f <- paint_format(c("a", "e\nf", "x\ty", "u\rv"))
  expect_equal(f$sig, c("a", "e f", "x y", "u v"))
  expect_false(any(grepl("[[:cntrl:]]", f$sig)))
  expect_false(any(grepl("[[:cntrl:]]", paste0(f$sig, f$insig))))
  # ASCII, so pdf() can encode it: the replacement is a plain space.
  expect_false(any(grepl("[^ -~]", f$sig)))
})

test_that("a value is escaped BEFORE it is truncated to max_chars", {
  # Eight letters joined by newlines is 15 characters. Escaping turns each
  # newline into a space, and it is that 15-character escaped string -- not the
  # raw one -- that is cut to 12.
  x <- paste(letters[1:8], collapse = "\n")
  f <- paint_format(x, max_chars = 12L)
  expect_equal(f$sig, "a b c d e...")
  expect_equal(nchar(f$sig), 12L)
  expect_false(grepl("[[:cntrl:]]", f$sig))
})

test_that("a factor with a control character in a level is clean", {
  f <- paint_format(factor(c("p\nq", "z")))
  expect_equal(f$sig, c("p q", "z"))
  expect_false(any(grepl("[[:cntrl:]]", f$sig)))
})

test_that("escaping a control character leaves an ordinary string untouched", {
  # The regression guard: only control characters change. "apple" stays "apple".
  plain <- c("apple", "banana cream", "x-1_2.3")
  f <- paint_format(plain)
  expect_equal(f$sig, plain)
  expect_equal(truncate_chr(plain, 20L), plain)
})

test_that("logical columns render TRUE/FALSE/NA and align right", {
  f <- paint_format(c(TRUE, FALSE, NA))
  expect_equal(f$sig, c("TRUE", "FALSE", "NA"))
  expect_equal(f$insig, rep("", 3))
  expect_equal(f$ink, c("black", "black", "red"))
  expect_equal(f$align, rep("right", 3))
  expect_equal(attr(f, "tag"), "<lgl>")
  expect_false(attr(f, "sci"))
})

test_that("integer columns are decimal-aligned and never flip to scientific", {
  f <- paint_format(c(1L, 123456789L))
  expect_equal(paste0(f$sig, f$insig), c("1", "123456789"))
  expect_equal(f$align, rep("decimal", 2))
  expect_equal(attr(f, "tag"), "<int>")
  expect_false(attr(f, "sci"))

  # The widest possible integer is 11 characters, under the 13 threshold.
  f <- paint_format(c(.Machine$integer.max, -.Machine$integer.max))
  expect_false(attr(f, "sci"))
  expect_equal(paste0(f$sig, f$insig), c("2147483647", "-2147483647"))

  # Integers do get the insignificant-digit split (pillar does this too).
  expect_equal(paint_format(123456789L)$sig, "123")
  expect_equal(paint_format(123456789L)$insig, "456789")

  expect_equal(paint_format(c(1L, NA_integer_))$ink, c("black", "red"))
})

test_that("factor columns render their labels and align left", {
  f <- paint_format(factor(c("a", "b", NA), levels = c("a", "b")))
  expect_equal(f$sig, c("a", "b", "NA"))
  expect_equal(f$ink, c("black", "black", "red"))
  expect_equal(f$align, rep("left", 3))
  expect_equal(attr(f, "tag"), "<fct>")

  # A factor's labels, not its integer codes -- the classic silent bug.
  f <- paint_format(factor(c("z", "y")))
  expect_equal(f$sig, c("z", "y"))
  expect_false(any(f$sig %in% c("1", "2")))

  # Long labels truncate like any other string.
  expect_equal(
    paint_format(factor("supercalifragilistic"), max_chars = 8L)$sig,
    "super..."
  )
})

test_that("the default method handles Date, POSIXct, difftime and complex", {
  f <- paint_format(as.Date(c("2024-01-01", "2024-06-30")))
  expect_equal(f$sig, c("2024-01-01", "2024-06-30"))
  expect_equal(attr(f, "tag"), "<date>")
  expect_equal(f$align, rep("left", 2))

  f <- paint_format(complex(real = c(1, 2), imaginary = c(1, -3)))
  expect_equal(f$sig, c("1+1i", "2-3i"))
  expect_equal(attr(f, "tag"), "<cpl>")

  # format() pads to a common width; the padding must be trimmed off.
  f <- paint_format(complex(real = c(1, 100), imaginary = c(1, 1)))
  expect_false(any(grepl("^ ", f$sig)))

  expect_equal(attr(paint_format(Sys.time()), "tag"), "<dttm>")
  expect_equal(attr(paint_format(as.difftime(3, units = "days")), "tag"), "<drtn>")

  expect_equal(paint_format(as.Date(c("2024-01-01", NA)))$ink, c("black", "red"))
})

test_that("a list is described element by element, in grey", {
  # It used to ignore its own `x` and stamp the constant "<list>" over every
  # element, so a frame with a list column said `<list>` five times and the reader
  # learned nothing -- not the type of what was in there, not even that the entries
  # differed. print() on a tibble has said `<int [3]>` for years.
  f <- paint_format(list(1:3, letters, NULL))
  expect_equal(f$sig, c("<int [3]>", "<chr [26]>", "<NULL>"))
  expect_equal(f$insig, rep("", 3))
  expect_equal(f$ink, rep("grey50", 3))
  # The UNIT is still a list, and its tag still says so.
  expect_equal(attr(f, "tag"), "<list>")
  expect_equal(nrow(f), 3L)

  # paste0(sig, insig) is the drawn token, as it is for every other method.
  expect_equal(paste0(f$sig, f$insig), f$sig)
  expect_equal(nrow(paint_format(list())), 0L)
})

test_that("elem_sum() describes a shape as a shape, in ASCII", {
  expect_equal(elem_sum(1:3), "<int [3]>")
  expect_equal(elem_sum("a"), "<chr [1]>")
  expect_equal(elem_sum(matrix(1:4, 2)), "<int [2 x 2]>")
  expect_equal(elem_sum(head(iris, 5)), "<df [5 x 5]>")
  expect_equal(elem_sum(list(1, 2)), "<list [2]>")
  expect_equal(elem_sum(NULL), "<NULL>")
  expect_equal(elem_sum(integer(0)), "<int [0]>")
  expect_equal(elem_sum(factor("a")), "<fct [1]>")
  expect_equal(elem_sum(array(1:24, c(2, 3, 4))), "<int [2 x 3 x 4]>")

  # ASCII "x", never U+00D7: pdf(), the device R CMD check draws on, cannot encode
  # it. Checked on the bytes, not on the glyph.
  for (s in c(elem_sum(matrix(1:4, 2)), elem_sum(head(iris, 5)))) {
    expect_true(all(charToRaw(s) < as.raw(128)), info = s)
  }

  # The TYPE half, on its own: it is what a summarised element's type lane reads,
  # and it must not be read off paint_format(), whose tag is "<list>".
  expect_equal(elem_type(matrix(1:4, 2)), "<int>")
  expect_equal(elem_type(NULL), "<NULL>")
  expect_equal(attr(paint_format(list(matrix(1:4, 2))), "tag"), "<list>")
})

test_that("type_tag() names every type paintr can paint", {
  expect_equal(type_tag(1.5), "<dbl>")
  expect_equal(type_tag(1L), "<int>")
  expect_equal(type_tag("a"), "<chr>")
  expect_equal(type_tag(TRUE), "<lgl>")
  expect_equal(type_tag(factor("a")), "<fct>")
  expect_equal(type_tag(complex(1)), "<cpl>")
  expect_equal(type_tag(Sys.Date()), "<date>")
  expect_equal(type_tag(Sys.time()), "<dttm>")
  # A POSIXlt is a datetime, and it must be asked BEFORE `is.list()`, which is TRUE
  # of it and would otherwise call it a `<list>`. It reaches here only as a thing
  # being named -- summarised as `<dttm [1]>` -- never expanded into its eleven
  # fields, which the type gate refuses.
  expect_equal(type_tag(as.POSIXlt(Sys.time())), "<dttm>")
  expect_equal(elem_sum(as.POSIXlt(Sys.time())), "<dttm [1]>")
  expect_equal(type_tag(as.difftime(1, units = "days")), "<drtn>")
  expect_equal(type_tag(list()), "<list>")
  expect_equal(type_tag(data.frame(a = 1)), "<df>")
})

# ---------------------------------------------------------------------------
# structure of the return value
# ---------------------------------------------------------------------------

test_that("paint_format() returns the contracted shape", {
  f <- paint_format(c(1, 1 / 3))
  expect_s3_class(f, "data.frame")
  expect_equal(names(f), c("sig", "insig", "head", "tail", "ink", "align"))
  expect_true(all(vapply(f, is.character, logical(1))))
  expect_equal(nrow(f), 2L)
  expect_type(attr(f, "tag"), "character")
  expect_type(attr(f, "sci"), "logical")
  expect_length(attr(f, "sci"), 1L)
})

test_that("zero-length input returns a zero-row frame, not an error", {
  for (x in list(double(0), integer(0), character(0), logical(0), factor(character(0)))) {
    f <- paint_format(x)
    expect_equal(nrow(f), 0L)
    expect_equal(names(f), c("sig", "insig", "head", "tail", "ink", "align"))
    expect_false(attr(f, "sci"))
  }
})

test_that("ink is red for NA, blue for Inf and NaN, black for finite", {
  f <- paint_format(c(1, NA, NaN, Inf, -Inf))
  expect_equal(f$ink, c("black", "red", "blue", "blue", "blue"))
  # is.na(NaN) is TRUE, so NaN must be tested before NA or it comes out red.
  expect_true(is.na(NaN))
  expect_equal(paint_format(NaN)$ink, "blue")
  expect_equal(paint_format(NA_real_)$ink, "red")
})

# ---------------------------------------------------------------------------
# internals
# ---------------------------------------------------------------------------

test_that("decs_to_exact() counts the decimals a value actually needs", {
  expect_equal(decs_to_exact(c(1, 1.5, 1 / 3)), c(0L, 1L, 15L))
  expect_equal(decs_to_exact(c(1e10, 100000, 0)), c(0L, 0L, 0L))
  expect_equal(decs_to_exact(123456.789), 3L)
  expect_equal(decs_to_exact(200.5), 1L)
  expect_equal(decs_to_exact(1.5e-8), 9L)
  expect_equal(decs_to_exact(0.0999), 4L)
  # Floating point noise is not precision: 0.1 + 0.2 needs one decimal, not 17.
  expect_equal(decs_to_exact(0.1 + 0.2), 1L)
  # Non-finite values report 0 and do not error.
  expect_equal(decs_to_exact(c(NA, NaN, Inf, -Inf)), c(0L, 0L, 0L, 0L))
  expect_equal(decs_to_exact(double(0)), integer(0))
})

test_that("lhs_digits() survives zero and the extremes", {
  expect_equal(lhs_digits(c(0, -0.0)), c(1L, 1L)) # log10(0) is -Inf
  expect_equal(lhs_digits(c(1, 10, 100, 1000)), c(1L, 2L, 3L, 4L))
  expect_equal(lhs_digits(c(0.1, 0.0999, 1e-5)), c(0L, -1L, -4L))
  expect_equal(lhs_digits(c(-30, -1e10)), c(2L, 11L))
  expect_equal(lhs_digits(.Machine$double.xmax), 309L)
  expect_equal(lhs_digits(c(NA, Inf)), c(1L, 1L))
})

test_that("formatC() does not vectorise over digits -- formatC_each() must", {
  # This is a real trap: formatC(x, digits = c(0, 3)) errors with
  # "the condition has length > 1".
  expect_error(formatC(c(1, 1 / 3), format = "f", digits = c(0L, 3L)))
  expect_equal(
    formatC_each(c(1, 1 / 3, 123456.789), c(0L, 3L, 0L)),
    c("1", "0.333", "123457")
  )
  expect_equal(formatC_each(double(0), integer(0)), character(0))
})

test_that("sigfig and friends are validated", {
  expect_error(paint_format(1, sigfig = 0L), "between 1 and 15")
  expect_error(paint_format(1, sigfig = 16L), "between 1 and 15")
  expect_error(paint_format(1, sigfig = c(2L, 3L)), "between 1 and 15")
  expect_error(paint_format(1, sigfig = NA_integer_), "between 1 and 15")
  expect_error(paint_format(1, max_chars = 0L), "positive")
  expect_error(paint_format(1, max_dec_width = 0L), "positive")
  expect_error(paint_format(1, subtle_digits = "grey"))

  # Errors are plain sentences: no cli, no rlang, no braces left unglued.
  msg <- tryCatch(paint_format(1, sigfig = 99L), error = conditionMessage)
  expect_false(grepl("\\{|\\}", msg))
})

test_that("sigfig is honoured end to end", {
  expect_equal(tok(123456.789, sigfig = 6L), "123457.")
  # All six significant digits are black; only the rounding marker is grey. This
  # is the same rule as the spec's "200." -> "200" | "." -- a lone trailing "."
  # is part of the insignificant span.
  expect_equal(paint_format(123456.789, sigfig = 6L)$sig, "123457")
  expect_equal(paint_format(123456.789, sigfig = 6L)$insig, ".")
  expect_equal(tok(1 / 3, sigfig = 5L), "0.33333")
  expect_equal(paint_format(100000, sigfig = 1L)$insig, "00000")
  expect_equal(paint_format(100000, sigfig = 1L)$sig, "1")
})
