# Tier 1: the cell table. Pure. No graphics, no device, no dependency.
#
# This file is where Bug 4 (data frames unsupported) dies -- not as a third code
# path, but as a data value. `fmt_group` is `rep(1L, n_col)` for a matrix or a
# vector and `seq_len(n_col)` for a data frame, and one cell builder serves all
# three structures. The paths cannot drift because there is only one path.
#
# Elision is decided on nrow/ncol alone, with no device consulted, and it happens
# BEFORE formatting: the formatting unit is the visible slice, so a hidden 1e15
# cannot flip 400 visible cells into scientific notation.

# ---------------------------------------------------------------------------
# elision
# ---------------------------------------------------------------------------

#' Choose which of `n` lanes to draw
#'
#' Middle elision, not head-only truncation: for a matrix you want all four
#' corners, so the budget is split between the head and the tail and one drawn
#' lane is spent on the `"..."` itself. The decision is pure integer arithmetic --
#' no device, no font, no data -- which is what makes it identical across all six
#' painters and reproducible across devices.
#'
#' @param n Number of rows (or columns) the data actually has.
#' @param max_n Most lanes that may be drawn, the `"..."` lane included.
#'
#' @return A list with components
#'   \describe{
#'     \item{`keep`}{integer vector of the original indices that are drawn}
#'     \item{`gap`}{the drawn position of the `"..."` lane, or `NA_integer_` when
#'       nothing was elided}
#'     \item{`hidden`}{how many indices were dropped}
#'   }
#'   `length(keep) + !is.na(gap)` is the number of drawn lanes, and it never
#'   exceeds `max_n`.
#'
#' @keywords internal
#' @noRd
elide_index <- function(n, max_n) {
  if (length(n) != 1L || is.na(n) || n < 0) {
    stop("`n` must be a single non-negative number.")
  }
  if (length(max_n) != 1L || is.na(max_n) || max_n < 1) {
    stop("`max_n` must be a single number of at least 1.")
  }
  n <- as.integer(n)
  max_n <- as.integer(max_n)

  if (n <= max_n) {
    return(list(keep = seq_len(n), gap = NA_integer_, hidden = 0L))
  }

  # One of the `max_n` lanes is spent on the "...".
  n_keep <- max_n - 1L
  n_head <- as.integer(ceiling(n_keep / 2))
  n_tail <- n_keep - n_head

  keep <- c(
    seq_len(n_head),
    if (n_tail > 0L) seq.int(n - n_tail + 1L, n) else integer(0)
  )

  list(keep = as.integer(keep), gap = n_head + 1L, hidden = n - n_keep)
}

#' Drawn position of each kept lane
#'
#' The kept lanes are drawn in order, skipping over the lane the `"..."` occupies.
#'
#' @param keep The `keep` component of [elide_index()].
#' @param gap The `gap` component of [elide_index()].
#'
#' @return An integer vector of drawn positions, the same length as `keep`.
#'
#' @keywords internal
#' @noRd
drawn_pos <- function(keep, gap) {
  p <- seq_along(keep)
  if (!is.na(gap)) {
    p[p >= gap] <- p[p >= gap] + 1L
  }
  as.integer(p)
}

#' The "# 18 more rows, 12 more columns" note
#'
#' Pluralised, with either half omitted when nothing is hidden in that direction.
#'
#' **A VECTOR HAS NO COLUMNS, AND IT HAS NO ROWS EITHER.** It is drawn on a grid
#' -- an n by 1 one when `layout = "vertical"`, a 1 by n one when it is
#' `"horizontal"` -- and that grid is an artefact of the drawing, not a fact about
#' the data. Reading the hidden count straight off the grid told a student that
#' `seq_len(40)` laid out horizontally has "26 more columns", which is precisely
#' the thing this package exists to un-teach. So the structure is threaded in as
#' data and a vector's elision is counted in ELEMENTS, in both layouts.
#'
#' A vector elides in exactly one direction -- whichever one it is laid out along
#' -- so the two counts are summed rather than reported separately: the other is
#' always 0.
#'
#' **A LIST HAS NEITHER.** Its columns are ELEMENTS and its rows are the VALUES
#' inside an element, and calling those "rows" and "columns" would teach the very
#' thing `paint_list()` exists to un-teach -- that the third value of one element
#' and the third value of the next are a record. `nouns` is that fact, threaded in
#' as data, exactly as `is_vec` is.
#'
#' **AN ARRAY HAS A THIRD DIRECTION**, and it is not a row and not a column: it is
#' a SLICE. `hidden_slices` counts what the slice axes hid, summed over both of
#' them -- a 4-D array elides on two of them at once, and "2 more slices, 1 more
#' slice" would be nonsense. What the reader needs to know is how many blocks are
#' not on the page.
#'
#' @param hidden_rows,hidden_cols Counts from [elide_index()].
#' @param is_vec Is the structure a vector? Then it has elements, not rows and
#'   columns.
#' @param nouns What this structure's rows and columns are called: `c("row",
#'   "column")` for a grid, `c("value", "element")` for a list.
#' @param hidden_slices How many of an array's slices were not drawn. `0` for
#'   every structure that has none, which is every structure but an array.
#'
#' @return A length-one character string, or `NA_character_` when nothing is
#'   hidden.
#'
#' @keywords internal
#' @noRd
elide_note <- function(hidden_rows, hidden_cols, is_vec = FALSE,
                       nouns = c("row", "column"), hidden_slices = 0L) {
  if (isTRUE(is_vec)) {
    n <- hidden_rows + hidden_cols
    if (n <= 0L) {
      return(NA_character_)
    }
    return(paste0("# ", n, " more element", if (n != 1L) "s" else ""))
  }

  parts <- character(0)
  if (hidden_rows > 0L) {
    parts <- c(parts, paste0(
      hidden_rows, " more ", nouns[[1L]], if (hidden_rows != 1L) "s" else ""
    ))
  }
  if (hidden_cols > 0L) {
    parts <- c(parts, paste0(
      hidden_cols, " more ", nouns[[2L]], if (hidden_cols != 1L) "s" else ""
    ))
  }
  if (hidden_slices > 0L) {
    parts <- c(parts, paste0(
      hidden_slices, " more slice", if (hidden_slices != 1L) "s" else ""
    ))
  }
  if (length(parts) == 0L) {
    return(NA_character_)
  }
  paste0("# ", paste(parts, collapse = ", "))
}

# ---------------------------------------------------------------------------
# the cell table
# ---------------------------------------------------------------------------

# How far below its cell's centre the `[i, j]` label is drawn, as a fraction of
# the row height. Two cells share one `(row, col)` -- the value and its index --
# so SOMETHING has to separate them vertically, and that something is data on the
# cell table rather than a rule inside a renderer: a renderer that has to know
# what a "cellindex" is has to be taught twice, and the second teacher is always
# late. With the nudge carried as `dy_rel`, `paint_resolve()` folds it into the
# cell's `y` and both backends inherit it without knowing it exists.
#
# Sign: `y` grows upwards, so BELOW is negative.
#
# Magnitude: 0.3 of a row, and it is PAIRED WITH `stacked_fontsize()`'s `h_ink`
# -- the two are one change and separating them regresses one or the other.
#
# 0.2 is what the previous release drew, and it was fitted against
# `strheight()`, which is the FONT ASCENT and not the ink box: `[i, j]`'s ink runs
# ~0.82 em against an ascent of ~0.56, so the pair was being fitted on a height
# 46% short of the truth and the value's ink came down THROUGH its index. Measured
# by rasterising and counting pixels, a 6x6 numeric matrix at
# `show_indices = "all"` on 7x5in overlapped by 0.0029in.
#
# `stacked_fontsize()` now fits the pair on the honest ink height, which costs the
# text about 16% of its size. This is what buys it back: that bound is LINEAR in
# the gap, so 0.2 -> 0.3 is a 1.5x on the size, and the net is a picture that is
# very slightly SMALLER than before and no longer struck through. The index's ink
# bottom still lands well inside the cell -- at the sizes autofit picks it clears
# the cell's lower edge by a comfortable margin, which `test-layout.R` asserts
# rather than assumes.
cellindex_dy <- -0.3

# How heavily the outline is stroked, as a multiple of an ordinary cell border.
#
# It is here, on the cell table, and NOT in a renderer, for exactly the reason
# `dy_rel` is: a renderer that has to know what an "outline" IS has to be taught
# twice, and the second teacher is always late. It WAS in a renderer -- base drew
# the outline in its own `rect()` call at a hard-coded `lwd = 2` while grid folded
# it into the ordinary cell rects and never set `lwd` at all -- and the two
# backends drew different pictures for a whole release: measured on `svglite`, base
# emitted nine rects at stroke-width 0.75 and ONE at 1.50, and ggplot2 emitted ten
# at 0.75. The heavy border that makes the block read as one object was simply
# absent from every `gpaint_*()` picture, and nothing in the suite noticed, because
# the only thing that could have noticed was a renderer asking a question no
# renderer is allowed to ask.
#
# As a column it is just a stroke weight on a rectangle. Both renderers pass it
# through to `rect(lwd =)` / `gpar(lwd =)` without knowing which row is which, and
# they cannot disagree.
outline_lwd <- 2

#' One chunk of the cell table
#'
#' Every cell in the table -- value, label, header, gap, outline -- is built by
#' this one constructor, so every chunk carries exactly the same eighteen columns
#' in exactly the same order and `rbind()` can never surprise us.
#'
#' @param kind One of `"value"`, `"outline"`, `"rowlabel"`, `"collabel"`,
#'   `"header"`, `"type"`, `"cellindex"`, `"slicelabel"`, `"ellipsis"`.
#' @param row,col Drawn positions, 1-based, row 1 at the top.
#' @param row_end,col_end The drawn position of the cell's LAST row and column.
#'   Both default to `row`/`col`, which is every cell that occupies exactly one
#'   box -- which is every cell of every picture except two.
#'
#'   **A SPAN IS DATA, AND THIS IS THE COLUMN THAT SAYS SO.** The two spanning
#'   cells are the `"outline"` (which runs around the whole value block) and a
#'   `"slicelabel"` (which runs across the block it titles), and before this
#'   column existed the outline's extent was RE-DERIVED inside `paint_resolve()`
#'   by taking `max()` over the value cells -- an arithmetic that has exactly one
#'   right answer only while there is exactly one block. An array draws several,
#'   so the derivation would have handed every block the bounding box of ALL of
#'   them. Carrying the extent as data is what makes `paint_resolve()`'s geometry
#'   ONE formula for every cell (see `R/layout.R`), and it is the same move that
#'   `lwd` and `dy_rel` already are: a renderer that has to know what an
#'   "outline" IS has to be taught twice, and the second teacher is always late.
#' @param i,j Original data indices, `NA` where the cell is not a datum.
#' @param head Defaults to `sig`, which is right for every non-numeric cell.
#' @param lwd Stroke weight of the cell's border, as a multiple of an ordinary
#'   one. `1` for every kind but `"outline"`. It is data, not a renderer's rule --
#'   see `outline_lwd`.
#' @param dy_rel Vertical nudge away from the cell's centre, in row heights.
#'   Negative is downwards. `0` -- dead centre -- for every kind but
#'   `"cellindex"`, which shares its `(row, col)` with a value and has to sit
#'   under it.
#'
#' @return A bare data frame.
#'
#' @keywords internal
#' @noRd
cell_rows <- function(kind, row, col,
                      row_end = NULL, col_end = NULL,
                      i = NA_integer_, j = NA_integer_,
                      fmt_group = NA_integer_,
                      sig = "", insig = "", head = NULL, tail = "",
                      ink = "black", fill = NA_character_, border = NA_character_,
                      lwd = 1,
                      align = "center", size_rel = 1, dy_rel = 0, fit = TRUE) {
  n <- max(length(row), length(col))
  if (is.null(head)) {
    head <- sig
  }
  # A cell occupies one box unless it says otherwise. Every existing cell of
  # every existing picture takes this default, which is what makes the span
  # column a no-op everywhere it is not wanted.
  if (is.null(row_end)) {
    row_end <- row
  }
  if (is.null(col_end)) {
    col_end <- col
  }
  data.frame(
    i = rep_len(as.integer(i), n),
    j = rep_len(as.integer(j), n),
    row = rep_len(as.integer(row), n),
    col = rep_len(as.integer(col), n),
    row_end = rep_len(as.integer(row_end), n),
    col_end = rep_len(as.integer(col_end), n),
    fmt_group = rep_len(as.integer(fmt_group), n),
    sig = rep_len(as.character(sig), n),
    insig = rep_len(as.character(insig), n),
    head = rep_len(as.character(head), n),
    tail = rep_len(as.character(tail), n),
    ink = rep_len(as.character(ink), n),
    fill = rep_len(as.character(fill), n),
    border = rep_len(as.character(border), n),
    lwd = rep_len(as.double(lwd), n),
    align = rep_len(as.character(align), n),
    size_rel = rep_len(as.double(size_rel), n),
    dy_rel = rep_len(as.double(dy_rel), n),
    fit = rep_len(as.logical(fit), n),
    kind = rep_len(as.character(kind), n),
    stringsAsFactors = FALSE
  )
}

#' Resolve a highlight selection to a logical matrix
#'
#' `NULL` means nothing is highlighted; a length-one logical is recycled (which
#' preserves the documented `highlight_area = FALSE` example); `NA` reads as
#' `FALSE`. A wrongly shaped mask is an error that reports the *actual* shape --
#' the old code silently mis-recycled it.
#'
#' @keywords internal
#' @noRd
resolve_highlight <- function(highlight_area, n_row, n_col, is_vec) {
  if (is.null(highlight_area)) {
    return(matrix(FALSE, nrow = n_row, ncol = n_col))
  }
  if (!is.logical(highlight_area)) {
    stop("`highlight_area` must be a logical vector or matrix, not: ", class(highlight_area)[1L])
  }
  h <- highlight_area
  h[is.na(h)] <- FALSE
  if (length(h) == 1L) {
    return(matrix(as.logical(h), nrow = n_row, ncol = n_col))
  }
  if (is_vec) {
    if (length(h) != n_row * n_col) {
      stop(
        "`highlight_area` must have ", n_row * n_col, " values to match the data, but it has ",
        length(h), "."
      )
    }
    return(matrix(as.logical(h), nrow = n_row, ncol = n_col))
  }
  d <- dim(h)
  if (is.null(d) || length(d) != 2L || d[1L] != n_row || d[2L] != n_col) {
    actual <- if (is.null(d)) paste0("a length-", length(h), " vector") else paste0(d[1L], " by ", d[2L])
    stop(
      "`highlight_area` must be a ", n_row, " by ", n_col,
      " logical matrix to match the data, but it is ", actual, "."
    )
  }
  matrix(as.logical(h), nrow = n_row, ncol = n_col)
}

#' Drop an `AsIs` wrapper before formatting
#'
#' `data.frame(x = I(list(1:3)))` -- the only way to build a list column with
#' `data.frame()` -- gives the column class `"AsIs"`. `paint_format()` then
#' dispatches to `.default` instead of `.list`, and the cell renders as the
#' deparsed contents (`"1, 2, 3"`) rather than the `"<list>"` placeholder the
#' design calls for. Stripping the wrapper restores dispatch on the underlying
#' type, whatever it is.
#'
#' @keywords internal
#' @noRd
strip_asis <- function(x) {
  if (inherits(x, "AsIs")) {
    oldClass(x) <- setdiff(oldClass(x), "AsIs")
  }
  x
}

#' Validate a lane vector
#'
#' A *lane vector* is an argument that switches on any number of independent
#' drawing lanes at once: `show_indices = c("row", "column")` draws both, and
#' `show_dimnames = c("row", "column")` does the same for the names. That rules out
#' `match.arg()`, which is length-one by construction -- widening these arguments
#' back out to a vector is the whole point of this function.
#'
#' The lanes are then chosen with `any(... %in% ...)`, so a value that is not
#' "none" turns its lane on regardless of what else is in the vector. `"none"`
#' therefore does not veto its companions: `c("none", "row")` draws the row lane.
#' It is a contradictory request, and the non-trivial half of it wins.
#'
#' **The arity mirrors the accessor.** `dimnames()` returns a LIST with one slot
#' per axis, so `show_dimnames` is a vector: the rownames-only and colnames-only
#' cases are what `table()` and a model matrix produce daily, and a scalar cannot
#' express them. `names()` returns ONE vector, so a vector's `show_names` is a
#' logical scalar. A vector's `show_indices` is a different, mutually exclusive
#' vocabulary ("inside"/"outside") and stays length-one under `match.arg()`.
#'
#' @param x What the caller passed.
#' @param valid The permitted values.
#' @param arg The argument's name, for the error message.
#' @param example Two of `valid`, shown as the "ask for both" hint.
#'
#' @return `x`, unchanged, when it is valid.
#'
#' @keywords internal
#' @noRd
check_lanes <- function(x, valid, arg, example = c("row", "column")) {
  ok <- is.character(x) &&
    length(x) > 0L &&
    !anyNA(x) &&
    all(x %in% valid)
  if (!ok) {
    got <- if (length(x) == 0L) {
      "empty"
    } else if (is.character(x)) {
      paste0("'", x, "'", collapse = ", ")
    } else {
      paste0("a length-", length(x), " ", class(x)[1L])
    }
    quoted <- paste0("'", valid, "'")
    listed <- if (length(quoted) > 1L) {
      paste0(paste(quoted[-length(quoted)], collapse = ", "), ", or ", quoted[[length(quoted)]])
    } else {
      quoted
    }
    stop(
      "`", arg, "` must be one or more of ", listed,
      ", but it was ", got, ". Ask for several at once with a vector, ",
      "such as c(\"", example[[1L]], "\", \"", example[[2L]], "\")."
    )
  }
  x
}

#' Validate `show_indices` for a matrix or a data frame
#'
#' One caller of [check_lanes()]. A grid's index lanes are independent, so a caller
#' can ask for `c("row", "column")` and get both, which is what the README does.
#'
#' @param show_indices What the caller passed.
#'
#' @return `show_indices`, unchanged, when it is valid.
#'
#' @keywords internal
#' @noRd
check_show_indices <- function(show_indices) {
  check_lanes(show_indices, c("none", "cell", "row", "column", "all"), "show_indices")
}

# ---------------------------------------------------------------------------
# names
# ---------------------------------------------------------------------------

#' The names of one axis, straight from the accessor
#'
#' `NULL` means "this axis has no names", which is what every caller tests.
#'
#' **A DATA FRAME'S COLUMN AXIS RETURNS `NULL`, AND IT MUST.** Its column names are
#' already drawn, by the `header` lane -- the lane that exists precisely because a
#' data frame's columns are named variables. Handing them back here as well would
#' draw them a SECOND time, once over the block and once in the header, which is
#' how a bug that is obvious in a picture stays invisible in a test suite that only
#' ever asks "is the name there?".
#'
#' A vector has exactly one axis, so it answers with `names()` whichever axis is
#' asked for; its caller knows which lane it laid that axis on (`layout`).
#'
#' @param data A vector, matrix, data frame, or array.
#' @param axis `"row"`, `"column"`, or a positive whole number -- the axis's
#'   ordinal. An array has more than two axes, and `dimnames()` is a list with one
#'   slot per axis, so the third and fourth are asked for by number. `"row"` is 1
#'   and `"column"` is 2; the two spellings are the same question.
#'
#' @return A character vector as long as the axis, or `NULL`.
#'
#' @keywords internal
#' @noRd
axis_names <- function(data, axis = c("row", "column")) {
  slot <- if (is.numeric(axis)) {
    as.integer(axis)[[1L]]
  } else {
    if (match.arg(axis) == "row") 1L else 2L
  }
  if (is.data.frame(data)) {
    if (slot != 1L) {
      return(NULL)
    }
    return(as.character(attr(data, "row.names")))
  }
  if (!is.null(dim(data))) {
    dn <- dimnames(data)
    if (is.null(dn)) {
      return(NULL)
    }
    # A 1-D dimnamed array has a dimnames list of length 1: there is no
    # second slot to ask for a column axis's names, and reaching for it
    # unconditionally is out of bounds. The same guard serves an array's
    # fourth axis, asked of a three-dimensional one.
    if (slot > length(dn)) {
      return(NULL)
    }
    return(dn[[slot]])
  }
  names(data)
}

#' Does a data frame have row names of its own?
#'
#' The default for `show_rownames`, and the one question it turns on: `mtcars` says
#' "Mazda RX4" and `iris` says nothing at all.
#'
#' `.row_names_info()` is the documented accessor -- it reports the row count,
#' NEGATIVE when the row names are the automatic `1:n` compact form -- and it is
#' the whole of the answer for a frame that has not been subsetted (`mtcars` = 32,
#' `iris` = -150).
#'
#' It is not the whole answer once one has been. **`.row_names_info(head(iris, 5))`
#' is 5, POSITIVE**: subsetting materialises the compact form into a real integer
#' vector `1:5`, so a rule that trusts the sign alone hands a gutter of `1 2 3 4 5`
#' to the single most common example in the package's own documentation. That is
#' the exact thing the row-name gutter exists NOT to draw -- a lane of ordinals
#' pretending to be data, which teaches a student that a row name is a row number.
#' So the second half of the test asks the question the sign is a proxy for: are
#' these names anything other than the row's own position? `iris[c(50, 100), ]`
#' keeps its `50` and `100`, which ARE data -- they say where the rows came from --
#' and it keeps its gutter.
#'
#' @param data A data frame.
#'
#' @return `TRUE` when the frame carries row names worth drawing.
#'
#' @keywords internal
#' @noRd
has_row_names <- function(data) {
  if (.row_names_info(data) <= 0L) {
    return(FALSE)
  }
  rn <- attr(data, "row.names")
  # `identical()` is type-strict: character row names reading "1", "2", "3"
  # (which `as.data.frame(as.matrix(...))` produces) are not `identical()` to
  # the integer `seq_len(n)`, and would otherwise slip past this guard and
  # draw the ordinal-noise gutter it exists to suppress. Comparing as
  # character on both sides catches that case without disturbing any other.
  !identical(as.character(rn), as.character(seq_len(nrow(data))))
}

#' Names, with no `NA` left in them
#'
#' **This runs AT CONSTRUCTION, before truncation, and it is load-bearing.**
#' `names(x)` can hold a true `NA` -- `names(v) <- c("a", NA, "")` is legal and
#' `print()` shows it as `<NA>` -- and no other path in this package has ever put an
#' `NA` into `cells$sig`: `paint_format()` emits the literal string `"NA"`. Names
#' are the first thing that could, and `inked_cells()` is the single predicate that
#' would then have to carry the weight of it in both renderers. Mapping the `NA` to
#' a token here means the cell table never holds one, so nothing downstream ever has
#' to have an opinion about it.
#'
#' `<NA>` is what `print()` writes, and it is ASCII.
#'
#' @param x A character vector, or `NULL`.
#'
#' @return `x` as character with every `NA` replaced, or `NULL`.
#'
#' @keywords internal
#' @noRd
normalize_names <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- as.character(x)
  x[is.na(x)] <- "<NA>"
  x
}

#' What a label lane draws: the name, or else the index
#'
#' A lane holds exactly ONE thing. The precedence is decided by the caller, which
#' passes `nms = NULL` when the user has explicitly asked for indices on that axis;
#' here the rule is simply "a name if there is one".
#'
#' @param nms The axis's names, or `NULL`.
#' @param idx The index labels the lane draws when there are no names.
#' @param max_chars Truncation width for a name. The index is never truncated: it
#'   is short, and half of an accessor is not an accessor.
#' @param ellipsis The truncation mark.
#'
#' @return A character vector the length of `idx`.
#'
#' @keywords internal
#' @noRd
lane_text <- function(nms, idx, max_chars = 8L, ellipsis = "...") {
  if (is.null(nms)) {
    return(idx)
  }
  truncate_chr(normalize_names(nms), max_chars, ellipsis)
}

# ---------------------------------------------------------------------------
# lists
# ---------------------------------------------------------------------------

#' Is this element drawn as CELLS, or as one token?
#'
#' A cell of `paint_list()` is one POSITION of a one-dimensional atomic vector.
#' That is the whole of the rule, and everything else follows from it:
#'
#'   * a matrix element has no positions this picture can lay in a column -- it has
#'     a *shape*, and flattening it into 4 cells would draw a wrong picture of it;
#'   * a data frame element likewise;
#'   * a SUBLIST has positions, and drawing them is exactly the NESTING this
#'     painter refuses (see `?paint_list`);
#'   * a zero-length element has no positions at all, and an empty column reads as
#'     a bug rather than as `integer(0)`.
#'
#' Each of those is drawn as ONE cell holding [elem_sum()]'s description of it.
#'
#' `!is.null(x)` is not redundant with `is.atomic(x)`: `is.atomic(NULL)` was TRUE
#' before R 4.4.0 and is FALSE from it, and this package supports R >= 4.2.0. The
#' explicit test makes the answer the same on every one of them.
#'
#' @param x One element of a list.
#'
#' @return `TRUE` when the element is drawn as one cell per value.
#'
#' @keywords internal
#' @noRd
elem_expands <- function(x) {
  !is.null(x) && is.atomic(x) && is.null(dim(x)) && length(x) > 0L
}

#' Rows of a list's highlight mask
#'
#' A list's mask is POSITIONS by ELEMENTS, and its shape is a fact about the DATA,
#' so `summarise` -- which is a fact about the drawing -- must not change it: the
#' same `highlight_columns(l, "b")` has to work with and without it. A summarised
#' element's one cell is filled when ANY of its positions is marked.
#'
#' The `1L` floor keeps a list of nothing but `NULL`s from producing a mask with
#' no rows, which nothing could then be indexed out of.
#'
#' @param data A bare list.
#'
#' @return A single integer.
#'
#' @keywords internal
#' @noRd
list_mask_rows <- function(data) {
  max(1L, lengths(data, use.names = FALSE))
}

#' What a list's header lane draws over each element
#'
#' **THIS IS THE ONE PLACE IN THE PACKAGE WHERE A LABEL LANE FALLS BACK PER
#' ELEMENT, AND IT IS DELIBERATE.** Every other lane holds exactly one thing --
#' names, or indices, never a mixture -- because an axis is named or it is not. A
#' LIST IS THE EXCEPTION: `names(list(a = 1, 2))` is `c("a", "")`, so an element is
#' named or unnamed ON ITS OWN, and R itself falls back element by element:
#'
#' ```
#' > print(list(a = 1, 2))
#' $a
#' [1] 1
#'
#' [[2]]
#' [1] 2
#' ```
#'
#' The lane draws what `print()` draws, and both halves are the expression that
#' fetches the element -- which is the promise the whole package is built on.
#'
#' A name that is not a syntactic name gets `[["my name"]]` rather than
#' `$my name`, because the second one is not something you can type. (`$` needs
#' backticks there, and a label the reader cannot type is worse than a longer one
#' they can.)
#'
#' @param nms `names(data)`, which may be `NULL`.
#' @param j The drawn elements' original indices.
#' @param max_chars Truncation width for a name. A list's header lane is FREE --
#'   each element is its own formatting unit, so a long name widens its own column
#'   and no other -- which is why it takes the full `max_chars` rather than a
#'   matrix's tighter `max_name_chars` cap.
#' @param ellipsis The truncation mark.
#'
#' @return A character vector as long as `j`.
#'
#' @keywords internal
#' @noRd
list_header <- function(nms, j, max_chars = 12L, ellipsis = "...") {
  out <- paste0("[[", j, "]]")
  if (is.null(nms)) {
    return(out)
  }
  nm <- normalize_names(nms)[j]
  named <- !is.na(nm) & nzchar(nm)
  if (!any(named)) {
    return(out)
  }
  # Syntacticness is asked of the WHOLE name, before truncation: a name is
  # typeable or it is not, and a truncation is the reader's problem to see, not a
  # fact about the object.
  syntactic <- nm[named] == make.names(nm[named])
  short <- truncate_chr(nm[named], max_chars, ellipsis)
  out[named] <- ifelse(syntactic, paste0("$", short), paste0("[[\"", short, "\"]]"))
  out
}

#' Build the cell table
#'
#' The one builder for all four structures. It elides first and formats second,
#' so the formatting unit is the *visible slice*: a hidden outlier cannot change
#' how the visible cells look.
#'
#' The formatting unit is carried as data, in `fmt_group`: `1L` for every cell of
#' a matrix or a vector (one unit, so the same value looks identical in every
#' cell), and the column's own ordinal for a data frame or a list (one unit per
#' column, because their columns are independent variables).
#'
#' RAGGEDNESS IS CARRIED AS DATA TOO, in `col_len` -- the drawn length of each data
#' column. **`col_len` is to raggedness what `fmt_group` is to formatting:** every
#' rectangular structure is the degenerate case where every entry is EQUAL, so
#' there is no list code path, only a list *value*. It never reaches the cell
#' table; it is local to this function, and it has exactly two consumers, both of
#' which ask it a question about SHAPE: `elide_index()`, which is asked once per
#' column instead of once per table, and the outline, which is drawn when every
#' entry is equal -- that being precisely when the block of cells is a rectangle.
#'
#' A data frame IS a list whose elements happen to share a length. That sentence is
#' this function's structure, not a slogan about it.
#'
#' The result is a **bare** data frame. It deliberately has no class and no print
#' method: a print method is a trap, because `expect_snapshot()` would dispatch to
#' it and silently stop catching coordinate regressions.
#'
#' @param data A vector, matrix, data frame, or bare list.
#' @param highlight_area `NULL`, a length-one logical, or a logical mask shaped
#'   like `data`. A list's mask is POSITIONS by ELEMENTS -- see
#'   [list_mask_rows()].
#' @param highlight_color The fill for a highlighted cell.
#' @param show_indices For a matrix or data frame, any number of `"none"`,
#'   `"cell"`, `"row"`, `"column"`, `"all"` -- the lanes are independent, so
#'   `c("row", "column")` turns on both. For a vector, exactly one of `"none"`,
#'   `"inside"`, `"outside"`, which are mutually exclusive. For a list, `"none"`,
#'   `"cell"` or `"all"`, where `"cell"` draws `[[j]][i]` under each value.
#' @param layout Vectors only: `"vertical"` (an n by 1 grid) or `"horizontal"`.
#' @param summarise Lists only: draw every element as ONE cell saying what it is
#'   (`<int [3]>`) rather than as one cell per value.
#' @param show_names For a data frame or a list, draw the column-name row. For a
#'   vector, draw its `names()` in the label lane its layout gives it. A logical
#'   scalar, because `names()` returns one vector.
#' @param show_types Data frames and lists only: draw the type-tag row.
#' @param show_dimnames Matrices only: any number of `"none"`, `"row"`, `"column"`,
#'   `"all"`. A character VECTOR, because `dimnames()` returns a LIST with one slot
#'   per axis and a scalar cannot express the rownames-only case.
#' @param show_rownames Data frames only. `NULL` (the default) draws the row names
#'   when the frame has names of its own -- see [has_row_names()].
#' @param name_align,type_align Data frames only: `"center"`, `"left"` or
#'   `"right"`, for the column-name lane and the type-tag lane respectively. They
#'   are independent of each other and of the values, which keep their own
#'   alignment -- a numeric column's values stay decimal-anchored however its
#'   label is set.
#' @param max_name_chars Truncation width for a name drawn in a lane that SHARES a
#'   formatting unit with the values under it -- a matrix's column names, and a
#'   horizontal vector's. Those lanes widen every cell they sit over (see
#'   [column_widths()]), so they are capped tighter than `max_chars`. A row-name
#'   gutter holds no value, sizes alone, and keeps the full `max_chars`.
#' @param sigfig,subtle_digits,max_chars,max_dec_width,ellipsis Passed to
#'   [paint_format()].
#' @param max_rows,max_cols Elision thresholds. `NULL` takes the default for the
#'   structure: 20 for a matrix or vector, 10 for a data frame (its columns are
#'   wide), and 10 by 8 for a list (its columns are as wide as a NAME).
#' @param max_slices Arrays only: elision threshold on each of the two SLICE axes.
#'   `NULL` takes 4. See [array_cells()].
#' @param show_all Skip elision entirely.
#'
#' @return A bare data frame with one row per drawn cell and the columns `i`, `j`,
#'   `row`, `col`, `fmt_group`, `sig`, `insig`, `head`, `tail`, `ink`, `fill`,
#'   `border`, `lwd`, `align`, `size_rel`, `dy_rel`, `fit`, `kind`; plus the
#'   attributes `n_row`, `n_col` (the drawn extent, in cells), `note` (the
#'   "# 18 more rows" string, or `NA`), `hidden_rows` and `hidden_cols`.
#'
#' @keywords internal
#' @noRd
paint_cells <- function(data,
                        highlight_area = NULL,
                        highlight_color = "lemonchiffon",
                        show_indices = "none",
                        layout = c("vertical", "horizontal"),
                        summarise = FALSE,
                        show_names = TRUE,
                        show_types = TRUE,
                        show_dimnames = "all",
                        show_rownames = NULL,
                        name_align = c("center", "left", "right"),
                        type_align = c("center", "left", "right"),
                        sigfig = 3L,
                        subtle_digits = c("insignificant", "rounded", "none"),
                        max_chars = 12L,
                        max_name_chars = 8L,
                        max_dec_width = 13L,
                        max_rows = NULL,
                        max_cols = NULL,
                        max_slices = NULL,
                        show_all = FALSE,
                        ellipsis = "...") {
  layout <- match.arg(layout)
  subtle_digits <- match.arg(subtle_digits)
  # Single-valued by nature -- a lane has one alignment -- so `match.arg()` is
  # the right tool here. It is NOT the right tool for `show_indices`, which is a
  # vector: see `check_show_indices()`.
  name_align <- match.arg(name_align)
  type_align <- match.arg(type_align)

  # -- what are we drawing? ---------------------------------------------------
  is_df <- is.data.frame(data)
  # A BARE list, and nothing else. `is.list()` alone is far too wide: it is TRUE
  # for `as.POSIXlt(Sys.time())` (which would draw as ELEVEN ragged columns of
  # sec/min/hour/... -- a wrong picture of a datetime), for `lm()`, for `t.test()`,
  # for `by()`, and for every other S3 class that keeps its innards in a list. See
  # `is_paint_list()`.
  is_list <- !is_df && is_paint_list(data)
  dims <- dim(data)

  # AN ARRAY OF RANK THREE OR MORE IS DRAWN AS BLOCKS, and the block builder is the
  # only thing it needs that this function does not already have. A rank-TWO array
  # is not routed there, and that is the whole of why `paint_array(matrix)` draws
  # the matrix: `is.array(matrix(1:4, 2))` is TRUE, a matrix IS the degenerate
  # array, and the degenerate case is served by NOT WRITING IT -- the 2-D thing
  # falls straight through to the matrix path below, so the two painters cannot
  # draw different pictures of the same object.
  if (!is_df && !is_list && !is.null(dims) && length(dims) >= 3L) {
    return(array_cells(
      data = data,
      highlight_area = highlight_area,
      highlight_color = highlight_color,
      show_indices = show_indices,
      show_dimnames = show_dimnames,
      sigfig = sigfig,
      subtle_digits = subtle_digits,
      max_chars = max_chars,
      max_name_chars = max_name_chars,
      max_dec_width = max_dec_width,
      max_rows = max_rows,
      max_cols = max_cols,
      max_slices = max_slices,
      show_all = show_all,
      ellipsis = ellipsis
    ))
  }
  # A 1-D array is neither a vector (it carries a `dim`) nor a grid. Catch it
  # before it can be silently flattened.
  if (!is_df && !is_list && !is.null(dims) && length(dims) != 2L) {
    stop("paintr can only draw one- and two-dimensional structures.")
  }
  is_mat <- !is_df && !is_list && !is.null(dims)
  is_vec <- !is_df && !is_mat && !is_list

  # THE ONE NEW CONCEPT. `col_len` is the drawn length of each DATA column, and a
  # rectangle is the case where every entry of it is equal.
  if (is_list) {
    n_col_data <- length(data)
    # An element is drawn as cells when it is a 1-D atomic vector, and as ONE cell
    # -- a description of itself -- when it is anything else. `summarise` turns
    # every element into that one cell.
    expanded <- !isTRUE(summarise) &
      vapply(data, elem_expands, logical(1), USE.NAMES = FALSE)
    col_len <- ifelse(expanded, lengths(data, use.names = FALSE), 1L)
    n_row_data <- if (n_col_data == 0L) 0L else max(col_len)
  } else {
    if (is_df || is_mat) {
      n_row_data <- nrow(data)
      n_col_data <- ncol(data)
    } else if (layout == "vertical") {
      n_row_data <- length(data)
      n_col_data <- 1L
    } else {
      n_row_data <- 1L
      n_col_data <- length(data)
    }
    expanded <- rep(TRUE, n_col_data)
    col_len <- rep(n_row_data, n_col_data)
  }

  if (n_row_data == 0L || n_col_data == 0L) {
    stop("Cannot paint an empty data structure.")
  }
  # Hard ceiling. It holds even under `show_all`.
  #
  # A ragged list has no `n_row * n_col` -- the product is the BOUNDING BOX, and
  # `list(1:1e5, 1)` would be refused for 2e5 cells it does not have. The data's
  # size is what it holds, which is `sum(lengths())`.
  n_cell <- if (is_list) {
    sum(as.double(lengths(data, use.names = FALSE)))
  } else {
    as.double(n_row_data) * as.double(n_col_data)
  }
  if (n_cell > 1e5) {
    if (is_list) {
      stop(
        "The list has ", n_col_data, " elements holding ", format(n_cell, scientific = FALSE),
        " values, which is more than the 100000 cells paintr will draw."
      )
    }
    stop(
      "The data has ", n_row_data, " rows and ", n_col_data,
      " columns, which is more than the 100000 cells paintr will draw."
    )
  }

  # -- which index lanes? -----------------------------------------------------
  if (is_vec) {
    show_indices <- match.arg(show_indices, c("none", "inside", "outside"))
    idx_cell <- show_indices == "inside"
    idx_row <- show_indices == "outside" && layout == "vertical"
    idx_col <- show_indices == "outside" && layout == "horizontal"
  } else if (is_list) {
    # A THIRD VOCABULARY IS NOT BUILT HERE, AND THAT IS THE POINT. A list gets
    # exactly one index lane -- the in-cell one -- because it is the only lane whose
    # label is true of a list: `[[j]][i]` fetches the value it is written under.
    #
    # The lanes that were NOT built: a shared `[i]` gutter down the left would say
    # that `l[[1]][2]` and `l[[2]][2]` are one record. In a data frame that reading
    # is TRUE and is the entire point of the picture. IN A LIST IT IS FALSE, and a
    # lane that teaches a falsehood is worse than no lane. A `[[j]]` lane along the
    # top would say nothing the header does not already say better.
    show_indices <- check_lanes(
      show_indices, c("none", "cell", "all"), "show_indices",
      example = c("none", "cell")
    )
    idx_cell <- any(show_indices %in% c("cell", "all"))
    idx_row <- FALSE
    idx_col <- FALSE
  } else {
    # A grid takes a VECTOR: its three lanes are independent, so `c("row",
    # "column")` means both. `any()`, not `==`, is what makes that work.
    show_indices <- check_show_indices(show_indices)
    idx_cell <- any(show_indices %in% c("cell", "all"))
    idx_row <- any(show_indices %in% c("row", "all"))
    idx_col <- any(show_indices %in% c("column", "all"))
  }

  # -- which NAME lanes? ------------------------------------------------------
  #
  # THE PRECEDENCE RULE. A label lane holds exactly ONE thing, and it is assigned
  # in this order:
  #
  #   1. `show_indices` names that axis      -> INDICES. The user typed it; it wins.
  #   2. else the axis is named, names on    -> NAMES.
  #   3. else                                -> no lane at all.
  #
  # Rule 1 NEVER FIRES AT THE DEFAULTS: `show_indices` is "none" on every painter.
  # So there is no collision to arbitrate, and when the user does ask for an index
  # lane the argument does exactly what its name says. No `missing()`, no magic.
  #
  # `show_indices = "cell"` (a matrix) and `"inside"` (a vector) are a DIFFERENT
  # lane -- the in-cell `dy_rel` stack -- so they compose with names for free: the
  # name above the column, `[2, 3]` under the value.
  #
  # The names are read once, here, so that only ONE thing below has to be true: a
  # lane draws `nm_row`/`nm_col` when it is not NULL, and its index otherwise.
  #
  # A LIST HAS NO NAME LANE AT ALL, and it is not an oversight: its names belong to
  # its ELEMENTS, which are its columns, and the `header` lane already draws exactly
  # that -- the lane that exists because a data frame's columns are named variables.
  # A list's are too. That one reuse is what makes the whole painter fit with no new
  # tier, no rowspan, and not one line in a renderer.
  nm_row <- NULL
  nm_col <- NULL
  if (is_list) {
    nm_row <- NULL
  } else if (is_vec) {
    # `names()` returns ONE vector, so this is a logical scalar. The lane it lands
    # in is whichever one the layout laid the vector's single axis on.
    if (isTRUE(show_names)) {
      nms <- axis_names(data, "row")
      if (layout == "vertical") nm_row <- nms else nm_col <- nms
    }
  } else if (is_mat) {
    # `dimnames()` returns a LIST, one slot per axis, so this is a lane VECTOR:
    # `table()` and a model matrix produce the half-named cases daily.
    #
    # `"slice"` is accepted and does nothing, which is the right answer rather than
    # a lax one: a matrix REACHES this branch from `paint_array()`, whose vocabulary
    # has five values, and a matrix has no slice axis for the fifth to name. An axis
    # with no names draws no lane; an axis that does not exist draws no lane either.
    # `paint_matrix()`'s own front door still refuses it, on the four-value set.
    show_dimnames <- check_lanes(
      show_dimnames, c("none", "row", "column", "slice", "all"), "show_dimnames"
    )
    if (any(show_dimnames %in% c("row", "all"))) nm_row <- axis_names(data, "row")
    if (any(show_dimnames %in% c("column", "all"))) nm_col <- axis_names(data, "column")
  } else {
    # A data frame's COLUMN names are the `header` lane's job, and `axis_names()`
    # returns NULL for that axis so they cannot also be drawn here.
    rn_on <- if (is.null(show_rownames)) has_row_names(data) else isTRUE(show_rownames)
    if (rn_on) nm_row <- axis_names(data, "row")
  }
  # Rule 1: an index lane the user asked for takes the axis.
  if (idx_row) nm_row <- NULL
  if (idx_col) nm_col <- NULL

  lane_row <- idx_row || !is.null(nm_row)
  lane_col <- idx_col || !is.null(nm_col)

  # -- elide FIRST ------------------------------------------------------------
  if (is.null(max_rows)) max_rows <- if (is_df || is_list) 10L else 20L
  if (is.null(max_cols)) max_cols <- if (is_df) 10L else if (is_list) 8L else 15L

  elide_one <- function(n) {
    if (isTRUE(show_all)) {
      list(keep = seq_len(n), gap = NA_integer_, hidden = 0L)
    } else {
      elide_index(n, max_rows)
    }
  }

  er <- elide_one(n_row_data)
  ec <- if (isTRUE(show_all)) {
    list(keep = seq_len(n_col_data), gap = NA_integer_, hidden = 0L)
  } else {
    elide_index(n_col_data, max_cols)
  }
  ki <- er$keep
  kj <- ec$keep

  # ELISION IS ASKED PER COLUMN, of `col_len` -- which for every rectangular
  # structure is the same number in every entry, so every column gets the same
  # answer and that answer IS `er`. The rectangle is not a special case here; it is
  # the case where the general question has one answer.
  #
  # It is what makes the ragged picture honest at both ends. `elide_index()`'s head
  # and tail counts depend only on `max_rows`, so every elided column shares one gap
  # row and one set of drawn rows -- but a column that hides NOTHING draws no gap
  # (a length-2 element must not announce values it does not have), and a column
  # that hides something draws its OWN tail rather than trailing off into the empty
  # space under a deeper element's.
  el <- lapply(kj, function(k) elide_one(col_len[[k]]))

  # -- the drawn grid ---------------------------------------------------------
  # A list's elements are named variables laid out as columns, exactly as a data
  # frame's are, so they get the same two lanes. This is the entire widening.
  header_on <- (is_df || is_list) && isTRUE(show_names)
  type_on <- (is_df || is_list) && isTRUE(show_types)

  lane <- 0L
  collab_row <- NA_integer_
  header_row <- NA_integer_
  type_row <- NA_integer_
  if (lane_col) {
    lane <- lane + 1L
    collab_row <- lane
  }
  if (header_on) {
    lane <- lane + 1L
    header_row <- lane
  }
  if (type_on) {
    lane <- lane + 1L
    type_row <- lane
  }
  lab_rows <- lane
  lab_cols <- as.integer(lane_row)

  row_of <- drawn_pos(ki, er$gap) + lab_rows
  col_of <- drawn_pos(kj, ec$gap) + lab_cols
  gap_row <- if (is.na(er$gap)) NA_integer_ else er$gap + lab_rows
  gap_col <- if (is.na(ec$gap)) NA_integer_ else ec$gap + lab_cols

  n_row <- lab_rows + length(ki) + as.integer(!is.na(er$gap))
  n_col <- lab_cols + length(kj) + as.integer(!is.na(ec$gap))

  # -- format the VISIBLE slice, once per formatting unit ---------------------
  fmt_args <- list(
    sigfig = sigfig,
    max_chars = max_chars,
    max_dec_width = max_dec_width,
    subtle_digits = subtle_digits,
    ellipsis = ellipsis
  )

  # THE DRAWN GRID, COLUMN BY COLUMN. Each drawn column contributes the positions
  # its own element keeps, in order, and `ci` says which column each cell came
  # from. Column-major -- one column's cells, then the next -- which is the order
  # every `paint_format()` result comes back in.
  #
  # FOR EVERY RECTANGULAR STRUCTURE THIS IS EXACTLY `expand.grid(ri, ci)`, cell for
  # cell, and `test-cell-order.R` says so against an independent reconstruction:
  # `col_len` is constant, so `keep_i` is `ki` repeated once per column, `ci` is
  # `seq_along(kj)` each-repeated, and the two `unlist()`s below are `ki[g$ri]` and
  # `row_of[g$ri]`. The generalisation costs the rectangle nothing.
  keep_i <- lapply(el, `[[`, "keep")
  rows_i <- lapply(el, function(e) drawn_pos(e$keep, e$gap) + lab_rows)

  ci <- rep(seq_along(kj), lengths(keep_i))
  i_v <- as.integer(unlist(keep_i, use.names = FALSE))
  j_v <- kj[ci]
  row_v <- as.integer(unlist(rows_i, use.names = FALSE))
  col_v <- col_of[ci]
  # A summarised element's cell is the ELEMENT, not one of its positions, so it has
  # no `i`. `[[2]]` is the accessor that returns it, and `[[2]][1]` is not.
  if (is_list && any(!expanded)) {
    i_v[!expanded[j_v]] <- NA_integer_
  }

  if (is_df) {
    fs <- lapply(seq_along(kj), function(cc) {
      do.call(paint_format, c(list(x = strip_asis(data[[kj[cc]]])[keep_i[[cc]]]), fmt_args))
    })
    f <- do.call(rbind, fs)
    # One formatting unit per column.
    fmt_group <- as.integer(ci)
    col_tag <- vapply(fs, function(z) attr(z, "tag"), character(1))
  } else if (is_list) {
    fs <- lapply(seq_along(kj), function(cc) {
      k <- kj[[cc]]
      if (expanded[[k]]) {
        return(do.call(paint_format, c(list(x = strip_asis(data[[k]])[keep_i[[cc]]]), fmt_args)))
      }
      # ONE cell for the whole element. `data[k]` -- single bracket -- is a
      # length-one LIST, so this is `paint_format.list()`, whose token is
      # `elem_sum()`'s description of the element.
      do.call(paint_format, c(list(x = data[k]), fmt_args))
    })
    f <- do.call(rbind, fs)
    # An element is its own formatting unit, exactly as a data frame's column is.
    fmt_group <- as.integer(ci)
    # AND ITS TAG IS NOT THE UNIT'S TAG. `paint_format(data[k])` comes back tagged
    # `"<list>"` -- correctly, it was handed a list -- so reading the tag off it
    # would print `<list>` in the type lane under a cell reading `<int [2 x 2]>`.
    # The type lane is asked of the ELEMENT.
    col_tag <- vapply(seq_along(kj), function(cc) {
      k <- kj[[cc]]
      if (expanded[[k]]) attr(fs[[cc]], "tag") else elem_type(data[[k]])
    }, character(1))
  } else {
    vis <- if (is_vec) {
      data[if (layout == "vertical") ki else kj]
    } else {
      data[ki, kj, drop = FALSE]
    }
    f <- do.call(paint_format, c(list(x = strip_asis(vis)), fmt_args))
    # ONE formatting unit for the whole structure. This is the entire point: the
    # same value looks identical in every cell.
    fmt_group <- rep(1L, nrow(f))
    col_tag <- rep(attr(f, "tag"), length(kj))
  }

  # A list's mask is POSITIONS by ELEMENTS -- the bounding box of the DATA, not of
  # the drawing -- so a summarised element still has a mask column to look at, and
  # its one cell is filled when any of its positions is marked.
  mask <- resolve_highlight(
    highlight_area,
    if (is_list) list_mask_rows(data) else n_row_data,
    n_col_data, is_vec
  )
  mask_vis <- unlist(lapply(seq_along(kj), function(cc) {
    k <- kj[[cc]]
    if (expanded[[k]]) {
      return(mask[keep_i[[cc]], k])
    }
    any(mask[seq_len(max(1L, length(data[[k]]))), k])
  }), use.names = FALSE)

  # -- the chunks -------------------------------------------------------------

  # The heavy border that makes the block read as one object. `lwd` is the whole
  # of what makes it heavy, and it is a column like any other -- so neither
  # renderer has to know that an "outline" exists to stroke it correctly.
  #
  # THE OUTLINE IS DRAWN WHEN THE BLOCK IS A RECTANGLE, AND THE BLOCK IS A RECTANGLE
  # WHEN EVERY COLUMN IS THE SAME DEPTH. That is `col_len`, and asking it is the same
  # question `elide_index()` is already asked per column, of the same vector: a
  # LENGTH is SHAPE, in the same class of fact as `nrow` and `ncol`, and shape is
  # what this file is allowed to decide on. (Data CONTENT is what it is not, and
  # nothing here reads a value.)
  #
  # Every rectangular structure has one number in every entry of `col_len`, so this
  # is unconditionally TRUE for a matrix, a vector and a data frame, and their
  # outline is exactly the outline they have always had. There is no `is_list`
  # branch, because raggedness is a VALUE and not a code path.
  #
  # A RAGGED LIST GETS NO OUTLINE, AND THAT IS THE OTHER HALF OF THE SAME RULE.
  # `outline_box()` runs to the bottom-right of the drawn cells, which for a ragged
  # list is the BOUNDING BOX of a shape that is not a rectangle: on a 4/1/3 list the
  # heavy border would run down to the bottom of the deepest element, and the
  # length-1 element would sit at the top of a tall, empty, heavily-boxed column --
  # a box around cells THAT DO NOT EXIST. Every cell still carries its own border,
  # so the block still reads as a block; it just reads as the ragged block it is.
  #
  # SO THE BOX IS THE LESSON. Give a list's elements a shared length and the
  # rectangle closes, exactly as it closes around the data frame that list could have
  # been; take the shared length away and the rectangle breaks. The one thing a data
  # frame adds is the one thing the heavy border draws.
  #
  # It is expressed by EMITTING OR NOT EMITTING A ROW. Both renderers already draw
  # nothing for a row that is not there. Zero lines in either.
  #
  # THE OUTLINE CARRIES ITS OWN EXTENT, in `row_end`/`col_end`. It used to carry
  # only its top-left, and `paint_resolve()` re-derived the bottom-right by taking
  # `max()` over every value cell in the table -- which is the right answer only
  # while the table holds exactly ONE block. It is the last drawn row and the last
  # drawn column, which is what that `max()` computed, and now it is a fact the
  # cell states rather than one the layout guesses.
  outline <- if (all(col_len == col_len[[1L]])) {
    cell_rows(
      kind = "outline",
      row = lab_rows + 1L, col = lab_cols + 1L,
      row_end = n_row, col_end = n_col,
      border = "black", lwd = outline_lwd, align = "center",
      fit = FALSE
    )
  } else {
    NULL
  }

  # A lane draws a NAME or an INDEX, never both, and the two differ only in what
  # they say and how loudly: a name is DATA -- it is part of the object, and
  # `print()` draws it -- so it is inked black at the header's weight, while an
  # index is metadata ABOUT the drawing and stays grey and small. That is a
  # difference in the data on the cell table, not a branch in a renderer.
  #
  # The column lane is capped at `max_name_chars` and the row gutter is not, and
  # that asymmetry is arithmetic, not taste. A matrix is ONE formatting unit, so
  # `column_widths()` gives every column of it the widest column's width -- THE
  # LONGEST COLUMN NAME THEREFORE WIDENS EVERY CELL IN THE MATRIX. The gutter holds
  # no value cell, so its `fmt_group` is NA, it sizes alone, and its names are free.
  #
  # The tempting alternative -- narrowing `max_cols` when a name lane is drawn --
  # is forbidden: it would make ELISION DEPEND ON DATA CONTENT, so the same
  # 12-column matrix would draw a different number of columns according to whether
  # it happened to have dimnames. Elision is decided on nrow/ncol alone, and it
  # stays that way; the cost goes to a truncation the reader can SEE instead.
  collabel <- NULL
  if (lane_col && length(kj) > 0L) {
    named <- !is.null(nm_col)
    collabel <- cell_rows(
      kind = "collabel",
      row = collab_row, col = col_of,
      j = kj,
      sig = lane_text(
        nms = nm_col[kj],
        idx = if (is_vec) paste0("[", kj, "]") else paste0("[, ", kj, "]"),
        max_chars = max_name_chars,
        ellipsis = ellipsis
      ),
      ink = if (named) "black" else "grey40",
      align = "center",
      size_rel = if (named) 0.9 else 0.8
    )
  }

  # The two label lanes are aligned by REQUEST, not by inheritance. They used to
  # take the column's value alignment, so a name and a type tag wandered with the
  # type of the thing underneath -- centred-ish over a character column, jammed
  # right over a numeric one -- and a mixed frame's labels lined up on nothing at
  # all. Centred is the default because a label names the WHOLE column, not its
  # last digit; `name_align`/`type_align` override each lane on its own.
  #
  # The values are untouched. Their alignment -- decimal for a numeric column --
  # is what anchors the digits, and it is not a label's business.
  #
  # A LIST'S HEADER FALLS BACK PER ELEMENT -- `$a`, then `[[2]]` -- and that is the
  # single exception to the rule that a lane holds exactly one kind of thing. It is
  # R-faithful: `names(list(a = 1, 2))` is `c("a", "")`, an element is named on its
  # own, and `print()` falls back element by element in exactly this way. See
  # `list_header()`, where the exception is written down.
  header <- NULL
  if (header_on && length(kj) > 0L) {
    header <- cell_rows(
      kind = "header",
      row = header_row, col = col_of,
      j = kj,
      sig = if (is_list) {
        list_header(names(data), kj, max_chars, ellipsis)
      } else {
        truncate_chr(names(data)[kj], max_chars, ellipsis)
      },
      ink = "black", align = name_align, size_rel = 0.9
    )
  }

  type <- NULL
  if (type_on && length(kj) > 0L) {
    type <- cell_rows(
      kind = "type",
      row = type_row, col = col_of,
      j = kj,
      sig = col_tag,
      ink = "grey50", align = type_align, size_rel = 0.8
    )
  }

  rowlabel <- NULL
  if (lane_row && length(ki) > 0L) {
    named <- !is.null(nm_row)
    rowlabel <- cell_rows(
      kind = "rowlabel",
      row = row_of, col = 1L,
      i = ki,
      # The gutter sizes alone, so a row name is FREE and keeps the full
      # `max_chars`. It is the same lane whatever it holds, so it keeps the lane's
      # alignment too: right, snug against the block it labels.
      sig = lane_text(
        nms = nm_row[ki],
        idx = if (is_vec) paste0("[", ki, "]") else paste0("[", ki, ", ]"),
        max_chars = max_chars,
        ellipsis = ellipsis
      ),
      ink = if (named) "black" else "grey40",
      align = "right",
      size_rel = if (named) 0.9 else 0.8
    )
  }

  value <- cell_rows(
    kind = "value",
    row = row_v, col = col_v,
    i = i_v, j = j_v,
    fmt_group = fmt_group,
    sig = f$sig, insig = f$insig, head = f$head, tail = f$tail,
    ink = f$ink,
    fill = ifelse(mask_vis, highlight_color, "white"),
    border = "black",
    align = f$align, size_rel = 1, fit = TRUE
  )

  # The index shares its `(row, col)` with the value it names, so it is `dy_rel`
  # -- and only `dy_rel` -- that keeps the two from being stamped on top of each
  # other. Without it the renderers centre both in the same cell and draw
  # `[1` + `10` + `1]` as one illegible smear.
  #
  # `[[j]][i]` IS THE BEST ACCESSOR LESSON IN THE PACKAGE. It is the expression
  # students get wrong most often -- `l[2]` is a list, `l[[2]]` is the vector,
  # `l[[2]][3]` is the value -- and here it is printed under the number it returns.
  # A summarised element has no `i`, and its accessor is the one that returns the
  # element itself: `[[j]]`.
  cellindex <- NULL
  if (idx_cell && nrow(value) > 0L) {
    cellindex <- cell_rows(
      kind = "cellindex",
      row = row_v, col = col_v,
      i = i_v, j = j_v,
      sig = if (is_vec) {
        paste0("[", if (layout == "vertical") i_v else j_v, "]")
      } else if (is_list) {
        ifelse(
          is.na(i_v),
          paste0("[[", j_v, "]]"),
          paste0("[[", j_v, "]][", i_v, "]")
        )
      } else {
        paste0("[", i_v, ", ", j_v, "]")
      },
      ink = "grey50", align = "center", size_rel = 0.7,
      dy_rel = cellindex_dy
    )
  }

  # The gap is ordinary cell rows. No renderer ever learns that elision exists.
  #
  # THE ROW GAP IS PER COLUMN, and for a rectangle that is a distinction without a
  # difference: every column hides the same rows, so every column draws the `"..."`
  # and the set of gap cells is the whole gap row, exactly as before. A RAGGED LIST
  # IS WHERE IT BITES. A length-2 element beside a length-40 one hides nothing, and
  # a `"..."` over it would announce values it does not have -- which is the one
  # thing a picture of a data structure must never do.
  #
  # The COLUMN gap -- the hidden ELEMENTS -- still runs the full depth of the block,
  # because the deepest drawn column fills it and the lane has to read as a lane.
  gap_row_of <- rep(NA_integer_, n_col)
  for (cc in seq_along(kj)) {
    if (!is.na(el[[cc]]$gap)) {
      gap_row_of[[col_of[[cc]]]] <- el[[cc]]$gap + lab_rows
    }
  }
  gap <- NULL
  if (any(!is.na(gap_row_of)) || !is.na(gap_col)) {
    grd <- expand.grid(
      row = seq.int(lab_rows + 1L, n_row),
      col = seq.int(lab_cols + 1L, n_col)
    )
    want <- gap_row_of[grd$col]
    hit <- (!is.na(want) & grd$row == want) | (!is.na(gap_col) & grd$col == gap_col)
    grd <- grd[hit, , drop = FALSE]
    # The row-label gutter shows the gap too, exactly as a tibble does.
    if (lane_row && !is.na(gap_row)) {
      grd <- rbind(data.frame(row = gap_row, col = 1L), grd)
    }
    gap <- cell_rows(
      kind = "ellipsis",
      row = grd$row, col = grd$col,
      sig = ellipsis,
      ink = "grey50", align = "center", size_rel = 1,
      fit = FALSE
    )
  }

  out <- rbind(outline, collabel, header, type, rowlabel, value, cellindex, gap)
  rownames(out) <- NULL

  # What is hidden DOWN the drawing. A rectangle hides the same rows in every
  # column, so one column's count IS the table's; a ragged list hides a different
  # number of values in each, and what the reader wants to know is how many values
  # are not on the page.
  hidden_rows <- if (is_list) {
    sum(vapply(el, function(e) as.integer(e$hidden), integer(1)))
  } else {
    er$hidden
  }

  attr(out, "n_row") <- as.integer(n_row)
  attr(out, "n_col") <- as.integer(n_col)
  attr(out, "hidden_rows") <- as.integer(hidden_rows)
  attr(out, "hidden_cols") <- as.integer(ec$hidden)
  # `is_vec` is threaded in because a vector has ELEMENTS. The grid it is drawn on
  # is the renderer's business, not the student's. `nouns` is threaded in for the
  # same reason: a list has ELEMENTS holding VALUES, and it has no rows at all.
  attr(out, "note") <- elide_note(
    hidden_rows, ec$hidden, is_vec,
    nouns = if (is_list) c("value", "element") else c("row", "column")
  )
  out
}

#' The width every cell has to fit into, in layout units
#'
#' `col_w[cells$col]` for every cell that occupies one column -- which is every
#' cell of every picture but two -- and the sum of the spanned columns' widths for
#' a cell that spans several.
#'
#' **THIS IS THE OTHER HALF OF `column_widths()`'S RULE 2, AND WITHOUT IT THE FIRST
#' HALF IS A BUG.** A spanning cell demands no width from any single column, so
#' nothing widens to hold it; if it were then FITTED against a single column, the
#' font would be driven down until the title fitted in a slot that was never
#' widened for it. Measured, on `Titanic` at 7x5in with `min_pt = 5`: charging the
#' title `, , Child, Yes` to its own column fits the whole picture at **4.87pt** --
#' BELOW the legibility floor, so the canonical example would warn at its own
#' defaults. Fitted against the block it actually spans, the same picture lands at
#' **15.96pt** and the title binds nothing at all.
#'
#' So the span is the constraint, and it is the honest one: a title is allowed
#' exactly the width of the block it titles, which is precisely the width within
#' which it must not collide with the next block's title. It binds only when it
#' genuinely does not fit -- a bare `2x2x2x2` array, whose blocks are two narrow
#' columns and whose title is `, , 1, 1`, is correctly pulled from 24.0 to 21.9pt.
#'
#' `edge` is the same cumulative-width vector `paint_resolve()` lays the cells out
#' on, so a span measured here and a rectangle drawn there cannot disagree.
#'
#' **THE SINGLE-COLUMN CASE IS `col_w[cells$col]`, LIFTED OUT, AND IT IS NOT AN
#' OPTIMISATION.** Writing the whole thing as `edge[col_end + 1] - edge[col]` is
#' correct in exact arithmetic and WRONG IN FLOATING POINT: `cumsum()` accumulates,
#' so `(a + b + c) - (a + b)` is not bit-identical to `c`, and the difference --
#' around 1e-16 of a layout unit -- travels straight into `avail_w`, through the
#' `min()` in `fit_fontsize()`, and out into a font size that differs in its last
#' bits. Measured: it moved the fitted size of a 30x20 matrix and the span offsets
#' of `iris` and of a ragged list. A cell that occupies one column must get that
#' column's width, the same double it has always had, so it is read straight and the
#' cumulative sum is asked only of the cells that actually span.
#'
#' @param cells A cell table from [paint_cells()].
#' @param col_w Column widths in layout units, from [column_widths()].
#'
#' @return A numeric vector, one width per cell, in layout units.
#'
#' @keywords internal
#' @noRd
span_widths <- function(cells, col_w) {
  out <- col_w[cells$col]
  k <- which(cells$col_end > cells$col)
  if (length(k) > 0L) {
    edge <- c(0, cumsum(col_w))
    out[k] <- edge[cells$col_end[k] + 1L] - edge[cells$col[k]]
  }
  out
}

#' The cells that get a rectangle, in the order they are drawn
#'
#' A cell is drawn as a rectangle when it has a fill or a border; `rect()` and
#' `rectGrob()` both take `NA` for "neither", vectorised, so the rest simply
#' contribute nothing.
#'
#' **Draw order is decided here, once, for both backends, and it is decided on the
#' DATA.** The rule is "a heavier stroke goes on top": `order()` is stable, so the
#' ordinary cells keep their table order and the outline -- the one row with a
#' weight above 1 -- lands last, where the cell borders it shares its edges with
#' cannot paint over it.
#'
#' That is deliberately not `order(kind == "outline")`, which is what
#' `paintr_children()` used to say. A renderer that sorts on `kind` is a renderer
#' that knows what an outline is, and the moment one of them knows something the
#' other does not, the two draw different pictures -- which is exactly what
#' happened to the outline's line weight. Sorting on the stroke weight itself needs
#' no such knowledge, and it generalises for free: a cell table that one day grows
#' a second emphasised box gets the same treatment without either renderer being
#' told.
#'
#' @param cells A cell table from [paint_cells()], usually already resolved.
#'
#' @return The subset of `cells` that is drawn as rectangles, in draw order.
#'
#' @keywords internal
#' @noRd
boxed_cells <- function(cells) {
  b <- cells[!is.na(cells$fill) | !is.na(cells$border), , drop = FALSE]
  if (nrow(b) == 0L) {
    return(b)
  }
  b[order(b$lwd), , drop = FALSE]
}

#' The cells that draw a given text span, or their union
#'
#' A span is drawn when it is a real, non-empty string: `!is.na(s) & nzchar(s)`.
#' `nzchar()` alone is not enough -- `nzchar(NA)` is TRUE, so a true `NA` would be
#' KEPT rather than skipped.
#'
#' This is exactly the predicate the two renderers once wrote out separately, and
#' got wrong in different directions. `R/render-base.R` tested `nzchar()` alone
#' and KEPT a row with a true NA `sig`; it was rescued only because
#' `graphics::text()` silently skips an NA label and draws nothing -- an accident,
#' not an agreement. `R/render-grid.R` tested `!is.na(s) & nzchar(s)` and DROPPED
#' that same row, because `grid::textGrob()` has no such mercy. Nothing puts a
#' true NA into `sig` or `insig` today (`paint_format()` emits the literal token
#' `"NA"` as a string), so the divergence was latent; drawing `names(x)`, where
#' `names()` can hold a true NA, is all it would take to make it visible. Asking
#' the question here, once, is what makes that divergence impossible rather than
#' merely documented.
#'
#' `R/render-grid.R` needs the `sig` rows and the `insig` rows separately, for two
#' `textGrob()`s. `R/render-base.R` needs their union, for one subset fed to two
#' vectorised `text()` calls. `span` picks which; the default is the union.
#'
#' @param cells A cell table from [paint_cells()].
#' @param span One or both of `"sig"`, `"insig"`. Both (the default) selects the
#'   union: a row where either span is a real, non-empty string.
#'
#' @return The subset of `cells` whose named span(s) are drawn.
#'
#' @keywords internal
#' @noRd
inked_cells <- function(cells, span = c("sig", "insig")) {
  span <- match.arg(span, several.ok = TRUE)
  drawn <- function(s) !is.na(cells[[s]]) & nzchar(cells[[s]])
  keep <- Reduce(`|`, lapply(span, drawn))
  cells[keep, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# arrays
# ---------------------------------------------------------------------------
#
# THE PANELS ARE IN THE CELL TABLE, AND THEY ARE NOT PANELS.
#
# The obvious way to draw an array is one graphics panel per slice --
# `par(mfrow =)`, `layout()`, or a grid viewport per block. All three are dead, and
# they are dead on ARITHMETIC, before hygiene is even discussed. `paint_resolve()`
# letterboxes ONE panel (`u = min(panel$w / sum(col_w), panel$h / n_row)`) and
# `fit_fontsize()` is a `min()` over ONE cell table. So k panels means k
# independent `u` and therefore k independent FONT SIZES. Measured, on three slices
# of one array: 24 / 12.42 / 23.05 pt under `par(mfrow = c(2, 2))`, and
# 24 / 13.69 / 24 pt inside three EQUAL grid viewports. Equal panels, unequal fonts
# -- the same number drawn at half the size two inches to the left. IT IS THE
# PER-PANEL FIT THAT IS BROKEN, NOT THE PANEL GEOMETRY, and no amount of care with
# `par()` fixes it.
#
# (`par(mfrow =)` is also a `cex` AND `csi` mutator -- it drops `cex` to 0.83 and
# `csi` by 17%, so every `strwidth()` taken after it is 17% short -- and
# `graphics::layout()` is worse still, because "layout" is not in
# `names(par(no.readonly = TRUE))` and `restore_par()` is therefore STRUCTURALLY
# incapable of putting it back. Both are recorded above `restore_par()`. Neither is
# the reason. The reason is the fit.)
#
# So the slices are FACETED IN THE CELL TABLE: every block, every slice title and
# every separator is an ordinary cell row, in one table, with one column-width
# vector and one letterbox. The shared font fit is then not a feature that had to
# be built -- it is what one table MEANS. Both renderers are untouched, to the
# line, because there is nothing here for them to know.

#' Which slice does each drawn block hold?
#'
#' The slice axes of an n-D array are axes 3..n, and they are laid out as a GRID of
#' blocks: axis 3 runs across (one block per level), and axes 4..n are folded into
#' the vertical direction, axis 4 varying fastest -- exactly the order `print()`
#' walks them in, and exactly the order R's own column-major storage does.
#'
#' **A 3-D ARRAY'S SLABS THEREFORE LAY OUT IN ONE LINE**, and that is a decision.
#' The tempting `ceiling(sqrt(k))` wrap would make four 3-D slabs PIXEL-IDENTICAL
#' to a genuine 4-D facet grid -- a layout artefact wearing a dimension's clothes,
#' and the reader has no way to tell which they are looking at. A picture of a data
#' structure must not invent structure. One line for a 3-D array; a real grid for a
#' 4-D one, whose two directions ARE two axes.
#'
#' @param b A block-row's ordinal, from 1 to `prod(hi)`.
#' @param hi The extents of axes 4..n, possibly empty.
#'
#' @return An integer vector as long as `hi`: the level of each of those axes.
#'
#' @keywords internal
#' @noRd
slice_sub <- function(b, hi) {
  out <- integer(length(hi))
  r <- as.integer(b) - 1L
  for (q in seq_along(hi)) {
    out[[q]] <- r %% hi[[q]] + 1L
    r <- r %/% hi[[q]]
  }
  out
}

#' Resolve a highlight selection to a logical array
#'
#' [resolve_highlight()]'s n-dimensional sibling. It is deliberately NOT a widening
#' of that function: `resolve_highlight()` is on the hot path of every picture the
#' package draws, and an array is the one structure that needs an n-D answer.
#'
#' @param highlight_area `NULL`, a length-one logical, or a logical array shaped
#'   like the data.
#' @param dims `dim(data)`.
#'
#' @return A logical array of dimension `dims`.
#'
#' @keywords internal
#' @noRd
resolve_highlight_dim <- function(highlight_area, dims) {
  if (is.null(highlight_area)) {
    return(array(FALSE, dim = dims))
  }
  if (!is.logical(highlight_area)) {
    stop("`highlight_area` must be a logical vector or array, not: ", class(highlight_area)[1L])
  }
  h <- highlight_area
  h[is.na(h)] <- FALSE
  if (length(h) == 1L) {
    return(array(as.logical(h), dim = dims))
  }
  d <- dim(h)
  if (is.null(d) || !identical(as.integer(d), as.integer(dims))) {
    actual <- if (is.null(d)) {
      paste0("a length-", length(h), " vector")
    } else {
      paste(d, collapse = " by ")
    }
    stop(
      "`highlight_area` must be a ", paste(dims, collapse = " by "),
      " logical array to match the data, but it is ", actual, "."
    )
  }
  array(as.logical(h), dim = dims)
}

#' Build the cell table for an array of rank three or more
#'
#' One table, one letterbox, one font. See the note above for why the slices are
#' faceted here rather than into panels.
#'
#' # The label under the cell is the expression you type
#'
#' That is this package's one distinguishing promise, and an array is where it is
#' easiest to break. Verified, on `a <- array(1:24, c(2, 3, 4))`:
#'
#' ```
#' a[1, ]      # Error: incorrect number of dimensions
#' a[2, 3]     # Error: incorrect number of dimensions
#' a[2, 3, 4]  # 24    <- the accessor the picture must print
#' ```
#'
#' A row lane reading `[1, ]` and a cell index reading `[2, 3]` -- which is what the
#' matrix's labels say, and what an array would inherit if the lanes were reused
#' unchanged -- would therefore teach a student to type expressions that ERROR, in
#' the `show_indices = "all"` call an instructor reaches for first. Every index this
#' function draws carries the array's FULL SUBSCRIPT ARITY, with the slice
#' subscripts filled in from the block the label sits in:
#'
#' ```
#'   rank 2    [1, ]       [, 2]        [1, 2]
#'   rank 3    [1, , 3]    [, 2, 3]     [1, 2, 3]
#'   rank 4    [1, , 3, 2] [, 2, 3, 2]  [1, 2, 3, 2]
#' ```
#'
#' They are filled in, rather than left blank as `[1, , ]`, because the label has to
#' name the thing it is DRAWN BESIDE. `a[1, , ]` is valid R -- it runs -- but it is
#' the first row of every block at once, and the gutter it would sit in is beside
#' the first row of ONE block. A label that names four blocks while pointing at one
#' is the same class of lie as a `[i]` gutter down a ragged list, and it is refused
#' for the same reason. `test-array.R` evaluates every index this function draws,
#' for ranks 2, 3 and 4, and none of them may error.
#'
#' # The formatting unit is the WHOLE array
#'
#' `fmt_group` is `1L` on every value cell -- one unit, as for a matrix -- and the
#' visible values of every block are formatted in a SINGLE `paint_format()` call.
#' So a `1e15` hiding in slice 3 flips slice 1 into scientific notation, and the
#' same number looks the same in every block.
#'
#' That is not an implementation convenience, it is what an array IS. A data frame's
#' columns are separate units because they are separate VARIABLES -- different
#' types, different units of measurement, and a `1e15` in `price` says nothing about
#' `count`. An array's slices are not variables. They are one homogeneous atomic
#' object, cut up for the drawing, and `a[1, 1, 1]` and `a[2, 3, 4]` are the same
#' measurement of the same thing. R agrees: `print()` formats an array to one common
#' width across every slice it prints. Formatting per block would draw `1.00` in one
#' block and `1.0e+00` in the next and teach that a slice is a separate variable,
#' which is false.
#'
#' # Elision
#'
#' Four axes elide, all of them on SHAPE alone and none of them on content: the rows
#' (`max_rows`), the columns (`max_cols`), and each of the two slice axes
#' (`max_slices`). A hidden slice draws a `"..."` block, because the package's
#' contract is that the gap is ALWAYS drawn.
#'
#' `max_slices = 4` is calibrated, not chosen. The three canonical teaching arrays
#' are all fully dimnamed contingency tables, and at 4 they draw at 15.1pt
#' (`Titanic`), 15.2pt (`HairEyeColor`) and 10.2pt (`UCBAdmissions`, eliding 3 of
#' its 6 departments) on knitr's default 7x5in canvas, against a `min_pt` of 5. At
#' `max_slices = 6` -- enough to hold all six of `UCBAdmissions`'s departments --
#' the same picture lands at 5.84pt: still silent, but with 17% of headroom left and
#' one longer dimname from warning at its own defaults. Eliding to a legible picture
#' and saying so in the note is what this package does to a 30-row matrix, and it is
#' what it does here. `max_slices = 6` (or `show_all`) buys the sixth department
#' back, at a size the user has then chosen.
#'
#' @param data An array of rank 3 or more. Rank 2 never reaches here -- see
#'   [paint_cells()].
#' @param max_slices Most blocks drawn along EACH slice axis, the `"..."` block
#'   included.
#' @inheritParams paint_cells
#'
#' @return A bare cell table, as [paint_cells()] returns.
#'
#' @keywords internal
#' @noRd
array_cells <- function(data,
                        highlight_area = NULL,
                        highlight_color = "lemonchiffon",
                        show_indices = "none",
                        show_dimnames = "all",
                        sigfig = 3L,
                        subtle_digits = "insignificant",
                        max_chars = 12L,
                        max_name_chars = 8L,
                        max_dec_width = 13L,
                        max_rows = NULL,
                        max_cols = NULL,
                        max_slices = NULL,
                        show_all = FALSE,
                        ellipsis = "...") {
  d <- dim(data)
  n_ax <- length(d)

  if (any(d == 0L)) {
    stop("Cannot paint an empty data structure.")
  }
  n_cell <- prod(as.double(d))
  if (n_cell > 1e5) {
    stop(
      "The array is ", paste(d, collapse = " by "), ", which holds ",
      format(n_cell, scientific = FALSE),
      " values -- more than the 100000 cells paintr will draw."
    )
  }

  # -- the caps. An array's block is a matrix, but there are up to sixteen of them
  # side by side, so the block cannot afford a matrix's 20 by 15: the picture's
  # width is `max_slices` times its block's. 10 by 8 is the list's calibration, for
  # the same reason the list took it -- the columns are as wide as a NAME.
  if (is.null(max_rows)) max_rows <- 10L
  if (is.null(max_cols)) max_cols <- 8L
  if (is.null(max_slices)) max_slices <- 4L

  # -- lanes. The vocabulary is the matrix's, because a block IS a matrix.
  show_indices <- check_show_indices(show_indices)
  idx_cell <- any(show_indices %in% c("cell", "all"))
  idx_row <- any(show_indices %in% c("row", "all"))
  idx_col <- any(show_indices %in% c("column", "all"))

  # `"slice"` is the third lane, and it is a lane like the other two: it decides
  # whether the block titles read `, , Male, Child` or `, , 1, 1`. The precedence
  # rule is PR 1's, unchanged -- an index lane the caller asks for WINS the axis it
  # names -- and the slice axis has no index lane to lose to, so it simply follows
  # `show_dimnames`.
  show_dimnames <- check_lanes(
    show_dimnames, c("none", "row", "column", "slice", "all"), "show_dimnames"
  )
  nm_row <- if (any(show_dimnames %in% c("row", "all"))) axis_names(data, 1L) else NULL
  nm_col <- if (any(show_dimnames %in% c("column", "all"))) axis_names(data, 2L) else NULL
  slice_named <- any(show_dimnames %in% c("slice", "all"))
  if (idx_row) nm_row <- NULL
  if (idx_col) nm_col <- NULL

  lane_row <- idx_row || !is.null(nm_row)
  lane_col <- idx_col || !is.null(nm_col)

  # -- elide FIRST, on shape, on all four axes ------------------------------
  hi <- if (n_ax >= 4L) d[4:n_ax] else integer(0)
  n_bx_data <- d[[3L]]
  n_by_data <- if (length(hi)) as.integer(prod(hi)) else 1L

  elide_one <- function(n, max_n) {
    if (isTRUE(show_all)) {
      list(keep = seq_len(n), gap = NA_integer_, hidden = 0L)
    } else {
      elide_index(n, max_n)
    }
  }
  er <- elide_one(d[[1L]], max_rows)
  ec <- elide_one(d[[2L]], max_cols)
  ex <- elide_one(n_bx_data, max_slices)
  ey <- elide_one(n_by_data, max_slices)
  ki <- er$keep
  kj <- ec$keep
  kx <- ex$keep
  ky <- ey$keep

  # -- the geometry of one block --------------------------------------------
  # A block is a matrix with a title row on top of it. Its lanes are the matrix's,
  # laid out exactly as `paint_cells()` lays them: the label lane above, the gutter
  # to the left.
  lab_rows_b <- 1L + as.integer(lane_col)
  lab_cols_b <- as.integer(lane_row)
  blk_rows <- lab_rows_b + length(ki) + as.integer(!is.na(er$gap))
  blk_cols <- lab_cols_b + length(kj) + as.integer(!is.na(ec$gap))

  # -- the geometry of the grid of blocks -----------------------------------
  # A drawn block SLOT is a real block, or the `"..."` that stands for the ones that
  # were elided. The gap slot is one lane wide, not a whole block wide: it announces
  # a hidden block, it does not reserve room for one.
  bx_pos <- drawn_pos(kx, ex$gap)
  by_pos <- drawn_pos(ky, ey$gap)
  n_bx <- length(kx) + as.integer(!is.na(ex$gap))
  n_by <- length(ky) + as.integer(!is.na(ey$gap))

  slot_w <- rep(blk_cols, n_bx)
  if (!is.na(ex$gap)) slot_w[[ex$gap]] <- 1L
  slot_h <- rep(blk_rows, n_by)
  if (!is.na(ey$gap)) slot_h[[ey$gap]] <- 1L

  # One blank column between block slots, so two blocks never share an edge and read
  # as one grid. No blank ROW is needed: the next block's title row is itself a row
  # of white space with a short label at its left, and it separates them.
  col0_of <- cumsum(c(1L, slot_w + 1L))[seq_len(n_bx)]
  row0_of <- cumsum(c(1L, slot_h))[seq_len(n_by)]
  n_col <- sum(slot_w) + (n_bx - 1L)
  n_row <- sum(slot_h)

  # -- format the VISIBLE values, ONCE, as one unit -------------------------
  # The blocks are walked in the order they will be emitted, their kept values
  # concatenated, and the whole run handed to ONE `paint_format()` call. That call
  # is what makes the array one formatting unit -- see this function's docs.
  blocks <- expand.grid(bx = seq_along(kx), by = seq_along(ky))
  slice_of <- lapply(seq_len(nrow(blocks)), function(b) {
    c(kx[[blocks$bx[[b]]]], slice_sub(ky[[blocks$by[[b]]]], hi))
  })
  # `expand.grid(ri, ci)` is the order every rectangular structure's value chunk
  # comes back in, and a block is a rectangle, so it is this one's too.
  g <- expand.grid(ri = seq_along(ki), ci = seq_along(kj))
  vals_of <- lapply(slice_of, function(s) {
    as.vector(do.call(`[`, c(list(data), list(ki, kj), as.list(s), list(drop = TRUE))))
  })
  f <- paint_format(
    unlist(vals_of, use.names = FALSE),
    sigfig = sigfig, max_chars = max_chars, max_dec_width = max_dec_width,
    subtle_digits = subtle_digits, ellipsis = ellipsis
  )

  mask <- resolve_highlight_dim(highlight_area, d)
  mask_of <- lapply(slice_of, function(s) {
    as.vector(do.call(`[`, c(list(mask), list(ki, kj), as.list(s), list(drop = TRUE))))
  })

  n_val <- nrow(g)
  chunks <- vector("list", 0L)
  add <- function(x) if (!is.null(x)) chunks[[length(chunks) + 1L]] <<- x

  for (b in seq_len(nrow(blocks))) {
    s <- slice_of[[b]]
    c0 <- col0_of[[bx_pos[[blocks$bx[[b]]]]]]
    r0 <- row0_of[[by_pos[[blocks$by[[b]]]]]]

    # The subscript tail this block's labels carry: "" for a matrix, ", 3" for a
    # 3-D array's third slab, ", 3, 2" for a 4-D one's. THE THREE FORMULAE BELOW ARE
    # RANK-AGNOSTIC because of it -- at rank 2 the tail is empty and they collapse
    # to `[1, ]`, `[, 2]` and `[1, 2]`, which is exactly what a matrix draws.
    tail_s <- paste0(", ", paste(s, collapse = ", "))

    cols_of <- c0 + lab_cols_b + drawn_pos(kj, ec$gap) - 1L
    rows_of <- r0 + lab_rows_b + drawn_pos(ki, er$gap) - 1L
    val0_r <- r0 + lab_rows_b
    val0_c <- c0 + lab_cols_b

    # THE SLICE TITLE, and it is what `print()` writes: `, , Male, Child`. It SPANS
    # its block -- `col_end` says so -- which is what lets it demand no width from
    # any single column and still be fitted against a width it can live in. Left
    # aligned, at the block's leading edge, exactly where `print()` puts it.
    add(cell_rows(
      kind = "slicelabel",
      row = r0, col = c0, col_end = c0 + blk_cols - 1L,
      sig = paste0(", , ", paste(
        vapply(
          seq_along(s),
          function(q) {
            nm <- if (slice_named) axis_names(data, q + 2L) else NULL
            if (is.null(nm)) as.character(s[[q]]) else nm[[s[[q]]]]
          },
          character(1)
        ),
        collapse = ", "
      )),
      ink = "grey30", align = "left", size_rel = 0.9
    ))

    if (lane_col) {
      add(cell_rows(
        kind = "collabel",
        row = r0 + 1L, col = cols_of, j = kj,
        sig = lane_text(
          nms = nm_col[kj],
          idx = paste0("[, ", kj, tail_s, "]"),
          max_chars = max_name_chars, ellipsis = ellipsis
        ),
        ink = if (!is.null(nm_col)) "black" else "grey40",
        align = "center",
        size_rel = if (!is.null(nm_col)) 0.9 else 0.8
      ))
    }

    if (lane_row) {
      add(cell_rows(
        kind = "rowlabel",
        row = rows_of, col = c0, i = ki,
        sig = lane_text(
          nms = nm_row[ki],
          idx = paste0("[", ki, ", ", tail_s, "]"),
          max_chars = max_chars, ellipsis = ellipsis
        ),
        ink = if (!is.null(nm_row)) "black" else "grey40",
        align = "right",
        size_rel = if (!is.null(nm_row)) 0.9 else 0.8
      ))
    }

    k <- (b - 1L) * n_val + seq_len(n_val)
    add(cell_rows(
      kind = "value",
      row = rows_of[g$ri], col = cols_of[g$ci],
      i = ki[g$ri], j = kj[g$ci],
      # ONE unit for the whole array.
      fmt_group = 1L,
      sig = f$sig[k], insig = f$insig[k], head = f$head[k], tail = f$tail[k],
      ink = f$ink[k],
      fill = ifelse(mask_of[[b]], highlight_color, "white"),
      border = "black",
      align = f$align[k], size_rel = 1, fit = TRUE
    ))

    if (idx_cell) {
      add(cell_rows(
        kind = "cellindex",
        row = rows_of[g$ri], col = cols_of[g$ci],
        i = ki[g$ri], j = kj[g$ci],
        sig = paste0("[", ki[g$ri], ", ", kj[g$ci], tail_s, "]"),
        ink = "grey50", align = "center", size_rel = 0.7,
        dy_rel = cellindex_dy
      ))
    }

    # A block's cells are all the same depth, so a block is always a rectangle and
    # always gets its outline. It carries its own extent, so the four blocks get
    # four boxes and not one box around all of them.
    add(cell_rows(
      kind = "outline",
      row = val0_r, col = val0_c,
      row_end = r0 + blk_rows - 1L, col_end = c0 + blk_cols - 1L,
      border = "black", lwd = outline_lwd, align = "center", fit = FALSE
    ))

    # The row and column gaps, inside the block, exactly as a matrix draws them: the
    # gutter shows the row gap too.
    gr <- if (is.na(er$gap)) NULL else r0 + lab_rows_b + er$gap - 1L
    gc <- if (is.na(ec$gap)) NULL else c0 + lab_cols_b + ec$gap - 1L
    if (!is.null(gr) || !is.null(gc)) {
      grd <- expand.grid(
        row = seq.int(val0_r, r0 + blk_rows - 1L),
        col = seq.int(val0_c, c0 + blk_cols - 1L)
      )
      hit_r <- if (is.null(gr)) rep(FALSE, nrow(grd)) else grd$row == gr
      hit_c <- if (is.null(gc)) rep(FALSE, nrow(grd)) else grd$col == gc
      grd <- grd[hit_r | hit_c, , drop = FALSE]
      if (lane_row && !is.null(gr)) {
        grd <- rbind(data.frame(row = gr, col = c0), grd)
      }
      add(cell_rows(
        kind = "ellipsis", row = grd$row, col = grd$col,
        sig = ellipsis, ink = "grey50", align = "center", size_rel = 1, fit = FALSE
      ))
    }
  }

  # THE GAP BLOCKS. The package's contract is that the gap is ALWAYS drawn, and a
  # hidden slice is a hidden slice whichever axis hid it. One `"..."` per gap slot
  # per drawn block on the other axis, centred on the blocks it stands between.
  if (!is.na(ex$gap)) {
    gc <- col0_of[[ex$gap]]
    add(cell_rows(
      kind = "ellipsis",
      row = row0_of[by_pos[seq_along(ky)]] + lab_rows_b + (length(ki) - 1L) %/% 2L,
      col = gc,
      sig = ellipsis, ink = "grey50", align = "center", size_rel = 1, fit = FALSE
    ))
  }
  if (!is.na(ey$gap)) {
    gr <- row0_of[[ey$gap]]
    add(cell_rows(
      kind = "ellipsis",
      row = gr,
      col = col0_of[bx_pos[seq_along(kx)]] + lab_cols_b + (length(kj) - 1L) %/% 2L,
      sig = ellipsis, ink = "grey50", align = "center", size_rel = 1, fit = FALSE
    ))
  }

  out <- do.call(rbind, chunks)
  rownames(out) <- NULL

  # What the two slice axes hid, together: a 4-D array elides on both at once, and
  # "2 more slices, 1 more slice" would be nonsense. What the reader wants is how
  # many BLOCKS are not on the page, which is every block the data has, less every
  # block that was drawn. See `elide_note()`.
  hidden_slices <- as.integer(
    n_bx_data * n_by_data - length(kx) * length(ky)
  )

  attr(out, "n_row") <- as.integer(n_row)
  attr(out, "n_col") <- as.integer(n_col)
  attr(out, "hidden_rows") <- as.integer(er$hidden)
  attr(out, "hidden_cols") <- as.integer(ec$hidden)
  attr(out, "hidden_slices") <- hidden_slices
  attr(out, "note") <- elide_note(
    er$hidden, ec$hidden,
    is_vec = FALSE, nouns = c("row", "column"),
    hidden_slices = hidden_slices
  )
  out
}

# ---------------------------------------------------------------------------
# column widths
# ---------------------------------------------------------------------------

#' Width of every drawn column, in row-height units
#'
#' A row is one unit tall, so a column of width `1` is a square cell. Width is
#' demanded by content: a column that has to hold `"Sepal.Length"` is wider than
#' one that holds `"1"`. The estimate is deliberately device-free -- character
#' counts, not `strwidth()` -- because the cell table must be identical on every
#' device.
#'
#' Columns that share a formatting unit share a width. That is the same
#' `fmt_group` rule again, and it is what keeps a matrix's cells square and equal
#' while letting a data frame's columns differ.
#'
#' @param cells A cell table from [paint_cells()].
#' @param char_w Width of one character, in row-height units, at `size_rel = 1`.
#' @param pad Total horizontal padding of a cell, in row-height units.
#' @param min_w Narrowest a column may be. `1` keeps a matrix of short values
#'   exactly square.
#'
#' @return A numeric vector of length `max(cells$col)`.
#'
#' @keywords internal
#' @noRd
column_widths <- function(cells, char_w = 0.35, pad = 0.3, min_w = 1) {
  n_col <- max(cells$col)

  demand <- nchar(paste0(cells$sig, cells$insig)) * cells$size_rel * char_w + pad

  # TWO CELLS DEMAND NOTHING, AND NEITHER RULE NAMES A `kind`. They used to be one
  # rule that did (`demand[cells$kind == "outline"] <- 0`), and a rule that names a
  # kind is a rule that has to be extended every time a kind is added -- which is
  # the failure mode this whole file is built to avoid.
  #
  #   1. A cell that draws NO INK needs no width. That is the outline, whose `sig`
  #      and `insig` are both empty: it is a rectangle, not text. Stated this way it
  #      needs no list of kinds, and it is exactly the predicate `inked_cells()`
  #      already uses to decide what to draw.
  #
  #   2. A cell that SPANS columns demands nothing from any SINGLE one of them.
  #
  # RULE 2 IS LOAD-BEARING AND IT IS NOT AN OPTIMISATION. A slice title spans its
  # block, and the only other place to charge its width is its leftmost column --
  # which for an array is a VALUE column, in the single formatting unit that every
  # value column of every block shares. `out[k] <- max(raw[k])` below would then
  # propagate the title's width into EVERY value column of EVERY block, and a
  # 4x2x2x2 table of two-digit counts would draw cells wide enough to hold
  # ", , Child, No". The title would bloat the whole picture to say nothing.
  #
  # A spanning cell is not unconstrained, though -- it is fitted against the width
  # it ACTUALLY spans, by `span_widths()`, which is what keeps it inside its block.
  demand[!nzchar(cells$sig) & !nzchar(cells$insig)] <- 0
  demand[cells$col_end > cells$col] <- 0

  raw <- vapply(
    seq_len(n_col),
    function(jj) {
      d <- demand[cells$col == jj]
      if (length(d) == 0L) 0 else max(d)
    },
    numeric(1)
  )

  # Which formatting unit does each drawn column hold?
  grp <- vapply(
    seq_len(n_col),
    function(jj) {
      gg <- cells$fmt_group[cells$col == jj & cells$kind == "value"]
      gg <- gg[!is.na(gg)]
      if (length(gg) == 0L) NA_integer_ else gg[1L]
    },
    integer(1)
  )

  out <- raw
  for (gg in unique(grp[!is.na(grp)])) {
    k <- which(!is.na(grp) & grp == gg)
    out[k] <- max(raw[k])
  }

  pmax(min_w, out)
}
