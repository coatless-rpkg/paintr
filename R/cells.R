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
#' @param hidden_rows,hidden_cols Counts from [elide_index()].
#' @param is_vec Is the structure a vector? Then it has elements, not rows and
#'   columns.
#'
#' @return A length-one character string, or `NA_character_` when nothing is
#'   hidden.
#'
#' @keywords internal
#' @noRd
elide_note <- function(hidden_rows, hidden_cols, is_vec = FALSE) {
  if (isTRUE(is_vec)) {
    n <- hidden_rows + hidden_cols
    if (n <= 0L) {
      return(NA_character_)
    }
    return(paste0("# ", n, " more element", if (n != 1L) "s" else ""))
  }

  parts <- character(0)
  if (hidden_rows > 0L) {
    parts <- c(parts, paste0(hidden_rows, " more row", if (hidden_rows != 1L) "s" else ""))
  }
  if (hidden_cols > 0L) {
    parts <- c(parts, paste0(hidden_cols, " more column", if (hidden_cols != 1L) "s" else ""))
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
#'   `"header"`, `"type"`, `"cellindex"`, `"ellipsis"`.
#' @param row,col Drawn positions, 1-based, row 1 at the top.
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
  data.frame(
    i = rep_len(as.integer(i), n),
    j = rep_len(as.integer(j), n),
    row = rep_len(as.integer(row), n),
    col = rep_len(as.integer(col), n),
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

#' Validate `show_indices` for a matrix or a data frame
#'
#' A grid takes a *vector* here, because its index lanes are independent: a caller
#' can ask for `c("row", "column")` and get both, which is what the README does.
#' That rules out `match.arg()`, which is length-one by construction -- widening
#' this argument back out to a vector is the whole point of this function.
#'
#' The lanes are then chosen with `any(... %in% ...)`, so a value that is not
#' "none" turns its lane on regardless of what else is in the vector. `"none"`
#' therefore does not veto its companions: `c("none", "row")` draws row indices.
#' It is a contradictory request, and the non-trivial half of it wins.
#'
#' A vector's `show_indices` is a different, mutually exclusive vocabulary
#' ("inside"/"outside"), and stays length-one under `match.arg()`.
#'
#' @param show_indices What the caller passed.
#'
#' @return `show_indices`, unchanged, when it is valid.
#'
#' @keywords internal
#' @noRd
check_show_indices <- function(show_indices) {
  valid <- c("none", "cell", "row", "column", "all")
  ok <- is.character(show_indices) &&
    length(show_indices) > 0L &&
    !anyNA(show_indices) &&
    all(show_indices %in% valid)
  if (!ok) {
    got <- if (length(show_indices) == 0L) {
      "empty"
    } else if (is.character(show_indices)) {
      paste0("'", show_indices, "'", collapse = ", ")
    } else {
      paste0("a length-", length(show_indices), " ", class(show_indices)[1L])
    }
    stop(
      "`show_indices` must be one or more of 'none', 'cell', 'row', 'column', or 'all', ",
      "but it was ", got, ". Ask for several at once with a vector, ",
      "such as c(\"row\", \"column\")."
    )
  }
  show_indices
}

#' Build the cell table
#'
#' The one builder for all three structures. It elides first and formats second,
#' so the formatting unit is the *visible slice*: a hidden outlier cannot change
#' how the visible cells look.
#'
#' The formatting unit is carried as data, in `fmt_group`: `1L` for every cell of
#' a matrix or a vector (one unit, so the same value looks identical in every
#' cell), and the column's own ordinal for a data frame (one unit per column,
#' because a data frame's columns are independent variables).
#'
#' The result is a **bare** data frame. It deliberately has no class and no print
#' method: a print method is a trap, because `expect_snapshot()` would dispatch to
#' it and silently stop catching coordinate regressions.
#'
#' @param data A vector, matrix, or data frame.
#' @param highlight_area `NULL`, a length-one logical, or a logical mask shaped
#'   like `data`.
#' @param highlight_color The fill for a highlighted cell.
#' @param show_indices For a matrix or data frame, any number of `"none"`,
#'   `"cell"`, `"row"`, `"column"`, `"all"` -- the lanes are independent, so
#'   `c("row", "column")` turns on both. For a vector, exactly one of `"none"`,
#'   `"inside"`, `"outside"`, which are mutually exclusive.
#' @param layout Vectors only: `"vertical"` (an n by 1 grid) or `"horizontal"`.
#' @param show_names,show_types Data frames only: draw the column-name row and the
#'   type-tag row.
#' @param name_align,type_align Data frames only: `"center"`, `"left"` or
#'   `"right"`, for the column-name lane and the type-tag lane respectively. They
#'   are independent of each other and of the values, which keep their own
#'   alignment -- a numeric column's values stay decimal-anchored however its
#'   label is set.
#' @param sigfig,subtle_digits,max_chars,max_dec_width,ellipsis Passed to
#'   [paint_format()].
#' @param max_rows,max_cols Elision thresholds. `NULL` takes the default for the
#'   structure: 20 for a matrix or vector, 10 for a data frame (its columns are
#'   wide).
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
                        show_names = TRUE,
                        show_types = TRUE,
                        name_align = c("center", "left", "right"),
                        type_align = c("center", "left", "right"),
                        sigfig = 3L,
                        subtle_digits = c("insignificant", "rounded", "none"),
                        max_chars = 12L,
                        max_dec_width = 13L,
                        max_rows = NULL,
                        max_cols = NULL,
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
  dims <- dim(data)
  # A 1-D or 3-D array is neither a vector nor a grid. Catch it before it can be
  # silently flattened.
  if (!is_df && !is.null(dims) && length(dims) != 2L) {
    stop("paintr can only draw one- and two-dimensional structures.")
  }
  is_mat <- !is_df && !is.null(dims)
  is_vec <- !is_df && !is_mat

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

  if (n_row_data == 0L || n_col_data == 0L) {
    stop("Cannot paint an empty data structure.")
  }
  # Hard ceiling. It holds even under `show_all`.
  if (as.double(n_row_data) * as.double(n_col_data) > 1e5) {
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
  } else {
    # A grid takes a VECTOR: its three lanes are independent, so `c("row",
    # "column")` means both. `any()`, not `==`, is what makes that work.
    show_indices <- check_show_indices(show_indices)
    idx_cell <- any(show_indices %in% c("cell", "all"))
    idx_row <- any(show_indices %in% c("row", "all"))
    idx_col <- any(show_indices %in% c("column", "all"))
  }

  # -- elide FIRST ------------------------------------------------------------
  if (is.null(max_rows)) max_rows <- if (is_df) 10L else 20L
  if (is.null(max_cols)) max_cols <- if (is_df) 10L else 15L

  er <- if (isTRUE(show_all)) {
    list(keep = seq_len(n_row_data), gap = NA_integer_, hidden = 0L)
  } else {
    elide_index(n_row_data, max_rows)
  }
  ec <- if (isTRUE(show_all)) {
    list(keep = seq_len(n_col_data), gap = NA_integer_, hidden = 0L)
  } else {
    elide_index(n_col_data, max_cols)
  }
  ki <- er$keep
  kj <- ec$keep

  # -- the drawn grid ---------------------------------------------------------
  header_on <- is_df && isTRUE(show_names)
  type_on <- is_df && isTRUE(show_types)

  lane <- 0L
  collab_row <- NA_integer_
  header_row <- NA_integer_
  type_row <- NA_integer_
  if (idx_col) {
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
  lab_cols <- as.integer(idx_row)

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

  # Column-major: `ri` varies fastest, which is the order every paint_format()
  # result comes back in.
  g <- expand.grid(ri = seq_along(ki), ci = seq_along(kj))

  if (is_df) {
    fs <- lapply(seq_along(kj), function(cc) {
      do.call(paint_format, c(list(x = strip_asis(data[[kj[cc]]])[ki]), fmt_args))
    })
    f <- do.call(rbind, fs)
    # One formatting unit per column.
    fmt_group <- rep(seq_along(kj), each = length(ki))
    col_tag <- vapply(fs, function(z) attr(z, "tag"), character(1))
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

  mask <- resolve_highlight(highlight_area, n_row_data, n_col_data, is_vec)
  mask_vis <- mask[ki, kj, drop = FALSE]

  # -- the chunks -------------------------------------------------------------
  i_v <- ki[g$ri]
  j_v <- kj[g$ci]

  # The heavy border that makes the block read as one object. `lwd` is the whole
  # of what makes it heavy, and it is a column like any other -- so neither
  # renderer has to know that an "outline" exists to stroke it correctly.
  outline <- cell_rows(
    kind = "outline",
    row = lab_rows + 1L, col = lab_cols + 1L,
    border = "black", lwd = outline_lwd, align = "center",
    fit = FALSE
  )

  collabel <- NULL
  if (idx_col && length(kj) > 0L) {
    collabel <- cell_rows(
      kind = "collabel",
      row = collab_row, col = col_of,
      j = kj,
      sig = if (is_vec) paste0("[", kj, "]") else paste0("[, ", kj, "]"),
      ink = "grey40", align = "center", size_rel = 0.8
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
  header <- NULL
  if (header_on && length(kj) > 0L) {
    header <- cell_rows(
      kind = "header",
      row = header_row, col = col_of,
      j = kj,
      sig = truncate_chr(names(data)[kj], max_chars, ellipsis),
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
  if (idx_row && length(ki) > 0L) {
    rowlabel <- cell_rows(
      kind = "rowlabel",
      row = row_of, col = 1L,
      i = ki,
      sig = if (is_vec) paste0("[", ki, "]") else paste0("[", ki, ", ]"),
      ink = "grey40", align = "right", size_rel = 0.8
    )
  }

  value <- cell_rows(
    kind = "value",
    row = row_of[g$ri], col = col_of[g$ci],
    i = i_v, j = j_v,
    fmt_group = fmt_group,
    sig = f$sig, insig = f$insig, head = f$head, tail = f$tail,
    ink = f$ink,
    fill = ifelse(as.vector(mask_vis), highlight_color, "white"),
    border = "black",
    align = f$align, size_rel = 1, fit = TRUE
  )

  # The index shares its `(row, col)` with the value it names, so it is `dy_rel`
  # -- and only `dy_rel` -- that keeps the two from being stamped on top of each
  # other. Without it the renderers centre both in the same cell and draw
  # `[1` + `10` + `1]` as one illegible smear.
  cellindex <- NULL
  if (idx_cell && nrow(value) > 0L) {
    cellindex <- cell_rows(
      kind = "cellindex",
      row = row_of[g$ri], col = col_of[g$ci],
      i = i_v, j = j_v,
      sig = if (is_vec) {
        paste0("[", if (layout == "vertical") i_v else j_v, "]")
      } else {
        paste0("[", i_v, ", ", j_v, "]")
      },
      ink = "grey50", align = "center", size_rel = 0.7,
      dy_rel = cellindex_dy
    )
  }

  # The gap is ordinary cell rows. No renderer ever learns that elision exists.
  gap <- NULL
  if (!is.na(gap_row) || !is.na(gap_col)) {
    grd <- expand.grid(
      row = seq.int(lab_rows + 1L, n_row),
      col = seq.int(lab_cols + 1L, n_col)
    )
    hit <- (!is.na(gap_row) & grd$row == gap_row) | (!is.na(gap_col) & grd$col == gap_col)
    grd <- grd[hit, , drop = FALSE]
    # The row-label gutter shows the gap too, exactly as a tibble does.
    if (idx_row && !is.na(gap_row)) {
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

  attr(out, "n_row") <- as.integer(n_row)
  attr(out, "n_col") <- as.integer(n_col)
  attr(out, "hidden_rows") <- as.integer(er$hidden)
  attr(out, "hidden_cols") <- as.integer(ec$hidden)
  # `is_vec` is threaded in because a vector has ELEMENTS. The grid it is drawn on
  # is the renderer's business, not the student's.
  attr(out, "note") <- elide_note(er$hidden, ec$hidden, is_vec)
  out
}

#' The outline rectangle, as a drawn box
#'
#' The outline is one cell-table row, so it has one `(row, col)`: the top-left of
#' the value block. Its extent is the bottom-right of the block, which is the last
#' `"value"` or `"ellipsis"` cell. Keeping the arithmetic here means both renderers
#' get the same box.
#'
#' @param cells A cell table from [paint_cells()].
#'
#' @return `NULL` when there is no outline, else a list of `row0`, `col0`, `row1`,
#'   `col1`, all drawn positions, inclusive.
#'
#' @keywords internal
#' @noRd
outline_box <- function(cells) {
  o <- cells[cells$kind == "outline", , drop = FALSE]
  b <- cells[cells$kind %in% c("value", "ellipsis"), , drop = FALSE]
  if (nrow(o) == 0L || nrow(b) == 0L) {
    return(NULL)
  }
  list(
    row0 = o$row[1L], col0 = o$col[1L],
    row1 = max(b$row), col1 = max(b$col)
  )
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
  # The outline is a rectangle, not text; it demands nothing.
  demand[cells$kind == "outline"] <- 0

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
