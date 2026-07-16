# THE HOT PATH OF paint_cells(), PINNED.
#
# Every picture the package draws is built by two lines of `paint_cells()`:
#
#     g    <- expand.grid(ri = seq_along(ki), ci = seq_along(kj))
#     i_v  <- ki[g$ri];  j_v <- kj[g$ci]
#
# and everything that hangs off them -- the drawn (row, col) of every value, the
# order the formatted spans come back in, the `fmt_group` each value belongs to,
# and the way the highlight mask is sliced -- is that one column-major ordering,
# reused four times. There is no test that says so, and so the ordering can be
# rewritten and nothing will notice until a picture comes out transposed.
#
# This file says so. It rebuilds the value chunk from `elide_index()`,
# `drawn_pos()` and a literal `expand.grid()`, for every rectangular structure the
# package draws, elided and not, in every layout -- and asserts the cell table
# matches it cell for cell. It is deliberately an INDEPENDENT reconstruction and
# not a snapshot: a snapshot pins what the code did, and this has to pin what the
# code MEANS, so that a builder rewritten to serve a ragged structure can be shown
# to be a no-op for every rectangular one.
#
# If a change to the builder breaks this file, the change is not a no-op.

# The cell table reports its own label lanes: the block's first value sits at
# (lab_rows + 1, lab_cols + 1) by construction, whatever lanes were switched on.
lab_offsets <- function(cells) {
  v <- cells[cells$kind == "value", , drop = FALSE]
  list(rows = min(v$row) - 1L, cols = min(v$col) - 1L)
}

# The data's true extent, before elision -- the two numbers elision is allowed to
# depend on, and the only two.
data_extent <- function(data, layout = "vertical") {
  if (is.data.frame(data) || !is.null(dim(data))) {
    return(list(n_row = nrow(data), n_col = ncol(data)))
  }
  if (layout == "vertical") {
    list(n_row = length(data), n_col = 1L)
  } else {
    list(n_row = 1L, n_col = length(data))
  }
}

# THE RECONSTRUCTION. `expand.grid()`, spelled out, exactly as the builder does it.
expect_expand_grid_order <- function(data, ..., layout = "vertical",
                                     max_rows = NULL, max_cols = NULL,
                                     show_all = FALSE, label = "") {
  cells <- paint_cells(
    data, layout = layout, max_rows = max_rows, max_cols = max_cols,
    show_all = show_all, ...
  )

  is_df <- is.data.frame(data)
  is_vec <- !is_df && is.null(dim(data))
  ext <- data_extent(data, layout)

  if (is.null(max_rows)) max_rows <- if (is_df) 10L else 20L
  if (is.null(max_cols)) max_cols <- if (is_df) 10L else 15L

  er <- if (show_all) {
    list(keep = seq_len(ext$n_row), gap = NA_integer_, hidden = 0L)
  } else {
    elide_index(ext$n_row, max_rows)
  }
  ec <- if (show_all) {
    list(keep = seq_len(ext$n_col), gap = NA_integer_, hidden = 0L)
  } else {
    elide_index(ext$n_col, max_cols)
  }
  ki <- er$keep
  kj <- ec$keep

  lab <- lab_offsets(cells)
  row_of <- drawn_pos(ki, er$gap) + lab$rows
  col_of <- drawn_pos(kj, ec$gap) + lab$cols

  # Column-major: `ri` varies fastest. This IS the builder.
  g <- expand.grid(ri = seq_along(ki), ci = seq_along(kj))

  v <- cells[cells$kind == "value", , drop = FALSE]

  expect_equal(nrow(v), nrow(g), info = label)
  expect_equal(v$i, as.integer(ki[g$ri]), info = label)
  expect_equal(v$j, as.integer(kj[g$ci]), info = label)
  expect_equal(v$row, as.integer(row_of[g$ri]), info = label)
  expect_equal(v$col, as.integer(col_of[g$ci]), info = label)

  # The formatting unit each value belongs to: one per column for a data frame,
  # one for the whole of a matrix or a vector.
  expect_equal(
    v$fmt_group,
    if (is_df) as.integer(rep(seq_along(kj), each = length(ki))) else rep(1L, nrow(g)),
    info = label
  )

  # The formatted spans come back in the SAME column-major order, and the cell
  # table simply lays them down. Re-format the visible slice and compare.
  if (is_df) {
    want <- do.call(rbind, lapply(kj, function(cc) paint_format(strip_asis(data[[cc]])[ki])))
  } else if (is_vec) {
    want <- paint_format(data[if (layout == "vertical") ki else kj])
  } else {
    want <- paint_format(data[ki, kj, drop = FALSE])
  }
  expect_equal(v$sig, want$sig, info = label)
  expect_equal(v$insig, want$insig, info = label)
  expect_equal(v$head, want$head, info = label)
  expect_equal(v$tail, want$tail, info = label)
  expect_equal(v$ink, want$ink, info = label)
  expect_equal(v$align, want$align, info = label)

  # The drawn extent, and the "# n more rows" arithmetic that hangs off it.
  expect_equal(
    attr(cells, "n_row"),
    as.integer(lab$rows + length(ki) + !is.na(er$gap)),
    info = label
  )
  expect_equal(
    attr(cells, "n_col"),
    as.integer(lab$cols + length(kj) + !is.na(ec$gap)),
    info = label
  )
  expect_equal(attr(cells, "hidden_rows"), as.integer(er$hidden), info = label)
  expect_equal(attr(cells, "hidden_cols"), as.integer(ec$hidden), info = label)

  # THE GAP. A rectangle elides a WHOLE row and a WHOLE column of the drawn block
  # -- every drawn column shows the row gap, every drawn row shows the column gap
  # -- plus the row gutter when there is one. Pin that too: the gap builder is the
  # second half of the same arithmetic.
  gap_row <- if (is.na(er$gap)) NA_integer_ else er$gap + lab$rows
  gap_col <- if (is.na(ec$gap)) NA_integer_ else ec$gap + lab$cols
  gaps <- cells[cells$kind == "ellipsis", c("row", "col"), drop = FALSE]
  if (is.na(gap_row) && is.na(gap_col)) {
    expect_equal(nrow(gaps), 0L, info = label)
  } else {
    n_row_drawn <- lab$rows + length(ki) + !is.na(er$gap)
    n_col_drawn <- lab$cols + length(kj) + !is.na(ec$gap)
    grd <- expand.grid(
      row = seq.int(lab$rows + 1L, n_row_drawn),
      col = seq.int(lab$cols + 1L, n_col_drawn)
    )
    hit <- (!is.na(gap_row) & grd$row == gap_row) | (!is.na(gap_col) & grd$col == gap_col)
    want_gap <- grd[hit, , drop = FALSE]
    if (lab$cols > 0L && !is.na(gap_row)) {
      want_gap <- rbind(data.frame(row = gap_row, col = 1L), want_gap)
    }
    key <- function(d) sort(paste(d$row, d$col, sep = ":"))
    expect_equal(key(gaps), key(want_gap), info = label)
  }

  invisible(cells)
}

# ---------------------------------------------------------------------------
# every rectangular structure, elided and not, in every layout
# ---------------------------------------------------------------------------

test_that("a vector's values are laid down in expand.grid order", {
  v <- c(-3, 5.25, NA, Inf, 2, 1)
  long <- seq_len(40)
  named <- c(alpha = 1, beta = 2, gamma = 3)

  expect_expand_grid_order(v, label = "vertical")
  expect_expand_grid_order(v, layout = "horizontal", label = "horizontal")
  expect_expand_grid_order(long, label = "vertical elided")
  expect_expand_grid_order(long, layout = "horizontal", label = "horizontal elided")
  expect_expand_grid_order(long, show_all = TRUE, label = "show_all")
  expect_expand_grid_order(named, label = "named")
  expect_expand_grid_order(named, layout = "horizontal", label = "named horizontal")
  expect_expand_grid_order(v, show_indices = "outside", label = "outside")
  expect_expand_grid_order(v, show_indices = "inside", label = "inside")
  expect_expand_grid_order(letters, label = "character elided")
  expect_expand_grid_order(long, max_rows = 5L, label = "tight max_rows")
})

test_that("a matrix's values are laid down in expand.grid order", {
  m <- matrix(1:12, nrow = 4)
  big <- matrix(seq_len(30 * 20), nrow = 30)
  dn <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))

  expect_expand_grid_order(m, label = "small")
  expect_expand_grid_order(big, label = "elided both ways")
  expect_expand_grid_order(big, show_all = TRUE, label = "show_all")
  expect_expand_grid_order(matrix(seq_len(30 * 3), nrow = 30), label = "rows elided only")
  expect_expand_grid_order(matrix(seq_len(3 * 30), nrow = 3), label = "columns elided only")
  expect_expand_grid_order(dn, label = "dimnamed")
  expect_expand_grid_order(dn, show_dimnames = "row", label = "row names only")
  expect_expand_grid_order(m, show_indices = "all", label = "all indices")
  expect_expand_grid_order(big, show_indices = c("row", "column"), label = "gutters, elided")
  expect_expand_grid_order(big, max_rows = 6L, max_cols = 4L, label = "tight caps")
})

test_that("a data frame's values are laid down in expand.grid order", {
  expect_expand_grid_order(head(iris, 5), label = "iris head")
  expect_expand_grid_order(iris, label = "iris elided")
  expect_expand_grid_order(mtcars, label = "mtcars, row-name gutter")
  expect_expand_grid_order(mtcars, show_rownames = FALSE, label = "no gutter")
  expect_expand_grid_order(head(iris, 5), show_types = FALSE, label = "no type lane")
  expect_expand_grid_order(head(iris, 5), show_names = FALSE, label = "no header lane")
  expect_expand_grid_order(iris, show_indices = "all", label = "all indices, elided")
  expect_expand_grid_order(head(mtcars, 3), show_all = TRUE, label = "show_all")
  expect_expand_grid_order(iris, max_rows = 4L, max_cols = 3L, label = "tight caps")
})

# ---------------------------------------------------------------------------
# the highlight mask is sliced in the SAME order
# ---------------------------------------------------------------------------
#
# `mask_vis <- mask[ki, kj, drop = FALSE]` and then `as.vector()`, which is
# column-major -- the fill of the k-th value cell is the k-th entry of the sliced
# mask, and it is the fourth consumer of the one ordering. Slice it independently
# and compare.

test_that("the highlight mask is sliced in the same column-major order", {
  cases <- list(
    list(data = matrix(1:12, nrow = 4), rows = 2, cols = NULL),
    list(data = matrix(seq_len(30 * 20), nrow = 30), rows = c(1, 30), cols = 20),
    list(data = head(iris, 5), rows = c(2, 4), cols = NULL),
    list(data = iris, rows = NULL, cols = 3)
  )
  for (case in cases) {
    d <- case$data
    mask <- highlight_data(d, rows = case$rows, columns = case$cols)
    cells <- paint_cells(d, highlight_area = mask)

    ext <- data_extent(d)
    is_df <- is.data.frame(d)
    er <- elide_index(ext$n_row, if (is_df) 10L else 20L)
    ec <- elide_index(ext$n_col, if (is_df) 10L else 15L)
    want <- as.vector(mask[er$keep, ec$keep, drop = FALSE])

    v <- cells[cells$kind == "value", , drop = FALSE]
    expect_equal(v$fill, ifelse(want, "lemonchiffon", "white"))
  }
})

test_that("a vector's highlight mask is sliced in the same order", {
  v <- seq_len(40)
  mask <- highlight_data(v, locations = c(1, 5, 40))
  for (lay in c("vertical", "horizontal")) {
    cells <- paint_cells(v, layout = lay, highlight_area = mask)
    er <- elide_index(if (lay == "vertical") 40L else 1L, 20L)
    ec <- elide_index(if (lay == "vertical") 1L else 40L, 15L)
    m <- matrix(mask, nrow = if (lay == "vertical") 40L else 1L)
    want <- as.vector(m[er$keep, ec$keep, drop = FALSE])
    vv <- cells[cells$kind == "value", , drop = FALSE]
    expect_equal(vv$fill, ifelse(want, "lemonchiffon", "white"), info = lay)
  }
})

# ---------------------------------------------------------------------------
# the in-cell index rides the same ordering
# ---------------------------------------------------------------------------

test_that("the in-cell index shares the values' ordering", {
  for (d in list(matrix(1:12, nrow = 4), head(iris, 4), seq_len(6))) {
    cells <- paint_cells(d, show_indices = if (is.null(dim(d)) && !is.data.frame(d)) "inside" else "cell")
    v <- cells[cells$kind == "value", , drop = FALSE]
    ci <- cells[cells$kind == "cellindex", , drop = FALSE]
    expect_equal(ci$i, v$i)
    expect_equal(ci$j, v$j)
    expect_equal(ci$row, v$row)
    expect_equal(ci$col, v$col)
  }
})
