# paint_list(). The structure the package could not draw at all: `paint_vector()`
# and `paint_data_frame()` both stopped, and there was no third painter.
#
# Three things are worth more than the rest of this file put together:
#
#   1. THE TYPE GATE. `is.list()` is TRUE for a POSIXlt, an lm, an htest and a by,
#      and a naive gate would draw `as.POSIXlt(Sys.time())` as eleven ragged columns
#      of sec/min/hour/... -- a wrong picture, drawn confidently.
#   2. THE RAGGED ELISION. A length-2 element must not draw a "..." announcing
#      values it does not have, and a length-12 element must not trail off into the
#      empty space under a length-40 one.
#   3. NESTING IS REFUSED. A sublist collapses to `<list [2]>` and stays there.

with_null_pdf <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

ragged <- list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA))

vals <- function(cells) {
  v <- cells[cells$kind == "value", , drop = FALSE]
  rownames(v) <- NULL
  v
}
lane <- function(cells, kind) cells[cells$kind == kind, , drop = FALSE]$sig

# ---------------------------------------------------------------------------
# the type gate
# ---------------------------------------------------------------------------

test_that("is_paint_list() accepts a bare list and refuses every classed one", {
  expect_true(is_paint_list(list(a = 1:3, b = "x")))
  expect_true(is_paint_list(list()))

  # All of these are is.list() TRUE. That is the entire point.
  expect_true(is.list(as.POSIXlt(Sys.time())))
  expect_true(is.list(stats::lm(mpg ~ cyl, mtcars)))
  expect_true(is.list(stats::t.test(1:10)))

  expect_false(is_paint_list(as.POSIXlt(Sys.time())))
  expect_false(is_paint_list(stats::lm(mpg ~ cyl, mtcars)))
  expect_false(is_paint_list(stats::t.test(1:10)))
  expect_false(is_paint_list(by(warpbreaks[, 1:2], warpbreaks[, "tension"], summary)))
  expect_false(is_paint_list(data.frame(a = 1)))
  # A list carrying a `dim` is a matrix of list cells, not a list of vectors.
  expect_false(is_paint_list(structure(list(1, 2, 3, 4), dim = c(2L, 2L))))
})

test_that("paint_list() refuses everything is_paint_list() refuses", {
  with_null_pdf({
    for (bad in list(
      as.POSIXlt(Sys.time()),
      stats::lm(mpg ~ cyl, mtcars),
      stats::t.test(1:10),
      by(warpbreaks[, 1:2], warpbreaks[, "tension"], summary),
      data.frame(a = 1),
      1:3
    )) {
      expect_error(paint_list(bad), "`list` type")
    }
  })
})

test_that("a datetime is refused rather than drawn as eleven ragged columns", {
  t <- as.POSIXlt("2026-07-14 12:00:00", tz = "UTC")
  expect_length(unclass(t), 11L)
  with_null_pdf(expect_error(paint_list(t), "`list` type"))
})

test_that("highlight_data()'s method set agrees with the gate exactly", {
  expect_no_error(highlight_data(list(a = 1:3), columns = 1))
  for (bad in list(
    as.POSIXlt(Sys.time()),
    stats::lm(mpg ~ cyl, mtcars),
    stats::t.test(1:10),
    by(warpbreaks[, 1:2], warpbreaks[, "tension"], summary)
  )) {
    expect_error(highlight_data(bad, rows = 1), "do not support")
  }
  # A tibble is a data frame, and BOTH accept it: the gate and the painter agree
  # about that too. It is built here rather than depended on -- a tibble IS this
  # object, and the test is about the class vector, not about the package.
  tb <- structure(
    list(a = 1:3),
    class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -3L)
  )
  expect_true(is.data.frame(tb))
  expect_false(is_paint_list(tb))
  expect_no_error(highlight_data(tb, columns = 1))
})

# ---------------------------------------------------------------------------
# the picture
# ---------------------------------------------------------------------------

test_that("paint_list() draws elements as columns and positions as rows", {
  cells <- paint_cells(ragged)
  v <- vals(cells)

  # One cell per value. NOT one per bounding-box cell: 4 + 1 + 3, not 4 * 3.
  expect_equal(nrow(v), 8L)
  expect_equal(as.vector(table(v$j)), c(4L, 1L, 3L))
  expect_equal(v$sig[v$j == 1], c("1", "2", "3", "4"))
  expect_equal(v$sig[v$j == 2], "x")
  expect_equal(v$sig[v$j == 3], c("TRUE", "FALSE", "NA"))

  # Each element is its own formatting unit, exactly as a data frame's column is.
  expect_equal(v$fmt_group, c(rep(1L, 4), 2L, rep(3L, 3)))
})

test_that("the header lane names the elements, and falls back per element", {
  expect_equal(lane(paint_cells(ragged), "header"), c("$a", "$b", "$c"))
  expect_equal(lane(paint_cells(list(1:3, b = 4)), "header"), c("[[1]]", "$b"))
  expect_equal(lane(paint_cells(list(1:3, 4:6)), "header"), c("[[1]]", "[[2]]"))
  # A name that is not a syntactic name is not typeable after a `$`.
  expect_equal(lane(paint_cells(list(`my name` = 1)), "header"), "[[\"my name\"]]")
  # A long name truncates at `max_chars`, as a data frame's does: the name is cut to
  # 12 characters, and the `$` that makes it an accessor is not part of the name.
  expect_equal(
    lane(paint_cells(list(supercalifragilistic = 1)), "header"),
    "$supercali..."
  )
  expect_length(lane(paint_cells(ragged, show_names = FALSE), "header"), 0L)
})

test_that("the type lane reads each element's own type", {
  expect_equal(lane(paint_cells(ragged), "type"), c("<int>", "<chr>", "<lgl>"))
  expect_length(lane(paint_cells(ragged, show_types = FALSE), "type"), 0L)
})

test_that("a RAGGED list draws no outline, because its block is not a rectangle", {
  # The heavy border is the BOUNDING BOX of the drawn cells. On a 4/1/3 list it
  # would run to the bottom of the deepest element, putting the length-1 element at
  # the top of a tall, empty, heavily-boxed column -- a box around cells that do not
  # exist. The cells keep their own borders, so the block still reads as a block.
  cells <- paint_cells(ragged)
  expect_equal(sum(cells$kind == "outline"), 0L)
  expect_null(outline_box(cells))
  # One element longer than the others is enough to break the rectangle.
  expect_equal(sum(paint_cells(list(a = 1:3, b = 4:6, c = 7:9, d = 10L))$kind == "outline"), 0L)
  # So is an element that is not drawn as cells at all: a matrix element is ONE
  # cell, and a column one cell deep beside a column three deep is ragged.
  expect_equal(sum(paint_cells(list(a = 1:3, b = matrix(1:4, 2)))$kind == "outline"), 0L)
})

test_that("an EQUAL-LENGTH list draws an outline, and that is the whole lesson", {
  # Give the elements a shared length and the rectangle closes; take it away and it
  # breaks. The outline keys on `col_len` -- a LENGTH, which is SHAPE -- and never on
  # a value, so elision's rule is untouched.
  cells <- paint_cells(list(a = 1:3, b = 4:6))
  expect_equal(sum(cells$kind == "outline"), 1L)
  o <- cells[cells$kind == "outline", ]
  expect_equal(o$lwd, outline_lwd)
  # It boxes the value block, top-left to bottom-right, exactly as a grid's does.
  expect_equal(outline_box(cells), list(row0 = 3L, col0 = 1L, row1 = 5L, col1 = 2L))

  # The SAME values, one of them one element shorter: no outline. The border is the
  # only thing that moved, and the length is the only thing that changed.
  expect_equal(sum(paint_cells(list(a = 1:3, b = 4:5))$kind == "outline"), 0L)

  # `summarise` draws every element as ONE cell, so the block is one row deep and
  # rectangular whatever the elements' lengths are.
  expect_equal(sum(paint_cells(ragged, summarise = TRUE)$kind == "outline"), 1L)
})

test_that("a data frame's outline is untouched by the list rule", {
  # The hot path. `col_len` is one number repeated for every rectangular structure,
  # so the question the outline asks is unconditionally TRUE for them and they draw
  # exactly the outline they always drew.
  for (data in list(head(iris, 3), matrix(1:6, 2), 1:5, head(mtcars, 2))) {
    cells <- paint_cells(data)
    o <- cells[cells$kind == "outline", ]
    expect_equal(nrow(o), 1L)
    expect_equal(o$lwd, outline_lwd)
    expect_false(is.null(outline_box(cells)))
  }
  # Elision does not change the answer: a data frame is a rectangle whether or not
  # its middle is drawn.
  expect_equal(sum(paint_cells(iris)$kind == "outline"), 1L)
})

# ---------------------------------------------------------------------------
# nesting is refused, and every non-vector element with it
# ---------------------------------------------------------------------------

test_that("a sublist collapses to a single <list [n]> token", {
  cells <- paint_cells(list(a = 1:3, b = list(1, 2)))
  v <- vals(cells)
  expect_equal(sum(v$j == 2), 1L)
  expect_equal(v$sig[v$j == 2], "<list [2]>")
  expect_equal(v$ink[v$j == 2], "grey50")
  expect_equal(lane(cells, "type"), c("<int>", "<list>"))
})

test_that("a matrix element is a shape, not four values", {
  cells <- paint_cells(list(m = matrix(1:4, 2), a = 1:2))
  v <- vals(cells)
  expect_equal(v$sig[v$j == 1], "<int [2 x 2]>")
  # ASCII "x", never U+00D7: pdf() cannot encode it.
  expect_true(all(charToRaw(v$sig[v$j == 1][[1]]) < as.raw(128)))
  # AND ITS TYPE LANE READS <int>, NOT <list>. `paint_format(data[k])` comes back
  # tagged "<list>", and reading the tag off it would print `<list>` over a cell
  # saying `<int [2 x 2]>`.
  expect_equal(lane(cells, "type")[[1]], "<int>")
})

test_that("a data frame element and a NULL element are summarised too", {
  cells <- paint_cells(list(d = head(iris, 5), n = NULL, a = 1:2))
  v <- vals(cells)
  expect_equal(v$sig[v$j == 1], "<df [5 x 5]>")
  expect_equal(v$sig[v$j == 2], "<NULL>")
  expect_equal(lane(cells, "type"), c("<df>", "<NULL>", "<int>"))
  # A zero-length element draws ONE cell, not a column of nothing.
  expect_equal(sum(v$j == 2), 1L)
})

test_that("a POSIXlt ELEMENT is a datetime, and says so", {
  # The type gate refuses a POSIXlt as a whole STRUCTURE -- it would draw as eleven
  # ragged columns of sec/min/hour/... -- and `elem_expands()` refuses it as an
  # element, so it is summarised. It used to be summarised as `<list [1]>`, because
  # `is.list()` is TRUE of it: a correct picture, but an uninformative one, sitting
  # next to a POSIXct element reading `<dttm [2]>`.
  ct <- as.POSIXct(c("2024-01-01 10:00:00", "2024-01-02 10:00:00"), tz = "UTC")
  lt <- as.POSIXlt("2024-01-01 10:00:00", tz = "UTC")
  cells <- paint_cells(list(ct = ct, lt = lt))
  v <- vals(cells)
  expect_equal(v$sig[v$j == 2], "<dttm [1]>")
  expect_equal(lane(cells, "type"), c("<dttm>", "<dttm>"))
  # The POSIXct element is drawn as its two VALUES; the POSIXlt is one token. The
  # tag agrees with both.
  expect_equal(sum(v$j == 1), 2L)
  expect_equal(sum(v$j == 2), 1L)
  # And it is still not expanded into its eleven fields.
  expect_false(elem_expands(lt))
})

test_that("summarise = TRUE draws every element as one cell", {
  cells <- paint_cells(list(a = 1:40, b = letters, m = matrix(1:4, 2)), summarise = TRUE)
  v <- vals(cells)
  expect_equal(nrow(v), 3L)
  expect_equal(v$sig, c("<int [40]>", "<chr [26]>", "<int [2 x 2]>"))
  expect_equal(v$row, rep(max(v$row), 3L))
  expect_equal(lane(cells, "type"), c("<int>", "<chr>", "<int>"))
  # Nothing is hidden: the whole list is on the page.
  expect_true(is.na(attr(cells, "note")))
})

# ---------------------------------------------------------------------------
# ragged elision
# ---------------------------------------------------------------------------

test_that("a short element draws no ellipsis it has no values for", {
  # THE PER-COLUMN GAP GATE. Element `b` has two values and hides none of them.
  cells <- paint_cells(list(a = 1:40, b = 1:2), max_rows = 10L)
  gaps <- cells[cells$kind == "ellipsis", , drop = FALSE]
  col_b <- unique(vals(cells)$col[vals(cells)$j == 2])
  expect_false(col_b %in% gaps$col)
  # And `a`, which hides 31 values, draws exactly one.
  col_a <- unique(vals(cells)$col[vals(cells)$j == 1])
  expect_equal(sum(gaps$col == col_a), 1L)
  expect_equal(sum(vals(cells)$j == 2), 2L)
})

test_that("no column trails off into empty space below its ellipsis", {
  # THE DANGLING ELLIPSIS. Elision is asked of each element separately, so a
  # length-12 element draws its OWN tail rather than the deepest element's.
  cells <- paint_cells(list(a = 1:40, b = 1:12), max_rows = 10L)
  v <- vals(cells)
  gaps <- cells[cells$kind == "ellipsis", , drop = FALSE]

  for (jj in 1:2) {
    col <- unique(v$col[v$j == jj])
    gap_row <- gaps$row[gaps$col == col]
    below <- v$row[v$j == jj & v$row > gap_row]
    expect_true(length(below) > 0L, info = paste("element", jj))
    # The bottom of every elided column lands on the same drawn row: the head and
    # tail counts depend on `max_rows` alone.
    expect_equal(max(below), max(v$row), info = paste("element", jj))
  }
  # Each element shows its own first five and its own last four.
  expect_equal(v$sig[v$j == 1], as.character(c(1:5, 37:40)))
  expect_equal(v$sig[v$j == 2], as.character(c(1:5, 9:12)))
})

test_that("a list counts its elision in values and elements, never rows", {
  cells <- paint_cells(list(a = 1:40, b = 1:2), max_rows = 10L)
  expect_equal(attr(cells, "hidden_rows"), 31L)
  expect_equal(attr(cells, "note"), "# 31 more values")

  wide <- paint_cells(stats::setNames(as.list(1:12), letters[1:12]), max_cols = 8L)
  expect_equal(attr(wide, "hidden_cols"), 5L)
  expect_equal(attr(wide, "note"), "# 5 more elements")

  both <- paint_cells(
    stats::setNames(lapply(1:12, function(i) seq_len(40)), letters[1:12]),
    max_rows = 10L, max_cols = 8L
  )
  expect_equal(attr(both, "note"), "# 217 more values, 5 more elements")
})

test_that("show_all draws every value of every element", {
  cells <- paint_cells(list(a = 1:40, b = 1:2), show_all = TRUE)
  expect_equal(nrow(vals(cells)), 42L)
  expect_equal(sum(cells$kind == "ellipsis"), 0L)
  expect_true(is.na(attr(cells, "note")))
})

test_that("elision is decided on structure, never on content", {
  # The same shape with different values draws the same number of cells.
  a <- paint_cells(list(x = 1:40, y = 1:3))
  b <- paint_cells(list(x = rnorm(40), y = c(1e15, 2, 3)))
  expect_equal(nrow(vals(a)), nrow(vals(b)))
  expect_equal(attr(a, "n_row"), attr(b, "n_row"))
  expect_equal(attr(a, "n_col"), attr(b, "n_col"))
})

# ---------------------------------------------------------------------------
# the accessor lane
# ---------------------------------------------------------------------------

test_that("show_indices = 'cell' draws [[j]][i] under each value", {
  cells <- paint_cells(list(a = 1:2, b = 5), show_indices = "cell")
  idx <- lane(cells, "cellindex")
  expect_equal(idx, c("[[1]][1]", "[[1]][2]", "[[2]][1]"))

  # Every label is an expression that RUNS, and returns the value drawn above it.
  l <- list(a = c(10, 20), b = 30)
  cells <- paint_cells(l, show_indices = "all")
  ci <- cells[cells$kind == "cellindex", , drop = FALSE]
  v <- vals(cells)
  for (k in seq_len(nrow(ci))) {
    got <- eval(parse(text = paste0("l", ci$sig[[k]])))
    same <- v$row == ci$row[[k]] & v$col == ci$col[[k]]
    expect_equal(as.character(got), v$sig[same])
  }
})

test_that("a summarised element's accessor is [[j]], not [[j]][1]", {
  cells <- paint_cells(list(a = 1:3, m = matrix(1:4, 2)), show_indices = "cell")
  idx <- lane(cells, "cellindex")
  expect_equal(idx[[4]], "[[2]]")
  expect_true(is.na(vals(cells)$i[[4]]))
})

test_that("show_indices takes only the list's own vocabulary", {
  expect_error(paint_cells(ragged, show_indices = "row"), "show_indices")
  expect_error(paint_cells(ragged, show_indices = "outside"), "show_indices")
  expect_no_error(paint_cells(ragged, show_indices = c("none", "cell")))
})

# ---------------------------------------------------------------------------
# highlighting
# ---------------------------------------------------------------------------

test_that("a list's mask is positions by elements", {
  m <- highlight_data(ragged, columns = "c")
  expect_equal(dim(m), c(4L, 3L))
  expect_true(all(m[, 3]))
  expect_false(any(m[, 1:2]))

  cells <- paint_cells(ragged, highlight_area = m)
  v <- vals(cells)
  expect_true(all(v$fill[v$j == 3] == "lemonchiffon"))
  expect_true(all(v$fill[v$j != 3] == "white"))
})

test_that("highlight_rows() marks a position inside every element", {
  m <- highlight_rows(ragged, 2)
  cells <- paint_cells(ragged, highlight_area = m)
  v <- vals(cells)
  # Element `b` has no second position, so nothing of it is filled.
  expect_equal(v$fill[v$j == 1], c("white", "lemonchiffon", "white", "white"))
  expect_equal(v$fill[v$j == 2], "white")
  expect_equal(v$fill[v$j == 3], c("white", "lemonchiffon", "white"))
})

test_that("a summarised element is filled when any of its positions is marked", {
  # The mask's SHAPE is a fact about the data, so the same mask works both ways.
  m <- highlight_rows(list(a = 1:4, b = 1:2), 3)
  cells <- paint_cells(list(a = 1:4, b = 1:2), summarise = TRUE, highlight_area = m)
  v <- vals(cells)
  expect_equal(v$fill, c("lemonchiffon", "white"))
})

# ---------------------------------------------------------------------------
# the painters
# ---------------------------------------------------------------------------

test_that("paint_list() draws, and defaults never warn", {
  with_null_pdf({
    expect_no_warning(expect_no_error(paint_list(ragged)))
    expect_no_warning(expect_no_error(paint_list(list(a = 1:40, b = letters))))
    expect_no_warning(expect_no_error(
      paint_list(stats::setNames(lapply(1:8, function(i) seq_len(10)), rep("abcdefghijkl", 8)))
    ))
    expect_no_error(paint_list(ragged, show_indices = "cell"))
    expect_no_error(paint_list(ragged, summarise = TRUE))
    expect_no_error(paint_list(ragged, name_align = "left", type_align = "right"))
    expect_no_error(paint_list(list(1:3, 4:6)))
  })
})

test_that("gpaint_list() draws the same picture", {
  skip_if_not_installed("ggplot2")
  with_null_pdf({
    expect_no_error(print(gpaint_list(ragged)))
    expect_no_error(print(gpaint_list(ragged, summarise = TRUE)))
    expect_no_error(print(gpaint_list(ragged, show_indices = "cell")))
  })
  expect_s3_class(gpaint_list(ragged), "ggplot")
})

test_that("the subtitle reports elements and their lengths, never dimensions", {
  expect_equal(
    list_subtitle(ragged),
    "Length: 3 elements of length 1 to 4 | Data Type: list"
  )
  expect_equal(
    list_subtitle(list(a = 1:3, b = 4:6)),
    "Length: 2 elements of length 3 | Data Type: list"
  )
  expect_false(grepl("Dimensions", list_subtitle(ragged)))
  # paint_size() must pick this arm, or it reserves its chrome against the string
  # "Dimensions:  rows x  columns".
  expect_no_error(s <- paint_size(ragged))
  expect_true(all(is.finite(s)))
})

test_that("an empty list is refused, and a huge one too", {
  with_null_pdf({
    expect_error(paint_list(list()), "empty")
    expect_error(
      paint_cells(list(a = seq_len(1e5), b = seq_len(1e5))),
      "100000 cells"
    )
    # ...but the BOUNDING BOX is not what is counted: this list holds 100001 values
    # in its bounding box and only two in fact.
    expect_no_error(paint_cells(list(a = seq_len(99999), b = 1)))
  })
})

test_that("highlight_data() refuses the empty list the painter refuses", {
  # THE INVARIANT RUNS BOTH WAYS. If a painter can draw it, `highlight_data()` can
  # mask it -- and if it CANNOT, `highlight_data()` must refuse. It used to hand back
  # a 1 by 0 matrix for a structure no painter draws.
  expect_error(highlight_data(list()), "empty")
  expect_error(highlight_columns(list(), 1), "empty")
  expect_error(highlight_rows(list(), 1), "empty")
  # A list with something in it is masked, exactly as before.
  expect_equal(dim(highlight_data(list(1, 2))), c(1L, 2L))
})

# ---------------------------------------------------------------------------
# THE TEACHING CLAIM, as an assertion
# ---------------------------------------------------------------------------

test_that("a data frame IS a list whose elements share a length", {
  # THE TEACHING CLAIM, AS AN ASSERTION. Two pictures, and the ONLY differences
  # between them are the header text and the subtitle -- neither of which is a fact
  # about the shape of the data. Everything else, the heavy outline included, is
  # `identical()`. A difference beyond those two would say nothing about the data and
  # everything about the painter, and a student setting the two side by side would
  # read it as a fact. That is the lie this test exists to prevent.
  df <- data.frame(a = 1:3, b = c(4.5, 5.5, 6.5))
  l <- list(a = 1:3, b = c(4.5, 5.5, 6.5))

  cd <- paint_cells(df)
  cl <- paint_cells(l)

  # The header lane is the one lane that MUST differ: a list's accessor is not a
  # column's. Blank it, and nothing at all is left to tell the two tables apart.
  blank_header <- function(cells) {
    cells$sig[cells$kind == "header"] <- ""
    cells$head[cells$kind == "header"] <- ""
    cells
  }
  expect_identical(blank_header(cl), blank_header(cd))

  # Said again, one piece at a time, so a failure says WHICH piece moved.
  expect_identical(vals(cl), vals(cd))
  expect_identical(lane(cl, "type"), lane(cd, "type"))
  expect_identical(attr(cl, "n_row"), attr(cd, "n_row"))
  expect_identical(attr(cl, "n_col"), attr(cd, "n_col"))
  expect_identical(column_widths(cl), column_widths(cd))

  # THE OUTLINE. The heavy border is the shared length, drawn -- so the list that
  # could have been this data frame is boxed exactly as the data frame is.
  expect_identical(cl[cl$kind == "outline", ], cd[cd$kind == "outline", ])
  expect_identical(outline_box(cl), outline_box(cd))

  # The two permitted differences, stated positively.
  expect_equal(lane(cd, "header"), c("a", "b"))
  expect_equal(lane(cl, "header"), c("$a", "$b"))
  expect_false(identical(list_subtitle(l), dims_subtitle(df)))

  # And the rectangle breaks on exactly one change: the shared length.
  expect_equal(sum(paint_cells(list(a = 1:3, b = c(4.5, 5.5)))$kind == "outline"), 0L)
})
