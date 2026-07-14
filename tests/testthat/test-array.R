# The array painters, and the one promise they exist to keep.

# ---------------------------------------------------------------------------
# THE ACCESSOR TEST: every index the picture draws is an expression that RUNS
# ---------------------------------------------------------------------------
#
# This is the package's one distinguishing promise and the array is where it is
# easiest to break. On a 3-D array `a[1, ]` and `a[2, 3]` are ERRORS, not shorthand,
# so a painter that reused the matrix's labels would teach students to type
# expressions that fail -- in the `show_indices = "all"` call an instructor reaches
# for first.
#
# So: take every label the cell table draws, paste it onto the object's name,
# EVALUATE it, and demand that it neither errors nor comes back empty.

# `pdf(NULL)` is the test device: full strwidth()/par() support, no file I/O, and no
# Rplots.pdf left in the working directory for R CMD check to complain about. It is
# the same helper `test-dispatch.R` uses, and for the same reason.
with_null_pdf <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

# The index lanes, and only them: a NAME is not an accessor ("Male" is not something
# you can subscript with, unquoted), and a slice title is a title.
index_labels <- function(cells) {
  lanes <- cells[cells$kind %in% c("rowlabel", "collabel", "cellindex"), , drop = FALSE]
  s <- lanes$sig
  s[startsWith(s, "[")]
}

expect_labels_run <- function(a, ..., label = NULL) {
  cells <- paint_cells(a, ...)
  labs <- index_labels(cells)
  expect_gt(length(labs), 0L)
  env <- new.env(parent = baseenv())
  assign("a", a, envir = env)
  for (s in labs) {
    expr <- paste0("a", s)
    got <- tryCatch(
      eval(parse(text = expr), envir = env),
      error = function(e) structure(conditionMessage(e), class = "paintr_label_error")
    )
    expect_false(
      inherits(got, "paintr_label_error"),
      info = paste0(label, ": `", expr, "` ERRORED: ", as.character(got))
    )
    expect_gt(length(got), 0L)
  }
  invisible(labs)
}

test_that("every index label RUNS, for a 2-D, 3-D and 4-D array", {
  a2 <- array(1:6, c(2, 3))
  a3 <- array(1:24, c(2, 3, 4))
  a4 <- array(1:48, c(2, 3, 2, 4))

  # The lanes hold indices only when no name has won them, so ask for the indices
  # explicitly -- which is exactly the call the promise is about.
  expect_labels_run(a2, show_indices = "all", label = "2-D")
  expect_labels_run(a3, show_indices = "all", label = "3-D")
  expect_labels_run(a4, show_indices = "all", label = "4-D")

  # And with dimnames present, where an index lane must still WIN the axis it names.
  expect_labels_run(Titanic, show_indices = "all", label = "Titanic")
  expect_labels_run(HairEyeColor, show_indices = "all", label = "HairEyeColor")
  expect_labels_run(UCBAdmissions, show_indices = "all", label = "UCBAdmissions")

  # And with the names explicitly off, which is the other way to get an index lane.
  expect_labels_run(Titanic, show_indices = "all", show_dimnames = "none", label = "no dimnames")
})

test_that("the index labels carry the array's full subscript ARITY", {
  a3 <- array(1:24, c(2, 3, 4))
  cells <- paint_cells(a3, show_indices = "all")

  rl <- unique(cells$sig[cells$kind == "rowlabel"])
  cl <- unique(cells$sig[cells$kind == "collabel"])
  ci <- unique(cells$sig[cells$kind == "cellindex"])

  # A 3-D array takes THREE subscripts. `a[1, ]` is an error; `a[1, , 3]` is the row.
  expect_true(all(grepl("^\\[[0-9]+, , [0-9]+\\]$", rl)))
  expect_true(all(grepl("^\\[, [0-9]+, [0-9]+\\]$", cl)))
  expect_true(all(grepl("^\\[[0-9]+, [0-9]+, [0-9]+\\]$", ci)))

  # The slice subscript is FILLED IN FROM THE BLOCK: the gutter beside block 3 names
  # block 3's row, not all four blocks' rows at once.
  expect_true("[1, , 1]" %in% rl)
  expect_true("[1, , 4]" %in% rl)
  expect_true("[2, 3, 4]" %in% ci)

  # Rank 4 takes four.
  a4 <- array(1:48, c(2, 3, 2, 4))
  c4 <- paint_cells(a4, show_indices = "all")
  expect_true(all(grepl(
    "^\\[[0-9]+, [0-9]+, [0-9]+, [0-9]+\\]$",
    c4$sig[c4$kind == "cellindex"]
  )))
})

test_that("a matrix's labels are UNCHANGED -- rank 2 adds no subscripts", {
  m <- matrix(1:6, nrow = 2)
  cells <- paint_cells(m, show_indices = "all")
  expect_true("[1, ]" %in% cells$sig[cells$kind == "rowlabel"])
  expect_true("[, 2]" %in% cells$sig[cells$kind == "collabel"])
  expect_true("[2, 3]" %in% cells$sig[cells$kind == "cellindex"])
})

# ---------------------------------------------------------------------------
# rank 2 is the degenerate case, and it is served by NOT WRITING IT
# ---------------------------------------------------------------------------

test_that("paint_array() draws a matrix, and draws it as paint_matrix() does", {
  m <- matrix(c(1, 0.333, 123456.7, 20, -1, NA), nrow = 3)
  expect_identical(paint_cells(m), paint_cells(m))

  # `is.array(matrix)` is TRUE, so the painter must accept it...
  expect_true(is.array(m))
  expect_true(is_paint_array(m))

  # ...and the two painters must produce the SAME cell table, because they run the
  # same code. This is the whole of the "k = 1 is the free degenerate case" claim,
  # and it is checked rather than asserted.
  with_null_pdf({
    a <- paint_array(m)
    b <- paint_matrix(m)
    expect_equal(a$cells, b$cells)
    expect_equal(a$fontsize, b$fontsize)

    # A dimnamed matrix, too.
    mn <- matrix(
      c(21, 6, 22.8, 4), nrow = 2, byrow = TRUE,
      dimnames = list(c("Mazda", "Datsun"), c("mpg", "cyl"))
    )
    expect_equal(paint_array(mn)$cells, paint_matrix(mn)$cells)
    expect_equal(
      paint_array(mn, show_indices = "cell")$cells,
      paint_matrix(mn, show_indices = "cell")$cells
    )

    # A 2-D table is an array and is drawn.
    tb <- table(c("a", "b", "a"), c("x", "x", "y"))
    expect_silent(paint_array(tb))
  })
})

test_that("a 2-D array draws NO slice title", {
  cells <- paint_cells(matrix(1:6, nrow = 2))
  expect_equal(sum(cells$kind == "slicelabel"), 0L)

  # ...and a 3-D one draws exactly one per block.
  c3 <- paint_cells(array(1:24, c(2, 3, 4)))
  expect_equal(sum(c3$kind == "slicelabel"), 4L)
})

test_that("rank 1 is refused, and rank 5 is drawn", {
  expect_error(paint_array(array(1:3)), "array")
  expect_error(paint_cells(array(1:3)), "one- and two-dimensional")
  expect_silent(paint_cells(array(1:32, c(2, 2, 2, 2, 2))))
})

test_that("a list carrying a dim is not an array", {
  l <- list(1, 2, 3, 4)
  dim(l) <- c(2, 2)
  expect_false(is_paint_array(l))
  expect_error(paint_array(l), "array")
})

# ---------------------------------------------------------------------------
# the slice titles
# ---------------------------------------------------------------------------

test_that("the slice title is what print() writes", {
  cells <- paint_cells(Titanic)
  titles <- cells$sig[cells$kind == "slicelabel"]
  expect_true(", , Child, No" %in% titles)
  expect_true(", , Adult, Yes" %in% titles)
  expect_equal(length(titles), 4L)

  # Names off -> the index form, still a title, still identifying the block.
  bare <- paint_cells(Titanic, show_dimnames = "none")
  bt <- bare$sig[bare$kind == "slicelabel"]
  expect_true(", , 1, 1" %in% bt)
  expect_true(", , 2, 2" %in% bt)

  # An undimnamed 3-D array titles by index.
  a3 <- paint_cells(array(1:24, c(2, 3, 4)))
  expect_equal(
    sort(a3$sig[a3$kind == "slicelabel"]),
    c(", , 1", ", , 2", ", , 3", ", , 4")
  )
})

test_that("the slice title SPANS its block and demands no column width", {
  cells <- paint_cells(Titanic)
  sl <- cells[cells$kind == "slicelabel", , drop = FALSE]
  expect_true(all(sl$col_end > sl$col))

  # A spanning cell demands nothing of any single column: it is fitted against the
  # span instead. Widening the title must not widen a single value cell.
  wide <- Titanic
  dimnames(wide)[[3L]] <- c("AAAAAAAAAAAAAAAAAAAA", "BBBBBBBBBBBBBBBBBBBB")
  expect_equal(column_widths(paint_cells(Titanic)), column_widths(paint_cells(wide)))
})

test_that("span_widths() is BIT-IDENTICAL to col_w[col] for a cell in one column", {
  # THE HOT PATH. `fit_fontsize()` and `span_dx()` ask `span_widths()` for the width
  # of EVERY cell of EVERY picture the package draws, so the single-column answer has
  # to be the number it has always been -- not merely equal to it.
  #
  # Writing the function as the one-liner `edge[col_end + 1] - edge[col]` is correct
  # in exact arithmetic and WRONG IN FLOATING POINT: `cumsum()` accumulates, so
  # `(a + b + c) - (a + b)` is not `identical()` to `c`. The ~1e-16 residue travels
  # into `avail_w`, through the `min()` in `fit_fontsize()`, and out into a font size
  # whose last bits have moved -- which it measurably did, for a 30x20 matrix, for
  # `iris`, and for a ragged list, before the single-column case was lifted out.
  #
  # `expect_equal()` would NOT catch this. It has a tolerance. `expect_identical()`
  # is the whole point.
  # `show_indices`'s vocabulary differs by structure -- a vector's is
  # none/inside/outside -- so each case carries its own.
  battery <- list(
    list(matrix(1:6, nrow = 3), "none"),
    list(matrix(c(1, 0.333, 123456.7, 20, -1, NA), nrow = 3), "all"),
    list(matrix(1:600, nrow = 30), "none"),
    list(matrix(letters[1:6], nrow = 2), "cell"),
    list(1:5, "outside"),
    list(c(alpha = 1, beta = 2, gamma = 3), "inside"),
    list(head(mtcars, 6), "all"),
    list(head(iris, 5), "none"),
    list(list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA)), "cell"),
    list(Titanic, "none"),
    list(Titanic, "all"),
    list(array(1:24, c(2, 3, 4)), "all")
  )
  for (case in battery) {
    cells <- paint_cells(case[[1L]], show_indices = case[[2L]])
    col_w <- column_widths(cells)
    one <- cells$col_end == cells$col
    expect_true(any(one))
    expect_identical(
      span_widths(cells, col_w)[one],
      col_w[cells$col][one],
      info = paste(class(case[[1L]])[1L], case[[2L]])
    )
  }
})

test_that("a 3-D array's slabs lay out in ONE line", {
  # ceiling(sqrt(4)) would wrap four slabs into a 2x2 grid and make them
  # PIXEL-IDENTICAL to a genuine 4-D facet -- a layout artefact wearing a
  # dimension's clothes. One line.
  cells <- paint_cells(array(1:24, c(2, 3, 4)))
  sl <- cells[cells$kind == "slicelabel", , drop = FALSE]
  expect_equal(length(unique(sl$row)), 1L)
  expect_equal(length(unique(sl$col)), 4L)

  # A 4-D array's two directions ARE two axes: across is dim 3, down is dim 4.
  c4 <- paint_cells(Titanic)
  s4 <- c4[c4$kind == "slicelabel", , drop = FALSE]
  expect_equal(length(unique(s4$row)), 2L)
  expect_equal(length(unique(s4$col)), 2L)
})

# ---------------------------------------------------------------------------
# the formatting unit is the WHOLE array
# ---------------------------------------------------------------------------

test_that("a 1e15 in slice 3 flips the WHOLE array to scientific", {
  a <- array(1, c(2, 2, 3))
  flat <- paint_cells(a)
  expect_false(any(grepl("e", flat$sig[flat$kind == "value"], fixed = TRUE)))

  a[1, 1, 3] <- 1e15
  sci <- paint_cells(a)
  v <- sci$sig[sci$kind == "value"]
  # EVERY value, in every block, not just slice 3's.
  expect_true(all(grepl("e", v, fixed = TRUE)))
  expect_equal(length(unique(sci$fmt_group[sci$kind == "value"])), 1L)
  expect_equal(unique(sci$fmt_group[sci$kind == "value"]), 1L)
})

test_that("elision runs BEFORE formatting: a hidden 1e15 cannot flip the visible cells", {
  a <- array(1, c(2, 2, 30))
  a[1, 1, 15] <- 1e15  # a slice the default max_slices will not draw
  cells <- paint_cells(a)
  expect_false(any(grepl("e", cells$sig[cells$kind == "value"], fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# elision, on the slice axis too
# ---------------------------------------------------------------------------

test_that("the slice axis elides, and the gap is DRAWN", {
  cells <- paint_cells(UCBAdmissions)  # 2 x 2 x 6, max_slices = 4
  expect_equal(sum(cells$kind == "slicelabel"), 3L)
  expect_equal(attr(cells, "hidden_slices"), 3L)
  expect_match(attr(cells, "note"), "3 more slices")
  # The gap is always drawn.
  expect_gt(sum(cells$kind == "ellipsis"), 0L)
})

test_that("elision is device-free and depends on SHAPE, never on content", {
  a <- array(1:24, c(2, 3, 4))
  b <- a
  b[] <- 1e9
  ca <- paint_cells(a)
  cb <- paint_cells(b)
  expect_equal(attr(ca, "n_row"), attr(cb, "n_row"))
  expect_equal(attr(ca, "n_col"), attr(cb, "n_col"))
  expect_equal(attr(ca, "hidden_slices"), attr(cb, "hidden_slices"))

  # And dimnames do not change WHICH cells are drawn.
  dimnames(b) <- list(c("r1", "r2"), c("c1", "c2", "c3"), c("s1", "s2", "s3", "s4"))
  expect_equal(
    sum(paint_cells(a)$kind == "value"),
    sum(paint_cells(b)$kind == "value")
  )
})

test_that("show_all draws every slice", {
  cells <- paint_cells(UCBAdmissions, show_all = TRUE)
  expect_equal(sum(cells$kind == "slicelabel"), 6L)
  expect_equal(attr(cells, "hidden_slices"), 0L)
  expect_true(is.na(attr(cells, "note")))
})

test_that("every block gets its OWN outline", {
  cells <- paint_cells(array(1:24, c(2, 3, 4)))
  o <- cells[cells$kind == "outline", , drop = FALSE]
  expect_equal(nrow(o), 4L)
  # Four boxes, not one box around all four: no two outlines share a column, and
  # none of them spans the whole picture.
  expect_equal(length(unique(o$col)), 4L)
  expect_true(all(o$col_end < attr(cells, "n_col")[[1L]] | o$col == max(o$col)))
  expect_true(all(o$col_end >= o$col))
  expect_true(all(o$row_end > o$row))
})

# ---------------------------------------------------------------------------
# dimnames, on ALL axes
# ---------------------------------------------------------------------------

test_that("show_dimnames draws the row and column names of a contingency table", {
  cells <- paint_cells(Titanic)
  expect_true("1st" %in% cells$sig[cells$kind == "rowlabel"])
  expect_true("Crew" %in% cells$sig[cells$kind == "rowlabel"])
  expect_true("Male" %in% cells$sig[cells$kind == "collabel"])
  expect_true("Female" %in% cells$sig[cells$kind == "collabel"])

  # An index lane WINS the axis it names -- PR 1's precedence rule, unchanged.
  idx <- paint_cells(Titanic, show_indices = "row")
  expect_false("1st" %in% idx$sig[idx$kind == "rowlabel"])
  expect_true(any(startsWith(idx$sig[idx$kind == "rowlabel"], "[")))
  # ...and the column axis keeps its names.
  expect_true("Male" %in% idx$sig[idx$kind == "collabel"])

  # show_dimnames = "none" draws no name lane at all.
  none <- paint_cells(Titanic, show_dimnames = "none")
  expect_equal(sum(none$kind == "rowlabel"), 0L)
  expect_equal(sum(none$kind == "collabel"), 0L)

  # A single axis can be asked for on its own.
  rows <- paint_cells(Titanic, show_dimnames = "row")
  expect_gt(sum(rows$kind == "rowlabel"), 0L)
  expect_equal(sum(rows$kind == "collabel"), 0L)
})

test_that("`slice` governs the block titles alone", {
  cells <- paint_cells(Titanic, show_dimnames = "slice")
  expect_true(", , Child, No" %in% cells$sig[cells$kind == "slicelabel"])
  expect_equal(sum(cells$kind == "rowlabel"), 0L)

  rc <- paint_cells(Titanic, show_dimnames = c("row", "column"))
  expect_true(", , 1, 1" %in% rc$sig[rc$kind == "slicelabel"])
  expect_true("1st" %in% rc$sig[rc$kind == "rowlabel"])
})

test_that("an unknown show_dimnames value is an error", {
  expect_error(paint_array(Titanic, show_dimnames = "slices"), "show_dimnames")
  expect_error(paint_matrix(matrix(1:4, 2), show_dimnames = "slice"), "show_dimnames")
})

# ---------------------------------------------------------------------------
# highlighting: if a painter can draw it, highlight_data() can mask it
# ---------------------------------------------------------------------------

test_that("highlight_data() masks an array of any rank", {
  m <- highlight_data(Titanic, rows = 1)
  expect_equal(dim(m), dim(Titanic))
  expect_type(m, "logical")
  expect_true(all(m[1, , , ]))
  expect_false(any(m[2, , , ]))

  # Selection BY NAME works -- the names are read off the data. The MASK itself
  # carries no dimnames, which is exactly the contract `highlight_data.matrix()` has
  # always had, so it is indexed by position.
  byname <- highlight_data(Titanic, rows = "Crew", columns = "Female")
  expect_null(dimnames(byname))
  expect_true(all(byname[4, , , ]))   # Crew
  expect_true(all(byname[, 2, , ]))   # Female
  expect_false(byname[1, 1, 1, 1])    # 1st / Male

  # A 3-D array.
  a <- array(1:24, c(2, 3, 4))
  expect_true(all(highlight_columns(a, 2)[, 2, ]))
  expect_equal(sum(highlight_rows(a, 1)), 12L)

  # A full coordinate reaches one cell.
  loc <- highlight_locations(a, rbind(c(2, 3, 4), c(1, 1, 1)))
  expect_true(loc[2, 3, 4])
  expect_true(loc[1, 1, 1])
  expect_equal(sum(loc), 2L)

  # A short coordinate is an error, not a wrong mask.
  expect_error(highlight_locations(a, rbind(c(2, 3))), "one coordinate per dimension")
})

test_that("Titanic is maskable because Titanic is paintable", {
  with_null_pdf(expect_silent(
    paint_array(Titanic, highlight_area = highlight_data(Titanic, rows = 1))
  ))
  cells <- paint_cells(Titanic, highlight_area = highlight_data(Titanic, rows = 1))
  v <- cells[cells$kind == "value", , drop = FALSE]
  expect_true(all(v$fill[v$i == 1L] == "lemonchiffon"))
  expect_true(all(v$fill[v$i != 1L] == "white"))
})

test_that("a rank-1 array is refused by highlight_data(), as no painter draws it", {
  expect_error(highlight_data(array(1:3), rows = 1), "dimension")
})

test_that("a wrongly shaped mask is an error that reports the real shape", {
  expect_error(
    paint_cells(array(1:24, c(2, 3, 4)), highlight_area = matrix(TRUE, 2, 3)),
    "2 by 3 by 4"
  )
  # A length-one logical is recycled, which preserves highlight_area = FALSE.
  expect_silent(paint_cells(array(1:24, c(2, 3, 4)), highlight_area = FALSE))
})

# ---------------------------------------------------------------------------
# the painters
# ---------------------------------------------------------------------------

test_that("paint_array() draws the canonical tables and returns a resolved table", {
  with_null_pdf(
    for (a in list(Titanic, HairEyeColor, UCBAdmissions, array(1:24, c(2, 3, 4)))) {
      r <- paint_array(a)
      expect_type(r, "list")
      expect_true(all(c("cells", "fontsize", "floored", "u") %in% names(r)))
      expect_false(r$floored)
    }
  )
})

test_that("the DEFAULT canonical arrays never warn", {
  # min_pt is a WARNING THRESHOLD, and a warning must always mean the caller raised
  # a cap. `max_slices = 4` is calibrated for exactly this.
  grDevices::pdf(NULL, width = 7, height = 5)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(paint_array(Titanic))
  expect_silent(paint_array(HairEyeColor))
  expect_silent(paint_array(UCBAdmissions))
  expect_silent(paint_array(array(1:24, c(2, 3, 4))))
})

test_that("the subtitle reports the array's full SHAPE, not two of its axes", {
  expect_equal(
    array_subtitle(Titanic),
    "Dimensions: 4 x 2 x 2 x 2 | Data Type: table"
  )
  # nrow()/ncol() are DEFINED for an array, so dims_subtitle() would not error --
  # it would lie.
  expect_equal(dims_subtitle(Titanic), "Dimensions: 4 rows x 2 columns | Data Type: table")

  # RANK TWO IS A MATRIX AND SAYS SO. `paint_array(m)` must draw the picture
  # `paint_matrix(m)` draws, chrome included -- so the rank test lives in ONE place
  # and `paint_size()` reserves the chrome the painter really draws.
  m <- matrix(1:6, nrow = 2)
  expect_equal(array_subtitle(m), dims_subtitle(m))
  expect_match(array_subtitle(m), "2 rows x 3 columns")

  with_null_pdf({
    r <- paint_array(Titanic)
    expect_match(r$graph_subtitle, "4 x 2 x 2 x 2")
    expect_equal(r$graph_title, "Data Object: Titanic")

    # NA opts out.
    expect_null(paint_array(Titanic, graph_subtitle = NA)$graph_subtitle)
  })
})

test_that("paint_size() answers for an array", {
  s <- paint_size(Titanic)
  expect_named(s, c("width", "height"))
  expect_true(all(s > 0))
  # And its subtitle is the array's, not the "4 rows x 2 columns" lie.
  expect_gt(paint_size(array(1:24, c(2, 3, 4)))[["width"]], 0)
})

test_that("gpaint_array() returns a ggplot", {
  skip_if_not_installed("ggplot2")
  p <- gpaint_array(Titanic)
  expect_s3_class(p, "ggplot")
  # `print()` with no device open would write an Rplots.pdf, which R CMD check finds.
  with_null_pdf(expect_silent(print(p)))
})

test_that("an empty or oversized array is refused", {
  expect_error(paint_cells(array(integer(0), c(0, 2, 2))), "empty")
  expect_error(
    paint_cells(array(1L, c(100, 100, 100)), show_all = TRUE),
    "100000 cells"
  )
})
