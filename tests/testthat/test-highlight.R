# Bug 5 ------------------------------------------------------------------------
# highlight_data() errored on character and logical vectors (and on data frames)
# because inherits(letters, "vector") is FALSE, so highlight_data.vector() was
# never dispatched for an atomic vector and control fell through to .default:
#
#   highlight_data(letters)        -> "We currently do not support ... character"
#   highlight_data(c(TRUE, FALSE)) -> "We currently do not support ... logical"
#   highlight_data(iris)           -> "We currently do not support ... data.frame"

test_that("bug 5: highlight_data() no longer errors on character, logical, or data frames", {
  expect_no_error(highlight_data(letters))
  expect_no_error(highlight_data(c(TRUE, FALSE)))
  expect_no_error(highlight_data(iris))

  # ...and each one comes back with the right shape rather than the .default stop().
  expect_length(highlight_data(letters), 26L)
  expect_length(highlight_data(c(TRUE, FALSE)), 2L)
  expect_equal(dim(highlight_data(iris)), dim(iris))
})

test_that("bug 5: the .vector dispatch trap is what made this fail", {
  # This is the fact the fix is built on: the methods must be fanned out
  # explicitly because a `vector` method can never be dispatched to.
  expect_false(inherits(letters, "vector"))
  expect_false(inherits(c(TRUE, FALSE), "vector"))

  # Every atomic type the painters accept now has its own method.
  for (cls in c("character", "logical", "complex", "factor", "numeric", "integer",
                "Date", "POSIXct", "data.frame")) {
    expect_true(is.function(get(paste0("highlight_data.", cls))), info = cls)
  }
})

test_that("bug 5: highlighting a character vector marks the right elements", {
  out <- highlight_locations(letters, c(2, 4))

  expect_type(out, "logical")
  expect_length(out, 26L)
  expect_equal(which(out), c(2L, 4L))
})

test_that("bug 5: highlighting a logical vector marks the right elements", {
  out <- highlight_data(c(TRUE, FALSE, TRUE, NA), locations = 3)

  expect_type(out, "logical")
  expect_length(out, 4L)
  expect_equal(which(out), 3L)
})

# The dispatch battery ---------------------------------------------------------

test_that("every supported 1D structure returns an all-FALSE mask of its own length", {
  cases <- list(
    integer   = 1:3,
    double    = c(1.5, 2),
    character = letters,
    logical   = c(TRUE, FALSE),
    factor    = factor(c("a", "b", "a")),
    date      = Sys.Date() + 0:4,
    posixct   = as.POSIXct("2026-07-12 10:00:00", tz = "UTC") + 0:2,
    complex   = complex(real = 1:3, imaginary = 3:1),
    special   = c(NA, NaN, Inf, -Inf)
  )

  for (nm in names(cases)) {
    x <- cases[[nm]]
    expect_no_error(highlight_data(x))
    out <- highlight_data(x)
    expect_type(out, "logical")
    expect_length(out, length(x))
    expect_false(any(out), info = nm)
  }
})

test_that("locations work on every supported 1D structure", {
  cases <- list(
    1:3,
    c(1.5, 2, 3),
    letters[1:3],
    c(TRUE, FALSE, TRUE),
    factor(c("a", "b", "c")),
    Sys.Date() + 0:2,
    complex(real = 1:3, imaginary = 3:1)
  )

  for (x in cases) {
    expect_equal(which(highlight_locations(x, c(1, 3))), c(1L, 3L))
  }
})

# Data frames ------------------------------------------------------------------

test_that("highlight_data.data.frame() returns a logical matrix matching dim(df)", {
  out <- highlight_data(iris)

  expect_true(is.matrix(out))
  expect_type(out, "logical")
  expect_equal(dim(out), dim(iris))
  expect_false(any(out))
})

test_that("data frame columns can be selected BY NAME", {
  out <- highlight_columns(iris, "Sepal.Width")

  expect_equal(dim(out), dim(iris))
  expect_true(all(out[, 2L]))
  expect_false(any(out[, -2L]))
})

test_that("data frame columns can be selected by several names at once", {
  out <- highlight_columns(iris, c("Sepal.Length", "Species"))

  expect_true(all(out[, c(1L, 5L)]))
  expect_false(any(out[, c(2L, 3L, 4L)]))
})

test_that("data frame columns can be selected BY INDEX", {
  by_index <- highlight_columns(iris, 2)
  by_name <- highlight_columns(iris, "Sepal.Width")

  expect_equal(by_index, by_name)
})

test_that("data frame rows can be selected by index and by row name", {
  df <- data.frame(a = 1:3, b = letters[1:3], row.names = c("x", "y", "z"))

  by_index <- highlight_rows(df, 2)
  by_name <- highlight_rows(df, "y")

  expect_equal(dim(by_index), c(3L, 2L))
  expect_true(all(by_index[2L, ]))
  expect_false(any(by_index[c(1L, 3L), ]))
  expect_equal(by_name, by_index)
})

test_that("a data frame accepts rows and columns together", {
  out <- highlight_data(iris, rows = 1, columns = "Species")

  expect_true(all(out[1L, ]))
  expect_true(all(out[, 5L]))
  expect_equal(sum(out), nrow(iris) + ncol(iris) - 1L)
})

test_that("a data frame accepts locations as an m by 2 matrix", {
  locations <- rbind(
    c(1, 1),
    c(2, 5),
    c(150, 3)
  )
  out <- highlight_locations(iris, locations)

  expect_equal(dim(out), dim(iris))
  expect_equal(sum(out), 3L)
  expect_true(out[1L, 1L])
  expect_true(out[2L, 5L])
  expect_true(out[150L, 3L])
})

test_that("data frame locations may name their columns", {
  locations <- cbind(c("1", "2"), c("Sepal.Length", "Species"))
  out <- highlight_locations(iris, locations)

  expect_equal(sum(out), 2L)
  expect_true(out[1L, 1L])
  expect_true(out[2L, 5L])
})

test_that("a data frame with a list column can still be highlighted", {
  df <- data.frame(a = 1:2)
  df$b <- list(1:3, letters)

  out <- highlight_columns(df, "b")

  expect_equal(dim(out), c(2L, 2L))
  expect_true(all(out[, 2L]))
  expect_false(any(out[, 1L]))
})

test_that("a zero-row data frame yields a zero-row mask", {
  out <- highlight_data(iris[0, ])

  expect_equal(dim(out), c(0L, 5L))
})

# Matrices: the existing API must not break ------------------------------------

test_that("the documented matrix examples still produce the same answers", {
  x <- matrix(1:12, nrow = 4)

  locations <- rbind(
    c(1, 3),
    c(2, 2),
    c(4, 1)
  )
  out <- highlight_locations(x, locations)
  expect_equal(dim(out), c(4L, 3L))
  expect_equal(sum(out), 3L)
  expect_true(out[1L, 3L])
  expect_true(out[2L, 2L])
  expect_true(out[4L, 1L])

  expect_true(all(highlight_rows(x, rows = c(1, 3))[c(1L, 3L), ]))
  expect_false(any(highlight_rows(x, rows = c(1, 3))[c(2L, 4L), ]))

  expect_true(all(highlight_columns(x, columns = ncol(x))[, 3L]))
  expect_true(all(highlight_columns(x, columns = 1)[, 1L]))

  both <- highlight_data(x, rows = 1, columns = 1)
  expect_equal(sum(both), 4L + 3L - 1L)
})

test_that("the documented vector example still produces the same answer", {
  vec <- c(3, NA, -1, 2, NaN, Inf, 42)
  out <- highlight_data(vec, locations = c(2, 4, 6))

  expect_length(out, 7L)
  expect_equal(which(out), c(2L, 4L, 6L))
})

test_that("a bare vector of locations still linear-indexes a matrix", {
  # Undocumented, but it worked before: ncol(vector) is NULL, so the old
  # stopifnot(ncol(locations) == 2) passed vacuously and `[<-` linear-indexed.
  x <- matrix(1:12, nrow = 4)
  out <- highlight_data(x, locations = c(1, 6))

  expect_equal(sum(out), 2L)
  expect_true(out[1L, 1L])
  expect_true(out[2L, 2L])
})

test_that("matrix rows and columns can be selected by dimname", {
  x <- matrix(1:4, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))

  expect_equal(highlight_columns(x, "c2"), highlight_columns(x, 2))
  expect_equal(highlight_rows(x, "r1"), highlight_rows(x, 1))
})

test_that("a character matrix dispatches to the matrix method, not the new character method", {
  x <- matrix(letters[1:6], nrow = 2)
  out <- highlight_columns(x, 2)

  expect_true(is.matrix(out))
  expect_equal(dim(out), c(2L, 3L))
  expect_true(all(out[, 2L]))
})

test_that("a logical matrix dispatches to the matrix method", {
  x <- matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2)
  out <- highlight_rows(x, 1)

  expect_true(is.matrix(out))
  expect_equal(dim(out), c(2L, 2L))
})

# Selection semantics ----------------------------------------------------------

test_that("named vectors can be highlighted by name", {
  x <- c(a = 1, b = 2, c = 3)

  # The mask itself carries no names, matching the matrix method, which returns a
  # dimnames-free logical matrix.
  expect_null(names(highlight_locations(x, "b")))
  expect_equal(which(highlight_locations(x, "b")), 2L)
  expect_equal(which(highlight_rows(x, c("a", "c"))), c(1L, 3L))
})

test_that("a logical mask selects, and NA in the mask reads as FALSE", {
  x <- 1:4

  expect_equal(which(highlight_locations(x, c(TRUE, FALSE, NA, TRUE))), c(1L, 4L))
  expect_equal(which(highlight_columns(iris, c(TRUE, FALSE, FALSE, FALSE, FALSE))[1L, ]), 1L)
})

test_that("negative indices exclude, as they do in base R", {
  out <- highlight_locations(1:4, -1)

  expect_equal(which(out), c(2L, 3L, 4L))
})

test_that("NULL selections mark nothing", {
  expect_false(any(highlight_data(letters)))
  expect_false(any(highlight_rows(iris, NULL)))
  expect_false(any(highlight_columns(iris, NULL)))
  expect_false(any(highlight_locations(iris, NULL)))
})

# Errors -----------------------------------------------------------------------

test_that("an unknown column name is a clear error, not a silent miss", {
  expect_error(highlight_columns(iris, "Petal.Wdith"), "Unknown column name")
})

test_that("selecting by name against unnamed data is a clear error", {
  expect_error(highlight_columns(matrix(1:4, 2), "a"), "no column names")
})

test_that("an out-of-range selection errors instead of silently growing the mask", {
  # `logical_vector[10] <- TRUE` on a length-3 vector used to extend it to length
  # 10 and pad with NA, which then fails a downstream dim check with a confusing
  # message. Fail here, where the cause is visible.
  expect_error(highlight_locations(1:3, 10), "out of range")
  expect_error(highlight_columns(iris, 9), "out of range")
})

test_that("a point outside the data errors", {
  expect_error(highlight_locations(iris, rbind(c(1, 9))), "falls outside the data")
  expect_error(highlight_locations(matrix(1:4, 2), rbind(c(3, 1))), "falls outside the data")
})

test_that("points for a 2D structure must have exactly two columns", {
  expect_error(
    highlight_locations(iris, cbind(1:2, 1:2, 1:2)),
    "two columns"
  )
})

test_that("points for a 1D structure must be a vector", {
  expect_error(
    highlight_locations(letters, rbind(c(1, 2))),
    "points must be a vector"
  )
})

# The array bug ----------------------------------------------------------------
# highlight_data() on a 3D array SILENTLY RETURNED A WRONG MASK. `.class2()` of an
# atomic array is c("array", "integer", "numeric"), and with no `array` method
# UseMethod() fell through to .integer -> .vector, which treats the array as a flat
# vector and drops its `dim`:
#
#   highlight_data(array(1:24, c(2, 3, 4)), rows = 1)
#   -> logical, length 24, dim NULL, exactly ONE TRUE. `rows = 1` marks TWELVE cells.
#
# This is the same dispatch trap as inherits(letters, "vector") being FALSE (bug 5),
# one type over. There is no n-D painter, so the fix is an honest stop(): a WRONG
# mask is worse than an error, and nothing could consume an n-D mask anyway.

test_that("the array bug: a 3D array is refused, not silently flattened", {
  expect_error(
    highlight_data(array(1:24, c(2, 3, 4)), rows = 1),
    "two-dimensional"
  )

  # The wrong answer it used to give, pinned so it cannot come back: a bare logical
  # vector of length 24 with a single TRUE and no `dim`.
  mask <- try(highlight_data(array(1:24, c(2, 3, 4)), rows = 1), silent = TRUE)
  expect_s3_class(mask, "try-error")
})

test_that("the array bug: every rank but two is refused", {
  expect_error(highlight_data(array(1:3, 3), rows = 1), "two-dimensional")
  expect_error(highlight_data(array(1:16, c(2, 2, 2, 2)), rows = 1), "two-dimensional")
})

test_that("the array bug: a 2D array is UNCHANGED -- it is a matrix and it masks", {
  # `.class2()` puts `matrix` ahead of `array` for a 2D one, so highlight_data.matrix()
  # is still dispatched and highlight_data.array() never fires. The mask must be the
  # matrix mask, exactly.
  a2 <- array(1:6, c(2, 3))

  expect_no_error(highlight_data(a2, rows = 1))
  expect_equal(dim(highlight_data(a2, rows = 1)), c(2L, 3L))
  expect_equal(sum(highlight_data(a2, rows = 1)), 3L)
  expect_identical(
    highlight_data(a2, rows = 1),
    highlight_data(matrix(1:6, nrow = 2), rows = 1)
  )
  expect_identical(
    highlight_data(a2, columns = 2, locations = rbind(c(2, 3))),
    highlight_data(matrix(1:6, nrow = 2), columns = 2, locations = rbind(c(2, 3)))
  )
})

# The table bug ----------------------------------------------------------------
# paint_matrix() DRAWS a 2D table, but highlight_data() REFUSED one: `.class2()` of a
# table is "table" alone -- a classed object dispatches on its own class vector only --
# so .matrix never fired and control landed on .default:
#
#   highlight_data(table(a, b), rows = 1) -> "We currently do not support ... table"
#
# That breaks the governing invariant: IF A PAINTER CAN DRAW IT, highlight_data() MUST
# BE ABLE TO MASK IT. `table(a, b)` is the structure a paintr user would most want to
# highlight a row of.

test_that("the table bug: a 2D table can be highlighted", {
  tab <- table(c("a", "b", "a"), c("x", "x", "y"))

  expect_no_error(highlight_data(tab, rows = 1))
  expect_equal(dim(highlight_data(tab, rows = 1)), c(2L, 2L))
  expect_equal(sum(highlight_data(tab, rows = 1)), 2L)
})

test_that("the table bug: the 2D table mask EQUALS the plain matrix mask", {
  tab <- table(c("a", "b", "a"), c("x", "x", "y"))
  # The same numbers, the same names, but a plain matrix. The table method delegates to
  # the matrix method rather than copying it, so these cannot drift apart.
  mat <- matrix(
    as.vector(tab),
    nrow = nrow(tab), ncol = ncol(tab),
    dimnames = dimnames(tab)
  )

  expect_identical(highlight_data(tab, rows = 1), highlight_data(mat, rows = 1))
  expect_identical(highlight_data(tab, columns = 2), highlight_data(mat, columns = 2))
  expect_identical(
    highlight_data(tab, locations = rbind(c(1, 2), c(2, 1))),
    highlight_data(mat, locations = rbind(c(1, 2), c(2, 1)))
  )
  # Names, too: a table's dimnames are where its row and column names live.
  expect_identical(highlight_data(tab, rows = "a"), highlight_data(mat, rows = "a"))
  expect_identical(highlight_data(tab, columns = "y"), highlight_data(mat, columns = "y"))
})

test_that("the table bug: a table of any rank but two is refused", {
  # 1D: table(x). Nothing paints it, so nothing may mask it.
  expect_error(highlight_data(table(c("a", "b", "a")), rows = 1), "two-dimensional")
  # 4D: Titanic.
  expect_error(highlight_data(Titanic, rows = 1), "two-dimensional")
})

test_that("unsupported structures still hit the .default error", {
  expect_error(
    highlight_data(list(1, 2)),
    "We currently do not support the data structure of"
  )
  expect_error(
    highlight_data(mean),
    "We currently do not support the data structure of"
  )
})
