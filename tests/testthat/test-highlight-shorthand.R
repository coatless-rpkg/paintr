# The `highlight_rows` / `highlight_columns` / `highlight_locations` pass-through
# arguments on the painters, and the prep-helper refactor that carries them.
#
# The claim under test: `paint_matrix(m, highlight_rows = 1)` draws exactly the
# picture `paint_matrix(m, highlight_area = highlight_rows(m, 1))` draws. It does,
# and it must, because the shorthand is built by the SAME `highlight_data()` the
# mask builders call -- so the two masks are byte-identical, and therefore so is
# every cell of the resolved table, fills included.

# `pdf(NULL)`: full strwidth()/par() support, no file I/O, no Rplots.pdf for
# R CMD check to trip over. The same helper the other painter suites use.
with_null_pdf <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

# The resolved cell table a base painter drew, with the device-dependent fields
# (fontsize and friends) left out: only the backend-independent table.
base_cells <- function(expr) {
  with_null_pdf(suppressWarnings(withVisible(expr)$value)$cells)
}

# The cell table a ggplot2 painter built, read off its single annotation_custom
# grob. No device needed: the table is fixed at build time.
g_cells <- function(g) g$layers[[1L]]$geom_params$grob$cells

m  <- matrix(seq_len(16), nrow = 4)
df <- head(iris, 5)
l  <- list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA))
v  <- c(-3, 5, NA, Inf, 2, 1)

# ---------------------------------------------------------------------------
# base backend: shorthand == explicit mask
# ---------------------------------------------------------------------------

test_that("paint_matrix(highlight_rows=) equals highlight_area=highlight_rows()", {
  # The fill assertion names the stable classic highlight token.
  withr::local_options(paintr.palette = "classic")
  expect_identical(
    base_cells(paint_matrix(m, show_indices = "row", highlight_rows = 1)),
    base_cells(paint_matrix(m, show_indices = "row",
                            highlight_area = highlight_rows(m, 1)))
  )
  # Non-vacuous: the highlight colour is actually painted onto some cell.
  filled <- base_cells(paint_matrix(m, highlight_rows = 1))$fill
  expect_true(any(filled == "lemonchiffon", na.rm = TRUE))
})

test_that("paint_matrix(highlight_columns=) equals the explicit mask", {
  expect_identical(
    base_cells(paint_matrix(m, highlight_columns = 2)),
    base_cells(paint_matrix(m, highlight_area = highlight_columns(m, 2)))
  )
})

test_that("paint_matrix(highlight_locations=) equals the explicit mask", {
  loc <- rbind(c(1, 1), c(3, 2))
  expect_identical(
    base_cells(paint_matrix(m, highlight_locations = loc)),
    base_cells(paint_matrix(m, highlight_area = highlight_locations(m, loc)))
  )
})

test_that("several shorthands compose as a union, matching highlight_data()", {
  expect_identical(
    base_cells(paint_matrix(m, highlight_rows = 1, highlight_columns = 2)),
    base_cells(paint_matrix(m,
      highlight_area = highlight_data(m, rows = 1, columns = 2)
    ))
  )
})

test_that("paint_data_frame() takes a column by name and a row by number", {
  expect_identical(
    base_cells(paint_data_frame(df, highlight_columns = "Sepal.Width")),
    base_cells(paint_data_frame(df,
      highlight_area = highlight_columns(df, "Sepal.Width")
    ))
  )
  expect_identical(
    base_cells(paint_data_frame(df, highlight_rows = c(1, 3))),
    base_cells(paint_data_frame(df, highlight_area = highlight_rows(df, c(1, 3))))
  )
})

test_that("paint_list() selects an element by name and a position by number", {
  expect_identical(
    base_cells(paint_list(l, highlight_columns = "c")),
    base_cells(paint_list(l, highlight_area = highlight_columns(l, "c")))
  )
  expect_identical(
    base_cells(paint_list(l, highlight_rows = 2)),
    base_cells(paint_list(l, highlight_area = highlight_rows(l, 2)))
  )
})

test_that("paint_vector() takes highlight_locations, its one axis", {
  expect_identical(
    base_cells(paint_vector(v, show_indices = "outside",
                            highlight_locations = c(2, 4, 6))),
    base_cells(paint_vector(v, show_indices = "outside",
                            highlight_area = highlight_locations(v, c(2, 4, 6))))
  )
})

test_that("a vector exposes only highlight_locations, not rows or columns", {
  # A vector has a single axis addressed by position. Rows/columns are not formals
  # of the vector painters, so naming them is an "unused argument" error.
  expect_error(paint_vector(v, highlight_rows = 1), "unused argument")
  expect_error(paint_vector(v, highlight_columns = 1), "unused argument")
})

test_that("paint_array() marks a row and a column of every slice", {
  expect_identical(
    base_cells(paint_array(Titanic, highlight_rows = 1)),
    base_cells(paint_array(Titanic, highlight_area = highlight_data(Titanic, rows = 1)))
  )
  expect_identical(
    base_cells(paint_array(Titanic, highlight_columns = 1)),
    base_cells(paint_array(Titanic,
      highlight_area = highlight_data(Titanic, columns = 1)
    ))
  )
})

# ---------------------------------------------------------------------------
# ggplot2 backend: the same table, built the same way
# ---------------------------------------------------------------------------

test_that("gpaint_* shorthands build the same cell table as the explicit mask", {
  skip_if_not_installed("ggplot2")
  expect_identical(
    g_cells(gpaint_matrix(m, highlight_rows = 1)),
    g_cells(gpaint_matrix(m, highlight_area = highlight_rows(m, 1)))
  )
  expect_identical(
    g_cells(gpaint_data_frame(df, highlight_columns = "Sepal.Width")),
    g_cells(gpaint_data_frame(df,
      highlight_area = highlight_columns(df, "Sepal.Width")
    ))
  )
  expect_identical(
    g_cells(gpaint_list(l, highlight_columns = "c")),
    g_cells(gpaint_list(l, highlight_area = highlight_columns(l, "c")))
  )
  expect_identical(
    g_cells(gpaint_vector(v, highlight_locations = c(2, 4))),
    g_cells(gpaint_vector(v, highlight_area = highlight_locations(v, c(2, 4))))
  )
  expect_identical(
    g_cells(gpaint_array(Titanic, highlight_rows = 1)),
    g_cells(gpaint_array(Titanic, highlight_area = highlight_data(Titanic, rows = 1)))
  )
})

# ---------------------------------------------------------------------------
# precedence: highlight_area and a shorthand together is an error
# ---------------------------------------------------------------------------

test_that("supplying highlight_area AND a shorthand is a plain error", {
  msg <- "Supply either `highlight_area`"
  with_null_pdf({
    expect_error(
      paint_matrix(m, highlight_area = highlight_rows(m, 1), highlight_rows = 2),
      msg, fixed = TRUE
    )
    expect_error(
      paint_data_frame(df, highlight_area = highlight_rows(df, 1),
                       highlight_columns = "Species"),
      msg, fixed = TRUE
    )
    expect_error(
      paint_vector(v, highlight_area = highlight_locations(v, 1),
                   highlight_locations = 2),
      msg, fixed = TRUE
    )
    expect_error(
      paint_list(l, highlight_area = highlight_columns(l, "c"), highlight_rows = 1),
      msg, fixed = TRUE
    )
    expect_error(
      paint_array(Titanic, highlight_area = highlight_data(Titanic, rows = 1),
                  highlight_rows = 2),
      msg, fixed = TRUE
    )
  })
})

test_that("gpaint_ enforces the same both-supplied precedence error", {
  skip_if_not_installed("ggplot2")
  expect_error(
    gpaint_matrix(m, highlight_area = highlight_rows(m, 1), highlight_rows = 2),
    "Supply either `highlight_area`", fixed = TRUE
  )
})

# ---------------------------------------------------------------------------
# the shorthand reuses highlight_data()'s validation, it does not reimplement it
# ---------------------------------------------------------------------------

test_that("a bad selection errors through highlight_data(), unchanged", {
  with_null_pdf({
    # Unknown column name -> highlight_data()'s own message.
    expect_error(
      paint_data_frame(df, highlight_columns = "Nope"),
      "Unknown column name"
    )
    # Out-of-range row -> highlight_data()'s own message.
    expect_error(
      paint_matrix(m, highlight_rows = 99),
      "out of range"
    )
  })
})

test_that("passing no highlight argument at all still highlights nothing", {
  # Pin classic so the highlight token this asserts on is stable and the check
  # stays meaningful (a cell that WAS highlighted would carry "lemonchiffon").
  withr::local_options(paintr.palette = "classic")
  none <- base_cells(paint_matrix(m))
  # "Nothing highlighted" means no cell carries the highlight colour; ordinary
  # value cells still fill white.
  expect_false(any(none$fill == "lemonchiffon", na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# Part A guard: the graph_title default must keep reading the caller's object
# name, never the literal "data" a `...` forwarder would deparse one frame down.
# ---------------------------------------------------------------------------

test_that("graph_title's substitute(data) default survives the prep refactor", {
  my_matrix <- matrix(1:4, 2)
  my_vector <- c(1, 2, 3)
  my_frame  <- head(iris, 3)
  my_list   <- list(a = 1:2, b = 3:4)
  my_array  <- array(1:24, c(2, 3, 4))
  with_null_pdf({
    expect_equal(paint_matrix(my_matrix)$graph_title,    "Data Object: my_matrix")
    expect_equal(paint_vector(my_vector)$graph_title,    "Data Object: my_vector")
    expect_equal(paint_data_frame(my_frame)$graph_title, "Data Object: my_frame")
    expect_equal(paint_list(my_list)$graph_title,        "Data Object: my_list")
    expect_equal(paint_array(my_array)$graph_title,      "Data Object: my_array")
  })
})

test_that("gpaint_ carries the same substitute(data) title", {
  skip_if_not_installed("ggplot2")
  my_matrix <- matrix(1:4, 2)
  expect_equal(gpaint_matrix(my_matrix)$labels$title, "Data Object: my_matrix")
})
