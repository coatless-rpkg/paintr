# The colour palette layer.
#
# Two claims are load-bearing and each is a test below:
#
#   1. `classic` is BYTE-IDENTICAL to the original look. It is not a hex table
#      that happens to equal the old tokens -- it is the ABSENCE of a remap, so
#      the resolved cell table still carries "black"/"red"/"lemonchiffon". If the
#      remap ever leaks into classic, the exactness test fails on the token.
#   2. The remap is the one thing both backends share. It happens at
#      `paint_resolve()`, upstream of either renderer, so base and grid cannot
#      draw a cell a different colour. The parity test resolves one table through
#      two measures and asserts the colour columns are identical.

# A null pdf device, torn down at the end of the calling test. `measure_base()`
# and `measure_grid()` both need an open device to measure against.
local_null_pdf <- function(env = parent.frame()) {
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  withr::defer(grDevices::dev.off(dev), envir = env)
  invisible(dev)
}

# Resolve a structure's cell table against a fake 7x5in panel under a palette,
# with no device open. The colours are a pure function of the cell table and the
# palette, so `measure_mono()` and `panel_fake()` are all this needs.
resolve_under <- function(data, palette, ..., measure = measure_mono()) {
  cells <- paint_cells(data, ...)
  paint_resolve(
    cells, column_widths(cells), attr(cells, "n_row"),
    panel_fake(7, 5), measure, paint_opts(palette = palette)
  )$cells
}

# ---------------------------------------------------------------------------
# resolve_palette()
# ---------------------------------------------------------------------------

test_that("resolve_palette() maps names, classic and the option", {
  # classic is the signal for "no remap".
  expect_null(resolve_palette("classic"))

  # A NULL argument follows the option, defaulting to mint.
  withr::local_options(paintr.palette = NULL)
  expect_identical(resolve_palette(NULL), .paintr_palette_defs$mint)

  withr::local_options(paintr.palette = "slate")
  expect_identical(resolve_palette(NULL), .paintr_palette_defs$slate)

  # A classic option resolves to NULL, exactly as the literal name does.
  withr::local_options(paintr.palette = "classic")
  expect_null(resolve_palette(NULL))
})

test_that("resolve_palette() returns the named palette's colours", {
  expect_identical(resolve_palette("mint"), .paintr_palette_defs$mint)
  expect_identical(resolve_palette("slate"), .paintr_palette_defs$slate)
  expect_identical(resolve_palette("warm"), .paintr_palette_defs$warm)
})

test_that("resolve_palette() rejects an unknown name", {
  expect_error(resolve_palette("neon"), "neon")
  expect_error(resolve_palette("neon"), "mint")
  expect_error(resolve_palette(c("mint", "slate")), "palette")
})

test_that("resolve_palette() accepts a custom list and defaults header/rule", {
  custom <- list(
    value = "#111111", na = "#ff0000", special = "#0000ff",
    insig = "#cccccc", label = "#666666", label2 = "#999999",
    grid = "#eeeeee", outline = "#dddddd", bg = "#ffffff",
    highlight = "#abcdef"
  )
  got <- resolve_palette(custom)
  expect_identical(got$value, "#111111")
  # A custom list need not carry `header`/`rule`: the band falls back to the grid
  # tint and `rule` to the outline, so neither role forces every list to grow.
  expect_identical(got$rule, custom$outline)
  expect_identical(got$header, custom$grid)

  # A list missing a required role is refused, and the message names the role.
  expect_error(resolve_palette(custom[setdiff(names(custom), "na")]), "na")
})

# ---------------------------------------------------------------------------
# apply_palette()
# ---------------------------------------------------------------------------

test_that("apply_palette(cells, NULL) is the identity", {
  cells <- paint_cells(matrix(c(1 / 3, NA, Inf, 2), nrow = 2), show_indices = "all")
  expect_identical(apply_palette(cells, NULL), cells)
})

# ---------------------------------------------------------------------------
# classic is byte-identical to the original look
# ---------------------------------------------------------------------------

test_that("classic leaves the classic tokens on the resolved cell table", {
  structures <- list(
    matrix = matrix(c(1 / 3, NA, Inf, 2), nrow = 2),
    data_frame = data.frame(n = c(1.5, NA), s = c("a", "b")),
    vector = c(3, NA, -1, NaN, Inf),
    list = list(a = 1:3, b = 4:6),
    array = array(1:24, c(2, 3, 4))
  )
  for (nm in names(structures)) {
    cells <- resolve_under(
      structures[[nm]], "classic",
      highlight_area = TRUE
    )
    # Every colour column still holds classic name-tokens, never a hex.
    # `grepl()` reads an NA cell as FALSE, so no `na.rm` is needed.
    expect_false(any(grepl("^#", cells$ink)), info = nm)
    expect_false(any(grepl("^#", cells$border)), info = nm)
    expect_false(any(grepl("^#", cells$fill)), info = nm)
    # And the specific tokens the renderers key on are exactly the old ones.
    expect_true(all(cells$fill[cells$kind == "value"] %in% c("white", "lemonchiffon")),
                info = nm)
    expect_identical(unique(cells$border[cells$kind == "outline"]), "black", info = nm)
  }
})

test_that("classic opts keep grey70, mint overrides it", {
  expect_identical(paint_opts(palette = "classic")$grey, "grey70")
  expect_null(paint_opts(palette = "classic")$palette)
  expect_identical(paint_opts(palette = "mint")$grey, "#C2C2C9")
})

# ---------------------------------------------------------------------------
# mint remaps every token
# ---------------------------------------------------------------------------

test_that("mint remaps ink, border and fill to its hexes", {
  m <- matrix(c(1 / 3, NA, Inf, 2), nrow = 2)
  # Highlight the finite value in the top-left so a highlight fill is present.
  cells <- resolve_under(
    m, "mint",
    highlight_area = matrix(c(TRUE, FALSE, FALSE, FALSE), nrow = 2),
    show_indices = "cell"
  )
  val <- cells[cells$kind == "value", , drop = FALSE]

  # ink: value -> value, NA -> na, Inf/NaN -> special.
  expect_identical(unique(val$ink[val$sig == "0.333"]), "#1A1A1E")
  expect_identical(unique(val$ink[val$sig == "NA"]), "#DC2626")
  expect_identical(unique(val$ink[val$sig == "Inf"]), "#2563EB")

  # border: the heavy outline and the inner grid part on lwd.
  expect_identical(unique(cells$border[cells$kind == "outline"]), "#AEB5BE")
  expect_identical(unique(val$border), "#CBD1D8")

  # fill: white -> bg, lemonchiffon -> highlight.
  expect_identical(unique(val$fill[val$fill != "#CFF0E2"]), "#FFFFFF")
  expect_true("#CFF0E2" %in% val$fill)

  # the in-cell index is a grey50 token -> label2.
  expect_identical(unique(cells$ink[cells$kind == "cellindex"]), "#9BA1A9")
})

test_that("mint remaps the data frame type-tag ink to label2", {
  df <- data.frame(n = c(1.5, 22.2), s = c("a", "bb"))
  cells <- resolve_under(df, "mint")
  # `<dbl>`/`<chr>` type tags are a grey50 token -> label2.
  expect_identical(unique(cells$ink[cells$kind == "type"]), "#9BA1A9")
  # Column-name headers are a black token -> value.
  expect_identical(unique(cells$ink[cells$kind == "header"]), "#1A1A1E")
})

# ---------------------------------------------------------------------------
# backend parity
# ---------------------------------------------------------------------------

test_that("the resolved colours are identical whichever backend resolves them", {
  # `paint_resolve()` is the one seam both `render_base()` and `paintr_grob()`
  # go through, so resolving the same cells with the two production measures --
  # base and grid -- must give the same colour columns. If a renderer ever
  # coloured a cell itself, this is what would catch the drift.
  local_null_pdf()
  m <- matrix(c(1 / 3, NA, Inf, 2), nrow = 2)
  cells <- paint_cells(
    m,
    highlight_area = matrix(c(TRUE, FALSE, FALSE, FALSE), nrow = 2),
    show_indices = "all"
  )
  cw <- column_widths(cells)
  nr <- attr(cells, "n_row")
  opts <- paint_opts(palette = "mint")

  base <- paint_resolve(cells, cw, nr, panel_fake(7, 5), measure_base("mono"), opts)$cells
  grid <- paint_resolve(cells, cw, nr, panel_fake(7, 5), measure_grid("mono"), opts)$cells

  expect_identical(base$ink, grid$ink)
  expect_identical(base$border, grid$border)
  expect_identical(base$fill, grid$fill)
})
