# Tier 4: FIGURE COUNT. A page that draws N base plots must emit N figures.
#
# This guards a regression that a plain replay test cannot see. `pkgdown`,
# `knitr`, `rmarkdown` and `quarto` all render a code block by running it through
# `evaluate::evaluate(new_device = TRUE)` and then REPLAYING each captured plot
# onto a figure device -- and both `evaluate` (`trim_intermediate_plots()`) and
# `downlit` (`merge_low_plot()` / `is_low_change()`) decide two consecutive plots
# are "the same, built up in stages" by testing whether one plot's display list is
# a PREFIX of the next. A plot that is a prefix of its successor is dropped.
#
# `plot.new()` is what breaks that prefix relation: it RESETS the engine display
# list, so plot N+1 does not begin with plot N. Wrap `plot.new()` inside
# `grDevices::recordGraphics()` and that reset is suppressed -- every recorded plot
# then carries all the previous ones as a prefix, and the prefix test collapses a
# whole multi-plot page down to a handful of figures. Measured on a full pkgdown
# build with the page wrapped: 47 reference figures fell to 17.
#
# So this exercises the EXACT pkgdown pipeline -- `downlit::evaluate_and_highlight()`
# with a `fig_save` (the same call `pkgdown:::highlight_examples()` makes) -- on a
# block of distinct painters, and asserts that every one of them still becomes its
# own figure file. On the wrapped-`plot.new()` bug this emitted a single figure.

# Count the figure files `downlit` writes for a code block, through the exact call
# pkgdown uses. `fig_save(plot, id)` is invoked once per figure downlit decides to
# keep; each one writes a PNG and returns pkgdown's `list(path, width, height)`.
figures_emitted <- function(code, dir) {
  n <- 0L
  fig_save <- function(plot, id) {
    n <<- n + 1L
    path <- file.path(dir, paste0("fig-", n, ".png"))
    grDevices::png(path, width = 200, height = 200)
    on.exit(grDevices::dev.off(), add = TRUE)
    grDevices::replayPlot(plot)
    list(path = path, width = 200L, height = 200L)
  }
  downlit::evaluate_and_highlight(code, fig_save = fig_save, env = environment())
  length(list.files(dir, pattern = "\\.png$"))
}

test_that("each base plot in a block emits its own figure through pkgdown's pipeline", {
  skip_on_cran()
  skip_if_not_installed("downlit")
  skip_if_not_installed("evaluate")

  # Four DISTINCT structures. Their display lists differ (different reserved
  # margins, window and content), so a correct pipeline keeps all four -- and a
  # `plot.new()` wrapped in `recordGraphics()` collapses them, because each plot's
  # recording then contains the previous one as a prefix. Measured: this exact
  # block emitted ONE figure on the bug, four on the fix.
  code <- paste(
    'paint_list(list(a = 1:4, b = "x"))',
    "paint_matrix(matrix(1:4, 2))",
    "paint_vector(letters[1:5])",
    "paint_data_frame(data.frame(x = 1:3))",
    sep = "\n"
  )

  skip_if_not(isTRUE(capabilities("png")), "no png device")

  dir <- withr::local_tempdir()
  expect_equal(figures_emitted(code, dir), 4L)
})
