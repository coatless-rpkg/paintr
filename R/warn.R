# The legibility-floor warning. ONE definition, shared by BOTH renderers.
#
# It lives in its own file, and not in either renderer, for a reason that this
# package has already been bitten by: `render-base.R` and `render-grid.R` are
# the two backends, and every function that exists in both of them is a function
# that WILL drift. (The bug this package exists to fix is exactly that: the
# identical broken `ifelse` chain, copy-pasted into `matrix.R` and `vector.R`.)
#
# Nothing here touches a device, `graphics`, or `grid`. It is string arithmetic,
# so it stays testable with nothing open.

#' The legibility floor warning
#'
#' `min_pt` is a **threshold**, never a clamp. `max(fontsize, min_pt)` would push
#' the text back up until it overlapped -- the exact smear this engine exists to
#' fix. So the text is drawn at its honest size and the user is told, once, with
#' the numbers of the device they actually have.
#'
#' When `dev_in` is supplied the message names the device that WOULD clear the
#' floor. That number is only knowable at draw time, which is why it is an
#' argument rather than something the message computes: at construction time
#' there is no device, and a suggestion computed against a fabricated reference
#' device is worse than none -- it warns when the user's real 14in device would
#' have been fine, and stays silent when their 3in one will not be.
#'
#' The suggested size is deliberately conservative. Font size is exactly linear
#' in the panel's linear size, so scaling the *device* by `min_pt / fontsize`
#' grows the *panel* by at least that factor (the margins do not scale with it),
#' and the answer is therefore never an under-estimate.
#'
#' @param resolved From `paint_resolve()`.
#' @param opts From `paint_opts()`.
#' @param dev_in The current device size in inches, from `grDevices::dev.size()`.
#'   `NULL` omits the enlargement advice.
#'
#' @return A length-one character string. **ASCII only** -- this string lands in
#'   `R CMD check` logs on latin1 and Windows machines.
#'
#' @keywords internal
#' @noRd
floor_message <- function(resolved, opts, dev_in = NULL) {
  n <- sum(resolved$cells$kind == "value")
  fs <- resolved$fontsize

  advice <- if (is.null(dev_in) || !all(is.finite(as.double(dev_in))) ||
                !is.finite(fs) || fs <= 0) {
    "  * Enlarge the graphics device, or set fig.width/fig.height in your knitr chunk.\n"
  } else {
    # Round UP to a tenth of an inch: a suggested size that is a hair too small
    # is worse than useless.
    need <- ceiling(as.double(dev_in) * (opts$min_pt / fs) * 10) / 10
    paste0(
      "  * Enlarge the device to at least ", need[[1L]], " x ", need[[2L]],
      " inches:\n",
      "      png(width = ", need[[1L]], ", height = ", need[[2L]],
      ", units = \"in\", res = 96)\n",
      "      or set fig.width/fig.height in your knitr chunk.\n"
    )
  }

  paste0(
    "paintr is drawing ", n, " cells at ",
    formatC(fs, format = "f", digits = 1), " pt, below the ",
    formatC(opts$min_pt, format = "f", digits = 1), " pt legibility floor.\n",
    advice,
    "  * Or draw fewer cells: lower `max_rows`/`max_cols`, or drop ",
    "`show_all = TRUE` to elide the middle.\n",
    "  Silence with options(paintr.warn_floor = FALSE)."
  )
}
