# Tier 3: the grid renderer.
#
# THIS FILE DEPENDS ON `grid` ALONE. It contains no reference to ggplot2, and it
# must not acquire one: that is what lets the hardest code in the package -- the
# deferred font fit -- be tested with zero Suggests installed and stay immune to
# ggplot2 4.x churn. `gpaint_*()` is a ~25-line skin over `paintr_grob()`, and
# the skin is the only place ggplot2 is allowed to appear.
#
# Why a custom grob at all, when it looks like an optimisation:
#
#   1. ggplot2 CANNOT MEASURE TEXT AT BUILD TIME. The panel is a
#      `null` unit and `convertWidth(unit(1, "null"), "in")` returns 0 outside a
#      layout, while the fitting size swings 4.7x across plausible devices. So the
#      font size MUST be computed at DRAW time, inside `makeContent()`, where the
#      panel is finally real. A size baked into the cell table is wrong on every
#      device but one.
#   2. TWO-TONE TEXT IS IMPOSSIBLE IN A LAYER. `geom_text()`'s `colour` is one
#      value per row, and the offset between the two spans is intrinsically in
#      inches (it derives from the font size) while `nudge_x` is in data units.
#
# One mechanism, two problems. It is load-bearing, not decorative.

# The grey the insignificant span is drawn in is `opts$grey`, read straight, in
# THIS file and in `render-base.R` alike. It used to be a `subtle_grey()` helper
# here and a hand-inlined `if (is.null(opts$grey)) "grey70"` there -- one value,
# two definitions, which is the shape of every bug in this package's history. The
# knob those two were both waiting for now exists (`paint_opts(grey =)`), so both
# fallbacks are gone and there is one source of truth again.
#
# It is an option rather than a cell-table column because every cell with a
# non-empty `insig` is a finite number: "NA", "Inf" and "NaN" carry no
# insignificant digits (`split_sig()` finds none to count past), and in scientific
# mode `insig` is forced to `""`. One colour serves the whole plot.

# ---------------------------------------------------------------------------
# the legibility-floor warning
# ---------------------------------------------------------------------------
#
# `floor_message()` itself lives in `R/warn.R`, shared with `render-base.R`. The
# once-per-object latch below is grid-specific: a base plot is drawn exactly once
# per `render_base()` call, but a grob is re-drawn on every device resize, page
# refresh and `ggsave()` replay.

#' Warn about the legibility floor at most once per grob
#'
#' A grob is re-drawn every time its device is resized, its page is refreshed, or
#' `ggsave()` replays it. Warning on each of those would be a torrent. The gTree
#' carries a small `environment()` as a field, which survives `makeContent()`
#' returning a modified copy of the grob (environments are references), so the
#' flag is per-object and exactly one warning is emitted no matter how many times
#' the object is drawn.
#'
#' @param state The grob's state environment.
#' @param enabled The grob's `warn_floor` field, threaded down from the painter.
#' @param resolved,opts,dev_in Passed to `floor_message()` (in `R/warn.R`).
#'
#' @return `TRUE` if a warning was emitted, invisibly.
#'
#' @keywords internal
#' @noRd
warn_floor_once <- function(state, enabled, resolved, opts, dev_in) {
  # Two gates, and both are deliberate. `enabled` is the option threaded down as
  # plain data by the painter (nothing below the painter is allowed to depend on
  # the user's .Rprofile for its *defaults*). The `getOption()` re-read is a live
  # kill switch: a ggplot object is built long before it is drawn, so a user who
  # sets the option between the two would otherwise still be shouted at.
  if (!isTRUE(enabled) || !isTRUE(getOption("paintr.warn_floor", TRUE))) {
    return(invisible(FALSE))
  }
  if (isTRUE(state$warned)) {
    return(invisible(FALSE))
  }
  state$warned <- TRUE
  warning(floor_message(resolved, opts, dev_in), call. = FALSE)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# the grob
# ---------------------------------------------------------------------------

#' A grid grob that fits its own text at draw time
#'
#' Holds the cell table and defers every device-dependent decision to
#' [makeContent.paintr_grob()]. Constructing it opens no device, reads no device,
#' and measures no text -- which is precisely why it can be handed to
#' `ggplot2::annotation_custom()`, whose panel does not exist yet.
#'
#' @param cells A cell table from `paint_cells()`.
#' @param col_w Column widths in layout units, from `column_widths()`.
#' @param n_row Drawn rows, from `attr(cells, "n_row")`.
#' @param opts From `paint_opts()`.
#' @param warn_floor Warn when the fitted size falls below `opts$min_pt`? The
#'   painter threads `getOption("paintr.warn_floor", TRUE)` in here as plain
#'   data.
#' @param name,vp Passed to [grid::gTree()].
#'
#' @return A gTree of class `"paintr_grob"`.
#'
#' @keywords internal
#' @noRd
paintr_grob <- function(cells, col_w, n_row, opts = paint_opts(),
                        warn_floor = TRUE, name = NULL, vp = NULL) {
  if (!is.data.frame(cells) || nrow(cells) == 0L) {
    stop("`cells` must be a non-empty cell table data frame.")
  }
  if (!is.numeric(col_w) || length(col_w) < max(cells$col)) {
    stop(
      "`col_w` has ", length(col_w), " widths but the cell table draws ",
      max(cells$col), " columns."
    )
  }
  if (length(n_row) != 1L || is.na(n_row) || n_row < 1) {
    stop("`n_row` must be a single positive number.")
  }

  # Reference semantics on purpose: `makeContent()` gets a *copy* of the grob, so
  # a plain logical field could never be flipped in a way the next draw would
  # see.
  state <- new.env(parent = emptyenv())
  state$warned <- FALSE

  grid::gTree(
    cells = cells,
    col_w = as.double(col_w),
    n_row = as.integer(n_row),
    opts = opts,
    warn_floor = isTRUE(warn_floor),
    state = state,
    name = name,
    vp = vp,
    cl = "paintr_grob"
  )
}

#' The children of a resolved cell table
#'
#' One `rectGrob` for every cell that has a fill or a border, and **one
#' `textGrob` per span** -- so two text calls for the whole plot, not two per
#' cell. That is the only mechanism that draws two-tone text in grid *or* in
#' base: a single call with `col = c("black", "grey70")` and one `x` draws one
#' span and warns.
#'
#' Both spans are LEFT-anchored (`hjust = 0`) at `x + dx`, which is what makes
#' `dx_insig = dx_sig + w(sig)` land the grey digits exactly where the black ones
#' stop.
#'
#' Vertically, both spans are centred on `y` -- and `y` is the cell's centre plus
#' its `dy_rel` nudge, already resolved. That is the whole of this backend's
#' knowledge of the `[i, j]` index that shares a cell with its value: none. Both
#' renderers read the same resolved `y` out of the same `paint_resolve()`, so they
#' cannot disagree about where it goes.
#'
#' @param res The list returned by `paint_resolve()`.
#' @param opts From `paint_opts()`.
#'
#' @return A [grid::gList()].
#'
#' @keywords internal
#' @noRd
paintr_children <- function(res, opts) {
  cells <- res$cells
  family <- opts$family
  grey <- opts$grey
  kids <- list()

  # -- rectangles -------------------------------------------------------------
  # Fill, border AND STROKE WEIGHT, all three straight off the table. The weight is
  # what was missing: this grob folded the outline in with the ordinary cells and
  # never set `lwd`, so it drew the heavy border at 1 while `draw_base()` drew it
  # at 2 -- the two backends drew different pictures, and the outline that makes
  # the block read as one object was absent from every ggplot2 rendering.
  #
  # `boxed_cells()` is shared with `draw_base()`: it selects the same cells and
  # puts them in the same order (heaviest stroke last, so the cell borders cannot
  # paint over the outline). Neither renderer names a `kind`.
  # Square cells stay ONE `rectGrob`; a refined palette's rounded cells (the block
  # outline and the header band) become polygon grobs tracing the SAME vertices
  # `draw_base()` does, from the shared `rounded_rect_xy()`, so neither backend can
  # round the card differently. Square first, rounded after, in `boxed_cells()`'s
  # heavy-last order.
  boxed <- boxed_cells(cells)
  sq <- boxed[boxed$radius <= 0, , drop = FALSE]
  rd <- boxed[boxed$radius > 0, , drop = FALSE]
  if (nrow(sq) > 0L) {
    kids[[length(kids) + 1L]] <- grid::rectGrob(
      x = grid::unit(sq$xl, "in"),
      y = grid::unit(sq$yb, "in"),
      width = grid::unit(sq$xr - sq$xl, "in"),
      height = grid::unit(sq$yt - sq$yb, "in"),
      just = c("left", "bottom"),
      gp = grid::gpar(fill = sq$fill, col = sq$border, lwd = sq$lwd),
      name = "paintr.rect"
    )
  }
  for (k in seq_len(nrow(rd))) {
    p <- rounded_rect_xy(rd$xl[[k]], rd$yb[[k]], rd$xr[[k]], rd$yt[[k]], rd$radius[[k]])
    kids[[length(kids) + 1L]] <- grid::polygonGrob(
      x = grid::unit(p$x, "in"),
      y = grid::unit(p$y, "in"),
      gp = grid::gpar(fill = rd$fill[[k]], col = rd$border[[k]], lwd = max(rd$lwd[[k]], 1)),
      name = paste0("paintr.round.", k)
    )
  }

  # -- the black span ---------------------------------------------------------
  # `inked_cells()` -- shared with `draw_base()` in R/render-base.R -- is the one
  # place that decides which spans are real, non-empty strings; see its own docs
  # for why `nzchar()` alone is not enough. This grob needs the `sig` and `insig`
  # rows separately, for two `textGrob()`s.
  sig <- inked_cells(cells, "sig")
  if (nrow(sig) > 0L) {
    kids[[length(kids) + 1L]] <- grid::textGrob(
      label = sig$sig,
      x = grid::unit(sig$x, "in") + grid::unit(sig$dx_sig, "in"),
      y = grid::unit(sig$y, "in"),
      hjust = 0,
      vjust = 0.5,
      gp = grid::gpar(
        col = sig$ink,
        fontsize = sig$fontsize,
        fontfamily = family,
        # `"plain"`/`"bold"` is grid's own gpar vocabulary, so the cell's column
        # rides straight in: bold only on a refined header's names, plain else.
        fontface = sig$fontface
      ),
      name = "paintr.sig"
    )
  }

  # -- the grey span ----------------------------------------------------------
  insig <- inked_cells(cells, "insig")
  if (nrow(insig) > 0L) {
    kids[[length(kids) + 1L]] <- grid::textGrob(
      label = insig$insig,
      x = grid::unit(insig$x, "in") + grid::unit(insig$dx_insig, "in"),
      y = grid::unit(insig$y, "in"),
      hjust = 0,
      vjust = 0.5,
      gp = grid::gpar(
        col = grey,
        fontsize = insig$fontsize,
        fontfamily = family,
        fontface = insig$fontface
      ),
      name = "paintr.insig"
    )
  }

  do.call(grid::gList, kids)
}

#' Fit the text and build the children, at draw time
#'
#' The whole reason the grob exists. `convertWidth(unit(1, "npc"), "in")` is `0`
#' at build time and the true panel width here, so this is the first and only
#' moment the font size can honestly be chosen.
#'
#' It calls the **shared** `paint_resolve()` -- the same function, on the same
#' cell table, that `render_base()` calls -- with `measure_grid()` swapped in for
#' `measure_base()`. Nothing about the fit is reimplemented here, which is what
#' makes "base and ggplot draw the same picture" a property of one function
#' rather than a coincidence between two.
#'
#' @param x A `paintr_grob`.
#'
#' @return `x`, with its children set.
#'
#' @keywords internal
#' @noRd
#' @exportS3Method grid::makeContent
makeContent.paintr_grob <- function(x) {
  panel <- panel_grid()
  res <- paint_resolve(
    x$cells, x$col_w, x$n_row,
    panel = panel,
    measure = measure_grid(x$opts$family),
    opts = x$opts
  )

  if (isTRUE(res$floored)) {
    warn_floor_once(
      state = x$state,
      enabled = x$warn_floor,
      resolved = res,
      opts = x$opts,
      dev_in = grDevices::dev.size("in")
    )
  }

  grid::setChildren(x, paintr_children(res, x$opts))
}
