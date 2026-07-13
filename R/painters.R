# The plumbing shared by all six painters.
#
# The six painters differ in exactly three things: what shape they accept, what
# `show_indices` means for that shape, and whether they draw a header row. Every
# other step -- read the options, build the option bundle, build the cell table,
# measure the column widths -- is identical, and the design document is explicit
# that a helper duplicated across `matrix.R` and `vector.R` is precisely how this
# package arrived at four copy-pasted bugs. So it is written once, here.
#
# Two rules are load-bearing:
#
#   1. OPTIONS ARE READ ONCE, HERE, AND THREADED DOWN AS PLAIN ARGUMENTS.
#      `paintr.ellipsis` and `paintr.warn_floor` never appear in a formal's
#      default. A `getOption()` in a default would make every snapshot in the
#      test suite a function of the user's `.Rprofile`, which is how R snapshot
#      suites go flaky. Nothing below the painter calls `getOption()` -- with the
#      single, deliberate exception of `warn_floor_once()`'s live kill switch, a
#      ggplot object being built long before it is drawn.
#
#   2. `graph_title`'s default stays in each painter's OWN formals. It is
#      `deparse(substitute(data))`, and `substitute()` evaluated one frame down
#      through a `...` forwarder yields the literal `"data"` -- so every title
#      would silently become "Data Object: data". That is why `painter_prep()`
#      takes no title and does no titling.

#' Everything a painter needs, before it picks a backend
#'
#' Builds the cell table and its column widths, and bundles them with the layout
#' options and the two `getOption()` values, which are read here and exactly here.
#'
#' @param data A vector, matrix, or data frame.
#' @param show_indices Already matched by the painter, because the permitted set
#'   differs between a vector and a grid.
#' @param highlight_area,highlight_color Passed to `paint_cells()`.
#' @param sigfig,subtle_digits,max_chars Passed to `paint_format()`.
#' @param max_rows,max_cols,show_all Elision, passed to `paint_cells()`.
#' @param fontsize,family Passed to `paint_opts()`.
#' @param layout Vectors only.
#' @param show_names,show_types Data frames only.
#'
#' @return A list with `cells`, `col_w`, `n_row`, `note` (a string, or `NULL`
#'   when nothing was elided), `opts`, and `warn_floor`.
#'
#' @keywords internal
#' @noRd
painter_prep <- function(data,
                         show_indices,
                         highlight_area,
                         highlight_color,
                         sigfig,
                         subtle_digits,
                         max_chars,
                         max_rows,
                         max_cols,
                         show_all,
                         fontsize,
                         family,
                         layout = "vertical",
                         show_names = TRUE,
                         show_types = TRUE) {
  # The two options, read once, in the one place that is allowed to read them.
  ellipsis <- getOption("paintr.ellipsis", "...")
  warn_floor <- isTRUE(getOption("paintr.warn_floor", TRUE))

  opts <- paint_opts(family = family, fontsize = fontsize)

  cells <- paint_cells(
    data = data,
    highlight_area = highlight_area,
    highlight_color = highlight_color,
    show_indices = show_indices,
    layout = layout,
    show_names = show_names,
    show_types = show_types,
    sigfig = sigfig,
    subtle_digits = subtle_digits,
    max_chars = max_chars,
    max_rows = max_rows,
    max_cols = max_cols,
    show_all = show_all,
    ellipsis = ellipsis
  )

  note <- attr(cells, "note")
  # NA means "nothing was elided". Both backends want an absent band, and
  # `ggplot2::labs(caption = NA_character_)` would draw the literal string "NA".
  if (length(note) != 1L || is.na(note)) {
    note <- NULL
  }

  list(
    cells = cells,
    col_w = column_widths(cells),
    n_row = attr(cells, "n_row"),
    note = note,
    opts = opts,
    warn_floor = warn_floor
  )
}

#' The ggplot2 skin
#'
#' The whole ggplot2 backend. It is a *shell*: one `annotation_custom()` holding
#' the grob, a meaningless `0..1` coordinate system that the grob owns outright,
#' and the chrome. There is no `aes()`, no geom, no `scale_fill_manual()`, and --
#' critically -- **no `ggproto()`**.
#'
#' The absent ggproto is what makes `Suggests: ggplot2` honest. A top-level
#' `GeomPaintr <- ggplot2::ggproto(...)` would be evaluated at *load* time, so on
#' a check machine without ggplot2 installed the package would fail to load.
#' `annotation_custom()` needs no ggproto at all, so no ggplot2 object is ever
#' constructed outside a `gpaint_*()` body, behind the `requireNamespace()` guard.
#'
#' `scale_*_continuous(limits = c(0, 1), expand = c(0, 0))` is not decoration: it
#' pins the panel to exactly the region `annotation_custom()`'s `-Inf..Inf`
#' extents resolve against. `coord_fixed()` is **not** used and would be actively
#' wrong -- the grob letterboxes itself in arithmetic, and the scales carry no
#' meaning to fix.
#'
#' @param prep From [painter_prep()].
#' @param graph_title,graph_subtitle Chrome. `NULL` draws nothing.
#'
#' @return A `ggplot` object.
#'
#' @keywords internal
#' @noRd
gpaint_skin <- function(prep, graph_title = NULL, graph_subtitle = NULL) {
  ggplot2::ggplot() +
    ggplot2::annotation_custom(
      paintr_grob(
        prep$cells,
        prep$col_w,
        prep$n_row,
        opts = prep$opts,
        warn_floor = prep$warn_floor
      )
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::labs(
      title = graph_title,
      subtitle = graph_subtitle,
      caption = prep$note
    ) +
    ggplot2::theme_void() +
    # BOTH BACKENDS DRAW THE SAME PICTURE, and that includes the chrome. The
    # "# 11 more rows" note is one string, resolved once by `painter_prep()`, and
    # `draw_bands()` puts it at the bottom LEFT (`mtext(adj = 0)`, at the panel's
    # left edge) -- but `plot.caption` inherits `hjust = 1` from the theme, so the
    # ggplot2 skin was quietly putting the same string at the bottom RIGHT. Same
    # data, two pictures, which is the exact class of drift the shared cell table
    # exists to make impossible. `hjust = 0` is base's answer, so it is this one's.
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
}

# ---------------------------------------------------------------------------
# the subtitle
# ---------------------------------------------------------------------------
#
# The subtitle's default CANNOT live in a painter's formals, even though that is
# where it lived before and where it reads most naturally. A default like
#
#     graph_subtitle = paste0("Dimensions: ", nrow(data), ...)
#
# is a promise over `data`, and `data` is forced by the type check -- so it works,
# right up until a painter wants to say "no subtitle". `graph_subtitle = NULL`
# would then be indistinguishable from "the user did not ask for one", because
# that IS the default. Computing the default in the BODY is what buys the third
# state, and the three are:
#
#     NULL           -> compute the default line          (the common case)
#     NA, or ""      -> draw no subtitle at all           (opt out)
#     "some text"    -> draw exactly that                 (opt in)
#
# `NULL` out of `resolve_subtitle()` means "no band" to BOTH backends, which is
# the whole reason the empty cases are collapsed to it here rather than at the
# renderer: `base_mai()` would reserve a band for the literal string "NA", and
# `ggplot2::labs(subtitle = NA_character_)` would happily draw it.

#' The default subtitle for a two-dimensional structure: a matrix or a data frame
#'
#' It describes the DATA, not the drawing. A 30-row matrix elided down to 20 drawn
#' rows still reports 30: the "# 10 more rows" note is what tells the reader that
#' the picture is partial, and the subtitle is what tells them what the picture is
#' of. Reading these dimensions off the cell table -- which knows only the drawn
#' extent -- would silently make the two lines say the same thing twice, and lose
#' the true shape entirely.
#'
#' One function, not one per painter. `nrow()`, `ncol()` and `class()` read a data
#' frame exactly as they read a matrix, so a `df_subtitle()` would be this body
#' pasted a second time -- and a second body is a second thing to forget to change.
#' The class is what distinguishes the two lines, and it is read from the data.
#'
#' **Not `grid_subtitle()`.** In this package `grid` is the name of a GRAPHICS
#' BACKEND -- `render_grid()`, `measure_grid()`, `panel_grid()`, `R/render-grid.R`
#' -- so `grid_subtitle()` reads as "the subtitle for the grid renderer", which it
#' is not: the BASE painter calls it, both painters call it, and it is not about a
#' renderer at all. It is about the DIMENSIONS of the data, which is what it is now
#' named for.
#'
#' @param data The matrix or data frame, before any elision.
#'
#' @return A length-one character string.
#'
#' @keywords internal
#' @noRd
dims_subtitle <- function(data) {
  paste0(
    "Dimensions: ", nrow(data), " rows x ", ncol(data), " columns", " | ",
    "Data Type: ", paste(class(data), collapse = ", ")
  )
}

#' The default subtitle for a vector
#'
#' @param data The vector, before any elision. `length()`, not the drawn count.
#'
#' @return A length-one character string.
#'
#' @keywords internal
#' @noRd
vector_subtitle <- function(data) {
  paste0(
    "Length: ", length(data), " elements | ",
    "Data Type: ", paste(class(data), collapse = ", ")
  )
}

#' Apply the subtitle contract
#'
#' @param graph_subtitle What the user passed: `NULL`, `NA`, `""`, or a string.
#' @param default What [dims_subtitle()] or [vector_subtitle()] computed.
#'
#' @return A length-one character string to draw, or `NULL` to draw nothing.
#'
#' @keywords internal
#' @noRd
resolve_subtitle <- function(graph_subtitle, default) {
  if (is.null(graph_subtitle)) {
    graph_subtitle <- default
  }
  # `has_text()` (R/render-base.R) is the one definition of "is there a band
  # here?", and it already reads NA, "" and character(0) as "no".
  if (!has_text(graph_subtitle)) {
    return(NULL)
  }
  as.character(graph_subtitle)[[1L]]
}

#' Stop unless ggplot2 is installed
#'
#' The guard that keeps `Suggests: ggplot2` true. Called at the very top of every
#' `gpaint_*()`, before any work is done.
#'
#' @keywords internal
#' @noRd
require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please make sure `ggplot2` is installed to use this function.")
  }
  invisible(TRUE)
}

#' Is this something `paint_vector()` will draw?
#'
#' **Not `is.vector()`.** `is.vector()` is FALSE for anything carrying an
#' attribute other than `names`, so `is.vector(factor("a"))` and
#' `is.vector(Sys.Date())` are both FALSE -- and the current `paint_vector()`
#' therefore rejects a factor and a Date outright. What we actually mean is "a
#' one-dimensional atomic thing": no `dim`, not a data frame, and a basic type
#' underneath, whatever class has been layered on top of it.
#'
#' @keywords internal
#' @noRd
is_paint_vector <- function(x) {
  is.atomic(x) && is.null(dim(x)) && !is.data.frame(x)
}
