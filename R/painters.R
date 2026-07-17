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
#' @param max_rows,max_cols,max_slices,show_all Elision, passed to `paint_cells()`.
#'   `max_slices` is an array's third elision axis and is `NULL` for every other
#'   structure.
#' @param slices_per_row Arrays only: how many slice blocks to wrap to a row.
#'   `NULL` is the array's own grid, and it is `NULL` for every other structure.
#' @param fontsize,family,palette Passed to `paint_opts()`.
#' @param layout Vectors only.
#' @param summarise Lists only: one cell per element, saying what it is.
#' @param show_names Data frames and lists (the column-name row) and vectors (their
#'   `names()`).
#' @param show_types Data frames and lists only.
#' @param show_dimnames Matrices only. Already checked by the painter.
#' @param show_rownames Data frames only. `NULL` decides on the data.
#' @param max_name_chars The cap on a name lane that shares a formatting unit with
#'   the values under it.
#' @param name_align,type_align Data frames only. Already matched by the painter.
#' @param gap Lists only: empty space between element columns, in column-width
#'   units. `0` (the default) is the no-op every other structure takes.
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
                         palette = NULL,
                         max_slices = NULL,
                         slices_per_row = NULL,
                         layout = "vertical",
                         summarise = FALSE,
                         show_names = TRUE,
                         show_types = TRUE,
                         show_dimnames = "all",
                         show_rownames = NULL,
                         max_name_chars = 8L,
                         name_align = "center",
                         type_align = "center",
                         gap = 0) {
  # The two options, read once, in the one place that is allowed to read them.
  ellipsis <- getOption("paintr.ellipsis", "...")
  warn_floor <- isTRUE(getOption("paintr.warn_floor", TRUE))

  opts <- paint_opts(family = family, fontsize = fontsize, palette = palette)

  # The refined header structure (bold names, a unified header card, a tinted
  # band) is the look of a non-classic palette. `paint_opts()` resolves `classic`
  # to a NULL palette, so a NULL palette IS classic -- and classic takes none of
  # the refinement, leaving its cell table byte-identical to the original.
  styled <- !is.null(opts$palette)

  cells <- paint_cells(
    data = data,
    highlight_area = highlight_area,
    highlight_color = highlight_color,
    show_indices = show_indices,
    layout = layout,
    summarise = summarise,
    show_names = show_names,
    show_types = show_types,
    show_dimnames = show_dimnames,
    show_rownames = show_rownames,
    name_align = name_align,
    type_align = type_align,
    sigfig = sigfig,
    subtle_digits = subtle_digits,
    max_chars = max_chars,
    max_name_chars = max_name_chars,
    max_rows = max_rows,
    max_cols = max_cols,
    max_slices = max_slices,
    slices_per_row = slices_per_row,
    show_all = show_all,
    gap = gap,
    styled = styled,
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
  # DELIBERATE DIVERGENCE FROM THE BASE BACKEND. `draw_bands()` fits the base
  # title/subtitle/note to the device width (`fit_band()`); here they are drawn by
  # `ggplot2::labs()`, so their size is the theme's to set and their overflow is
  # ggplot2's to handle. paintr does not fit them, on purpose -- the chrome font is
  # not part of the cell diagram, and only the cell diagram must be pixel-identical
  # across backends.
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

#' The default subtitle for a list
#'
#' **A LIST HAS NO DIMENSIONS, AND `dims_subtitle()` WOULD SAY SO IN THE WORST
#' POSSIBLE WAY.** `nrow()` and `ncol()` are NULL for a list, and `paste0()` drops a
#' NULL rather than erroring, so the line would come out as the literal
#' "Dimensions:  rows x  columns" -- a blank where the number should be, on every
#' picture, silently. What a list has instead is a LENGTH, in elements, exactly as a
#' vector does.
#'
#' The element lengths are what make it a list rather than a data frame, so the line
#' reports their range. `list(a = 1:4, b = "x", c = c(TRUE, FALSE, NA))` reads "3
#' elements of length 1 to 4"; a list whose elements happen to share a length -- a
#' data frame in all but class -- reads "of length 3", and the reader can see for
#' themselves that it is a rectangle.
#'
#' @param data The list, before any elision.
#'
#' @return A length-one character string.
#'
#' @keywords internal
#' @noRd
list_subtitle <- function(data) {
  n <- lengths(data, use.names = FALSE)
  span <- if (length(n) == 0L) {
    ""
  } else if (min(n) == max(n)) {
    paste0(" of length ", min(n))
  } else {
    paste0(" of length ", min(n), " to ", max(n))
  }
  paste0(
    "Length: ", length(data), " element", if (length(data) != 1L) "s" else "", span,
    " | Data Type: ", paste(class(data), collapse = ", ")
  )
}

#' The default subtitle for an array
#'
#' **`dims_subtitle()` CANNOT SPEAK ABOUT AN ARRAY OF RANK THREE OR MORE.** `nrow()`
#' and `ncol()` are defined for one -- they report `dim(x)[1]` and `dim(x)[2]` -- so
#' it does not error; it LIES. `Titanic` would read "Dimensions: 4 rows x 2 columns",
#' which is true of a slice and false of the object, and the two axes it forgot to
#' mention are the entire reason the picture looks the way it does. An array has a
#' SHAPE, and the shape is every one of its extents.
#'
#' `4 x 2 x 2 x 2` is R's own vocabulary for it, and it is ASCII: the multiplication
#' sign is the letter `x`, never U+00D7, for the same reason `elem_sum()`'s is.
#'
#' **RANK TWO IS HANDED STRAIGHT BACK TO `dims_subtitle()`, AND THAT IS THE WHOLE
#' POINT OF PUTTING THE TEST HERE.** A matrix drawn by `paint_array()` is a matrix,
#' and "2 rows x 3 columns" is the truth about it -- so `paint_array(m)` draws the
#' picture `paint_matrix(m)` draws, down to the subtitle, and the two are byte-identical
#' rather than merely similar.
#'
#' It also keeps `paint_size()` honest, and that is not a nicety: `paint_size()` must
#' reserve the chrome the painter will actually DRAW, and it cannot know which painter
#' the caller will reach for. With the rank test written out at both call sites they
#' would eventually disagree, and a subtitle reserved at one width and drawn at another
#' is a picture whose title runs off the device. One function, one rule, asked by both.
#'
#' @param data The array, before any elision.
#'
#' @return A length-one character string.
#'
#' @keywords internal
#' @noRd
array_subtitle <- function(data) {
  if (length(dim(data)) <= 2L) {
    return(dims_subtitle(data))
  }
  paste0(
    "Dimensions: ", paste(dim(data), collapse = " x "), " | ",
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

#' Resolve the highlight mask from `highlight_area` or the shorthand selectors
#'
#' `highlight_area` and the `highlight_rows` / `highlight_columns` /
#' `highlight_locations` shorthands are two spellings of one thing: a logical mask
#' shaped like the data. When only the shorthands are given, the mask is built by
#' [highlight_data()] -- the SAME builder `highlight_rows()` and friends call -- so
#' every bit of its validation and dispatch is reused rather than reimplemented, and
#' a cell in ANY selected row, column or location is filled (they compose as a
#' union, exactly as `highlight_data(rows =, columns =, locations =)` already does).
#'
#' Supplying `highlight_area` AND a shorthand is a mistake worth reporting rather
#' than papering over, because the two would silently disagree; it is a plain error.
#'
#' @param data The structure being painted. Passed straight to [highlight_data()],
#'   so a shorthand selects on the SAME axes the mask builders already understand
#'   (a column by name, a list element by name, an array row of every slice).
#' @param highlight_area What the caller passed to `highlight_area`: a mask, or
#'   `NULL`.
#' @param rows,columns,locations The shorthand selections, each `NULL` by default.
#'   A vector passes only `locations`, because it has a single axis.
#'
#' @return A logical mask, or `NULL` when nothing was asked for (which
#'   [painter_prep()] then reads as "highlight nothing").
#'
#' @keywords internal
#' @noRd
resolve_highlight_area <- function(data, highlight_area,
                                   rows = NULL, columns = NULL, locations = NULL) {
  asked <- !is.null(rows) || !is.null(columns) || !is.null(locations)
  if (!is.null(highlight_area)) {
    if (asked) {
      stop(
        "Supply either `highlight_area` or the ",
        "`highlight_rows`/`columns`/`locations` arguments, not both."
      )
    }
    return(highlight_area)
  }
  if (!asked) {
    return(NULL)
  }
  # Reuse ALL of `highlight_data()`'s validation and dispatch. It refuses an
  # unpaintable structure, resolves names and negative indices, and composes the
  # three selections as a union -- none of which is reimplemented here.
  highlight_data(data, rows = rows, columns = columns, locations = locations)
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

#' Is this something `paint_list()` will draw?
#'
#' **NOT `is.list(x) && !is.data.frame(x) && is.null(dim(x))`.** That test is the
#' same family of trap as `inherits(1:3, "vector")` being FALSE, which this package
#' has now been bitten by twice, and it is just as silent: R's list is a STORAGE
#' MODE, not a concept, and every S3 class that keeps its innards in one answers
#' `is.list()` with TRUE. All of these are verified:
#'
#' ```
#' is.list(as.POSIXlt(Sys.time()))   # TRUE -- dim NULL, not a data frame
#' is.list(lm(mpg ~ cyl, mtcars))    # TRUE
#' is.list(t.test(1:10))             # TRUE
#' is.list(by(...))                  # TRUE
#' ```
#'
#' Under the naive test, `paint_list(as.POSIXlt(Sys.time()))` would draw a datetime
#' as ELEVEN ragged columns of `sec`, `min`, `hour`, `mday`, `mon`, `year`, ... --
#' a wrong picture of the object, produced confidently, with no error. A fitted
#' model would draw as its internals. That is worse than refusing.
#'
#' A BARE LIST HAS NO CLASS ATTRIBUTE. `attr(list(a = 1), "class")` is NULL, and
#' `attr()` -- unlike `class()`, which invents an implicit class for everything --
#' answers the question actually being asked: has anyone declared this to be
#' something more than a list? `oldClass()` would do as well; `attr()` says it
#' plainly.
#'
#' `is.null(dim(x))` is not redundant. `dim(l) <- c(2, 2)` on a list is legal, and
#' that object is a matrix of list cells, not a list of vectors.
#'
#' **`highlight_data()`'s method set must agree with this function exactly**, or the
#' two disagree about what a list is. It does, and by construction rather than by
#' coincidence: a bare list dispatches to `highlight_data.list()` on its implicit
#' class, and every classed list above dispatches on its own class instead and lands
#' in `.default`, which refuses. `test-dispatch.R` pins the whole battery.
#'
#' @keywords internal
#' @noRd
is_paint_list <- function(x) {
  is.list(x) && is.null(attr(x, "class")) && is.null(dim(x))
}

#' Is this something `paint_array()` will draw?
#'
#' **A MATRIX IS AN ARRAY, AND `paint_array()` MUST DRAW IT.** `is.array(matrix(1:4,
#' 2))` is TRUE (verified), a `table()` of two factors is an array, and rank two is
#' the free degenerate case of the block builder -- one block, no title. Refusing it
#' would be exactly the "a code path, not data" move that `fmt_group` exists to
#' prevent, and it is the same doctrine that let ONE cell builder serve a vector, a
#' matrix, a data frame AND a list. So the gate is rank, and the floor is two.
#'
#' A rank-ONE array is refused, and that is not an oversight either: it carries a
#' `dim`, so `paint_vector()` will not take it, and it has no second axis for a grid
#' to lay out. It is the one shape between the two painters, and it stays there
#' rather than growing a third degenerate case to serve it.
#'
#' `is.list(x)` is the guard that keeps a `dim`-carrying LIST out -- `dim(l) <-
#' c(2, 2)` is legal, and that object is a matrix of list cells, not an array of
#' values. `is_paint_list()` refuses it from the other side, for the same reason.
#'
#' @keywords internal
#' @noRd
is_paint_array <- function(x) {
  !is.data.frame(x) && !is.list(x) && is.array(x) && length(dim(x)) >= 2L
}
