# The colour palette layer.
#
# Colour is the one thing the cell table carries that is not geometry, and it is
# the one thing a reader might want to restyle without touching a single number.
# So it is factored out here as a PURE REMAP: `paint_cells()` always emits the
# classic colour-name tokens ("black", "red", "lemonchiffon", ...), and the remap
# to a palette's hex values happens once, at `paint_resolve()`, the single choke
# point both renderers share. Base and grid therefore cannot disagree about a
# colour any more than they can disagree about a font size.
#
# `classic` is the original look, and it is byte-identical to it by CONSTRUCTION:
# it resolves to a NULL palette, `apply_palette()` short-circuits on NULL, and
# `opts$grey` keeps its default. There is no "classic hex table" that could drift
# from the tokens the cell builder writes -- the identity is the absence of a
# remap, not a remap that happens to be the identity.

#' The built-in palettes
#'
#' Each palette is a named list of ROLES, not of cells: `value` is the ink of a
#' finite number, `na` the ink of an `NA`, `outline` the heavy block border, and
#' so on. The remap in [apply_palette()] is what turns a role into the classic
#' token it replaces, so a palette never has to know that "black" was the token --
#' it only has to say what a value should look like.
#'
#' `header` is the tint of the refined header band; `rule` is carried but unused,
#' a role a later change will need, and defaulting it here keeps that change from
#' having to touch every palette definition.
#'
#' @keywords internal
#' @noRd
.paintr_palette_defs <- list(
  mint = list(
    value = "#1A1A1E", na = "#DC2626", special = "#2563EB",
    insig = "#C2C2C9", label = "#6B7178", label2 = "#9BA1A9",
    grid = "#CBD1D8", outline = "#AEB5BE", rule = "#D3D8DF",
    bg = "#FFFFFF", highlight = "#CFF0E2", header = "#F4F7F8"
  ),
  slate = list(
    value = "#232A31", na = "#C0392B", special = "#2563EB",
    insig = "#B7C0CB", label = "#5B6673", label2 = "#8A94A0",
    grid = "#E1E6EB", outline = "#AAB4BF", rule = "#CDD5DD",
    bg = "#FFFFFF", highlight = "#E4EFFB", header = "#F1F5F9"
  ),
  warm = list(
    value = "#2B2A28", na = "#B23A2E", special = "#3C6DA6",
    insig = "#C4BCAE", label = "#6E665B", label2 = "#9A9184",
    grid = "#EBE6DC", outline = "#C3B9A8", rule = "#DED7CA",
    bg = "#FDFCFA", highlight = "#FBE7B6", header = "#F6F2EA"
  )
)

# The roles every palette must carry. `header` and `rule` are deliberately NOT
# here: they are for a later change, and a caller's custom list should not be
# forced to supply a colour for a role nothing yet draws.
.paintr_palette_roles <- c(
  "value", "na", "special", "insig",
  "label", "label2", "grid", "outline", "bg", "highlight"
)

#' Resolve a `palette` argument to a palette list, or to NULL
#'
#' The one place `getOption("paintr.palette")` is read, and the one place a
#' palette name is validated. Returns either a named list of role -> colour, or
#' `NULL` -- and `NULL` is the SIGNAL for "classic": [apply_palette()] short
#' circuits on it and [paint_opts()] leaves its grey alone, so classic stays
#' byte-identical to the original look with no hex table to drift.
#'
#' @param palette `NULL` (follow the option), one of `"mint"`/`"slate"`/`"warm"`/
#'   `"classic"`, or a named list of colours.
#'
#' @return A named list of colours, or `NULL` for classic.
#'
#' @keywords internal
#' @noRd
resolve_palette <- function(palette = NULL) {
  if (is.null(palette)) {
    palette <- getOption("paintr.palette", "mint")
  }
  if (is.list(palette)) {
    missing <- setdiff(.paintr_palette_roles, names(palette))
    if (length(missing) > 0L) {
      stop(
        "A custom `palette` list is missing the colour role",
        if (length(missing) != 1L) "s" else "", ": ",
        paste(missing, collapse = ", "), "."
      )
    }
    # `header` tints the refined band; `rule` is for a later change. Default them
    # so a custom list need not carry a colour for either -- a band falls back to
    # the grid tint, which is a safe, quiet default.
    if (is.null(palette$rule)) {
      palette$rule <- palette$outline
    }
    if (is.null(palette$header)) {
      palette$header <- palette$grid
    }
    return(palette)
  }
  if (length(palette) != 1L || !is.character(palette) || is.na(palette)) {
    stop(
      "`palette` must be one of \"mint\", \"slate\", \"warm\", \"classic\", ",
      "or a named list of colours."
    )
  }
  if (identical(palette, "classic")) {
    # The signal for "no remap". Classic is the absence of a palette.
    return(NULL)
  }
  if (!palette %in% names(.paintr_palette_defs)) {
    stop(
      "`palette` must be one of \"mint\", \"slate\", \"warm\", \"classic\", ",
      "or a named list of colours; got \"", palette, "\"."
    )
  }
  .paintr_palette_defs[[palette]]
}

#' Remap the classic colour tokens on a cell table to a palette
#'
#' Pure: a function of the cell table and the resolved palette, and the identity
#' when the palette is `NULL` (classic). It reads the ORIGINAL columns to build
#' every mask before it writes any of them, so no cell is mapped twice and the
#' order of the rules does not matter.
#'
#' The three colour columns and their tokens:
#'
#'   * `ink`: `"black"` (a finite value), `"red"` (`NA`), `"blue"` (`Inf`/`NaN`),
#'     `"grey30"`/`"grey40"` (labels), `"grey50"` (type tags, slice titles, the
#'     `<list>` token).
#'   * `border`: `"black"` at `lwd >= 1.5` is the heavy block OUTLINE; `"black"`
#'     at `lwd < 1.5` is the inner cell GRID; `NA` is no border.
#'   * `fill`: `"white"` is the cell background; `"lemonchiffon"` the highlight;
#'     `"headerband"` the refined header tint (styled tables only); `NA` is none.
#'
#' `%in%` for the ink comparisons so an `NA` ink -- there is none today, but the
#' column is nullable -- can never poison a mask; `!is.na` guards `border` and
#' `fill`, which ARE `NA` on most cells.
#'
#' @param cells A cell table.
#' @param pal A resolved palette list, or `NULL` for classic (the identity).
#'
#' @return The cell table, with `ink`/`border`/`fill` remapped when `pal` is
#'   non-NULL.
#'
#' @keywords internal
#' @noRd
apply_palette <- function(cells, pal) {
  if (is.null(pal)) {
    return(cells)
  }

  # -- ink --------------------------------------------------------------------
  # Build every mask off the ORIGINAL column, then write once, so a value that
  # maps to a token another rule matches cannot be mapped a second time.
  ink <- cells$ink
  new_ink <- ink
  new_ink[ink %in% "black"] <- pal$value
  new_ink[ink %in% "red"] <- pal$na
  new_ink[ink %in% "blue"] <- pal$special
  new_ink[ink %in% c("grey30", "grey40")] <- pal$label
  new_ink[ink %in% "grey50"] <- pal$label2
  cells$ink <- new_ink

  # -- border -----------------------------------------------------------------
  # `"black"` is both the heavy outline and the inner grid; `lwd` is what tells
  # them apart -- the outline is drawn at `outline_lwd` (2), the grid at 1.
  border <- cells$border
  lwd <- cells$lwd
  is_black_border <- !is.na(border) & border == "black"
  new_border <- border
  new_border[is_black_border & lwd >= 1.5] <- pal$outline
  new_border[is_black_border & lwd < 1.5] <- pal$grid
  cells$border <- new_border

  # -- fill -------------------------------------------------------------------
  fill <- cells$fill
  new_fill <- fill
  new_fill[!is.na(fill) & fill == "white"] <- pal$bg
  new_fill[!is.na(fill) & fill == "lemonchiffon"] <- pal$highlight
  # The refined header band. Only a styled palette's cell table carries this token
  # (`paint_cells(styled = TRUE)` emits it, and classic reaches here with a NULL
  # `pal` and returns early), so there is nothing to map on a classic picture.
  new_fill[!is.na(fill) & fill == "headerband"] <- pal$header
  cells$fill <- new_fill

  cells
}
