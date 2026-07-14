#' Highlight data
#'
#' Generate a logical structure codifies active areas.
#'
#' @param x          A vector, factor, matrix, or data frame.
#' @param rows       A vector of valid row locations, given either as integer
#'                   indices, as row names, or as a logical mask.
#' @param columns    A vector of valid column locations, given either as integer
#'                   indices, as column names, or as a logical mask.
#' @param locations  An m by 2 matrix with points listed in row, column format for
#'                   a 2D object or a vector of integer indices in a 1D format.
#' @param ...        Additional values (not used)
#' @return
#' A logical matrix or vector with the required rows and/or columns or points set to
#' `TRUE`. All other values are given as `FALSE`. The result always has the same
#' shape as `x`: a logical vector of `length(x)` for 1D structures, and a logical
#' matrix of `dim(x)` for matrices and data frames.
#'
#' @section Supported structures:
#' Methods exist for `numeric`, `integer`, `character`, `logical`, `complex`,
#' `factor`, `Date`, `POSIXct`, `matrix`, `array`, `table`, and `data.frame`.
#' Anything else is an error.
#'
#' The governing invariant: **if a painter can draw a structure, `highlight_data()`
#' must be able to mask it** -- and a *wrong* mask is worse than an error, so a
#' structure no painter accepts is refused outright rather than reshaped into a mask
#' nothing could consume. A 2D `table` is drawn by `paint_matrix()`, so it is masked
#' here; an array of any rank but two is drawn by nothing, so it stops.
#'
#' There is deliberately no reliance on a `vector` method being *dispatched*:
#' `inherits(letters, "vector")` is `FALSE`, so `highlight_data.vector()` is never
#' selected by `UseMethod()` for an atomic vector. The atomic methods are therefore
#' fanned out explicitly, and each one forwards to `highlight_data.vector()` by a
#' direct call. `array` and `table` are the same trap one type over: an atomic array
#' dispatches on `c("array", "integer", "numeric")`, so without an `array` method it
#' would land on `highlight_data.integer()` and be silently flattened, and a `table`
#' dispatches on `"table"` alone, so without a `table` method it would land on
#' `highlight_data.default()` and be refused despite being paintable.
#'
#' @rdname highlight-data
#' @export
#' @examples
#' ## 2D Highlighting for Matrices ----
#' # Example data
#' x <- matrix(1:12, nrow = 4)
#'
#' # Highlight points using a row, column pairing
#' locations <- rbind(
#'   c(1, 3),
#'   c(2, 2),
#'   c(4, 1)
#' )
#' highlight_locations(x, locations)
#'
#' # Highlight entries only in the 1st and 3rd rows.
#' highlight_rows(x, rows = c(1, 3))
#'
#' # Highlight entries only in the first two rows:
#' highlight_rows(x, rows = 1:2)
#'
#' # Highlight entries in the last column
#' highlight_columns(x, columns = ncol(x))
#'
#' # Highlight entries in the first column
#' highlight_columns(x, columns = 1)
#'
#' # Highlight entries in the first column or first row.
#' highlight_data(x, rows = 1, columns = 1)
#'
#' ## 1D Highlighting for Vectors ----
#' vec <- c(3, NA, -1, 2, NaN, Inf, 42)
#' highlight_data(vec, locations = c(2, 4, 6))
#'
#' # Character and logical vectors work the same way.
#' highlight_locations(letters[1:5], c(2, 4))
#' highlight_data(c(TRUE, FALSE, TRUE))
#'
#' ## 2D Highlighting for Data Frames ----
#' # Columns may be named instead of numbered.
#' highlight_columns(iris[1:5, ], "Sepal.Width")
#' highlight_rows(iris[1:5, ], rows = c(1, 3))
#' highlight_locations(iris[1:5, ], rbind(c(1, 1), c(2, 5)))
highlight_data <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  UseMethod("highlight_data")
}

# Plumbing to ensure vectors will pass through nicely to the vector generic
#' @rdname highlight-data
#' @export
highlight_data.numeric <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data.vector(x = x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.integer <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data.vector(x = x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.character <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data.vector(x = x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.logical <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data.vector(x = x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.complex <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data.vector(x = x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.factor <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data.vector(x = x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.Date <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data.vector(x = x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.POSIXct <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data.vector(x = x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.vector <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  # Create a logical vector with the same length as 'x'
  logical_vector <- rep(FALSE, length(x))

  # Nothing to mark.
  if (is.null(rows) && is.null(columns) && is.null(locations)) {
    return(logical_vector)
  }

  n <- length(x)
  element_names <- names(x)

  if (!is.null(locations)) {
    stopifnot("points must be a vector when highlighting a vector structure" = is.vector(locations))
    logical_vector[highlight_index(locations, n, element_names, "element")] <- TRUE
  }

  # Enable rows
  if (!is.null(rows)) {
    logical_vector[highlight_index(rows, n, element_names, "element")] <- TRUE
  }

  # Enable columns
  if (!is.null(columns)) {
    logical_vector[highlight_index(columns, n, element_names, "element")] <- TRUE
  }

  logical_vector
}

#' @rdname highlight-data
#' @export
highlight_data.matrix <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {

  # Create a logical matrix with the same dimensions as 'x'
  logical_matrix <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))

  # Nothing to mark.
  if (is.null(rows) && is.null(columns) && is.null(locations)) {
    return(logical_matrix)
  }

  highlight_fill_2d(logical_matrix, x, rows, columns, locations)
}

#' @rdname highlight-data
#' @export
highlight_data.array <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data_dim(x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.table <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  highlight_data_dim(x, rows = rows, columns = columns, locations = locations, ...)
}

#' @rdname highlight-data
#' @export
highlight_data.data.frame <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {

  # Create a logical matrix with the same dimensions as 'x'
  logical_matrix <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))

  # Nothing to mark.
  if (is.null(rows) && is.null(columns) && is.null(locations)) {
    return(logical_matrix)
  }

  highlight_fill_2d(logical_matrix, x, rows, columns, locations)
}

#' @rdname highlight-data
#' @export
highlight_data.default <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  stop("We currently do not support the data structure of: ", class(x))
}

#' @rdname highlight-data
#' @export
highlight_rows <- function(x, rows = NULL) {
  highlight_data(x, rows = rows)
}

#' @rdname highlight-data
#' @export
highlight_columns <- function(x, columns = NULL) {
  highlight_data(x, columns = columns)
}

#' @rdname highlight-data
#' @export
highlight_locations <- function(x, locations = NULL) {
  highlight_data(x, locations = locations)
}

# Internal helpers -------------------------------------------------------------

# Mask a `dim`-carrying structure, or refuse it honestly.
#
# THE GOVERNING INVARIANT: if a painter can DRAW a structure, highlight_data() must
# be able to MASK it. Its corollary is the reason this function stops rather than
# improvises: a WRONG mask is worse than an error.
#
# `array` and `table` both land here, and both are paintable exactly as far as their
# rank allows -- `paint_matrix()` draws a 2D one and refuses every other rank
# (verified: a 1D, 3D or 4D one is turned away by the painter). So a 2D one is
# DELEGATED to the matrix method -- the same body, not a copy of it, because
# duplicated logic between siblings is what produced this package's original bug
# list -- and every other rank stops. There is no n-D painter, so an n-D mask is not
# a thing anything could consume; building one would only be a wrong answer with
# extra steps.
#
# Two dispatch traps are what make these two methods necessary at all, and both are
# silent:
#
#   * `.class2(array(1:24, c(2, 3, 4)))` is c("array", "integer", "numeric"). With no
#     `array` method, `UseMethod()` falls through to `.integer` -> `.vector`, which
#     drops `dim` and returns a FLAT mask: `rows = 1` marked ONE cell of 24 where it
#     should have marked twelve.
#   * `.class2(table(a, b))` is "table" alone -- a classed object dispatches on its
#     own class vector only -- so `.matrix` never fired and `.default` refused an
#     object `paint_matrix()` draws happily.
#
# The rank test below is deliberately not "assume 2D can't get here". A 2D array
# never does reach `highlight_data.array()` -- `.class2()` puts `matrix` ahead of
# `array`, so `.matrix` is dispatched first -- but a method that is correct only
# because of the order of a class vector is a method waiting to be wrong, and a 2D
# `table` reaches this same body for real.
#' @keywords internal
#' @noRd
highlight_data_dim <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  n_dim <- length(dim(x))

  if (n_dim == 2L) {
    return(highlight_data.matrix(
      x = x, rows = rows, columns = columns, locations = locations, ...
    ))
  }

  stop(
    "We can only highlight a two-dimensional ", class(x)[1L], ", but this one has ",
    n_dim, if (n_dim == 1L) " dimension." else " dimensions."
  )
}

# Mark rows, columns, and points on a 2D logical mask.
#
# Shared by the `matrix` and `data.frame` methods, which differ only in where their
# names live. One body means the two structures cannot drift.
#
# mask:      logical matrix of the correct dimensions, all FALSE
# x:         the original matrix or data frame, consulted only for its names
# rows:      row selection: indices, names, or a logical mask
# columns:   column selection: indices, names, or a logical mask
# locations: an m by 2 matrix of row/column pairs, or a vector of linear
#            (column-major) cell indices
#
# Returns the logical matrix with the selected cells set to TRUE.
#' @keywords internal
#' @noRd
highlight_fill_2d <- function(mask, x, rows, columns, locations) {
  n_row <- nrow(mask)
  n_col <- ncol(mask)

  # A data frame's row and column names do not live where a matrix's do.
  if (is.data.frame(x)) {
    row_names <- rownames(x)
    col_names <- names(x)
  } else {
    dn <- dimnames(x)
    row_names <- if (is.null(dn)) NULL else dn[[1L]]
    col_names <- if (is.null(dn)) NULL else dn[[2L]]
  }

  if (!is.null(locations)) {
    mask[highlight_points(locations, n_row, n_col, row_names, col_names)] <- TRUE
  }

  # Enable rows
  if (!is.null(rows)) {
    mask[highlight_index(rows, n_row, row_names, "row"), ] <- TRUE
  }

  # Enable columns
  if (!is.null(columns)) {
    mask[, highlight_index(columns, n_col, col_names, "column")] <- TRUE
  }

  mask
}

# Resolve a row/column/element selection to integer positions.
#
# Accepts integer indices (positive, or base-R style all-negative), names, or a
# logical mask. NA in a logical mask is treated as FALSE, matching the way the
# painters treat NA in a highlight area.
#
# idx:  the user's selection, or NULL for "nothing"
# n:    the number of available positions
# nms:  the available names, or NULL if the data has none
# what: noun used in error messages: "row", "column", "element", or "cell"
#
# Returns an integer vector of positions within seq_len(n).
#' @keywords internal
#' @noRd
highlight_index <- function(idx, n, nms = NULL, what = "index") {
  if (is.null(idx)) {
    return(integer(0))
  }

  if (is.factor(idx)) {
    idx <- as.character(idx)
  }

  if (is.logical(idx)) {
    if (length(idx) == 0L) {
      return(integer(0))
    }
    if (length(idx) > n) {
      stop(
        "The logical ", what, " selection is longer than the data: ",
        length(idx), " values were given, but there are only ", n, "."
      )
    }
    # which() drops NA, so an NA in the mask reads as FALSE.
    return(which(rep_len(idx, n)))
  }

  if (is.character(idx)) {
    if (is.null(nms)) {
      stop("Cannot select a ", what, " by name because the data has no ", what, " names.")
    }
    pos <- match(idx, nms)
    if (anyNA(pos)) {
      stop("Unknown ", what, " name: ", paste0(idx[is.na(pos)], collapse = ", "))
    }
    return(as.integer(pos))
  }

  if (!is.numeric(idx)) {
    stop(
      "A ", what, " selection must be numeric, character, or logical, not: ",
      class(idx)[1L]
    )
  }
  if (anyNA(idx)) {
    stop("A ", what, " selection must not contain NA.")
  }

  idx <- idx[idx != 0]
  if (length(idx) == 0L) {
    return(integer(0))
  }
  if (all(idx < 0)) {
    return(as.integer(seq_len(n)[idx]))
  }
  if (any(idx < 0)) {
    stop("A ", what, " selection cannot mix positive and negative values.")
  }
  if (any(idx > n)) {
    stop(
      "The ", what, " selection is out of range: the data has ", n, " ", what,
      "s, but ", max(idx), " was requested."
    )
  }

  as.integer(idx)
}

# Resolve points to an index usable on a 2D logical mask.
#
# Two forms are accepted, both of which `[<-` understands:
#
#  * an m by 2 matrix (or data frame) of row/column pairs, whose entries may be
#    names as well as numbers. This becomes an m by 2 integer index matrix.
#  * a bare vector of linear, column-major cell indices. The original code
#    supported this by accident -- `stopifnot(ncol(locations) == 2)` passes
#    vacuously when `ncol()` is NULL -- and it is kept deliberately.
#
# Returns an integer matrix with two columns, or an integer vector.
#' @keywords internal
#' @noRd
highlight_points <- function(locations, n_row, n_col, row_names = NULL, col_names = NULL) {
  if (is.data.frame(locations)) {
    locations <- as.matrix(locations)
  }

  # A bare vector means linear (column-major) indices into the cells.
  if (!is.matrix(locations)) {
    return(highlight_position(locations, n_row * n_col, NULL, "cell"))
  }

  stopifnot("points must contain only two columns." = ncol(locations) == 2)

  cbind(
    highlight_position(locations[, 1L], n_row, row_names, "row"),
    highlight_position(locations[, 2L], n_col, col_names, "column")
  )
}

# Resolve one coordinate of a set of points.
#
# Unlike highlight_index(), a coordinate is a plain position: no logical masks and
# no negative indices, because a point either exists or it does not.
#
# Returns an integer vector of positions within seq_len(n).
#' @keywords internal
#' @noRd
highlight_position <- function(pos, n, nms = NULL, what = "index") {
  if (is.factor(pos)) {
    pos <- as.character(pos)
  }

  if (is.character(pos)) {
    if (is.null(nms)) {
      stop("Cannot select a ", what, " by name because the data has no ", what, " names.")
    }
    out <- match(pos, nms)
    if (anyNA(out)) {
      stop("Unknown ", what, " name: ", paste0(pos[is.na(out)], collapse = ", "))
    }
    return(as.integer(out))
  }

  if (!is.numeric(pos)) {
    stop("The ", what, " coordinate of a point must be numeric or a name, not: ", class(pos)[1L])
  }
  if (anyNA(pos)) {
    stop("The ", what, " coordinate of a point must not contain NA.")
  }
  if (length(pos) > 0L && any(pos < 1 | pos > n)) {
    stop(
      "A point falls outside the data: its ", what, " coordinate must be between 1 and ",
      n, ", but ", max(pos), " was requested."
    )
  }

  as.integer(pos)
}
