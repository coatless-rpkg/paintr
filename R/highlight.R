#' Highlight data
#' 
#' Generate a logical structure codifies active areas.
#' 
#' @param x          A matrix.
#' @param rows       An interger vector with valid row index locations.
#' @param columns    An integer vector containing valid column indexlocations.
#' @param locations  An m by 2 matrix with points listed in x, y format for a 2D object
#'                   or a vector of integer indices in a 1D format.
#' @param ...        Additional values (not used)
#' @return 
#' A logical matrix or vector with the required rows and/or columns or points set to
#' `TRUE`. All other values are given as `FALSE`.
#'
#' @rdname highlight-data
#' @export
#' @examples
#' ## 2D Highlighting for Matrices ----
#' # Example data
#' x <- matrix(1:12, nrow = 4)
#' 
#' # Highlight points using an x, y pairing
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
highlight_data.vector <- function(x, rows = NULL, columns = NULL, locations = NULL, ...) {
  # Create a logical matrix with the same dimensions as 'x'
  logical_vector <- rep(FALSE, length(x))
  
  # Nothing to mark.
  if (is.null(rows) && is.null(columns) && is.null(locations)) {
    return(logical_vector)
  }
  
  if(!is.null(locations)){
    stopifnot("points must be a vector when highlighting a vector structure" = is.vector(locations))
    logical_vector[locations] <- TRUE
  }
  
  # Enable rows
  if(!is.null(rows)){
    logical_vector[rows] <- TRUE
  }
  
  # Enable columns
  if(!is.null(columns)){
    logical_vector[columns] <- TRUE
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
  
  if(!is.null(locations)){
    stopifnot("points must contain only two columns." = ncol(locations) == 2)
    logical_matrix[locations] <- TRUE
  }
  
  # Enable rows
  if(!is.null(rows)){
    logical_matrix[rows, ] <- TRUE
  }
  
  # Enable columns
  if(!is.null(columns)){
    logical_matrix[, columns] <- TRUE
  }
  
  logical_matrix
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