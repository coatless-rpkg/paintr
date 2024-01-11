#' Highlight a matrix
#' 
#' Generate a matrix with active areas.
#' 
#' @param x       A matrix.
#' @param rows    An interger vector with valid row index locations.
#' @param columns An integer vector containing valid column indexlocations.
#' @param points  An m by 2 matrix with points listed in x, y format.
#'
#' @return 
#' A logical matrix with the required rows and/or columns or points set to
#' `TRUE`. All other values are given as `FALSE`.
#'
#' @export
#' @examples
#' 
#' x = matrix(1:12, nrow = 4)
#' 
#' # Highlight entries only in the 1st and 3rd rows.
#' highlight_matrix(x, rows = c(1, 3))
#' 
#' # Highlight entries only in the first two rows:
#' highlight_matrix(x, rows = 1:2)
#' 
#' # Highlight entries in the last row
#' highlight_matrix(x, rows = nrow(x))
#' 
#' # Highlight entries in the first column
#' highlight_matrix(x, columns = 1)
#' 
#' # Highlight entries in the first column or first row.
#' highlight_matrix(x, rows = 1, columns = 1)
highlight_matrix <- function(x, rows = NULL, columns = NULL, points = NULL) {
  
  # Create a logical matrix with the same dimensions as 'x'
  logical_matrix <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
  
  # Nothing to mark.
  if (is.null(rows) && is.null(columns) && is.null(points)) {
    return(logical_matrix)
  }
  
  if(!is.null(points)){
    stopifnot("points must contain only two columns." = ncol(points) == 2)
    logical_matrix[points] <- TRUE
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
