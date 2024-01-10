#' Visualize Data Inside of a Matrix
#' 
#' Generate a graph showing the contents of a matrix.
#' 
#' @param data            A object that has the class of `matrix`. 
#' @param show_indices    Identify the text index. Default: `TRUE`
#' @param highlight_cells Matrix of logical values that provide a mask for what
#'                        cells should be filled. Default: None.
#' @param highlight_color Color to use to fill the background of a cell. 
#' 
#' @importFrom graphics rect text
#' @export
#' @examples
#' # Visualize a 3x3
#' mat_3x3 = matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
#' visualize_matrix_data(mat_3x3)
#' 
#' # Disable the indices
#' visualize_matrix_data(mat_3x3, show_indices = FALSE)
#' 
#' # Highlight a row
#' mat_2x2 = matrix(c(1, 2, 3, 4), nrow = 2)
#' mat_2x2_mask = matrix(c(TRUE, TRUE, FALSE, FALSE), nrow = 2)
#' visualize_matrix_data(mat_2x2, highlight_cells = mat_2x2_mask)
#' 
#' # Highlight values above 5
#' mat_3x5 = matrix(rnorm(15, 5, 2), ncol = 5)
#' visualize_matrix_data(mat_3x5, highlight_cells = mat_3x5 > 2) 
visualize_matrix_data <- function(data, show_indices = TRUE, 
                                  highlight_cells = matrix(FALSE, nrow(data), ncol(data)), 
                                  highlight_color = "lemonchiffon") {
  
  if (!is.matrix(data)) {
    message("Please double check the data supplied is of a matrix type.")
  }
  
  nrow <- nrow(data)
  ncol <- ncol(data)
  
  # Initialize plot at the origin
  plot(0, 0,
       xlim = c(0, ncol + 1), ylim = c(-1, nrow + .5), type = "n", xlab = "", ylab = "")
  
  # TODO: Re-write to remove for loops.
  for (i in seq_len(nrow)) {
    for (j in seq_len(ncol)) {
      
      # Draw rectangle around each cell
      rect(
        # xleft, ybottom
        j - 0.5, nrow - i + 1, 
        # xright, ytop
        j + 0.5, nrow - i, 
        col = ifelse(highlight_cells[i, j], highlight_color, "white"), 
        border = "black"
      )
      
      # Differentiate between missing and present values
      point_contents <- data[i, j]
      if (!is.na(point_contents)) {
        text(j, nrow - i + 0.5, data[i, j], cex = 1.25, col = "black")  
      } else {
        text(j, nrow - i + 0.5, "NA", cex = 1.25, col = "red")  
      }
      
      # Label each entry inside of the matrix
      if (show_indices) {
        text(j, nrow - i + 0.3, paste("[", i, ", ", j, "]", sep = ""), cex = 1, col = "grey")
      }
    }
  }
  # Draw a rectangle around all cells in the matrix
  rect(0.5, nrow, ncol + 0.5 , 0, border = "black", lwd = 2)
  
  # Add text annotation with data object name, dimensions, and data type
  text(0.5 + 0.5 * ncol, -0.25, 
       paste(
         "Data Object:", deparse(substitute(data)),
         "\nDimensions:", paste(nrow, "rows", ncol, "columns"),
         "\nData Type:", paste0(class(data), collapse=", ")
       ),
       cex = 1.2)
  
}