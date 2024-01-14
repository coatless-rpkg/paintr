## Base matrix ----

#' Visualize Data Inside of a Vector
#' 
#' Generate a graph showing the contents of a Vector
#' 
#' @param data                A object that has the class of `vector`. 
#' @param show_cell_indices   Display cell indices inside the matrix cell, e.g. `[i]`. Default: `FALSE`
#' @param layout              Orientation of the vector. Default: `"vertical"`.
#' @param highlight_area      Vector of logical values that provide a mask for what
#'                            cells should be filled. Default: None.
#' @param highlight_color     Color to use to fill the background of a cell. 
#' 
#' @importFrom graphics rect text mtext par plot.new plot.window
#' @rdname draw-vector
#' @export
#' @examples
#' # Base graphics
#' 
#' # Visualize a 3x3
#' vec_9 <- round(rnorm(5, 0, 4), 2)
#' draw_vector(vec_9)
draw_vector <- function(
    data,
    layout = c("vertical", "horizontal"),
    show_cell_indices = FALSE, 
    highlight_area = rep(FALSE, length(data)), 
    highlight_color = "lemonchiffon"
) {
  
  if (!is.vector(data)) {
    stop("Please double check the data supplied is of a `vector` type.")
  }
  
  layout <- match.arg(layout)
  
  nrow <- length(data)
  ncol <- 1
  
  is_row_layout <- layout == "horizontal"
  
  if (is_row_layout) {
    swap <- ncol
    ncol <- nrow
    nrow <- swap
  }
  
  # Remove exterior margin
  par(mar = c(0.1, 0.1, 2, 0.1))
  
  # Initialize plot at the origin
  plot.new()
  plot.window(
    xlim = c(0, ncol + 1), ylim = c(-.1, nrow + .2)
  )
  
  # TODO: Re-write to remove for loops.
  for (i in seq_len(nrow)) {
    for (j in seq_len(ncol)) {
      
      # Draw rectangle around each cell
      rect(
        # xleft, ybottom
        j - 0.5, nrow - i + 1, 
        # xright, ytop
        j + 0.5, nrow - i, 
        col = ifelse(highlight_area[i], highlight_color, "white"), 
        border = "black"
      )
      
      # Differentiate between missing and present values
      point_contents <- data[i]
      if (is.finite(point_contents) ) {
        text(j, nrow - i + 0.5, data[i], cex = 1.25, col = "black")  
      } else if( is.infinite(point_contents) || is.nan(point_contents) ) {
        text(j, nrow - i + 0.5, data[i], cex = 1.25, col = "blue")  
      } else {
        # NA
        text(j, nrow - i + 0.5, "NA", cex = 1.25, col = "red")  
      }
      
      # Label each entry inside of the matrix
      if (show_cell_indices) {
        text(j, nrow - i + 0.3, paste("[", i, "]", sep = ""), cex = .9, col = "grey")
      }
    }
  }
  
  # Add row indices to the left
  if (show_cell_indices && is_row_layout) {
    for (i in seq_len(nrow)) {
      text(0.25, nrow - i + 0.5, paste("[", i, "]", sep = ""), cex = .95, col = "grey")
    }
  }
  
  # Add column indices to the top
  if (show_cell_indices && !is_row_layout ) {
    for (j in seq_len(ncol)) {
      text(j, nrow + 0.15, paste("[", j, "]", sep = ""), cex = .95, col = "grey")
    }
  }
  
  # Draw a rectangle around all cells in the matrix
  rect(0.5, nrow, ncol + 0.5 , 0, border = "black", lwd = 2)
  
  # Add title with data object name, dimensions, and data type
  graph_title = paste0("Data Object: ", deparse(substitute(data)))
  graph_subtitle = paste0(
    "Dimensions: ", paste(nrow, "rows", ncol, "columns"), " | ",
    "Data Type: ", paste0(class(data), collapse=", ")
  )
  
  # Left-align title inside of the margins of text
  mtext(
    text = graph_title, 
    side = 3, line = 1, at = -0.07, adj = 0, cex = 1
  )
  mtext(
    text = graph_subtitle,
    side = 3, line = 0, at = -0.07, adj = 0, cex = 0.7
  )
  
}