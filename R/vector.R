## Base matrix ----

#' Visualize Data Inside of a Vector
#' 
#' Generate a graph showing the contents of a Vector
#' 
#' @param data                A object that has the class of `vector`. 
#' @param layout              Orientation of the vector. Default: `"vertical"`.
#' @param show_indices        Display data indices either: `"inside"`, `"outside"`, or `"none"`
#'                            the vector cell, e.g. `[i]`. Default: `"none"`
#' @param highlight_area      Vector of logical values that provide a mask for what
#'                            cells should be filled. Default: None.
#' @param highlight_color     Color to use to fill the background of a cell. 
#' @param graph_title         Title to appear in the upper left hand corner of the graph.
#' @param graph_subtitle      Subtitle to appear immediately under the graph title
#'                            in the upper left hand side of the graph.
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
    show_indices = c("none", "inside", "outside"), 
    highlight_area = rep(FALSE, length(data)), 
    highlight_color = "lemonchiffon",
    graph_title = paste0("Data Object: ", deparse(substitute(data))),
    graph_subtitle = paste0(
      "Length: ", paste(length(data), "elements"), " | ",
      "Data Type: ", paste0(class(data), collapse=", ")
    )
) {
  
  if (!is.vector(data)) {
    stop("Please double check the data supplied is of a `vector` type.")
  }
  
  layout <- match.arg(layout)
  show_indices <- match.arg(show_indices)
  
  n_elem <- length(data)
  
  is_row_layout <- layout == "horizontal"
  is_column_layout <- !is_row_layout
  
  if(is_row_layout) {
    n_row <- 1
    n_col <- n_elem
  } else {
    n_row <- n_elem
    n_col <- 1
  }
  
  # Remove exterior margin
  par(mar = c(0.1, 0.1, 2, 0.1))
  
  # Initialize plot at the origin
  plot.new()
  plot.window(
    xlim = c(0, n_col + 1), ylim = c(-.1, n_row + .1)
  )
  
  # TODO: Re-write to remove for loops.
  # This is a messay backport ... 
  for (i in seq_len(n_row)) {
    for (j in seq_len(n_col)) {
      
      position <- max(i,j)
      
      # Draw rectangle around each cell
      rect(
        # xleft, ybottom
        j - 0.5, n_row - i + 1, 
        # xright, ytop
        j + 0.5, n_row - i, 
        col = ifelse(highlight_area[position], highlight_color, "white"), 
        border = "black"
      )
      
      # Differentiate between missing and present values
      point_contents <- data[position]
      if (is.finite(point_contents) ) {
        text(j, n_row - i + 0.5, point_contents, cex = 1.25, col = "black")  
      } else if( is.infinite(point_contents) || is.nan(point_contents) ) {
        text(j, n_row - i + 0.5, point_contents, cex = 1.25, col = "blue")  
      } else {
        # NA
        text(j, n_row - i + 0.5, "NA", cex = 1.25, col = "red")  
      }
      
      # Label each entry inside of the matrix
      if (show_indices == "inside") {
        text(j, n_row - i + 0.3, paste("[", position, "]", sep = ""), cex = .9, col = "grey")
      }
    }
  }
  
  # Add row indices to the left
  if (show_indices == "outside" && is_column_layout) {
    for (i in seq_len(n_row)) {
      text(0.25, n_row - i + 0.5, paste("[", i, "]", sep = ""), cex = .95, col = "grey")
    }
  }
  
  # Add column indices to the top
  if (show_indices == "outside" && is_row_layout ) {
    for (j in seq_len(n_col)) {
      text(j, n_row + 0.1, paste("[", j, "]", sep = ""), cex = .95, col = "grey")
    }
  }
  
  # Draw a rectangle around all cells in the matrix
  rect(0.5, n_row, n_col + 0.5 , 0, border = "black", lwd = 2)
  
  # Add title with data object name, dimensions, and data type
  
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