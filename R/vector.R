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
#' # Visualize a vector with 5 elements
#' vec_5 <- round(rnorm(5, 0, 4), 2)
#' draw_vector(vec_5)
#' 
#' # Visualize a 6 element vector with indices underneath data
#' vec_6 <- c(-3, 5, NA, Inf, 2, 1)
#' draw_vector(vec_6, show_indices = "inside")
#' 
#' # Highlight the 2nd, 4th, and 6th cell with indices shown outside
#' draw_vector(vec_6, show_indices = "outside", highlight_area = c(2, 4, 6))
draw_vector <- function(
    data,
    layout = c("vertical", "horizontal"),
    show_indices = c("none", "inside", "outside"),
    highlight_area = rep(FALSE, length(data)),
    highlight_color = "lemonchiffon",
    graph_title = paste0("Data Object: ", deparse(substitute(data))),
    graph_subtitle = paste0(
      "Length: ", length(data), " elements | ",
      "Data Type: ", paste(class(data), collapse=", ")
    )
) {
  
  if (!is.vector(data)) {
    stop("Please double-check the data supplied is of a `vector` type.")
  }
  
  layout <- match.arg(layout)
  show_indices <- match.arg(show_indices)
  
  n_elem <- length(data)
  
  is_row_layout <- layout == "horizontal"
  is_column_layout <- !is_row_layout
  
  n_row <- if (is_row_layout) 1 else n_elem
  n_col <- if (is_row_layout) n_elem else 1
  
  # Remove exterior margin
  par(mar = c(0.1, 0.1, 2, 0.1))
  
  # Initialize plot at the origin
  plot.new()
  plot.window(
    xlim = c(0, n_col + 1), ylim = c(-.1, n_row + .1)
  )
  
  position_matrix <- outer(seq_len(n_row), seq_len(n_col), pmax)
  
  # Draw rectangles and labels
  fill_color_values <- ifelse(highlight_area[position_matrix], highlight_color, "white")
  text_matrix <- matrix(
    ifelse(is.finite(data[position_matrix]), as.character(data[position_matrix]), "NA"),
    n_row, n_col
  )
  text_matrix[is.infinite(data[position_matrix]) | is.nan(data[position_matrix])] <- "NA"
  
  # Draw a rectangle around all cells in the matrix
  rect(0.5, n_row, n_col + 0.5, 0, border = "black", lwd = 2)
  
  if (is_column_layout) {
    rect(
      xleft = rep(seq_len(n_col) + 0.5, times = n_row),
      ybottom = rep(n_row:1 - 1, times = n_col),
      xright = rep(seq_len(n_col) - 0.5, times = n_row),
      ytop = rep(n_row:1, times = n_col),
      col = fill_color_values,
      border = "black"
    )
  } else {
    rect(
      rep(seq_len(n_col) - 0.5, each = n_row),
      rep(n_row:1, each = n_row),
      rep(seq_len(n_col) + 0.5, each = n_row),
      rep(n_row:1 - 1, each = n_row),
      col = fill_color_values,
      border = "black"
    )
  }
  
  text(
    x = rep(seq_len(n_col), each = n_row),
    y = rep(n_row:1, times = n_col) - 0.5,
    labels = text_matrix,
    cex = 1.25,
    col = ifelse(is.finite(data[position_matrix]), "black", "blue")
  )
  
  # Label each entry inside of the matrix
  if (show_indices == "inside") {
    text(
      x = rep(seq_len(n_col), each = n_row),
      y = rep(n_row:1, times = n_col) - 0.7,
      labels = paste("[", position_matrix, "]", sep = ""),
      cex = .9,
      col = "grey"
    )
  }
  
  # Add row indices to the left
  if (show_indices == "outside" && is_column_layout) {
    text(
      x = 0.25,
      y = n_row:1 - 0.5,
      labels = paste("[", seq_len(n_row), "]", sep = ""),
      cex = .95,
      col = "grey"
    )
  }
  
  # Add column indices to the top
  if (show_indices == "outside" && is_row_layout) {
    text(
      x = rep(seq_len(n_col), each = n_row),
      y = n_row + 0.05,
      labels = paste("[", seq_len(n_col), "]", sep = ""),
      cex = .95,
      col = "grey"
    )
  }
  
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