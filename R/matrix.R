## Base matrix ----

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
#' @rdname draw-matrix
#' @export
#' @examples
#' # Base graphics
#' 
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
  
  # Remove exterior margin
  par(mar = c(0.1, 0.1, 2, 0.1))
  
  # Initialize plot at the origin
  plot.new()
  plot.window(
    xlim = c(0, ncol + 1), ylim = c(-.1, nrow + .1)
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

## ggplot2 matrix ----

#' @rdname draw-matrix
#' @export
#' @examples
#' # ggplot2 graphics ----
#' 
#' # Visualize a 3x3
#' mat_3x3 = matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
#' visualize_matrix_data(mat_3x3)
#' 
#' # View the matrix without indices present
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
visualize_matrix_data_ggplot <- function(data, show_indices = TRUE, 
                                         highlight_cells = matrix(FALSE, nrow(data), ncol(data)), 
                                         highlight_color = "lemonchiffon") {
  
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Please make sure `ggplot2` is installed to use this function.")
  }
  
  if (!is.matrix(data)) {
    message("Please double-check the data supplied is of a matrix type.")
  }
  
  nrow <- nrow(data)
  ncol <- ncol(data)
  
  # Create a data frame for ggplot
  df <- expand.grid(row = nrow:1, col = 1:ncol)
  df$highlight <- as.vector(highlight_cells)
  df$value <- as.vector(data)
  
  # TODO: fix no visible binding for global variable
  
  g <- ggplot2::ggplot(df) +
    ggplot2::aes(x = col, y = row, label = value, fill = highlight) + 
    ggplot2::geom_tile(color = "black", width = 1, height = 1) +
    ggplot2::scale_fill_manual(values = c("white", highlight_color)) +
    ggplot2::geom_rect(
      xmin = 0.5, xmax = ncol(data) + 0.5, 
      ymin = 0.5, ymax = nrow(data) + 0.5, 
      fill = NA, color = "black", size = 1
    ) + 
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(is.na(df$value), "NA", df$value)
      ), 
      size = 5, color = "black") +
    # Include the indices
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(show_indices, paste0("[", row, ", ", col, "]"), "")
      ), 
      color = "grey", hjust = 0.5, vjust = 0.5, nudge_y = -0.15, size = 3) +
    # Provide details on the object plotted
    ggplot2::labs(
      title = paste("Data Object: ", deparse(substitute(data))),
      subtitle = paste0(
           "Dimensions: ", paste(nrow, "rows", ncol, "columns"), " | ",
           "Data Type: ", paste0(class(data), collapse=", "))
    ) +    
    # Set the limits and breaks of the axes
    ggplot2::scale_x_continuous(limits = c(0.5, ncol + 0.5), breaks = seq_len(ncol), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.5, nrow + 0.5), breaks = seq_len(nrow), expand = c(0, 0)) +
    # Remove everything
    ggplot2::theme_void() + 
    # Disable showing the legend's highlight call.
    ggplot2::theme(legend.position = "none")
  
  g
}
