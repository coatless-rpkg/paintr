## Base matrix ----

#' Visualize Data Inside of a Matrix
#' 
#' Generate a graph showing the contents of a matrix.
#' 
#' @param data            A object that has the class of `matrix`. 
#' @param show_indices    Display indices based on location. Options are:
#'                        `"none"`: no indices, `"cell"`:  matrix cell indices `[i, j]`,
#'                        `"row"`: row indices `[i, ]` to the left of the matrix,
#'                        `"column"`: column indices `[,j]` above matrix, and
#'                        `"all"`: row, column, and inside options. Default: `"none"`.
#' @param highlight_area  Matrix of logical values that provide a mask for what
#'                        cells should be filled. Default: None.
#' @param highlight_color Color to use to fill the background of a cell. 
#' @param graph_title         Title to appear in the upper left hand corner of the graph.
#' @param graph_subtitle      Subtitle to appear immediately under the graph title
#'                            in the upper left hand side of the graph.
#' 
#' @importFrom graphics rect text mtext par plot.new plot.window
#' @rdname draw-matrix
#' @export
#' @examples
#' # Base graphics
#' 
#' # Visualize a 3x3
#' mat_3x3 = matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
#' draw_matrix(mat_3x3)
#' 
#' # Show the cell indices
#' draw_matrix(mat_3x3, show_indices = "cell")
#' 
#' # Highlight a row
#' mat_4x4 = matrix(seq_len(16), nrow = 4)
#' draw_matrix(
#'   mat_4x4, show_indices = "row", 
#'   highlight_area = highlight_rows(mat_4x4, rows = 1)
#' )
#' 
#' # Highlight values above 5
#' mat_2x4 = matrix(round(rnorm(16, 5, 2), 2), ncol = 4)
#' draw_matrix(mat_2x4, highlight_area = mat_2x4 > 2) 
draw_matrix <- function(
    data,
    show_indices = "none",
    highlight_area = matrix(FALSE, nrow = nrow(data), ncol = ncol(data)),
    highlight_color = "lemonchiffon",
    graph_title = paste0("Data Object: ", deparse(substitute(data))),
    graph_subtitle = paste0(
      "Dimensions: ", paste(n_row, "rows x", n_col, "columns"), " | ",
      "Data Type: ", paste(class(data), collapse=", ")
    )
) {
  
  if (!is.matrix(data)) {
    stop("Please double-check the data supplied is of a `vector` type.")
  }
  
  if (!all(show_indices %in% c("none", "cell", "row", "column", "all"))) {
    stop("Please only use the values of 'none', 'cell', 'row', 'column', or 'all' in `show_indices`.")
  } 
  
  n_row <- nrow(data)
  n_col <- ncol(data)
  n_elem <- n_row * n_col
  
  # Remove exterior margin
  par(mar = c(0.1, 0.1, 2, 0.1))
  
  # Initialize plot at the origin
  plot.new()
  plot.window(
    xlim = c(0, n_col + 1), ylim = c(-.1, n_row + .1)
  )
  
  position_array <- seq_len(n_elem)
  
  # Draw rectangles and labels
  fill_color_values <- ifelse(highlight_area, highlight_color, "white")
  text_values <- ifelse(
    is.finite(data) | is.infinite(data) | is.nan(data), data, 
    ifelse(is.na(data), "NA", "Unknown")
  )  
  text_color_values <- ifelse(
    is.finite(data), "black",
    ifelse(
      is.infinite(data) | is.nan(data), "blue", "red"
    )
  )
  
  # Draw a rectangle around all cells in the matrix
  rect(0.5, n_row, n_col + 0.5, 0, border = "black", lwd = 2)
  
  # Obtain all (x, y) coordinate pairs
  rect_coords <- expand.grid(
    x = seq(0.5, n_col)+0.5,
    y = seq(0.5, n_row)+0.5
  )
  
  # Draw the cell rectangles
  rect(
    xleft = rect_coords$x - 0.5,
    ybottom = rect_coords$y - 1,
    xright = rect_coords$x + 0.5,
    ytop = rect_coords$y,
    col = fill_color_values,
    border = "black"
  )
  
  # Show the cell contents with appropriate color
  text(
    x = rep(seq_len(n_col), each = n_row),
    y = rep(n_row:1, times = n_col) - 0.5,
    labels = text_values,
    cex = 1.25,
    col = text_color_values
  )
  
  # Label each entry inside of the matrix
  if (any(show_indices %in% c("cell", "all"))) {
    id_x = rep(seq_len(n_col), each = n_row)
    id_y = rep(n_row:1, times = n_col)
    text(
      x = id_x,
      y = id_y - 0.7,
      labels = paste("[", rev(id_y), ", ", id_x,"]", sep = ""),
      cex = .9,
      col = "grey"
    )
  }
  
  # Add row indices to the left
  if (any(show_indices %in% c("row", "all"))) {
    text(
      x = 0.25,
      y = n_row:1 - 0.5,
      labels = paste("[", seq_len(n_row), ", ]", sep = ""),
      cex = .95,
      col = "grey"
    )
  }
  
  # Add column indices to the top
  if (any(show_indices %in% c("column", "all")) ) {
    text(
      x = seq_len(n_col),
      y = n_row + 0.15,
      labels = paste("[ , ", seq_len(n_col), "]", sep = ""),
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

## ggplot2 matrix ----

#' @rdname draw-matrix
#' @export
#' @examples
#' # ggplot2 graphics ----
#' 
#' # Visualize a 3x3
#' mat_3x3 = matrix(c(10, 200, -30, 40, 500, 30, 90, -55, 10), ncol = 3)
#' gdraw_matrix(mat_3x3)
#' 
#' # View the matrix without indices present
#' gdraw_matrix(mat_3x3, highlight_area = FALSE)
#' 
#' # Highlight a row
#' mat_2x2 = matrix(c(1, 2, 3, 4), nrow = 2)
#' mat_2x2_mask = matrix(c(TRUE, TRUE, FALSE, FALSE), nrow = 2)
#' gdraw_matrix(mat_2x2, highlight_area = mat_2x2_mask)
#' 
#' # Highlight values above 5
#' mat_3x5 = matrix(round(rnorm(15, 5, 2), 2), ncol = 5)
#' gdraw_matrix(mat_3x5, highlight_area = mat_3x5 > 2) 
gdraw_matrix <- function(
    data,
    show_indices = "none",
    highlight_area = matrix(FALSE, nrow(data), ncol(data)), 
    highlight_color = "lemonchiffon",
    graph_title = paste0("Data Object: ", deparse(substitute(data))),
    graph_subtitle = paste0(
      "Dimensions: ", paste(n_row, "rows x", n_col, "columns"), " | ",
      "Data Type: ", paste(class(data), collapse=", ")
    )
) {
  
  # Define a value to avoid using the .data pronoun and incorporate rlang...
  highlight = value = NULL
  
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please make sure `ggplot2` is installed to use this function.")
  }
  
  if (!is.matrix(data)) {
    stop("Please double-check the data supplied is of a `vector` type.")
  }
  
  if (!all(show_indices %in% c("none", "cell", "row", "column", "all"))) {
    stop("Please only use the values of 'none', 'cell', 'row', 'column', or 'all' in `show_indices`.")
  } 
  
  n_row <- nrow(data)
  n_col <- ncol(data)
  n_elem <- n_row * n_col
  
  col_ind <- seq_len(n_col)
  row_ind <- seq_len(n_row)
  
  # Create a data frame for ggplot
  df <- expand.grid(row = rev(row_ind), col = col_ind)
  df$highlight <- as.vector(highlight_area)
  df$value <- as.vector(data)
  
  # TODO: fix no visible binding for global variable
  
  g <- ggplot2::ggplot(df) +
    ggplot2::aes(x = col, y = row, label = value, fill = highlight) + 
    ggplot2::geom_tile(color = "black", width = 1, height = 1) +
    ggplot2::scale_fill_manual(values = c("white", highlight_color), na.value="white") +
    ggplot2::geom_rect(
      xmin = 0.5, xmax = ncol(data) + 0.5, 
      ymin = 0.5, ymax = nrow(data) + 0.5, 
      fill = NA, color = "black", size = .25
    ) + 
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          is.finite(df$value) | is.infinite(df$value) | is.nan(df$value),
          df$value, "NA"
        )
      ), 
      size = 5, 
      color = ifelse(
        is.finite(df$value), "black", 
          ifelse(is.infinite(df$value) | is.nan(df$value), "blue", "red")
      )
    ) +
    # Provide details on the object plotted
    ggplot2::labs(
      title = graph_title,
      subtitle = graph_subtitle
    ) +    
    # Set the limits and breaks of the axes
    ggplot2::scale_x_continuous(limits = c(0, n_col + 1), breaks = seq_len(n_col), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, n_row + 1), breaks = seq_len(n_row), expand = c(0, 0)) +
    # Remove everything
    ggplot2::theme_void() + 
    # Disable showing the legend's highlight call.
    ggplot2::theme(legend.position = "none")
  
  
  # Include the cell indices
  if  (any(show_indices %in% c("cell", "all"))) {
    g <- g + 
      ggplot2::geom_text(
        label = paste0("[", n_row - df$row + 1, ", ", df$col, "]"),
        color = "grey", hjust = 0.5, vjust = 0.5, nudge_y = -0.15, size = 4)
  }
  
  # Add row indices to the left
  if  (any(show_indices %in% c("row", "all"))) {
    g <- g + 
      ggplot2::annotate(
        geom = "text",
        label = paste0("[", rev(row_ind), ", ]"), 
        x = 0.4, y = row_ind, color = "grey", hjust = 1, vjust = 0.5, size = 4)
  }
  
  # Add column indices to the top
  if (any(show_indices %in% c("column", "all"))) {
    g <- g + 
      ggplot2::annotate(
        geom = "text",
        label = paste0("[ , ", col_ind, "]"),
        x = col_ind, y = n_row + 0.65, color = "grey", hjust = 0.5, vjust = 0.5, size = 4)
  }
    
  g
}
