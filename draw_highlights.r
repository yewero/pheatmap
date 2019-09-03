function (matrix, highlights_rows, highlights_cols, highlights_color = "black", gaps_rows, gaps_cols) 
{
  ###
  if( (length(highlights_rows)%%2 != 0) || (length(highlights_cols)%%2 != 0) ) {
    stop("The row or column nunmber for highlights is odd number, exit.")
  }
  if(length(highlights_rows) != length(highlights_cols)) {
    stop("The row and column nunmber for highlights are different, exit.")
  }
  highlights_rows_mat <- matrix(highlights_rows, ncol = 2, byrow = T)
  highlights_cols_mat <- matrix(highlights_cols, ncol = 2, byrow = T)
  highlights_num <- nrow(highlights_rows_mat)
  ###
  
  n = nrow(matrix)
  m = ncol(matrix)
  coord_x = find_coordinates(m, gaps_cols)
  coord_y = find_coordinates(n, gaps_rows)
  
  ###
  highlights_LS <- list(x = NULL, y = NULL, width = NULL, height = NULL)
  for(i in 1:highlights_num) {
    ind_x <- highlights_cols_mat[i, , drop = T]
    ind_y <- highlights_rows_mat[i, , drop = T]
    # x and y
    x0 <- coord_x$coord[ind_x[1]] - coord_x$size
    y0 <- coord_y$coord[ind_y[1]] - coord_y$size
    x1 <- coord_x$coord[ind_x[2]]
    y1 <- coord_y$coord[ind_y[2]]
    # conversion of y
    y0 <- unit(1, "npc") - y0
    y1 <- unit(1, "npc") - y1
    # coord
    x <- (x0 + x1) * 0.5
    y <- (y0 + y1) * 0.5
    width = x1 - x0
    height = y0 - y1
    # 
    if(i == 1) {
      highlights_LS[["x"]] <- x
      highlights_LS[["y"]] <- y
      highlights_LS[["width"]] <- width
      highlights_LS[["height"]] <- height
    } else {
      highlights_LS[["x"]] <- unit.c(highlights_LS[["x"]], x)
      highlights_LS[["y"]] <- unit.c(highlights_LS[["y"]], y)
      highlights_LS[["width"]] <- unit.c(highlights_LS[["width"]], width)
      highlights_LS[["height"]] <- unit.c(highlights_LS[["height"]], height)
    }
  }
  
  res = gList()
  res[["rect"]] = rectGrob(x = highlights_LS$x, y = highlights_LS$y, width = highlights_LS$width, 
                           height = highlights_LS$height, gp = gpar(fill = NA, col = highlights_color))
  res = gTree(children = res)
  return(res)
  ###
}

