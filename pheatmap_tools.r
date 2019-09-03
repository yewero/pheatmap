draw_highlights <- function (matrix, highlights_rows, highlights_cols, highlights_color = "black", gaps_rows, gaps_cols) 
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
environment(draw_highlights) <- asNamespace('pheatmap')

heatmap_motor_new <- function (matrix, border_color, cellwidth, cellheight, tree_col, 
    tree_row, treeheight_col, treeheight_row, filename, width, 
    height, breaks, color, legend, annotation_row, annotation_col, 
    annotation_colors, annotation_legend, annotation_names_row, 
    annotation_names_col, main, fontsize, fontsize_row, fontsize_col, 
    hjust_col, vjust_col, angle_col, fmat, fontsize_number, number_color, 
    gaps_col, gaps_row, labels_row, labels_col, 
    highlights_row, highlights_col, highlights_color, ...) 
{
    lo = lo(coln = labels_col, rown = labels_row, nrow = nrow(matrix), 
        ncol = ncol(matrix), cellwidth = cellwidth, cellheight = cellheight, 
        treeheight_col = treeheight_col, treeheight_row = treeheight_row, 
        legend = legend, annotation_col = annotation_col, annotation_row = annotation_row, 
        annotation_colors = annotation_colors, annotation_legend = annotation_legend, 
        annotation_names_row = annotation_names_row, annotation_names_col = annotation_names_col, 
        main = main, fontsize = fontsize, fontsize_row = fontsize_row, 
        fontsize_col = fontsize_col, angle_col = angle_col, gaps_row = gaps_row, 
        gaps_col = gaps_col, ...)
    res = lo$gt
    mindim = lo$mindim
    if (!is.na(filename)) {
        if (is.na(height)) {
            height = convertHeight(gtable_height(res), "inches", 
                valueOnly = T)
        }
        if (is.na(width)) {
            width = convertWidth(gtable_width(res), "inches", 
                valueOnly = T)
        }
        r = regexpr("\\.[a-zA-Z]*$", filename)
        if (r == -1) 
            stop("Improper filename")
        ending = substr(filename, r + 1, r + attr(r, "match.length"))
        f = switch(ending, pdf = function(x, ...) pdf(x, ...), 
            png = function(x, ...) png(x, units = "in", res = 300, 
                ...), jpeg = function(x, ...) jpeg(x, units = "in", 
                res = 300, ...), jpg = function(x, ...) jpeg(x, 
                units = "in", res = 300, ...), tiff = function(x, 
                ...) tiff(x, units = "in", res = 300, compression = "lzw", 
                ...), bmp = function(x, ...) bmp(x, units = "in", 
                res = 300, ...), stop("File type should be: pdf, png, bmp, jpg, tiff"))
        f(filename, height = height, width = width)
        gt = heatmap_motor_new(matrix, cellwidth = cellwidth, cellheight = cellheight, 
            border_color = border_color, tree_col = tree_col, 
            tree_row = tree_row, treeheight_col = treeheight_col, 
            treeheight_row = treeheight_row, breaks = breaks, 
            color = color, legend = legend, annotation_col = annotation_col, 
            annotation_row = annotation_row, annotation_colors = annotation_colors, 
            annotation_legend = annotation_legend, annotation_names_row = annotation_names_row, 
            annotation_names_col = annotation_names_col, filename = NA, 
            main = main, fontsize = fontsize, fontsize_row = fontsize_row, 
            fontsize_col = fontsize_col, hjust_col = hjust_col, 
            vjust_col = vjust_col, angle_col = angle_col, fmat = fmat, 
            fontsize_number = fontsize_number, number_color = number_color, 
            labels_row = labels_row, labels_col = labels_col, 
            gaps_col = gaps_col, gaps_row = gaps_row, 
            highlights_row = highlights_row, highlights_col = highlights_col, highlights_color = highlights_color, ...)
        grid.draw(gt)
        dev.off()
        return(gt)
    }
    if (mindim < 3) 
        border_color = NA
    if (!is.na(main)) {
        elem = draw_main(main, fontsize = 1.3 * fontsize, ...)
        res = gtable_add_grob(res, elem, t = 1, l = 3, name = "main", 
            clip = "off")
    }
    if (!is.na2(tree_col) & treeheight_col != 0) {
        elem = draw_dendrogram(tree_col, gaps_col, horizontal = T)
        res = gtable_add_grob(res, elem, t = 2, l = 3, name = "col_tree")
    }
    if (!is.na2(tree_row) & treeheight_row != 0) {
        elem = draw_dendrogram(tree_row, gaps_row, horizontal = F)
        res = gtable_add_grob(res, elem, t = 4, l = 1, name = "row_tree")
    }
    elem = draw_matrix(matrix, border_color, gaps_row, gaps_col, 
        fmat, fontsize_number, number_color)
    res = gtable_add_grob(res, elem, t = 4, l = 3, clip = "off", 
        name = "matrix")
    ###
    if (length(highlights_row) != 0) {
    elem = draw_highlights(matrix, highlights_row, highlights_col, highlights_color = highlights_color, 
        gaps_row, gaps_col)
    res = gtable_add_grob(res, elem, t = 4, l = 3, clip = "off", 
        name = "highlights")
    }
    ###
    if (length(labels_col) != 0) {
        pars = list(labels_col, gaps = gaps_col, fontsize = fontsize_col, 
            hjust_col = hjust_col, vjust_col = vjust_col, angle_col = angle_col, 
            ...)
        elem = do.call(draw_colnames, pars)
        res = gtable_add_grob(res, elem, t = 5, l = 3, clip = "off", 
            name = "col_names")
    }
    if (length(labels_row) != 0) {
        pars = list(labels_row, gaps = gaps_row, fontsize = fontsize_row, 
            ...)
        elem = do.call(draw_rownames, pars)
        res = gtable_add_grob(res, elem, t = 4, l = 4, clip = "off", 
            name = "row_names")
    }
    if (!is.na2(annotation_col)) {
        converted_annotation = convert_annotations(annotation_col, 
            annotation_colors)
        elem = draw_annotations(converted_annotation, border_color, 
            gaps_col, fontsize, horizontal = T)
        res = gtable_add_grob(res, elem, t = 3, l = 3, clip = "off", 
            name = "col_annotation")
        if (annotation_names_col) {
            elem = draw_annotation_names(annotation_col, fontsize, 
                horizontal = T)
            res = gtable_add_grob(res, elem, t = 3, l = 4, clip = "off", 
                name = "col_annotation_names")
        }
    }
    if (!is.na2(annotation_row)) {
        converted_annotation = convert_annotations(annotation_row, 
            annotation_colors)
        elem = draw_annotations(converted_annotation, border_color, 
            gaps_row, fontsize, horizontal = F)
        res = gtable_add_grob(res, elem, t = 4, l = 2, clip = "off", 
            name = "row_annotation")
        if (annotation_names_row) {
            elem = draw_annotation_names(annotation_row, fontsize, 
                horizontal = F, hjust_col = hjust_col, vjust_col = vjust_col, 
                angle_col = angle_col)
            res = gtable_add_grob(res, elem, t = 5, l = 2, clip = "off", 
                name = "row_annotation_names")
        }
    }
    annotation = c(annotation_col[length(annotation_col):1], 
        annotation_row[length(annotation_row):1])
    annotation = annotation[unlist(lapply(annotation, function(x) !is.na2(x)))]
    if (length(annotation) > 0 & annotation_legend) {
        elem = draw_annotation_legend(annotation, annotation_colors, 
            border_color, fontsize = fontsize, ...)
        t = ifelse(is.null(labels_row), 4, 3)
        res = gtable_add_grob(res, elem, t = t, l = 6, b = 5, 
            clip = "off", name = "annotation_legend")
    }
    if (!is.na2(legend)) {
        elem = draw_legend(color, breaks, legend, fontsize = fontsize, 
            ...)
        t = ifelse(is.null(labels_row), 4, 3)
        res = gtable_add_grob(res, elem, t = t, l = 5, b = 5, 
            clip = "off", name = "legend")
    }
    return(res)
}
environment(heatmap_motor_new) <- asNamespace('pheatmap')

pheatmap_new <- function (mat, color = colorRampPalette(rev(brewer.pal(n = 7, 
    name = "RdYlBu")))(100), kmeans_k = NA, breaks = NA, border_color = "grey60", 
    cellwidth = NA, cellheight = NA, scale = "none", cluster_rows = TRUE, 
    cluster_cols = TRUE, clustering_distance_rows = "euclidean", 
    clustering_distance_cols = "euclidean", clustering_method = "complete", 
    clustering_callback = identity2, cutree_rows = NA, cutree_cols = NA, 
    treeheight_row = ifelse((class(cluster_rows) == "hclust") || 
        cluster_rows, 50, 0), treeheight_col = ifelse((class(cluster_cols) == 
        "hclust") || cluster_cols, 50, 0), legend = TRUE, legend_breaks = NA, 
    legend_labels = NA, annotation_row = NA, annotation_col = NA, 
    annotation = NA, annotation_colors = NA, annotation_legend = TRUE, 
    annotation_names_row = TRUE, annotation_names_col = TRUE, 
    drop_levels = TRUE, show_rownames = T, show_colnames = T, 
    main = NA, fontsize = 10, fontsize_row = fontsize, fontsize_col = fontsize, 
    angle_col = c("270", "0", "45", "90", "315"), display_numbers = F, 
    number_format = "%.2f", number_color = "grey30", fontsize_number = 0.8 * 
        fontsize, gaps_row = NULL, gaps_col = NULL, labels_row = NULL, 
    labels_col = NULL, filename = NA, width = NA, height = NA, 
    silent = FALSE, na_col = "#DDDDDD", 
    highlights_row = NULL, highlights_col = NULL, highlights_color = "black", ...) 
{
    if (is.null(labels_row)) {
        labels_row = rownames(mat)
    }
    if (is.null(labels_col)) {
        labels_col = colnames(mat)
    }
    mat = as.matrix(mat)
    if (scale != "none") {
        mat = scale_mat(mat, scale)
        if (is.na2(breaks)) {
            breaks = generate_breaks(mat, length(color), center = T)
        }
    }
    if (!is.na(kmeans_k)) {
        km = kmeans(mat, kmeans_k, iter.max = 100)
        mat = km$centers
        t = table(km$cluster)
        labels_row = sprintf("Cluster: %s Size: %d", names(t), 
            t)
    }
    else {
        km = NA
    }
    if (is.matrix(display_numbers) | is.data.frame(display_numbers)) {
        if (nrow(display_numbers) != nrow(mat) | ncol(display_numbers) != 
            ncol(mat)) {
            stop("If display_numbers provided as matrix, its dimensions have to match with mat")
        }
        display_numbers = as.matrix(display_numbers)
        fmat = matrix(as.character(display_numbers), nrow = nrow(display_numbers), 
            ncol = ncol(display_numbers))
        fmat_draw = TRUE
    }
    else {
        if (display_numbers) {
            fmat = matrix(sprintf(number_format, mat), nrow = nrow(mat), 
                ncol = ncol(mat))
            fmat_draw = TRUE
        }
        else {
            fmat = matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
            fmat_draw = FALSE
        }
    }
    if ((class(cluster_rows) == "hclust") || cluster_rows) {
        if (class(cluster_rows) == "hclust") {
            tree_row = cluster_rows
        }
        else {
            tree_row = cluster_mat(mat, distance = clustering_distance_rows, 
                method = clustering_method)
            tree_row = clustering_callback(tree_row, mat)
        }
        mat = mat[tree_row$order, , drop = FALSE]
        fmat = fmat[tree_row$order, , drop = FALSE]
        labels_row = labels_row[tree_row$order]
        if (!is.na(cutree_rows)) {
            gaps_row = find_gaps(tree_row, cutree_rows)
        }
        else {
            gaps_row = NULL
        }
    }
    else {
        tree_row = NA
        treeheight_row = 0
    }
    if ((class(cluster_cols) == "hclust") || cluster_cols) {
        if (class(cluster_cols) == "hclust") {
            tree_col = cluster_cols
        }
        else {
            tree_col = cluster_mat(t(mat), distance = clustering_distance_cols, 
                method = clustering_method)
            tree_col = clustering_callback(tree_col, t(mat))
        }
        mat = mat[, tree_col$order, drop = FALSE]
        fmat = fmat[, tree_col$order, drop = FALSE]
        labels_col = labels_col[tree_col$order]
        if (!is.na(cutree_cols)) {
            gaps_col = find_gaps(tree_col, cutree_cols)
        }
        else {
            gaps_col = NULL
        }
    }
    else {
        tree_col = NA
        treeheight_col = 0
    }
    attr(fmat, "draw") = fmat_draw
    if (!is.na2(legend_breaks) & !is.na2(legend_labels)) {
        if (length(legend_breaks) != length(legend_labels)) {
            stop("Lengths of legend_breaks and legend_labels must be the same")
        }
    }
    if (is.na2(breaks)) {
        breaks = generate_breaks(as.vector(mat), length(color))
    }
    if (!is.infinite(min(breaks))) {
        breaks = c(-Inf, breaks)
        color = c(color[1], color)
    }
    if (!is.infinite(max(breaks))) {
        breaks = c(breaks, Inf)
        color = c(color, color[length(color)])
    }
    non_inf_breaks = breaks[!is.infinite(breaks)]
    if (legend & is.na2(legend_breaks)) {
        legend = grid.pretty(range(as.vector(non_inf_breaks)))
        names(legend) = legend
    }
    else if (legend & !is.na2(legend_breaks)) {
        legend = legend_breaks[legend_breaks >= min(non_inf_breaks) & 
            legend_breaks <= max(non_inf_breaks)]
        if (!is.na2(legend_labels)) {
            legend_labels = legend_labels[legend_breaks >= min(non_inf_breaks) & 
                legend_breaks <= max(non_inf_breaks)]
            names(legend) = legend_labels
        }
        else {
            names(legend) = legend
        }
    }
    else {
        legend = NA
    }
    mat = scale_colours(mat, col = color, breaks = breaks, na_col = na_col)
    if (is.na2(annotation_col) & !is.na2(annotation)) {
        annotation_col = annotation
    }
    if (!is.na2(annotation_col)) {
        annotation_col = annotation_col[colnames(mat), , drop = F]
    }
    if (!is.na2(annotation_row)) {
        annotation_row = annotation_row[rownames(mat), , drop = F]
    }
    annotation = c(annotation_row, annotation_col)
    annotation = annotation[unlist(lapply(annotation, function(x) !is.na2(x)))]
    if (length(annotation) != 0) {
        annotation_colors = generate_annotation_colours(annotation, 
            annotation_colors, drop = drop_levels)
    }
    else {
        annotation_colors = NA
    }
    if (!show_rownames) {
        labels_row = NULL
    }
    if (!show_colnames) {
        labels_col = NULL
    }
    angle_col = as.character(angle_col)
    angle_col = match.arg(angle_col)
    if (angle_col == "0") {
        angle_col = 0
        hjust_col = 0.5
        vjust_col = 1
    }
    if (angle_col == "45") {
        angle_col = 45
        hjust_col = 1
        vjust_col = 1
    }
    if (angle_col == "90") {
        angle_col = 90
        hjust_col = 1
        vjust_col = 0.5
    }
    if (angle_col == "270") {
        angle_col = 270
        hjust_col = 0
        vjust_col = 0.5
    }
    if (angle_col == "315") {
        angle_col = 315
        hjust_col = 0
        vjust_col = 1
    }
    #pdf(file = NULL)
    gt = heatmap_motor_new(mat, border_color = border_color, cellwidth = cellwidth, 
        cellheight = cellheight, treeheight_col = treeheight_col, 
        treeheight_row = treeheight_row, tree_col = tree_col, 
        tree_row = tree_row, filename = filename, width = width, 
        height = height, breaks = breaks, color = color, legend = legend, 
        annotation_row = annotation_row, annotation_col = annotation_col, 
        annotation_colors = annotation_colors, annotation_legend = annotation_legend, 
        annotation_names_row = annotation_names_row, annotation_names_col = annotation_names_col, 
        main = main, fontsize = fontsize, fontsize_row = fontsize_row, 
        fontsize_col = fontsize_col, hjust_col = hjust_col, vjust_col = vjust_col, 
        angle_col = angle_col, fmat = fmat, fontsize_number = fontsize_number, 
        number_color = number_color, gaps_row = gaps_row, gaps_col = gaps_col, 
        labels_row = labels_row, labels_col = labels_col, 
        highlights_row = highlights_row, highlights_col = highlights_col, highlights_color = highlights_color, ...)
    #dev.off()
    if (is.na(filename) & !silent) {
        grid.newpage()
        grid.draw(gt)
    }
    invisible(structure(list(tree_row = tree_row, tree_col = tree_col, 
        kmeans = km, gtable = gt), class = "pheatmap"))
}
environment(pheatmap_new) <- asNamespace('pheatmap')

