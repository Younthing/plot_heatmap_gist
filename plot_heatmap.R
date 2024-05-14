library(pheatmap)
library(tidyverse)
heatmap_plot <- function(
    expr, path = "heatmap.pdf",
    group_anno = NA,
    colors = c("#0547aa", "white", "#f11313")) {
  lapply(graphics.off(), dev.off)
  pdf(path)
  pheatmap(expr,
    cluster_rows = TRUE,
    show_rownames = TRUE,
    show_colnames = FALSE,
    scale = "row",
    cluster_cols = FALSE,
    fontsize_row = 6,
    fontsize_col = 6,
    annotation_col = group_anno,
    annotation_colors = NA,
    color = colorRampPalette(colors)(50), # nolint
    # color = colorRampPalette(c("green3", "white", "blue4"))(100),#换颜色
    angle_col = 45, # 修改横轴坐标名倾斜度
  )
  dev.off()
}

local({
  argparser::include("./0-argparse_source.R")
  heatmap_params <- global_params$heatmap

  # 传参 -----------------
  expr_file <- global_params$expr_file
  group_file <- global_params$group_file
  gene_file <- heatmap_params$gene_file
  group_levels <- eval(parse(text = heatmap_params$group_levels))
  colors <- eval(parse(text = heatmap_params$colors))
  output_dir <- heatmap_params$output_dir
  output_file <- heatmap_params$output_file

  # 分析 ------------

  ## 读取数据
  expr_data <- read.csv(expr_file, row.names = 1)
  group <- read.csv(group_file)
  genes <- read.csv(gene_file)[[1]]

  ## 整理数据
  group <- group |>
    dplyr::select(sample, group) |>
    dplyr::mutate(sample = make.names(sample)) |>
    dplyr::filter(group %in% group_levels) |> # filter 不和which一起用
    dplyr::mutate(
      group = factor(group,
        levels = group_levels
      ) # 关键
    ) |>
    dplyr::arrange(group)

  rownames(group) <- group$sample
  group_anno <- group[, -1, drop = FALSE]
  group_anno <- group_anno %>% dplyr::arrange(group)

  deg_gene_expr <- expr_data[genes, rownames(group_anno)]

  # 地址
  spical_dir <- gsub("\\.[^.]*$", "", basename(gene_file))
  fs::dir_create(fs::path(output_dir, spical_dir))
  path <- fs::path(output_dir, spical_dir, output_file)

  # end
  heatmap_plot(deg_gene_expr, path, group_anno, colors)
})
