library(tidyverse)
library(scales)

split_plot <- function(p, ncol = 3) {
  # p <- plot_demo()
  # p <- ggplot(aes(x = height, y = name), data = starwars) + geom_point()

  p_data <- p$data
  y_name <- quo_name(p$mapping$y)
  #build_p <- ggplot_build(p)

  if (n_distinct(paste(p_data$y, p_data$x)) != nrow(p_data)) {
    warning("Data should be rolled up prior to creating plot")
  }

  nested_data <-
    p_data |>
    mutate(.plot_y = get(y_name)) |>
    nest(data = -.plot_y) |>
    arrange(desc(.plot_y)) |>
    mutate(.plot_id = row_number())

  rows_per_facet <- ceiling(nrow(nested_data) / ncol)
  total_rows <- rows_per_facet * ncol

  base_table <-
    tibble(.plot_id = 1:(rows_per_facet * ncol)) |>
    mutate(
      .plot_y_row = rep(1:rows_per_facet, ncol),
      # needs something to facet on, need '-' to keep highest values on left
      .plot_facet = rep(1:ncol, each = rows_per_facet),
      .plot_placeholder = str_pad("", rows_per_facet + 1 - .plot_y_row, pad = " ")
    ) |>
    print(n = Inf)


  p$data <-
    base_table |>
    left_join(nested_data) |>
    unnest(data, keep_empty = TRUE) |>
    mutate(
      y =
        coalesce(y, .plot_placeholder) |>
        fct_inorder(ordered = TRUE) |>
        fct_rev()
    ) |>
    print()


  # max_rows <- ceiling(length(build_p$data[[1]]$PANEL) / ncol)
  # # needs something to facet on, need '-' to keep highest values on left
  # new_facets <- 1 + (seq_along(facet_id) %/% max_rows)
  #
  # build_p$data[[1]]$PANEL <- new_facets
  #
  # ggplot_gtable(build_p)
  #


  # plot
   p +
  #p$data |> ggplot(aes(x, y)) + geom_point() +
    facet_wrap(~.plot_facet, ncol = ncol, scales = "free_y") +
    # geom_tile(color = "white", size = 0.5) +
    #scale_fill_stepsn(na.value = "white") +
    theme(
#      strip.background.y = element_blank(),
  #    strip.text = element_blank()#,
  #    axis.text = element_text(size = 7)
      axis.ticks.y = element_blank()
    )
}

coord_square_heatmap <- function(p) {
  # p <- plot_demo() |> split_plot(ncol = 3)

  build_p <- ggplot_build(p)

  # identify aspect ratio
  aspect <-
    build_p$data[[1]] |>
    select(x, y, facet = PANEL) |>
    summarise_all(n_distinct) |>
    mutate(ratio = (y / x) * 0.95) |>
    print()

  p + theme(aspect.ratio = aspect$ratio)
}


plot_demo <- function() {
  USJudgeRatings |>
    as_tibble(rownames = "y") |>
    gather(x, fill, -y) |>
    mutate(
      x = fct_reorder(x, fill, median),
      y = fct_reorder(y, fill, sum, .desc = FALSE)
    ) |>
    ggplot(aes(x, y, fill = fill)) +
    geom_tile(color = "white") +
    scale_fill_stepsn(
      na.value = "white",
      breaks = breaks_pretty(6),
      colours = viridis_pal()(5)
    )  +
    theme(axis.text = element_text(size = 7)) +
    labs(x = NULL, y = NULL)
}

