#' Sand paint
#'
#' Sand paints a set of line segments
#' @param points A data frame with x, y, xend, yend coordinates for the line segments
#' @param n_grains Number of grains distributed across each line segment
#' @keywords sand paint
#' @export
#' @examples
#' sand_point()

library(tidyverse)

sand_paint <- function(edges, n_grains = 100) {
  # Function for sand painting a single edge
  sand_paint_edge <- function(edge_num) {
    a <- as.numeric(edges[edge_num, c("x", "y")])
    b <- as.numeric(edges[edge_num, c("xend", "yend")])
    data.frame(alpha = runif(n_grains)) %>%
      dplyr::mutate(x = a[1] * (1 - alpha) + b[1] * alpha,
                    y = a[2] * (1 - alpha) + b[2] * alpha) %>%
      dplyr::select(-alpha)
  }
  # Sand paint all the edges
  1:nrow(edges) %>%
    purrr::map_df(~sand_paint_edge(.x), id = "id")
}

data <- data.frame(id = c(1, 2, 3), x = c(2, 2, 2), y = c(1, 2, 3), xend = c(500, 500, 500), yend = c(-100, 100, 200))

data_sand <- sand_paint(data, n_grains = 100)

ggplot(data_sand) +
  geom_point(aes(x = x, y = y)) +
  theme_minimal()

ggplot(data) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  theme_minimal()

