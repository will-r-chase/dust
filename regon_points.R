library(tidyverse)

regon_pts <- function(points = 10000, edges = 3, start_angle = 0, r = 10) {
  a = start_angle * (pi / 180)
  n = points / edges
  
  corners <- tibble(angle = seq(0 + a, 2*pi + a, length.out = edges + 1), x = r*cos(angle), y = r*sin(angle))
  
  edges_list <- list()
  
  for(i in 1:edges) {
    edges_list[[i]] <- tibble(id = 1:n) %>%
      mutate(d = id / n, 
             x = (1 - d) * corners$x[i] + d * corners$x[i + 1],
             y = (1 - d) * corners$y[i] + d * corners$y[i + 1])
  }
  
  bind_rows(edges_list)
}


square <- regon_pts(edges = 3, start_angle = 100)



ggplot(square) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  coord_equal()
ggsave("regon2.png", device = "png", type = "cairo", width = 6, height = 6)
