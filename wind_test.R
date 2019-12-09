#initiate, a set of points that draws a shape/line/whatever
#sand paint with lots of grains
#give grains a weight/inertia
#apply wind force with direction
#move grains according to force/intertia in the direction
#apply random jitter to points in direction and perp to direction
#apply more jitter the further the point travels (= the less the inertia)

library(tidyverse)
library(EnvStats)
library(zoo)

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

line <- data.frame(x = 1, xend = 100, y = 1, yend = 100)

colors <- colorRampPalette(c("red", "blue"))

rtnorm <- function(n, mean = 0, sd = 1, min = 0, max = 1) {
  bounds <- pnorm(c(min, max), mean, sd)
  u <- runif(n, bounds[1], bounds[2])
  qnorm(u, mean, sd)
}

##plan::
#sample rows and insert breaks at that point
#for each subsample, ether make inertia 0, or let it vary with distr
#for the ones that are 0, add some small variation
#see if you can make more variation around ends
gen_seed <- function(seed, n_grains = 10000, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1) {
  sand <- sand_paint(seed, n_grains = n_grains) %>%
    arrange(x) %>%
    mutate(id = 1:nrow(.))
  
  splits <- sand %>% sample_n(nrow(.) / split_mod)
  
  sand_split <- 
    sand %>%
    left_join(splits, by = "id") %>%
    mutate(split = ifelse(is.na(x.y), NA, id))
  
  sand_split$split[1] <- 0
  split2 <- na.locf(sand_split$split)
  
  sand_split %>%
    mutate(split = split2, group_split = split2) %>%
    select(id, x = x.x, y = y.x, split, group_split) %>%
    group_by(group_split) %>%
    group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
    map( ~ mutate(., id = 1:nrow(.))) %>%
    map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
    bind_rows(.id = "group_id")
}

gen_seed_cir <- function(n_grains = 10000, r = 30, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1) {
  sand <- circle(n_grains, r) %>%
    mutate(id = 1:nrow(.))
  
  splits <- sand %>% sample_n(nrow(.) / split_mod)
  
  sand_split <- 
    sand %>%
    left_join(splits, by = "id") %>%
    mutate(split = ifelse(is.na(x.y), NA, id))
  
  sand_split$split[1] <- 0
  split2 <- na.locf(sand_split$split)
  
  sand_split %>%
    mutate(split = split2, group_split = split2) %>%
    select(id, x = x.x, y = y.x, split, group_split) %>%
    group_by(group_split) %>%
    group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
    map( ~ mutate(., id = 1:nrow(.))) %>%
    map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
    bind_rows(.id = "group_id")
}

gen_seed_regon <- function(n_grains = 10000, r = 30, edges = 3, start_angle = 0, split_mod = 200, inertia_prob1 = 1, inertia_prob2 = 1) {
  sand <- regon_pts(points = n_grains, r = r, edges = edges, start_angle = start_angle) %>%
    mutate(id = 1:nrow(.))
  
  splits <- sand %>% sample_n(nrow(.) / split_mod)
  
  sand_split <- 
    sand %>%
    left_join(splits, by = "id") %>%
    mutate(split = ifelse(is.na(x.y), NA, id))
  
  sand_split$split[1] <- 0
  split2 <- na.locf(sand_split$split)
  
  sand_split %>%
    mutate(split = split2, group_split = split2) %>%
    select(id, x = x.x, y = y.x, split, group_split) %>%
    group_by(group_split) %>%
    group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(inertia_prob1, inertia_prob2)))) %>%
    map( ~ mutate(., id = 1:nrow(.))) %>%
    map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
    bind_rows(.id = "group_id")
}

paint <- function(line, grains) {
  sand_paint(line, n_grains = grains) %>%
    arrange(x) %>%
    mutate(id = 1:nrow(.))
}

splitter2 <- function(data, seed, inertia_mean = 1, inertia_sd = 5, inertia_min = 1, inertia_max = 100) {
  data %>%
    mutate(split = seed$split, inertia2 = seed$inertia2) %>%
    select(id, x, y, split, inertia2) %>%
    group_by(split) %>%
    group_map( ~ mutate(., id = 1:nrow(.))) %>%
    map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
    bind_rows(.id = "group_id") %>%
    mutate(inertia = ifelse(is.na(inertia2), rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100), inertia2)) %>%
    ungroup() 
}

fray <- function(data, fray_length = 50) {
  frayed <- 
    data %>%
    filter(inertia != 0) %>%
    mutate(group_id2 = group_id) %>%
    group_by(group_id2) %>%
    group_map( ~ filter(., id %in% 1:sample(floor(1:max(id)/fray_length), 1) | id %in% (max(id)-sample(floor(1:max(id)/fray_length), 1)):max(id))) %>%
    bind_rows() %>%
    ungroup() %>%
    mutate(inertia2 = sample(c(0, 1), nrow(.), replace = TRUE, prob = c(1, 10)),
           inertia = ifelse(inertia2 == 0, 0, inertia))
  
  full_join(data, frayed, by = c("group_id", "id")) %>%
    mutate(inertia = ifelse(is.na(inertia.y), inertia.x, inertia.y)) %>%
    select(group_id, id, x = x.x, y = y.x, inertia, diff = diff.x)
  
}

circle <- function(points = 10000, r = 30) {
  tibble(angle = seq(0, 2*pi, length.out = points), x = r*cos(angle), y = r*sin(angle)) %>%
    mutate(id = 1:nrow(.)) %>%
    select(id, x, y)
}

test_seed <- gen_seed(line, n_grains = 50000, split_mod = 1500)

lines <- list(a = data.frame(x = 1, xend = 100, y = 1, yend = 100),
              b = data.frame(x = 1.25, xend = 100.25, y = 1, yend = 100),
              c = data.frame(x = 1.5, xend = 100.5, y = 1, yend = 100),
              d = data.frame(x = 1.75, xend = 100.75, y = 1, yend = 100),
              e = data.frame(x = 2, xend = 101, y = 1, yend = 100))

testing <- map(lines, ~ paint(., 50000)) %>%
  map( ~ splitter2(., seed = test_seed)) %>%
  map_dfr( ~ fray(.))

#circle test
circle_seed <- gen_seed_cir(n_grains = 20000, r = 50)

circles <- list(a = circle(20000, r = 50),
                b = circle(20000, r = 50.25),
                c = circle(20000, r = 50.5),
                d = circle(20000, r = 50.75),
                e = circle(20000, r = 51)
)

circles_test <- 
  circles %>%
  map( ~ splitter2(., seed = circle_seed)) %>%
  map_dfr( ~ fray(.))


#regon test
regon_seed <- gen_seed_regon(n_grains = 40000, r = 50, split_mod = 400)
regons <- list(a = regon_pts(40000, r = 50),
                b = regon_pts(40000, r = 50.2),
                c = regon_pts(40000, r = 50.4),
                d = regon_pts(40000, r = 50.6),
                e = regon_pts(40000, r = 50.8)
)

regons_test <- 
  regons %>%
  map( ~ splitter2(., seed = regon_seed)) %>%
  map_dfr( ~ fray(.))

 gust <- function(data, angle = 0, force = 100, diff_mod = 100, inertia_mod = 50, jitter_min = 0.1, jitter_max = 1) {
  a <- angle * pi / 180
  perp <- a + (pi / 2)
  
  pts_new <- 
    data %>%
    mutate(x = ifelse(inertia == 0, x + cos(a)*force, x + cos(a) * (force * inertia) - cos(perp)*(diff * diff_mod)*(inertia * inertia_mod)*sample(seq(jitter_min, jitter_max, 0.01), 1)),
           y = ifelse(inertia == 0, y + sin(a)*force, y + sin(a) * (force * inertia) - sin(perp)*(diff * diff_mod)*(inertia * inertia_mod)*sample(seq(jitter_min, jitter_max, 0.01), 1)))
  
  return(pts_new)
}


wind2 <- gust(regons_test, angle = 0, force = 5, diff_mod = 0.013, inertia_mod = 0.001, jitter_min = 2, jitter_max = 20)

ggplot(wind2) +
  geom_point(aes(x = x, y = y), alpha = 0.1, size = 0.1, color = "#1e1e1e", shape = 46) +
  scale_color_identity() +
  theme_void() +
  coord_equal()

ggsave("triangle_1.png", device = "png", type = "cairo", width = 10, height = 10)


## old
sand_split2 <- 
  sand_split %>%
  mutate(split = split2) %>%
  select(id, x = x.x, y = y.x, inertia = inertia.x, color = color.x, split) %>%
  group_by(split) %>%
  group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(1, 1)))) %>%
  map( ~ mutate(., id = 1:nrow(.))) %>%
  map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
  bind_rows(.id = "group_id") %>%
  mutate(inertia = ifelse(is.na(inertia2), inertia, inertia2)) %>%
  ungroup() 


sand <- sand_paint(line, n_grains = 50000) %>%
  arrange(x) %>%
  mutate(color = colors(50000), 
         inertia = rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100),
         id = 1:nrow(.))

####
sand <- sand_paint(line, n_grains = 10000) %>%
  arrange(x) %>%
  mutate(id = 1:nrow(.))

splits <- sand %>% sample_n(nrow(.) / 200)

sand_split <- 
  sand %>%
  left_join(splits, by = "id") %>%
  mutate(split = ifelse(is.na(x.y), NA, id))

sand_split$split[1] <- 0
split2 <- na.locf(sand_split$split)

inertia_seed <- 
  sand_split %>%
  mutate(split = split2) %>%
  select(id, x = x.x, y = y.x, split) %>%
  group_by(split) %>%
  group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(1, 1)))) %>%
  map( ~ mutate(., id = 1:nrow(.))) %>%
  map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
  bind_rows(.id = "group_id")

sand_split2 <- 
  sand_split %>%
  mutate(split = split2) %>%
  select(id, x = x.x, y = y.x, split) %>%
  group_by(split) %>%
  group_map( ~ mutate(., inertia2 = sample(c(NA, 0), 1, prob = c(1, 1)))) %>%
  map( ~ mutate(., id = 1:nrow(.))) %>%
  map( ~ mutate(., diff = id - nrow(.) / 2)) %>%
  bind_rows(.id = "group_id") %>%
  mutate(inertia = ifelse(is.na(inertia2), rtnorm(nrow(.), mean = 1, sd = 5, min = 1, max = 100), inertia2)) %>%
  ungroup() 

frayed <- 
  sand_split2 %>%
  filter(inertia != 0) %>%
  mutate(group_id2 = group_id) %>%
  group_by(group_id2) %>%
  group_map( ~ filter(., id %in% 1:sample(floor(1:max(id)/50), 1) | id %in% (max(id)-sample(floor(1:max(id)/50), 1)):max(id))) %>%
  bind_rows() %>%
  ungroup() %>%
  mutate(inertia2 = sample(c(0, 1), nrow(.), replace = TRUE, prob = c(1, 10)),
         inertia = ifelse(inertia2 == 0, 0, inertia))

sand_split3 <- 
  full_join(sand_split2, frayed, by = c("group_id", "id")) %>%
  mutate(inertia = ifelse(is.na(inertia.y), inertia.x, inertia.y)) %>%
  select(group_id, id, x = x.x, y = y.x, inertia, diff = diff.x)