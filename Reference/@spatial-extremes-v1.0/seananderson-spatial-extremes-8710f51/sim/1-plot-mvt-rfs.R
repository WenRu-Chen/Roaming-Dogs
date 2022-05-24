library(dplyr)
library(ggplot2)
library(ggsidekick) # devtools::install_github("seananderson/ggsidekick")
library(glmmfields)

g <- expand.grid(lon = seq(1, 50, 0.5),  #所有的組合都來一遍
  lat = seq(1, 50, 0.5))

exp_cor <- function(delta_ij, phi) {
  exp(-phi * delta_ij)
}

draws <- lapply(c(2, 1e9),
  function(x) {
    g <- expand.grid(lon = seq(1, 10, length.out = 50),
      lat = seq(1, 10, length.out = 25))
    draws <- 3
    s <- glmmfields::sim_glmmfields(df = x, n_draws = draws,
      gp_theta = 1.6, gp_sigma = 0.3, n_knots = 30, seed = 9,
      obs_error =  "poisson",
      g = g, n_data_points = nrow(g))
    out <- reshape2::melt(s$proj)
    names(out) <- c("i", "pt", "re")
    out <- arrange(out, i, pt)
    out$nu <- x
    out$lon <- rep(g$lon, draws)
    out$lat <- rep(g$lat, draws)
    out
}) %>% bind_rows()

draws <- mutate(draws, i = paste("Draw", i))

labels <- tibble::tibble(
  nu = c(2, 1e9),
  nu_lab = c("MVT, v = 2", "MVN")
)

draws <- inner_join(draws, labels, by = "nu")

# dcast.df <- reshape2::dcast(draws, i + pt + lon+ lat + nu_lab ~ nu, value.var="re")

p <- draws %>%
  ggplot(aes(x = lon, y = lat, z = re, fill = re)) +
  facet_grid(nu_lab~i) +
  geom_raster() +
  viridis::scale_fill_viridis(option = "C") +
  theme_sleek() +
  theme(axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank()) +
  theme(panel.spacing = unit(-0.15, "lines"))

p

p_pdf = ggplot(draws[draws$i == "Draw 1",])+
  geom_histogram(aes(x = re, y = ..density.., group = nu_lab))
  
p_pdf  
  
ggsave("figs/nu-rf-illustration-small.pdf", width = 4.5, height = 3)

#-----------------

# Test 

# 結論：
# (1) Poisson 來說，gp_sigma 越大，y 與 "proj"(我還是不知道這個是甚麼) 會分布越廣。
# (2) Poisson 來說，gp_theta 越大，proj 因距離產生的關聯性越顯著（影響範圍較廣）。

g <- expand.grid(lon = seq(1, 10, length.out = 25),
                 lat = seq(1, 10, length.out = 25))

s_poisson_1 <- glmmfields::sim_glmmfields(df = 2, n_draws = 1,
                                gp_theta = 1.6, gp_sigma = .6, n_knots = 30, seed = 9,
                                obs_error =  "poisson",
                                g = g, n_data_points = nrow(g))

s_poisson_2 <- glmmfields::sim_glmmfields(df = 2, n_draws = 1,
                                        gp_theta = 1.6, gp_sigma = 0.3, n_knots = 30, seed = 9,
                                        obs_error =  "poisson",
                                        g = g, n_data_points = nrow(g))

# s_normal <- glmmfields::sim_glmmfields(df = 2, n_draws = 1,
#                                         gp_theta = 1.6, gp_sigma = 0.3, n_knots = 30, seed = 9,
#                                         obs_error =  "normal",
#                                         g = g, n_data_points = nrow(g))

# plot ####

# x_1 =  s_poisson_1[["dat"]][["y"]]; x_2 =  s_poisson_2[["dat"]][["y"]]
# mean(x_1); var(x_1);mean(x_2); var(x_2)

x_1 =  as.vector(s_poisson_1[["proj"]] %>% t); x_2 =  s_poisson_2[["proj"]] %>% t %>% as.vector
mean(x_1); var(x_1);mean(x_2); var(x_2)

p_pdf_01 = ggplot()+
  geom_histogram(aes(x = x_1, y = ..density..))

p_pdf_02 = ggplot()+
  geom_histogram(aes(x =x_2, y = ..density..))

gridExtra::grid.arrange(p_pdf_01,p_pdf_02, ncol = 2)


p_point_1 = ggplot()+
  geom_point(aes(x = s_poisson_1[["g"]][["lon"]], y = s_poisson_1[["g"]][["lat"]], color = x_1 ))

p_point_2 = ggplot()+
  geom_point(aes(x = s_poisson_2[["g"]][["lon"]], y = s_poisson_2[["g"]][["lat"]], color = x_2 ))

gridExtra::grid.arrange(p_point_1,p_point_2, ncol = 2)

out <- reshape2::melt(s_poisson_1.6$proj)
names(out) <- c("i", "pt", "re")
