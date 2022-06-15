# https://github.com/GuangchuangYu/hexSticker
library(hexSticker)
library(ggplot2)


set.seed(1)
df <- data.frame(x = rnorm(200), y = rnorm(200))

p<-ggplot(df, aes(x = x, y = y)) +
  geom_density_2d(aes(color = ..level..), show.legend = FALSE) +
  scale_color_viridis_c()+
  theme_void()+
  theme_transparent()
p

s<-sticker(p, 
           package = "raermod", 
           p_color = "#025301",
           p_size = 20, 
           p_x = 1,
           p_y = 1.5,
           s_x = 1,
           s_y = 0.7, 
           s_width = 1, 
           s_height = 1,
           spotlight = FALSE,
           l_width = 1.1,
           l_height = 1.1,
           l_alpha = 0.6,
           l_x = 0.48,
           l_y = 1.0,
           h_fill = "white",
           h_color = "#025301",
           filename="./raermod.png")

plot(s)
