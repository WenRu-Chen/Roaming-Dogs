library(ggplot2)
library(dplyr)

x = runif(10000, min = 0, max = 8)

H_df = data.frame(x = x,
          y_0.5 = exp(-x^2/(2*.5)),
           y_1 = exp(-x^2/(2*1)),
           y_1.5 = exp(-x^2/(2*1.5)),
           y_2 = exp(-x^2/(2*2))
           )

H_df_2 = reshape2::melt(H_df, id = "x")

ggplot(H_df_2, aes(x = x, y = value, group = variable, color = variable))+
  geom_line()
