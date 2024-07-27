library(tidyverse)
library(reshape2)
library(scico)

FN_network <- scan('.\\network\\network_FN\\sub-4.txt')
FN_network <- matrix(FN_network, ncol = 246, byrow = TRUE)

a <- melt(FN_network)
a <- melt(FN_network[nrow(FA_network):1, ]) # Reserve diagnosis

# Matrix plot
ggplot(a, aes(Var1, Var2)) +
  geom_raster(aes(fill = value), interpolate = F) +
  scale_fill_scico(palette = 'lapaz') +
  theme_classic() + # You can customize the theme as desired
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.text.y = element_blank(),  # Remove y-axis text
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank())
