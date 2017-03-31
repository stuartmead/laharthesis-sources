#Boxplot of design stress
library(dplyr)
library(ggplot2)
plot <- ggplot(ultimateMoments, aes(factor(type), designStress*1e-3)) + 
  geom_line(aes(size=factor(width), alpha = factor(width))) + 
  geom_point(data = filter(ultimateMoments, 
                           type == '1A' | type == '1B' | 
                             type == '2B' | type == '5'),
             aes(alpha = factor(width)),
             size = 4) +
  geom_point(data = filter(ultimateMoments, 
                           type == '2A' & width == 0.15),
             col = 'black', size = 4) +
  scale_size_discrete(range = c(3, 6)) +
  scale_alpha_manual(values = c(1,0.2), guide = FALSE) +
  xlab("Building type") +
  ylab("Design compressive stress (kPa)") +
  theme_classic() +
  labs(size = "Brick width (m)") +
  #guides(alpha = guide_legend()) +
  theme(legend.position = "bottom",
        text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16)
  )
plot
ggsave("./ds_box.svg")

#Moment & shear plot
moments <- data.frame(length = walls, 
                      mean15 = momUlt(ft, ds, walls, 0.15),
                      max15 = momUlt(ft, ds_max, walls, 0.15),
                      min15 = momUlt(ft, ds_min, walls, 0.15),
                      mean25 = momUlt(ft, ds2, walls, 0.25),
                      max25 = momUlt(ft, ds2_max, walls, 0.25),
                      min25 = momUlt(ft, ds2_min, walls, 0.25)
                      )
shear <- data.frame(length = walls, 
                    mean15 = shearUlt(dshear, walls, 0.15),
                    max15 = shearUlt(dshear_max, walls, 0.15),
                    min15 = shearUlt(dshear_min, walls, 0.15),
                    mean25 = shearUlt(dshear, walls, 0.25),
                    max25 = shearUlt(dshear_max, walls, 0.25),
                    min25 = shearUlt(dshear_min, walls, 0.25)
)
plot <- ggplot(moments) +
  geom_line(aes(walls, mean15*1e-3), linetype = 1, color = '#0571b0', size = 1) +
  geom_line(aes(walls, max15*1e-3), linetype = 2, color = '#0571b0', size = 1) +
  geom_line(aes(walls, min15*1e-3), linetype = 3, color = '#0571b0', size = 1) +
  geom_line(aes(walls, mean25*1e-3), linetype = 1, color = "#ca0020", size = 1) +
  geom_line(aes(walls, max25*1e-3), linetype = 2, color = "#ca0020", size = 1) +
  geom_line(aes(walls, min25*1e-3), linetype = 3, color = "#ca0020", size = 1) +
  scale_color_manual("", breaks = c("Moment", "Shear"), values = c('#92c5de','#0571b0' )) +
  theme_classic() +
  xlab("Wall length (m)") +
  ylab("Applied force (kN)") +
  theme(legend.position = "bottom",
        text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16)
  )
plot
ggsave("./building_strength.png")
#############



