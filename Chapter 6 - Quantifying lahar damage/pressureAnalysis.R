#rm(list=ls())
#Function
library(dplyr)
library(ggplot2)

deg2rad <- function(deg) {(deg * pi) / (180)}

giveTheta <- function(block, orientation){
  if (block == '9.11'){
    if (orientation == 'par')
    {
      theta = -9.231
    }
    else
    {
      theta = -3.798
    }
  }  
  if (block == '9.7'){
    if (orientation == 'par')
    {
      theta = -5.246
    }
    else
    {
      theta = -4.729
    }
  }
  if (block == '9.3'){
      theta = -3.914
  }
  if (block == '8.12'){
      theta = -6.86
  }
  if (block == '8.4'){
    if (orientation == 'par')
    {
      theta = -6.771
    }
    else
    {
      theta = -3.271
    }
  }
  return(theta)
}

normalForce <- function(orientation, theta, px, py)
{
  if (orientation == 'par'){
    normP = cos(deg2rad(theta))*px - sin(deg2rad(theta))*py
  }
  if (orientation == 'perp'){
    normP = sin(deg2rad(theta))*px + cos(deg2rad(theta))*py
  }
  return(normP)
}

load(file = "C:/Dev/laharthesis-sources/Chapter 6 - Quantifying lahar damage/dahliaPressData_z.RData")

#Filter by orientation
filter(dahliaData, flowrate != '100') %>%
  mutate(flowrate = replace(flowrate, flowrate == '100_v2', '100') ) %>%
  group_by(orientation, block) %>% 
  mutate(theta = giveTheta(block, orientation)) -> data2 

data2 %>% 
  mutate(pressureNormal_mean = normalForce(orientation, theta, px_mean, py_mean)) %>%
  mutate(pressureNormal_max = normalForce(orientation, theta, px_max, py_max)) -> data2

#####Flowrates#####
aes_max = aes(time, pressureNormal_max*1e-3)
aes_mean = aes(time, pressureNormal_mean*1e-3)
#aes_mag_mean = aes(time, pmag_mean*1e-3)
map = aes_mean
#map = aes_mag_mean
fname = "_mean"

data2$block <- factor(data2$block, levels = c('8.12', '8.4', '9.3', '9.7', '9.11'))

#####Smoothed#####
plot100 <- ggplot(filter(data2, flowrate == '100'), mapping = map) +
  geom_smooth(aes(linetype = flowtype), color = 'black', se = FALSE) + 
  scale_x_continuous(limits = c(0, 45)) + 
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(block~orientation, scales = "free",
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.11" = "East 3",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2")
                                 
             )) +
  ggtitle(expression(100~m^3/s)) +
  xlab("Time") +
  ylab("Dynamic Pressure (kPa)") +
  scale_linetype_manual(name = "Flow type", labels = c("DF", "NF", "HCF"),
                        values = c("DF" = 1, "FF" = 2, "HCF" = 3)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))
ggsave(paste("100cms", fname, ".svg", sep = ''))

plot75 <- ggplot(filter(data2, flowrate == '75'), mapping = map) +
  geom_smooth(aes(linetype = flowtype), color = 'black', se = FALSE) + 
  scale_x_continuous(limits = c(0, 45)) + 
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(block~orientation, scales = "free",
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.11" = "East 3",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2")
                                 
             )) +
  ggtitle(expression(75~m^3/s)) +
  xlab("Time") +
  ylab("Dynamic Pressure (kPa)") +
  scale_linetype_manual(name = "Flow type", labels = c("DF", "NF", "HCF"),
                        values = c("DF" = 1, "FF" = 2, "HCF" = 3)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))
ggsave(paste("75cms", fname, ".svg", sep = ''))

plot50 <- ggplot(filter(data2, flowrate == '50'), mapping = map) +  
  geom_smooth(aes(linetype = flowtype), color = 'black', se = FALSE) + 
  scale_x_continuous(limits = c(0, 45)) + 
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(block~orientation, scales = "free",
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.11" = "East 3",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2")
                                 
             )) +
  ggtitle(expression(50~m^3/s)) +
  xlab("Time") +
  ylab("Dynamic Pressure (kPa)") +
  scale_linetype_manual(name = "Flow type", labels = c("DF", "NF", "HCF"),
                        values = c("DF" = 1, "FF" = 2, "HCF" = 3)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))
ggsave(paste("50cms", fname, ".svg", sep = ''))

plot25 <- ggplot(filter(data2, flowrate == '25'), mapping = map) +
  geom_smooth(aes(linetype = flowtype), color = 'black', se = FALSE) + 
  scale_x_continuous(limits = c(0, 45)) + 
  scale_y_continuous(limits = c(0, NA)) +
  facet_grid(block~orientation, scales = "free",
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.11" = "East 3",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2")
               
             )) +
  ggtitle(expression(25~m^3/s)) +
  xlab("Time") +
  ylab("Dynamic Pressure (kPa)") +
  scale_linetype_manual(name = "Flow type", labels = c("DF", "NF", "HCF"),
                        values = c("DF" = 1, "FF" = 2, "HCF" = 3)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))
ggsave(paste("25cms", fname, ".svg", sep = ''))

#####Directional pressure plot
plotComp <- ggplot(filter(data2, flowrate == '75' & block == 9.7)) +
  scale_x_continuous(limits = c(0, 45)) + 
  scale_y_continuous(limits = c(0, NA)) +
  geom_smooth(aes(time, pmag_mean*1e-3, linetype = flowtype), color = 'gray', se = FALSE) +
  geom_smooth(aes(time, pressureNormal_mean*1e-3, linetype = flowtype), color = 'black', se = FALSE)  +
  facet_grid(orientation~., scales = "free",
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"))) +
  xlab("Time") +
  ylab("Dynamic Pressure (kPa)") +
  scale_linetype_manual(name = "Flow type", labels = c("DF", "NF", "HCF"),
                          values = c("DF" = 1, "FF" = 2, "HCF" = 3)) +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))
ggsave("direction_pressure.svg")
