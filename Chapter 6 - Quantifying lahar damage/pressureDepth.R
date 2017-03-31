#rm(list = ls())
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

#Different
depthPressure <- data.frame()
depth <- seq(0, 3.0, length = 50)
rho <- c(1000, 1500, 1915)
dyPressure <- seq(0, 2e4, length = 50)

#Constants
g = 9.81
height = 3.0

#####Functions#####
findBendingMoment <- function(bottomForce, topForce, hydroForce, dynForce, 
                              xDynamic, xHydro)
{
  # |========|=========|============|
  #Bot       B         C           Top
  #     d/3       d/2       l         
  
  #Moment A = 0
  #Moment B = Fa*dist - Fhydro*dist
  momB <- (bottomForce*xHydro)
  #Moment C = Fa*dist - Fhydro*dist
  momC <- (bottomForce*xDynamic) - (hydroForce*(xDynamic - xHydro))
  #At depth
  momD <- (bottomForce*(xDynamic*2)) - (hydroForce*(2*xHydro)) - (dynForce*xDynamic)
  return(c(momB, momC, momD))
}

plotVPressure <- function(bType, depthPressure, moments, bricks)
{
  if (bricks == 0.15)
  {
    m_mean <- select(filter(moments, class == bType), mean15)[1,1]
    m_min <- select(filter(moments, class == bType), min15)[1,1]
    m_max <- select(filter(moments, class == bType), max15)[1,1]
    lims <- c(0,1.25)
    bks <- c(0, 0.5, 1.0)
  } else if (bricks == 0.25) {
    m_mean <- select(filter(moments, class == bType), mean25)[1,1]
    m_min <- select(filter(moments, class == bType), min25)[1,1]
    m_max <- select(filter(moments, class == bType), max25)[1,1]
    lims <- c(0,2.0)
    bks <- c(0, 0.5, 1.0, 1.5, 2.0)
  }
  plot <- ggplot(depthPressure, aes(pressure*1e-3, depth)) + 
    coord_cartesian(ylim = lims, xlim = c(0,20)) +
    scale_y_continuous(expand = c(0,0), breaks = bks, minor_breaks = c(0.25, 0.75, 1.25)) +
    scale_x_continuous(expand = c(0,0)) +
    geom_contour(data = filter(depthPressure, rho == 1000), 
                 aes(z = max_mom/m_mean),
                 breaks = c(1), color = '#cccccc', size = 1, linetype = 1) + 
    geom_contour(data = filter(depthPressure, rho == 1000), 
                 aes(z = max_mom/m_min),
                 breaks = c(1), color = '#cccccc', size = 1, linetype = 3) +
    geom_contour(data = filter(depthPressure, rho == 1000), 
                 aes(z = max_mom/m_max),
                 breaks = c(1), color = '#cccccc', size = 1, linetype = 2) +
    #HCF
    geom_contour(data = filter(depthPressure, rho == 1500),
                 aes(z = max_mom/m_mean),
                 breaks = c(1), color = '#969696', size = 1, linetype = 1) +
    geom_contour(data = filter(depthPressure, rho == 1500), 
                 aes(z = max_mom/m_min),
                 breaks = c(1), color = '#969696', size = 1, linetype = 3) +
    geom_contour(data = filter(depthPressure, rho == 1500), 
                 aes(z = max_mom/m_max),
                 breaks = c(1), color = '#969696', size = 1, linetype = 2) +
    #DF
    geom_contour(data = filter(depthPressure, rho == 1915),
                 aes(z = max_mom/m_mean),
                 breaks = c(1), color = '#525252', size = 1, linetype = 1) +
    geom_contour(data = filter(depthPressure, rho == 1915), 
                 aes(z = max_mom/m_min),
                 breaks = c(1), color = '#525252', size = 1, linetype = 3) +
    geom_contour(data = filter(depthPressure, rho == 1915), 
                 aes(z = max_mom/m_max),
                 breaks = c(1), color = '#525252', size = 1, linetype = 2) +
    
    ylab("Depth (m)") +
    xlab("Dynamic pressure (kPa)") +
    theme_classic() +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
    )
  return(plot)
}

plotTypePressure <- function(fRho, depthPressure, moments, bricks)
{
  if (bricks == 0.15)
  {
    A0 <- select(filter(moments, class == 'A0'), class, contains("15")) %>%
      select(class, mean = starts_with("mean"), min = starts_with("min"), max = starts_with("max"))
    A <- select(filter(moments, class == 'A'), class, contains("15")) %>%
      select(class, mean = starts_with("mean"), min = starts_with("min"), max = starts_with("max"))
    B <- select(filter(moments, class == 'B'), class, contains("15")) %>%
      select(class, mean = starts_with("mean"), min = starts_with("min"), max = starts_with("max"))
    lims <- c(0,1.25)
  } else if (bricks == 0.25) {
    A0 <- select(filter(moments, class == 'A0'), class, contains("25"), class) %>%
      select(class, mean = starts_with("mean"), min = starts_with("min"), max = starts_with("max"))
    A <- select(filter(moments, class == 'A'), class, contains("25"), class) %>%
      select(class, mean = starts_with("mean"), min = starts_with("min"), max = starts_with("max"))
    B <- select(filter(moments, class == 'B'), class, contains("25"), class) %>%
      select(class, mean = starts_with("mean"), min = starts_with("min"), max = starts_with("max"))
    lims <- c(0,2.0)
  }
  sze = 0.5
  plot <- ggplot(depthPressure, aes(pressure*1e-3, depth)) + 
    coord_cartesian(ylim = lims, xlim = c(0,20)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    #A0
    geom_contour(data = filter(depthPressure, rho == fRho), 
                 aes(z = max_mom/A0$mean),
                 breaks = c(1), color = '#cccccc', size = sze, linetype = 1) + 
    geom_contour(data = filter(depthPressure, rho == fRho), 
                 aes(z = max_mom/A0$min),
                 breaks = c(1), color = '#cccccc', size = sze, linetype = 3) +
    geom_contour(data = filter(depthPressure, rho == fRho), 
                 aes(z = max_mom/A0$max),
                 breaks = c(1), color = '#cccccc', size = sze, linetype = 2) +
    #A
    geom_contour(data = filter(depthPressure, rho == fRho),
                 aes(z = max_mom/A$mean),
                 breaks = c(1), color = '#969696', size = sze, linetype = 1) +
    geom_contour(data = filter(depthPressure, rho == fRho), 
                 aes(z = max_mom/A$min),
                 breaks = c(1), color = '#969696', size = sze, linetype = 3) +
    geom_contour(data = filter(depthPressure, rho == fRho), 
                 aes(z = max_mom/A$max),
                 breaks = c(1), color = '#969696', size = sze, linetype = 2) +
    #B
    geom_contour(data = filter(depthPressure, rho == fRho),
                 aes(z = max_mom/B$mean),
                 breaks = c(1), color = '#525252', size = sze, linetype = 1) +
    geom_contour(data = filter(depthPressure, rho == fRho), 
                 aes(z = max_mom/B$min),
                 breaks = c(1), color = '#525252', size = sze, linetype = 3) +
    geom_contour(data = filter(depthPressure, rho == fRho), 
                 aes(z = max_mom/B$max),
                 breaks = c(1), color = '#525252', size = sze, linetype = 2) +
    
    ylab("Depth (m)") +
    xlab("Dynamic pressure (kPa)") +
    theme_classic() +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
    )
  return(plot)
}

#####
for (r in 1:length(rho))
{
  for (i in 1:length(depth))
    {
      hydroForce <- (rho[r]*g*(depth[i]^2))/2
      #Centre of action
      xHydro <- depth[i]/3
      xDynamic <- depth[i]/2
      for (j in 1:length(dyPressure))
      {
        dynForce <- depth[i]*dyPressure[j]
        #Balance of forces
        topForce <- ((xHydro*hydroForce) + (xDynamic*dynForce))/height
        bottomForce <- hydroForce + dynForce - topForce
        ###Determine bending moment
        moms <- findBendingMoment(bottomForce, topForce, hydroForce, dynForce,
                          xDynamic, xHydro)
        depthPressure <- rbind(depthPressure, data.frame(depth = depth[i], 
                                                         pressure = dyPressure[j],
                                                         rho = rho[r],
                                                         moms[1], moms[2], moms[3]))
      }
  }
}

find_mom_ratio <- function(flowtype, depth, dyPressure,
                           moments, typ, brickwidth){
  if (flowtype == 'FF')
  {
    rho = 1000
  } else if (flowtype == 'HCF')
  {
    rho = 1500
  } else if (flowtype == 'DF')
  {
    rho = 1915
  }
  hydroForce <- (rho*g*(depth^2))/2
  xHydro <- depth/3
  xDynamic <- depth/2
  dynForce <- depth*dyPressure
  #Balance of forces
  topForce <- ((xHydro*hydroForce) + (xDynamic*dynForce))/height
  bottomForce <- hydroForce + dynForce - topForce
  moms <- findBendingMoment(bottomForce, topForce, hydroForce, dynForce,
                            xDynamic, xHydro)
  if (brickwidth == 0.15)
  {
    ratio <- pmax(moms)/select(filter(moments, class == typ), mean15)
  } else if (brickwidth == 0.25)
  {
    ratio <- pmax(moms)/select(filter(moments, class == typ), mean25)
  }
  return(ratio)

}


depthPressure %>% select(moms.1., moms.2., moms.3.) %>% mutate(max_mom = do.call(pmax, (.))) %>%
  select(max_mom) %>% cbind(depthPressure) -> depthPressure
####0 hydro
depthnoHydro <- data.frame()
for (i in 1:length(depth))
{
  hydroForce <- 0
  #Centre of action
  xHydro <- depth[i]/3
  xDynamic <- depth[i]/2
  for (j in 1:length(dyPressure))
  {
    dynForce <- depth[i]*dyPressure[j]
    #Balance of forces
    topForce <- ((xHydro*hydroForce) + (xDynamic*dynForce))/height
    bottomForce <- hydroForce + dynForce - topForce
    ###Determine bending moment
    moms <- findBendingMoment(bottomForce, topForce, hydroForce, dynForce,
                              xDynamic, xHydro)
    depthnoHydro <- rbind(depthnoHydro, data.frame(depth = depth[i], 
                                                     pressure = dyPressure[j],
                                                     rho = rho[r],
                                                     moms[1], moms[2], moms[3]))
  }
}
depthnoHydro %>% select(moms.1., moms.2., moms.3.) %>% mutate(max_mom = do.call(pmax, (.))) %>%
  select(max_mom) %>% cbind(depthnoHydro) -> depthnoHydro


#####Get the plots  & allocate classes
span <- 1
type <- c('1A', '1B', '2A', '2B', '3', '4', '5', '6A', '6B', '6C')
ultimateMoments %>% mutate(class = ifelse(type == '1A' | type == '1B' | type == '2A' | type == '2B', 'A0',
                                          ifelse(type == '3' | type == '5', 'A',
                                                  'B'))) -> ultimateMoments
mean15 <- momUlt(ft, ultimateMoments %>% 
                   group_by(class) %>% 
                   filter(width == 0.15) %>% 
                   summarise(mean(designStress)) %>% 
                   select(-class), 
                 span,
                 0.15)
max15 <- momUlt(ft, ultimateMoments %>% 
                   group_by(class) %>% 
                   filter(width == 0.15) %>% 
                   summarise(max(designStress)) %>% 
                   select(-class), 
                 span,
                 0.15)
min15 <- momUlt(ft, ultimateMoments %>% 
                  group_by(class) %>% 
                  filter(width == 0.15) %>% 
                  summarise(min(designStress)) %>% 
                  select(-class), 
                span,
                0.15)
mean25 <- momUlt(ft, ultimateMoments %>% 
                   group_by(class) %>% 
                   filter(width == 0.25) %>% 
                   summarise(mean(designStress)) %>% 
                   select(-class), 
                 span,
                 0.25)
max25 <- momUlt(ft, ultimateMoments %>% 
                  group_by(class) %>% 
                  filter(width == 0.25) %>% 
                  summarise(max(designStress)) %>% 
                  select(-class), 
                span,
                0.25)
min25 <- momUlt(ft, ultimateMoments %>% 
                  group_by(class) %>% 
                  filter(width == 0.25) %>% 
                  summarise(min(designStress)) %>% 
                  select(-class), 
                span,
                0.25)
ultimateMoments %>% 
  group_by(class) %>% 
  filter(width == 0.25) %>% 
  summarise(min(designStress)) %>% select(class) -> class
moments <- data.frame(class,
                      mean15,
                      min15,
                      max15,
                      mean25,
                      min25,
                      max25)
colnames(moments) <- c('class', 'mean15', 'min15', 'max15', 'mean25', 'min25', 'max25')

cl <- c('A0', 'A', 'B')
plots1 <- lapply(as.vector(cl), plotVPressure,
            depthPressure = depthPressure,
            #depthPressure = depthnoHydro,
            moments = moments, bricks = 0.15)
plots2 <- lapply(as.vector(cl), plotVPressure,
                 depthPressure = depthPressure,
                 #depthPressure = depthnoHydro,
                moments = moments, bricks = 0.25)

top <- plot_grid(plotlist = plots1, 
          ncol = 3,
          labels = cl,
          label_size = 14,
          hjust = 0,
          vjust = 1)
bottom <- plot_grid(plotlist = plots2, 
                    ncol = 3,
                    labels = cl,
                    label_size = 14,
                    hjust = 0,
                    vjust = 1)

plot_grid(top, bottom, 
          nrow = 2)
ggsave("./building_class_criteria.svg")


######Plot pressure/depth####
wwp <- 0.15 #width plot
depthPlFF <- plotTypePressure(fRho = 1000, 
                              depthPressure = depthPressure,
                              moments = moments,
                              bricks = wwp)
depthPlHCF <- plotTypePressure(fRho = 1500, 
                              depthPressure = depthPressure,
                              moments = moments,
                              bricks = wwp)
depthPlDF <- plotTypePressure(fRho = 1915, 
                               depthPressure = depthPressure,
                               moments = moments,
                               bricks = wwp)
#Pressure
data2 %>% filter(time < 46) %>%
  mutate(depth = z_max - z_min) %>%
  group_by(flowrate) %>% 
  group_by(flowtype, add = TRUE) %>% 
  group_by(block, add = TRUE) %>%
  group_by(orientation, add = TRUE) %>%
  filter(pressureNormal_mean == max(pressureNormal_mean)) %>%
  distinct(.keep_all=TRUE) -> tempPressData

   

#
data2 %>% filter(time < 46) %>%
  mutate(depth = z_max - z_min) %>%
  group_by(flowrate) %>% 
  group_by(flowtype, add = TRUE) %>% 
  group_by(block, add = TRUE) %>%
  group_by(orientation, add = TRUE) %>%
  filter(depth == max(depth)) %>% 
  distinct(.keep_all=TRUE) -> tempHtData

#
tempPressData %>% mutate(depth = ifelse(depth > 2.0, 2.0, depth)) %>%
  mutate(pressureN = ifelse(pressureNormal_mean > 20*1e3, 20*1e3, pressureNormal_mean)) -> tempPressData
tempPressData$block <- factor(tempPressData$block, levels = c('8.12', '8.4', '9.3', '9.7', '9.11'))
tempPressData$flowrate <- factor(tempPressData$flowrate, levels = c('100', '75', '50', '25'))

#
tempHtData %>% mutate(depth = ifelse(depth > 2.0,2.0, depth)) %>%
  mutate(pressureN = ifelse(pressureNormal_mean > 20*1e3, 20*1e3, pressureNormal_mean)) -> tempHtData
tempHtData$block <- factor(tempHtData$block, levels = c('8.12', '8.4', '9.3', '9.7', '9.11'))
tempHtData$flowrate <- factor(tempHtData$flowrate, levels = c('100', '75', '50', '25'))


pressurePlFF <- depthPlFF +
                geom_point(data = filter(tempPressData, flowtype == "FF"), mapping = aes(x = pressureN*1e-3, #tempPressData
                                                                                         y = depth,
                                                                                         shape = flowrate
                                                                                         ), size = 3) +
  scale_shape(name = "Flow rate", solid = FALSE) +
  facet_grid(block~orientation, 
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2",
                                           "9.11" = "East 3")
                                 
             )) +
  scale_y_continuous(labels = c("0.0", "0.4", "0.8", ">1.2")) +
  #scale_y_continuous(labels = c("0.0", "0.5", "1.0", "1.5", ">2.0")) +
  scale_x_continuous(labels = c("0", "5", "10", "15", ">20")) +
  theme_bw()

pressurePlHCF <- depthPlHCF +
  geom_point(data = filter(tempPressData, flowtype == "HCF"), mapping = aes(x = pressureN*1e-3, #tempPressData
                                                                            y = depth,
                                                                            shape = flowrate
                                                                            ), size = 3) +
  scale_shape(name = "Flow rate", solid = FALSE) +
  facet_grid(block~orientation, 
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2",
                                           "9.11" = "East 3")
                                 
             )) +
  scale_y_continuous(labels = c("0.0", "0.4", "0.8", ">1.2")) +
  #scale_y_continuous(labels = c("0.0", "0.5", "1.0", "1.5", ">2.0")) +
  scale_x_continuous(labels = c("0", "5", "10", "15", ">20")) +
  theme_bw()

pressurePlDF <- depthPlDF +
  geom_point(data = filter(tempPressData, flowtype == "DF"), mapping = aes(x = pressureN*1e-3, #tempPressData
                                                                           y = depth,
                                                                           shape = flowrate
                                                                           ), size = 3) +
  scale_shape(name = "Flow rate", solid = FALSE) +
  facet_grid(block~orientation, 
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2",
                                           "9.11" = "East 3")
                                 
             )) +
  scale_y_continuous(labels = c("0.0", "0.4", "0.8", ">1.2")) +
  #scale_y_continuous(labels = c("0.0", "0.5", "1.0", "1.5", ">2.0")) +
  scale_x_continuous(labels = c("0", "5", "10", "15", ">20")) +
  theme_bw()
#ggsave(plot = pressurePlFF, "C:/Users/smead/Dropbox/PhD Stuart - current documents/Modelling/Fragility/images/revision/press/FF_buildings_015.svg", width = 270, height = 220, units = "mm")
ggsave(plot = pressurePlFF,
       "./FF_buildings_015.svg")
ggsave(plot = pressurePlHCF,
       "./HCF_buildings_015.svg")
ggsave(plot = pressurePlDF,
       "./DF_buildings_015.svg")

######Depth#####
pressureDFF <- depthPlFF +
  geom_point(data = filter(tempHtData, flowtype == "FF"), mapping = aes(x = pressureN*1e-3, #tempPressData
                                                                        y = depth,
                                                                        shape = flowrate
  ), size = 3) +
  scale_shape(name = "Flow rate", solid = FALSE) +
  facet_grid(block~orientation, 
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2",
                                           "9.11" = "East 3")
                                 
             )) +
  scale_y_continuous() +
  scale_x_continuous() +
  theme_bw()

pressureDHCF <- depthPlHCF +
  geom_point(data = filter(tempHtData, flowtype == "HCF"), mapping = aes(x = pressureN*1e-3, #tempPressData
                                                                         y = depth,
                                                                         shape = flowrate
  ), size = 3) +
  scale_shape(name = "Flow rate", solid = FALSE) +
  facet_grid(block~orientation, 
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2",
                                           "9.11" = "East 3")
                                 
             )) +
  scale_y_continuous() +
  scale_x_continuous() +
  theme_bw()

pressureDDF <- depthPlDF +
  geom_point(data = filter(tempHtData, flowtype == "DF"), mapping = aes(x = pressureN*1e-3, #tempPressData
                                                                        y = depth,
                                                                        shape = flowrate
  ), size = 3) +
  scale_shape(name = "Flow rate", solid = FALSE) +
  facet_grid(block~orientation, 
             labeller = labeller(orientation = c(par = "Parallel",
                                                 perp = "Perpendicular"),
                                 block = c("8.12" = "West 1",
                                           "8.4" = "West 2",
                                           "9.3" = "East 1",
                                           "9.7" = "East 2",
                                           "9.11" = "East 3")
                                 
             )) +
  scale_y_continuous() +
  scale_x_continuous() +
  theme_bw()

ggsave(plot = pressureDFF,
       "./FF_buildings-maxdepth_025.png")
ggsave(plot = pressureDHCF,
       "./HCF_buildings-maxdepth_025.png")
ggsave(plot = pressureDDF,
       "./DF_buildings-maxdepth_025.png")

#####Losses#####
tempPressData %>% 
  mutate(A0_25 = ifelse(flowtype == 'FF', find_mom_ratio('FF', 
                                                         depth,
                                                         pressureNormal_mean,
                                                         moments,
                                                         'A0',
                                                         0.25),
         ifelse(flowtype == 'HCF', find_mom_ratio('HCF', 
                                                 depth,
                                                 pressureNormal_mean,
                                                 moments,
                                                 'A0',
                                                 0.25),
                find_mom_ratio('DF', 
                               depth,
                               pressureNormal_mean,
                               moments,
                               'A0',
                               0.25)
                )
         )
         ) %>%
  mutate(A0_15 = ifelse(flowtype == 'FF', find_mom_ratio('FF', 
                                                         depth,
                                                         pressureNormal_mean,
                                                         moments,
                                                         'A0',
                                                         0.15),
                        ifelse(flowtype == 'HCF', find_mom_ratio('HCF', 
                                                                 depth,
                                                                 pressureNormal_mean,
                                                                 moments,
                                                                 'A0',
                                                                 0.15),
                               find_mom_ratio('DF', 
                                              depth,
                                              pressureNormal_mean,
                                              moments,
                                              'A0',
                                              0.15)
                        )
  )
  ) %>%
  mutate(A_25 = ifelse(flowtype == 'FF', find_mom_ratio('FF', 
                                                         depth,
                                                         pressureNormal_mean,
                                                         moments,
                                                         'A',
                                                         0.25),
                        ifelse(flowtype == 'HCF', find_mom_ratio('HCF', 
                                                                 depth,
                                                                 pressureNormal_mean,
                                                                 moments,
                                                                 'A',
                                                                 0.25),
                               find_mom_ratio('DF', 
                                              depth,
                                              pressureNormal_mean,
                                              moments,
                                              'A',
                                              0.25)
                        )
  )
  ) %>%
  mutate(A_15 = ifelse(flowtype == 'FF', find_mom_ratio('FF', 
                                                        depth,
                                                        pressureNormal_mean,
                                                        moments,
                                                        'A',
                                                        0.15),
                       ifelse(flowtype == 'HCF', find_mom_ratio('HCF', 
                                                                depth,
                                                                pressureNormal_mean,
                                                                moments,
                                                                'A',
                                                                0.15),
                              find_mom_ratio('DF', 
                                             depth,
                                             pressureNormal_mean,
                                             moments,
                                             'A',
                                             0.15)
                       )
  )
  ) %>%
  mutate(B_25 = ifelse(flowtype == 'FF', find_mom_ratio('FF', 
                                                        depth,
                                                        pressureNormal_mean,
                                                        moments,
                                                        'B',
                                                        0.25),
                       ifelse(flowtype == 'HCF', find_mom_ratio('HCF', 
                                                                depth,
                                                                pressureNormal_mean,
                                                                moments,
                                                                'B',
                                                                0.25),
                              find_mom_ratio('DF', 
                                             depth,
                                             pressureNormal_mean,
                                             moments,
                                             'B',
                                             0.25)
                       )
  )
  ) %>%
  mutate(B_15 = ifelse(flowtype == 'FF', find_mom_ratio('FF', 
                                                        depth,
                                                        pressureNormal_mean,
                                                        moments,
                                                        'B',
                                                        0.15),
                       ifelse(flowtype == 'HCF', find_mom_ratio('HCF', 
                                                                depth,
                                                                pressureNormal_mean,
                                                                moments,
                                                                'B',
                                                                0.15),
                              find_mom_ratio('DF', 
                                             depth,
                                             pressureNormal_mean,
                                             moments,
                                             'B',
                                             0.15)
                       )
  )
  ) %>%
  select(-(z_min:vy_sd)) -> lossData

mutate_each(lossData, funs(as.numeric), A0_25:B_15) %>%
  select(-time, -theta, -pressureNormal_mean, -pressureNormal_max, -depth) %>%
  group_by(block) %>%
  group_by(flowtype, add = TRUE) %>%
  group_by(flowrate, add = TRUE) %>%
  summarise(A0_25 = max(A0_25), 
            A_25 = max(A_25),
            B_25 = max(B_25),
            A0_15 = max(A0_15), 
            A_15 = max(A_15),
            B_15 = max(B_15)) -> lossPlt
  
lossPlt <- mutate_each(lossPlt, funs(between(.,1, Inf)), A0_25:B_15)

building_count <- data_frame(block = c('8.12', '8.4', '9.3', '9.7', '9.11'),
                             A0_25 = c(3, 0, 2, 3, 0),
                             A_25 = c(2, 1, 1, 1, 2),
                             B_25 = c(2, 4, 0, 0, 1),
                             A0_15 = c(3, 0, 2, 3, 0),
                             A_15 = c(2, 1, 1, 1, 2),
                             B_15 = c(2, 4, 0, 0, 1))

llPlot <- data_frame(class = rep(c('A0', 'A', 'B'), 6), 
            flowtype = rep(c(rep('NF', 3), rep('HCF', 3), rep('DF', 3)), 2), 
            width = c(rep(0.25, 9), rep(0.15, 9)), 
            '25' = 0,
            '50' = 0,
            '75' = 0,
            '100' = 0)

lossPlt %>% ungroup() %>%
  group_by(flowtype) %>%
  group_by(flowrate, add = TRUE) %>%
  inner_join(building_count, by = 'block') %>%
  transmute(A0_25 = A0_25.x * A0_25.y,
           A_25 = A_25.x * A_25.y,
           B_25 = B_25.x * B_25.y,
           A0_15 = A0_15.x * A0_15.y,
           A_15 = A_15.x * A_15.y,
           B_15 = B_15.x * B_15.y) -> lossPlt

lossPlt %>% summarise_each(funs(sum), A0_25:B_15) %>%
      mutate(A0_25 = A0_25/8,
            A_25 = A_25/7,
            B_25 = B_25/7,
            A0_15 = A0_15/8,
            A_15 = A_15/7,
            B_15 = B_15/7) -> lossPlt2

library(tidyr)

lossPlt2 <- tidyr::gather(lossPlt2, key = class, value = number, A0_25:B_15)

lossPlt2$flowrate <- factor(lossPlt2$flowrate, levels = c(100, 75, 50, 25))

lossDF <- ggplot(filter(lossPlt2, flowtype == 'DF' & grepl("_25", class))) +
  geom_bar(aes(x = flowrate, y = number, fill = class), stat = "identity", position = "dodge") +
  scale_fill_grey(name = "Class", labels = c("A0", "A", "B")) +
  theme_classic() +
  xlab("Flow rate") +
  ylab("Loss fraction") + 
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
  )

lossFF <- ggplot(filter(lossPlt2, flowtype == 'FF' & grepl("_25", class))) +
  geom_bar(aes(x = flowrate, y = number, fill = class), stat = "identity", position = "dodge") +
  scale_fill_grey(name = "Class", labels = c("A0", "A", "B")) +
  theme_classic() +
  xlab("Flow rate") +
  ylab("Loss fraction") + 
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
  )


lossHCF <- ggplot(filter(lossPlt2, flowtype == 'HCF' & grepl("_25", class))) +
  geom_bar(aes(x = flowrate, y = number, fill = class), stat = "identity", position = "dodge") +
  scale_fill_grey(name = "Class", labels = c("A0", "A", "B")) +
  theme_classic() +
  xlab("Flow rate") +
  ylab("Loss fraction") + 
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
  )

ggsave(file = "./25cm_loss.png",
       plot_grid(lossFF, lossHCF, lossDF, 
                    ncol = 3,
                    labels = c("NF", "HCF", "DF"),
                    label_size = 14,
                    hjust = 0.1,
                    vjust = 1.0),
       width = 300, height = 80, units = "mm")

lossDF <- ggplot(filter(lossPlt2, flowtype == 'DF' & grepl("_15", class))) +
  geom_bar(aes(x = flowrate, y = number, fill = class), stat = "identity", position = "dodge") +
  scale_fill_grey(name = "Class", labels = c("A0", "A", "B")) +
  theme_classic() +
  xlab("Flow rate") +
  ylab("Loss fraction") + 
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
  )

lossFF <- ggplot(filter(lossPlt2, flowtype == 'FF' & grepl("_15", class))) +
  geom_bar(aes(x = flowrate, y = number, fill = class), stat = "identity", position = "dodge") +
  scale_fill_grey(name = "Class", labels = c("A0", "A", "B")) +
  theme_classic() +
  xlab("Flow rate") +
  ylab("Loss fraction") + 
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
  )


lossHCF <- ggplot(filter(lossPlt2, flowtype == 'HCF' & grepl("_15", class))) +
  geom_bar(aes(x = flowrate, y = number, fill = class), stat = "identity", position = "dodge") +
  scale_fill_grey(name = "Class", labels = c("A0", "A", "B")) +
  theme_classic() +
  xlab("Flow rate") +
  ylab("Loss fraction") + 
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
  )

ggsave(file = "./15cm_loss.png",
       plot_grid(lossFF, lossHCF, lossDF, 
                 ncol = 3,
                 labels = c("NF", "HCF", "DF"),
                 label_size = 14,
                 hjust = 0.1,
                 vjust = 1.0),
       width = 300, height = 80, units = "mm")  

