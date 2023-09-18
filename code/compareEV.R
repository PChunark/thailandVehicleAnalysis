# Comparing the current cumulative EV  with forecast EV

library(gtable)
lsdata <- ls(plotdata)

# 1. Sedan ####
data <-
  
full_join(
plotdata[["thaSedanBEV"]] %>% 
  filter(year >= 2563),

plotdata[["forecastSedanBEV"]] %>% 
  filter(year >= 2563 & year <= 2566),

by = c("thaBEV" = "thaBEV",
       "year" = "year")
) %>% 
  select(year, thaBEV, "current" = vehicle.x, "forecast" = vehicle.y) 

plot<-
data %>% 
  ggplot()+
  geom_point(aes(x = current, y = forecast, group = year, color = year),
             shape = 16,
             size = 2)+
  geom_abline(slope = 1)+
  facet_wrap(~thaBEV)+
  ThemeLine+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = seq(0,180000,20000),
                     limits = c(0,180000),
                     labels = comma)+
  scale_y_continuous(breaks = seq(0,180000,20000),
                     limits = c(0,180000),
                     labels = comma)+
  scale_color_manual(name = NULL,
                     values = linepalette1)+
  labs(x = "ค่าจริงจำนวนรถ BEV สะสม (คัน)",
       y = "ค่าพยากรณ์จำนวนรถ BEV สะสม (คัน)")


plotdata <- c(plotdata, list("compareSedanBEV" = data))
figures <- c(figures, list("cumCompareSedanBEV" = plot))

# 2. Pick up ####
data <-
  
  full_join(
    plotdata[["thaPickupBEV"]] %>% 
      filter(year >= 2563),
    
    plotdata[["forecastPickupBEV"]] %>% 
      filter(year >= 2563 & year <= 2566),
    
    by = c("thaBEV" = "thaBEV",
           "year" = "year")
  ) %>% 
  select(year, thaBEV, "current" = vehicle.x, "forecast" = vehicle.y) 

plot<-
  data %>% 
  ggplot()+
  geom_point(aes(x = current, y = forecast, group = year, color = year),
             shape = 16,
             size = 2)+
  geom_abline(slope = 1)+
  facet_wrap(~thaBEV)+
  ThemeLine+
  theme(legend.position = c(0.9, 0.3))+
  scale_x_continuous(breaks = seq(0,72000,9000),
                     limits = c(0,72000),
                     labels = comma)+
  scale_y_continuous(breaks = seq(0,72000,9000),
                     limits = c(0,72000),
                     labels = comma)+
  scale_color_manual(name = NULL,
                     values = linepalette1)+
  labs(x = "ค่าจริงจำนวนรถ BEV สะสม (คัน)",
       y = "ค่าพยากรณ์จำนวนรถ BEV สะสม (คัน)")


plotdata <- c(plotdata, list("comparePickupBEV" = data))
figures <- c(figures, list("cumComparePickupBEV" = plot))

# 3. two three wheeler ####
data <-
  
  full_join(
    plotdata[["tha2-3wheelerBEV"]] %>% 
      filter(year >= 2563),
    
    plotdata[["forecast2-3wheelerBEV"]] %>% 
      filter(year >= 2563 & year <= 2566),
    
    by = c("thaBEV" = "thaBEV",
           "year" = "year")
  ) %>% 
  select(year, thaBEV, "current" = vehicle.x, "forecast" = vehicle.y) 

plot<-
  data %>% 
  ggplot()+
  geom_point(aes(x = current, y = forecast, group = year, color = year),
             shape = 16,
             size = 2)+
  geom_abline(slope = 1)+
  facet_wrap(~thaBEV)+
  ThemeLine+
  theme(legend.position = c(0.9, 0.3))+
  scale_x_continuous(breaks = seq(0,400000,50000),
                     limits = c(0,400000),
                     labels = comma)+
  scale_y_continuous(breaks = seq(0,400000,50000),
                     limits = c(0,400000),
                     labels = comma)+
  scale_color_manual(name = NULL,
                     values = linepalette1)+
  labs(x = "ค่าจริงจำนวนรถ BEV สะสม (คัน)",
       y = "ค่าพยากรณ์จำนวนรถ BEV สะสม (คัน)")


plotdata <- c(plotdata, list("compare2-3wheelerBEV" = data))
figures <- c(figures, list("cumCompare2-3wheelerBEV" = plot))

# 4. Truck wheeler ####
data <-
  
  full_join(
    plotdata[["truckBEV"]] %>% 
      filter(year >= 2563),
    
    plotdata[["forecastTruckBEV"]] %>% 
      filter(year >= 2563 & year <= 2566),
    
    by = c("thaBEV" = "thaBEV",
           "year" = "year")
  ) %>% 
  select(year, thaBEV, "current" = vehicle.x, "forecast" = vehicle.y) 

plot<-
  data %>% 
  ggplot()+
  geom_point(aes(x = current, y = forecast, group = year, color = year),
             shape = 16,
             size = 2)+
  geom_abline(slope = 1)+
  facet_wrap(~thaBEV)+
  ThemeLine+
  theme(legend.position = c(0.9, 0.3))+
  scale_x_continuous(breaks = seq(0,16000,2000),
                     limits = c(0,16000),
                     labels = comma)+
  scale_y_continuous(breaks = seq(0,16000,2000),
                     limits = c(0,16000),
                     labels = comma)+
  scale_color_manual(name = NULL,
                     values = linepalette1)+
  labs(x = "ค่าจริงจำนวนรถ BEV สะสม (คัน)",
       y = "ค่าพยากรณ์จำนวนรถ BEV สะสม (คัน)")


plotdata <- c(plotdata, list("compareTruckBEV" = data))
figures <- c(figures, list("cumCompareTruckBEV" = plot))

# 5. Bus wheeler ####
data <-
  
  full_join(
    plotdata[["busBEV"]] %>% 
      filter(year >= 2563),
    
    plotdata[["forecastBusBEV"]] %>% 
      filter(year >= 2563 & year <= 2566),
    
    by = c("thaBEV" = "thaBEV",
           "year" = "year")
  ) %>% 
  select(year, thaBEV, "current" = vehicle.x, "forecast" = vehicle.y) 

plot<-
  data %>% 
  ggplot()+
  geom_point(aes(x = current, y = forecast, group = year, color = year),
             shape = 16,
             size = 2)+
  geom_abline(slope = 1)+
  facet_wrap(~thaBEV)+
  ThemeLine+
  theme(legend.position = c(0.9, 0.3))+
  scale_x_continuous(breaks = seq(0,3000,500),
                     limits = c(0,3000),
                     labels = comma)+
  scale_y_continuous(breaks = seq(0,3000,500),
                     limits = c(0,3000),
                     labels = comma)+
  scale_color_manual(name = NULL,
                     values = linepalette1)+
  labs(x = "ค่าจริงจำนวนรถ BEV สะสม (คัน)",
       y = "ค่าพยากรณ์จำนวนรถ BEV สะสม (คัน)")


plotdata <- c(plotdata, list("compareBusBEV" = data))
figures <- c(figures, list("cumCompareBusBEV" = plot))

# 6. Total ####
data <-
  
  full_join(
    plotdata[["totalBEV"]] %>% 
      filter(year >= 2563),
    
    plotdata[["forecastTotalBEV"]] %>% 
      filter(year >= 2563 & year <= 2566),
    
    by = c("thaBEV" = "thaBEV",
           "year" = "year")
  ) %>% 
  select(year, thaBEV, "current" = vehicle.x, "forecast" = vehicle.y) 

plot<-
  data %>% 
  ggplot()+
  geom_point(aes(x = current, y = forecast, group = year, color = year),
             shape = 16,
             size = 2)+
  geom_abline(slope = 1)+
  facet_wrap(~thaBEV)+
  ThemeLine+
  theme(legend.position = c(0.9, 0.3))+
  scale_x_continuous(breaks = seq(0,640000, 80000),
                     limits = c(0,640000),
                     labels = comma)+
  scale_y_continuous(breaks = seq(0,640000, 80000),
                     limits = c(0,640000),
                     labels = comma)+
  scale_color_manual(name = NULL,
                     values = linepalette1)+
  labs(x = "ค่าจริงจำนวนรถ BEV สะสม (คัน)",
       y = "ค่าพยากรณ์จำนวนรถ BEV สะสม (คัน)")


plotdata <- c(plotdata, list("compareTotalBEV" = data))
figures <- c(figures, list("cumCompareTotalBEV" = plot))

## Mergeplot ####
sz = 8
blktitle <- theme(axis.title = element_blank(),
                  axis.text.x = element_text(size = sz),
                  axis.text.y = element_text(size = sz),
                  legend.position = "none")

# Get legend
p_legend1 <- gtable::gtable_filter(ggplotGrob(figures[["cumCompareSedanBEV"]] + 
                                                guides(shape = FALSE)+
                                                guides(shape=guide_legend(ncol=2))+theme(legend.position="bottom")), 
                                   pattern = "guide-box")
plot1 <-
  cowplot::plot_grid(figures$cumCompareSedanBEV+blktitle,
                     figures$cumComparePickupBEV+blktitle,
                     figures$`cumCompare2-3wheelerBEV`+blktitle,
                     figures$cumCompareTruckBEV+blktitle,
                     figures$cumCompareBusBEV+blktitle,
                     figures$cumCompareTotalBEV+blktitle,
                     ncol = 2,
                     labels = c("ก","ข","ค","ง","จ","ฉ"),
                     label_size = sz,
                     label_fontfamily = text,
                     align = "h")

plot1 <-
  cowplot::plot_grid(plot1,
                     p_legend1,
                     nrow = 2,
                     rel_heights = c(1,.05)
                     )

x.grob <- grid::textGrob("ค่าสถิติจำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                           gp=gpar(fontfamily = "Kanit", fontsize=15))
y.grob <- grid::textGrob("ค่าพยากรณ์จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                         gp=gpar(fontfamily = "Kanit", fontsize=15), rot=90)
plot <-
  gridExtra::grid.arrange(arrangeGrob(plot1, bottom = x.grob,left = y.grob))

# save multiplots
ggsave("figures/cumCompareBEV.png",plot, width = 6, height = 6, units = "in")
figures <- c(figures, list("mergeCompareBEV" = plot))
