# Compare total current and forecast BEV ####
# Line plot
library(ggtext)
library(glue)
library(ggpattern)

data <-
  full_join(
plotdata$totalBEV %>% 
  filter(year >= 2563 &
           year <= 2566),
plotdata$forecastTotalBEV %>% 
  filter(year >= 2563 &
           year <= 2566),
by = c("thaBEV" = "thaBEV",
       "year" = "year")
) %>% 
  select(thaBEV, year, "current" = vehicle.x, "forecast" = vehicle.y)

plot <- 
  data %>% 
  ggplot()+
  geom_line(aes(x = year, y = current, group = thaBEV, color = "1"))+
  geom_line(aes(x = year, y = forecast, group = thaBEV, color = "2"))+
  geom_point(aes(x = year,y = current, group = thaBEV, color = "1"),
             shape = 21,
             size = 2,
             fill = "grey",
             show.legend = FALSE)+
  geom_point(aes(x = year,y = forecast, group = thaBEV, color = "2"),
             shape = 21,
             size = 2,
             fill = "grey",
             show.legend = FALSE)+
  ThemeLine+
  theme(legend.box = "vertical",
        legend.position = "bottom")+
  scale_color_manual(name = NULL,
                     breaks = c("1","2"),
                     values = linepalette1,
                     labels = c("ค่าสถิติ", 
                                "ค่าพยากรณ์"))+
  scale_x_discrete(expand = c(0,0.1))+
  scale_y_continuous(breaks = seq(0,700000, 100000),
                     limits = c(0,700000),
                     labels = comma)+
  labs(x = NULL,
       y = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)")
plotdata <- c(plotdata, list("compareTotalCurentForcastBEV" = data))
figures <- c(figures, list("compareTotalCurentForcastBEV" = plot))

# Current Stacked BEV bar chart ####

data <- 
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
           sheet = "M3_BEV",
           range = "B27:M32", 
           col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  select(!mar66)%>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year != "share" &
           thaBEV != "รวมทั้งหมด")

data2 <-
read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
           sheet = "M3_BEV",
           range = "B45:M50",
           col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  replace(is.na(.),0) %>% 
  select(!mar66) %>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "growth") %>% 
  filter(year != "share")


plot<-
  data %>%
  filter(year >= 2563) %>% 
  ggplot()+
  geom_col(aes(x = year , y = vehicle, fill = reorder(thaBEV, vehicle)))+
  geom_point(data = data2 %>% filter(year >= 2563&thaBEV == "รวมทั้งหมด") %>% mutate(growth = growth*100),
             aes(x = year, y = growth*500),
             size = 2.5,
             shape = 21,
             color = "salmon",
             fill = "grey")+
  ThemeLine+
  theme(legend.title = element_blank(),
        legend.box = "vertical",
        legend.position = "bottom")+
  scale_y_continuous(name = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                     breaks = seq(0,160000, 20000),
                     limits = c(0,160000),
                     labels = comma,
                     sec.axis = sec_axis(~./500, 
                                         name="อัตราเติบโต (% ต่อปี)",
                                         labels = label_number(suffix = "%"),
                                         breaks = seq(0,350,25)))+
  scale_fill_manual(values = linepalette1)+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL)

plotdata <- c(plotdata, list("curentByTypeBEV" = data))
plotdata <- c(plotdata, list("growthCurentBEV" = data2))
figures <- c(figures, list("stackedCurrent&GrowthBEV" = plot))

# Current BEV by Type and Growth ####
data <-
  plotdata$curentByTypeBEV %>% 
    filter(year %in% c(year(Sys.Date())+543)) 

data2 <-
  plotdata$growthCurentBEV %>% 
    filter(year == year(Sys.Date())+543) %>% 
  filter(thaBEV != "รวมทั้งหมด")


plot<-
  data %>% 
    ggplot()+
    geom_col(aes(x = reorder(thaBEV,vehicle), 
                 y = vehicle, fill = thaBEV), position = "dodge",
             show.legend = FALSE)+
    geom_point(data = data2,
               aes(x = reorder(thaBEV, growth), y = growth*7000),
               size = 2.5,
               shape = 21,
               color = "salmon",
               fill = "grey")+
    ThemeLine+
    theme(plot.title = element_textbox_simple(margin = margin(b = 10)),
          plot.title.position = "plot")+
  scale_y_continuous(name = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                     breaks = seq(0,70000, 10000),
                     limits = c(0,70000),
                     labels = comma,
                     sec.axis = sec_axis(~./70, 
                                         name="อัตราเติบโต (% ต่อปี)",
                                         labels = label_number(suffix = "%"),
                                         breaks = seq(0,1000,100)))+
    scale_fill_manual(values = c("#E41A1C","#4DAF4A","#FF7F00","#377EB8","#984EA3"))+
  labs(x = NULL,
       title = glue("สถิติรถ BEV จดทะเบียนสะสม ณ <span style='color:dodgerblue'> 
                    {month.name[month(Sys.Date())-1]} {year(Sys.Date())}</span>"))
  
plotdata <- c(plotdata, list("currentBEVTypeGrowth" = data))
figures <- c(figures, list("currentBEVTypeGrowth" = plot))

# Stacked total BEV car and total BEV growth ####

data <-
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
           sheet = "M3_BEV",
           range = "O45:AF50", 
           col_names = c(as.character(2563:2580))) %>% 
  mutate(thaBEV = c("รถยนต์",
                    "รถกระบะ (Van & Pick Up)",
                    "รถ 2 และ 3 ล้อ",
                    "รถบรรทุก",
                    "รถบัส",
                    "รวมทั้งหมด"),
         .before = "2563") %>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "growth") %>% 
  filter(year >= 2564 & year <= year(Sys.Date())+543)


plot<-
  plotdata$compareTotalCurentForcastBEV %>%
  mutate(togo = forecast - current) %>% 
  pivot_longer(-thaBEV&-year, names_to = "type", values_to = "vehicle") %>%
  filter(type != "forecast") %>% 
  mutate(type = fct_reorder(type,vehicle, .desc = TRUE)) %>% 
  ggplot()+
  geom_col(aes(x = year, y = vehicle, fill = type))+
  geom_point(data = data %>% filter(thaBEV == "รวมทั้งหมด"), 
             aes(x = year, y = growth*34000, color = "1"),
             size = 2.5,
             shape = 15
             )+
  geom_point(data = plotdata$growthCurentBEV %>% filter(thaBEV == "รวมทั้งหมด"&
                                                          year >= 2563),
             aes(x = year, y = growth*34000, color = "2"),
             size = 2.5,
             shape = 17
             )+
  scale_y_continuous(name = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                     breaks = seq(0,800000, 100000),
                     limits = c(0,800000),
                     labels = comma,
                     sec.axis = sec_axis(~./340, 
                                         name="อัตราเติบโต (% ต่อปี)",
                                         breaks = seq(0,2400, 400),
                                         labels = label_number(big.mark = ",", suffix = "%")
                     ))+
  scale_fill_manual(name = "จำนวนรถ BEV", 
                    labels = c("ค่าพยากรณ์",
                               "ค่าสถิติ"),
                    values = c("#D9D9D9","#4DAF4A"))+
  scale_color_manual(name = "อัตราเติบโต", 
                     limits = c("1", "2"), 
                     labels = c("ค่าพยากรณ์",
                                "ค่าสถิติ"),
                     values = c("dodgerblue","red"))+
  guides(colour = guide_legend(override.aes = list(pch = c(15,17))))+
  ThemeLine+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))+
  labs(x = NULL)

plotdata <- c(plotdata, list("forecastBEVTypeGrowth" = data))
figures <- c(figures, list("stackTotalBEVcurrentForecastAndGrowth" = plot))

# Stacked Current BEV by car type and total BEV growth ####

data <-
read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
           sheet = "M3_BEV",
           range = "O27:AF32", 
           col_names = c(as.character(2563:2580))) %>% 
  mutate(thaBEV = c("รถยนต์",
                    "รถกระบะ (Van & Pick Up)",
                    "รถ 2 และ 3 ล้อ",
                    "รถบรรทุก",
                    "รถบัส",
                    "รวมทั้งหมด"),
         .before = "2563") %>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year == 2566,
         thaBEV != "รวมทั้งหมด")

data2<-
  full_join(plotdata$currentBEVTypeGrowth,
          data,
          by = c("thaBEV" = "thaBEV",
                 "year" = "year")) %>% 
  select(year, thaBEV, "current" = vehicle.x, "forecast" = vehicle.y) %>% 
  mutate(togo = forecast - current) %>% 
  pivot_longer(-year & -thaBEV, names_to = "type", values_to = "vehicle")

plot<-
  data2 %>% filter(type != "forecast") %>%
  mutate(type = fct_reorder(type,vehicle, .desc = TRUE),
         thaBEV = fct_reorder(thaBEV,vehicle, .desc = FALSE)) %>% 
  ggplot()+
  geom_col(aes(x = thaBEV, 
               y = vehicle, 
               fill = type))+
  geom_point(data = plotdata$growthCurentBEV %>% 
                    filter(year == 2566,
                           thaBEV != "รวมทั้งหมด") %>% 
                    mutate(thaBEV = fct_reorder(thaBEV,growth, .desc = FALSE)),
             aes(x = thaBEV, 
                 y =growth * 45000,
                 color = "2"),
             shape = 17,
             size = 2.5)+  
  geom_point(data = plotdata$forecastBEVTypeGrowth %>%  
                    filter(year == 2566,
                           thaBEV != "รวมทั้งหมด"),
             aes(x = thaBEV, 
                 y = growth*45000, 
                 color = "1"),
             shape = 15,
             size = 2.5)+
  scale_fill_manual(name = "จำนวนรถ BEV", 
                      labels = c("ค่าพยากรณ์",
                                 "ค่าสถิติ"),
                      values = c("#D9D9D9","#4DAF4A"))+
    scale_color_manual(name = "อัตราเติบโต", 
                       limits = c("1", "2"), 
                       labels = c("ค่าพยากรณ์",
                                  "ค่าสถิติ"),
                       values = c("dodgerblue","red"))+
    guides(colour = guide_legend(override.aes = list(pch = c(15,17))))+
  scale_y_continuous(name = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                     breaks = seq(0,400000, 100000),
                     limits = c(0,400000),
                     labels = comma,
                     sec.axis = sec_axis(~./450, 
                                         name="อัตราเติบโต (% ต่อปี)",
                                         breaks = seq(0,900, 100),
                                         labels = label_number(big.mark = ",", suffix = "%")
                     ))+
  ThemeLine+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))+
  labs(x = NULL)

plotdata <- c(plotdata, list("forecastBEVTypeCurrentYear" = data))
plotdata <- c(plotdata, list("currentForecastBEVTypeCurrentyear" = data2))
figures <- c(figures, list("stackBEVTypeCurrentForecastAndGrowth" = plot))
  
  
  
# Mergeplot ####
sz = 8
blktitle <- theme(axis.title = element_blank(),
                  axis.text.x = element_text(size = sz),
                  axis.text.y = element_text(size = sz))

# Get legend
p_legend1 <- gtable::gtable_filter(ggplotGrob(figures[["stackedCurrent&GrowthBEV"]]),
                                   pattern = "guide-box")
plot1 <-
  cowplot::plot_grid(figures$"stackedCurrent&GrowthBEV"+blktitle + theme(legend.position = "none"),
                     figures$currentBEVTypeGrowth+blktitle+theme(plot.title = element_blank()),
                     ncol = 2,
                     labels = c("ก","ข"),
                     label_size = sz,
                     label_fontfamily = text,
                     align = "hv")
  
plot1<-
  cowplot::plot_grid(plot1,
                     p_legend1,
                     nrow = 2,
                     rel_heights = c(1,.05))
  
p_legend2 <- gtable::gtable_filter(ggplotGrob(figures[["stackTotalBEVcurrentForecastAndGrowth"]]),
                                   pattern = "guide-box")
plot2 <-
  cowplot::plot_grid(figures$stackTotalBEVcurrentForecastAndGrowth+blktitle+theme(legend.position = "none"),
                     figures$stackBEVTypeCurrentForecastAndGrowth+blktitle+theme(legend.position = "none"),
                     ncol = 2,
                     labels = c("ค","ง"),
                     label_size = sz,
                     label_fontfamily = text,
                     align = "hv")
plot2<-
    cowplot::plot_grid(plot2,
                       p_legend2,
                       nrow = 2,
                       rel_heights = c(1,.05))  

plot <-
    cowplot::plot_grid(plot1,
                       plot2,
                       nrow = 2)
  
y.grob <- grid::textGrob("จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                         gp=gpar(fontfamily = "Kanit", fontsize=15), rot=90)
ysec.grob <- grid::textGrob("อัตราการเติบโตต่อปี (%)",
                            gp=gpar(fontfamily = "Kanit", fontsize=15), rot=270)
plot <-
  gridExtra::grid.arrange(arrangeGrob(plot, left = y.grob, right = ysec.grob))

# save multiplots
ggsave("figures/cumCompareCurrentForecastBEVAndGrowth.png",plot, width = 8, height = 6, units = "in")
figures <- c(figures, list("cumCompareCurrentForecastBEVAndGrowth" = plot))

# Stacked total BEV car and total additional BEV amount ####

data <-
read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
           sheet = "M3_BEV",
           range = "B36:M41", 
           col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  select(!mar66)%>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year != "share")


data2 <-
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
             sheet = "M3_BEV",
             range = "O36:AF41", 
             col_names = c(as.character(2563:2580))) %>% 
  mutate(thaBEV = c("รถยนต์",
                    "รถกระบะ (Van & Pick Up)",
                    "รถ 2 และ 3 ล้อ",
                    "รถบรรทุก",
                    "รถบัส",
                    "รวมทั้งหมด"),
         .before = "2563") %>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year >= 2564 & year <= year(Sys.Date())+543)


plot<-
  plotdata$compareTotalCurentForcastBEV %>%
  mutate(togo = forecast - current) %>% 
  pivot_longer(-thaBEV&-year, names_to = "type", values_to = "vehicle") %>%
  filter(type != "forecast") %>% 
  mutate(type = fct_reorder(type,vehicle, .desc = TRUE)) %>% 
  ggplot()+
  geom_col(aes(x = year, y = vehicle, fill = type))+
  geom_point(data = data %>% filter(year >= 2563, thaBEV == "รวมทั้งหมด"), 
             aes(x = year, y = vehicle*2, color = "1"),
             size = 2.5,
             shape = 17
  )+
  geom_line(data = data %>% filter(year >= 2563, thaBEV == "รวมทั้งหมด"), 
            aes(x = year, y = vehicle*2, group = thaBEV, color = "1"), show.legend = FALSE)+  
  geom_point(data = data2%>% filter(year >= 2563, thaBEV == "รวมทั้งหมด"),
             aes(x = year, y = vehicle*2, color = "2"),
             size = 2.5,
             shape = 15)+
  geom_line(data = data2%>% filter(year >= 2563, thaBEV == "รวมทั้งหมด"),
               aes(x = year, y = vehicle*2, group = thaBEV, color = "2"),  show.legend = FALSE)+ 
  scale_y_continuous(name = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                     breaks = seq(0,700000, 100000),
                     limits = c(0,700000),
                     labels = comma,
                     sec.axis = sec_axis(~./2, 
                                         name="จำนวนรถ BEV ส่วนเพิ่ม (คันต่อปี)",
                                         breaks = seq(0,350000, 50000),
                                         labels = label_number(big.mark = ",")
                     ))+
  scale_fill_manual(name = "จำนวนรถ BEV", 
                    labels = c("ค่าพยากรณ์",
                               "ค่าสถิติ"),
                    values = c("#D9D9D9","#4DAF4A"))+
  scale_color_manual(name = "จำนวนรถ BEV ส่วนเพิ่ม", 
                     limits = c("1", "2"), 
                     labels = c("ค่าสถิติ",
                                "ค่าพยากรณ์"),
                     values = c("red", "dodgerblue")) +
  guides(colour = guide_legend(override.aes = list(pch = c(17,15))))+
  ThemeLine+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))+
  labs(x = NULL)

plotdata <- c(plotdata, list("additionCurrentBEVType" = data))
plotdata <- c(plotdata, list("additionForecastBEVType" = data2))
figures <- c(figures, list("stackBEVTypeCurrentForecastAndAdditionalBEV" = plot))
  
# Stacked Current BEV by car type and additional BEV growth amount ####

plot<-
  plotdata$currentForecastBEVTypeCurrentyear %>% filter(type != "forecast") %>%
  mutate(type = fct_reorder(type,vehicle, .desc = TRUE),
         thaBEV = fct_reorder(thaBEV,vehicle, .desc = FALSE)) %>% 
  ggplot()+
  geom_col(aes(x = thaBEV, 
               y = vehicle, 
               fill = type))+
  geom_point(data = plotdata$additionCurrentBEVType %>% 
               filter(year == 2566,
                      thaBEV != "รวมทั้งหมด") %>% 
               mutate(thaBEV = fct_reorder(thaBEV,vehicle, .desc = FALSE)),
             aes(x = thaBEV, 
                 y = vehicle*2,
                 color = "2"),
             shape = 17,
             size = 2.5)+  
  geom_point(data = plotdata$additionForecastBEVType %>%  
               filter(year == 2566,
                      thaBEV != "รวมทั้งหมด"),
             aes(x = thaBEV, 
                 y = vehicle*2, 
                 color = "1"),
             shape = 15,
             size = 2.5)+
  scale_fill_manual(name = "จำนวนรถ BEV", 
                    labels = c("ค่าพยากรณ์",
                               "ค่าสถิติ"),
                    values = c("#D9D9D9","#4DAF4A"))+
  scale_color_manual(name = "จำนวนรถ BEV ส่วนเพิ่ม", 
                     limits = c("1", "2"), 
                     labels = c("ค่าพยากรณ์",
                                "ค่าสถิติ"),
                     values = c("dodgerblue", "red"))+
  guides(colour = guide_legend(override.aes = list(pch = c(15,17))))+
  scale_y_continuous(name = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                     breaks = seq(0,400000, 100000),
                     limits = c(0,400000),
                     labels = comma,
                     sec.axis = sec_axis(~./2, 
                                         name="จำนวนรถ BEV ส่วนเพิ่ม (คันต่อปี)",
                                         breaks = seq(0,200000, 50000),
                                         labels = label_number(big.mark = ",")
                     ))+
  ThemeLine+
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"))+
  labs(x = NULL)

figures <- c(figures, list("stackBEVTypeCurrentAndAdditionalBEV" = plot))

# Mergeplot ####
sz = 8
blktitle <- theme(axis.text.x = element_text(size = sz),
                  axis.text.y = element_text(size = sz))

# Get legend
p_legend1 <- gtable::gtable_filter(ggplotGrob(figures[["stackedCurrent&GrowthBEV"]]),
                                   pattern = "guide-box")
plot1 <-
  cowplot::plot_grid(figures$"stackedCurrent&GrowthBEV"+blktitle + theme(legend.position = "none",
                                                                         axis.title = element_blank()),
                     figures$currentBEVTypeGrowth+blktitle+theme(plot.title = element_blank(),
                                                                 axis.title.y.left = element_blank()),
                     ncol = 2,
                     labels = c("ก","ข"),
                     label_size = sz,
                     label_fontfamily = text,
                     align = "hv")

plot1<-
  cowplot::plot_grid(plot1,
                     p_legend1,
                     nrow = 2,
                     rel_heights = c(1,.05))

# ysec.grob <- grid::textGrob("อัตราการเติบโตต่อปี (%)",
#                             gp=gpar(fontfamily = "Kanit", fontsize=15), rot=270)
# plot1<-
#   gridExtra::grid.arrange(arrangeGrob(plot1, right = ysec.grob))


p_legend2 <- gtable::gtable_filter(ggplotGrob(figures[["stackBEVTypeCurrentForecastAndAdditionalBEV"]]),
                                   pattern = "guide-box")
plot2 <-
  cowplot::plot_grid(figures$stackBEVTypeCurrentForecastAndAdditionalBEV+blktitle+theme(legend.position = "none",
                                                                                        axis.title = element_blank()),
                     figures$stackBEVTypeCurrentAndAdditionalBEV+blktitle+theme(legend.position = "none",
                                                                                axis.title.y.left = element_blank()),
                     ncol = 2,
                     labels = c("ค","ง"),
                     label_size = sz,
                     label_fontfamily = text,
                     align = "hv")
plot2<-
  cowplot::plot_grid(plot2,
                     p_legend2,
                     nrow = 2,
                     rel_heights = c(1,.05))  

# ysec.grob <- grid::textGrob("จำนวนรถ BEV ส่วนเพิ่ม (คัน)",
                            # gp=gpar(fontfamily = "Kanit", fontsize=15), rot=270)
# plot2<-
  # gridExtra::grid.arrange(arrangeGrob(plot2, right = ysec.grob))

plot <-
  cowplot::plot_grid(plot1,
                     plot2,
                     nrow = 2)

y.grob <- grid::textGrob("จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                         gp=gpar(fontfamily = "Kanit", fontsize=15), rot=90)

plot <-
  gridExtra::grid.arrange(arrangeGrob(plot, left = y.grob))

# save multiplots
ggsave("figures/stackCurrentForecastAndAdditionalBEV.png",plot, width = 8, height = 6, units = "in")
figures <- c(figures, list("stackCurrentForecastAndAdditionalBEV" = plot))
