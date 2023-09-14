# Compare total current and forecast BEV ####
# Line plot

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
                     breaks = seq(0,100000, 20000),
                     limits = c(0,100000),
                     labels = comma,
                     sec.axis = sec_axis(~./500, 
                                         name="การเติบโต (%)",
                                         labels = label_number(suffix = "%"),
                                         breaks = seq(0,200,25)))+
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


data %>% 
    ggplot()+
    geom_col(aes(x = reorder(thaBEV, vehicle) , 
                 y = vehicle, fill = thaBEV), position = "dodge",
             show.legend = FALSE)+
    geom_point(data = data2,
               aes(x = reorder(thaBEV, growth), y = growth*6500),
               size = 2.5,
               shape = 21,
               color = "salmon",
               fill = "grey")+
    ThemeLine+
  scale_y_continuous(name = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                     breaks = seq(0,60000, 10000),
                     limits = c(0,60000),
                     labels = comma,
                     sec.axis = sec_axis(~./65, 
                                         name="การเติบโต (%)",
                                         labels = label_number(suffix = "%"),
                                         breaks = seq(0,900,100)))+
    scale_fill_manual(values = linepalette1)+
  labs(x = NULL)
  