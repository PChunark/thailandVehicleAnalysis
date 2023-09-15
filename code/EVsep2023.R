library(readxl)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(extrafont)
library(extrafontdb)

figures <- list()
plotdata <- list()

# Current BEV ####
# 1 Sedan ####
data <-
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
           sheet = "M3_BEV",
           range = "B27:M32", 
           col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  select(!mar66)%>% 
pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
filter(year != "share" &
       thaBEV == "รถยนต์")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,60000, 10000),
                     limits = c(0,60000),
                      labels = comma)+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)")

# save in list
plotdata <- c(plotdata, list("thaSedanBEV" = data))
figures <- c(figures, list("cumSedanRegBEVAUG2023" = plot))

# 2. Pick up ####
data <-
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
             sheet = "M3_BEV",
             range = "B27:M32", 
             col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  select(!mar66)%>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year != "share" &
         thaBEV == "รถกระบะ (Van & Pick Up)")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,200, 50),
                     limits = c(0,200),
                     labels = comma)+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)")

#save in list
plotdata <- c(plotdata, list("thaPickupBEV" = data))
figures <- c(figures, list("cumPickupRegBEVAUG2023" = plot))

#3 two three wheeler ####

data <-
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
             sheet = "M3_BEV",
             range = "B27:M32", 
             col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  select(!mar66)%>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year != "share" &
         thaBEV == "รถ 2 และ 3 ล้อ")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,40000, 10000),
                     limits = c(0,40000),
                     labels = comma)+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)")

#Save in list
plotdata <- c(plotdata, list("tha2-3wheelerBEV" = data))
figures <- c(figures, list("cum23WheelerRegBEVAUG2023" = plot))

#4 Truck ####

data <-
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
             sheet = "M3_BEV",
             range = "B27:M32", 
             col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  select(!mar66)%>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year != "share" &
         thaBEV == "รถบรรทุก")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,300, 50),
                     limits = c(0,300),
                     labels = comma)+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)")

#Save in list
plotdata <- c(plotdata, list("truckBEV" = data))
figures <- c(figures, list("cumTruckRegBEVAUG2023" = plot))

#5 Bus ####

data <-
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
             sheet = "M3_BEV",
             range = "B27:M32", 
             col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  select(!mar66)%>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year != "share" &
         thaBEV == "รถบัส")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,2500, 500),
                     limits = c(0,2500),
                     labels = comma)+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)")

#Save in list
plotdata <- c(plotdata, list("busBEV" = data))
figures <- c(figures, list("cumBusRegBEVAUG2023" = plot))

# 6 Total ####

data <-
  read_excel("rawdata/01 EGAT_EV_เตรียมข้อมูลประชุม_Sep2023.xlsx",
             sheet = "M3_BEV",
             range = "B27:M32", 
             col_names = c("thaBEV", 2558:2565, "mar66",2566,"share")) %>% 
  select(!mar66)%>% 
  pivot_longer(-thaBEV, names_to = "year", values_to = "vehicle") %>% 
  filter(year != "share" &
         thaBEV == "รวมทั้งหมด")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,100000, 25000),
                     limits = c(0,100000),
                     labels = comma)+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "จำนวนรถ BEV จดทะเบียนสะสม (คัน)")

#Save in list
plotdata <- c(plotdata, list("totalBEV" = data))
figures <- c(figures, list("cumTotalRegBEVAUG2023" = plot))

## Mergeplot ####
blktitle <- theme(axis.title = element_blank())

plot1 <-
  cowplot::plot_grid(figures$cumSedanRegBEVAUG2023+blktitle,
          figures$cumPickupRegBEVAUG2023+blktitle,
          figures$cum23WheelerRegBEVAUG2023+blktitle,
          figures$cumTruckRegBEVAUG2023+blktitle,
          figures$cumBusRegBEVAUG2023+blktitle,
          figures$cumTotalRegBEVAUG2023+blktitle,
          ncol = 2,
          labels = c("ก","ข","ค","ง","จ","ฉ"),
          label_size = 10,
          label_fontfamily = text)

y.grob <- grid::textGrob("จำนวนรถ BEV จดทะเบียนสะสม (คัน)",
                   gp=gpar(fontfamily = "Kanit", fontsize=15), rot=90)
plot <-gridExtra::grid.arrange(arrangeGrob(plot1, left = y.grob))

# save multiplots
ggsave("figures/cumBEV.png",plot, width = 6, height = 6, units = "in")
figures <- c(figures, list("mergeRegBEVAUG2023" = plot))

# Forecast BEV ####
# 1 Sedan ####
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
  filter(year != "share" &
         thaBEV == "รถยนต์")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,8000000, 1000000),
                     limits = c(0,8000000),
                     labels = label_number(big.mark = ",", scale = 1e-3))+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "ค่าพยากรณ์จำนวนรถ BEV จดทะเบียนสะสม (1,000 คัน)")

# save in list
plotdata <- c(plotdata, list("forecastSedanBEV" = data))
figures <- c(figures, list("cumForecastSedanBEV" = plot))

# 2 Pick up ####
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
  filter(year != "share" &
         thaBEV == "รถกระบะ (Van & Pick Up)")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,2500000, 500000),
                     limits = c(0,2500000),
                     labels = label_number(big.mark = ",", scale = 1e-3))+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "ค่าพยากรณ์จำนวนรถ BEV จดทะเบียนสะสม (1,000 คัน)")

# save in list
plotdata <- c(plotdata, list("forecastPickupBEV" = data))
figures <- c(figures, list("cumForecastPickupBEV" = plot))

# 3 two three wheeler ####
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
  filter(year != "share" &
         thaBEV == "รถ 2 และ 3 ล้อ")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,15000000, 2500000),
                     limits = c(0,15000000),
                     labels = label_number(big.mark = ",", scale = 1e-3))+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "ค่าพยากรณ์จำนวนรถ BEV จดทะเบียนสะสม (1,000 คัน)")

# save in list
plotdata <- c(plotdata, list("forecast2-3wheelerBEV" = data))
figures <- c(figures, list("cumForecast23WheelerBEV" = plot))

# 4 Truck ####
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
  filter(year != "share" &
         thaBEV == "รถบรรทุก")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,700000, 100000),
                     limits = c(0,700000),
                     labels = label_number(big.mark = ",", scale = 1e-3))+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "ค่าพยากรณ์จำนวนรถ BEV จดทะเบียนสะสม (1,000 คัน)")

# save in list
plotdata <- c(plotdata, list("forecastTruckBEV" = data))
figures <- c(figures, list("cumForecastTruckBEV" = plot))

# 5 Bus ####
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
  filter(year != "share" &
         thaBEV == "รถบัส")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,100000, 10000),
                     limits = c(0,100000),
                     labels = label_number(big.mark = ",", scale = 1e-3))+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "ค่าพยากรณ์จำนวนรถ BEV จดทะเบียนสะสม (1,000 คัน)")

# save in list
plotdata <- c(plotdata, list("forecastBusBEV" = data))
figures <- c(figures, list("cumForecastBusBEV" = plot))

# 6 Total ####
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
  filter(year != "share" &
         thaBEV == "รวมทั้งหมด")

plot <-
  data %>% 
  ggplot(aes(x = year, y = vehicle))+
  geom_line(aes(group = thaBEV, color = thaBEV),
            show.legend = FALSE)+
  geom_point(shape = 21,
             size = 2,
             color = "salmon",
             fill = "grey")+
  facet_wrap(~thaBEV,
             scales = "free_y")+
  ThemeLine+
  theme(strip.text.x = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0,25000000, 2500000),
                     limits = c(0,25000000),
                     labels = label_number(big.mark = ",", scale = 1e-3))+
  scale_color_manual(values = linepalette1)+
  labs(x = NULL,
       y = "ค่าพยากรณ์จำนวนรถ BEV จดทะเบียนสะสม (1,000 คัน)")

# save in list
plotdata <- c(plotdata, list("forecastTotalBEV" = data))
figures <- c(figures, list("cumForecastTotalBEV" = plot))

## Mergeplot ####
sz = 8
blktitle <- theme(axis.title = element_blank(),
                  axis.text.x = element_text(size = sz),
                  axis.text.y = element_text(size = sz))

plot1 <-
  cowplot::plot_grid(figures$cumForecastSedanBEV+blktitle,
                     figures$cumForecastPickupBEV+blktitle,
                     figures$cumForecast23WheelerBEV+blktitle,
                     figures$cumForecastTruckBEV+blktitle,
                     figures$cumForecastBusBEV+blktitle,
                     figures$cumForecastTotalBEV+blktitle,
                     ncol = 2,
                     labels = c("ก","ข","ค","ง","จ","ฉ"),
                     label_size = sz,
                     label_fontfamily = text)

y.grob <- grid::textGrob("ค่าพยากรณ์จำนวนรถ BEV จดทะเบียนสะสม (1,000 คัน)",
                         gp=gpar(fontfamily = "Kanit", fontsize=15), rot=90)
plot <-
  gridExtra::grid.arrange(arrangeGrob(plot1, left = y.grob))

# save multiplots
ggsave("figures/cumForecastBEV.png",plot, width = 6, height = 6, units = "in")
figures <- c(figures, list("mergeForecastBEV" = plot))
