#----------------------------------------------
# load library, data, font
#----------------------------------------------
library(tidyverse)
library(lubridate)
library(ggthemes)
library(rcartocolor)
library(showtext)
library(devtools)
library(ggthemr)
library(gganimate)
library(gifski)
library(scales)
library(extrafont)
library(mapmate)

font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Noto Sans KR", "Noto Sans KR")
showtext_auto() 
Sys.setlocale("LC_TIME", "English")
windows()
font_import()
loadfonts(device = "win")

tmp <- read.csv("tmp.csv")
heat1 <- read.csv("heatindex1.csv")
heat2 <- read.csv("heatindex2.csv")

heat <- bind_rows(heat1,heat2)

#----------------------------------------------
# Data Cleaning
#----------------------------------------------

heat$date <- ymd(heat$date)

heat_f <- heat %>% filter(month(date) >= 6 & month(date) <= 8)
heat_f$fake <- ymd(str_c("2021-",str_sub(heat_f$date,6,10)))

heat_f <- heat_f %>% mutate(now = case_when(year(heat_f$date) == 2021 & month(heat_f$date) == 7 & day(heat_f$date) > 11 ~ "Last 7 Days",
                                      TRUE ~ "Past Data"))

tmp$날짜 <- ymd(tmp$날짜)

tmp$fake <- ymd(str_c("2021-",str_sub(tmp$날짜,6,10)))

tmp <- tmp %>% mutate(now = case_when(year(tmp$날짜) == 2021 & month(tmp$날짜) == 7 & day(tmp$날짜) > 11 ~ "Last 7 Days",
                                      TRUE ~ "Past Data"))

tmp <- tmp %>% mutate(range = case_when(year(tmp$날짜)>=1975 & year(tmp$날짜)<1980 ~ "1975-1980",
                                        year(tmp$날짜)>=1980 & year(tmp$날짜)<1985 ~ "1980-1985",
                                        year(tmp$날짜)>=1985 & year(tmp$날짜)<1990 ~ "1985-1990",
                                        year(tmp$날짜)>=1990 & year(tmp$날짜)<1995 ~ "1990-1995",
                                        year(tmp$날짜)>=1995 & year(tmp$날짜)<2000 ~ "1995-2000",
                                        year(tmp$날짜)>=2000 & year(tmp$날짜)<2005 ~ "2000-2005",
                                        year(tmp$날짜)>=2005 & year(tmp$날짜)<2010 ~ "2005-2010",
                                        year(tmp$날짜)>=2010 & year(tmp$날짜)<2015 ~ "2010-2015",
                                        TRUE ~ "2015-2021",
                                        ))

#----------------------------------------------
# Max temp - Scatter plot
#----------------------------------------------

tmp %>% 
  ggplot(aes(x=fake,y=최고기온,color=now)) + 
  geom_jitter(alpha = 0.6) + 
  scale_x_date(date_labels = "%b",breaks= seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="1 month"))+
  labs(x = NULL, y = "Max Temperature (°C)",color = NULL,title = "Max temperature in Seoul(1975-2021)",caption = "Data: Korea Meteorological Administration")+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14),
        legend.position = "top",
        legend.key.width= unit(2, 'cm'),
        panel.border = element_blank())+
  scale_colour_manual(values = c("red", "grey"))

#----------------------------------------------
# Max temp - Heat map
#----------------------------------------------
mid <- mean(tmp$최고기온,na.rm=T)

tmp %>% 
  ggplot(aes(x=fake,y=desc(year(날짜)),fill=최고기온)) + 
  geom_tile() + 
  #scale_color_carto_c(palette = "BurgYl")+
  #scale_color_gradient(low = "#DD8A0B",
                      #high = "#32A676")+
  scale_fill_gradient2(midpoint = mid,low = "#54B2C8", mid = "#E6E6E6", high = "#FD353A", na.value = NA)+
  scale_x_date(date_labels = "%b",breaks= seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="1 month"),position = "top")+
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5,
                               label.hjust = .5))+
  labs(x = NULL, y = NULL, fill = "Max Temperature (°C)",title = "Max temperature in Seoul(1975-2021)",caption = "Data: Korea Meteorological Administration")+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14),
        legend.position = "top",
        legend.key.width= unit(3, 'cm'),
        panel.border = element_blank())

#----------------------------------------------
# Heat index - Scatter plot
#----------------------------------------------

heat_f %>% 
  filter(year(date) > 2010) %>% 
  ggplot(aes(x=fake,y=heatindex,color=now))+
  geom_point()+scale_colour_manual(values = c("red", "grey"))+
  labs(x = NULL, y = "Heat Index(°C)", color = NULL,
       title = "Heat Index(a.k.a.How Hot it really feels)",
       subtitle="Based on humidity and temperature",
       caption = "Data Source: 2011-2021 heat index data of Seoul(Korea Meteorological Administration)")+
  geom_hline(yintercept = 32.23,lty="dashed",size = 0.5,color="red")+
  annotate(geom = 'text',
           label = 'Extreme Caution(up to 32.23°C)',
           size = 3,
           x = ymd("2021-06-15"),
           y = 33)+
  #geom_text(aes(x=ymd("2021-07-15"),y=35),label="Extreme Caution(up to 32.23°C)",hjust=1)+
  scale_y_continuous(breaks=c(0,10,20,30,40),limits = c(0,40))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14),
        legend.position = "top",
        legend.key.width= unit(3, 'cm'),
        panel.border = element_blank())

#----------------------------------------------
# Heat index - bar graph namimation
#----------------------------------------------

#data cleansing
heat_smooth <- heat_f %>% 
  mutate(fake2 = str_c(str_sub(heat_f$fake,6,7),"월",str_sub(heat_f$fake,9,10),"일")) %>% 
  filter(ymd(fake)<="2021-07-17" & year(date) >=2011) %>% 
  group_by(fake2) %>% 
  mutate(rank = min_rank(-heatindex)*1) %>% 
  ungroup()

heat_smooth_en <- heat_f %>% 
  mutate(fake2 = format(fake, "%b %d")) %>% 
  filter(ymd(fake)<="2021-07-17" & year(date) >=2011) %>% 
  group_by(fake2) %>% 
  mutate(rank = min_rank(-heatindex)*1) %>% 
  ungroup()

options("device" = "windows")

#make color palette
ani_col2 <- c( "#89112b", "#9d1431", "#af1637", "#ba3246", "#c54756", "#d05966", "#da6c76", "#e47d87", "#ed8f97", "#f6a0a8", "#ffb2b9")

#Korean ver. data 
p_kor <- heat_smooth %>% 
  ggplot(aes(rank,group=year(date),fill=as.factor(year(date)),color=as.factor(year(date))))+
  geom_tile(aes(y = heatindex/2,
                height = heatindex,
                width = 0.9), alpha = 0.8, color = NA)+
  geom_text(aes(y = 0, label = paste(year(date), " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=heatindex,label = str_c(round(heatindex,1),"°C"), hjust=-.1)) +
  scale_fill_manual(values=ani_col2)+
  scale_color_manual(values=ani_col2)+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(y = "열지수(체감온도°C)", title = '{closest_state}',
       subtitle  =  "서울, 가장 더웠던 해는...",
       caption  = "데이터: 2011-2021년 6월 1일 - 7월 18일 서울 지역 일일 열지수(기상청)")+
  theme(text=element_text(family="Noto Sans Korean Bold"), 
        plot.title = element_text(margin = margin(10, 0, 10, 0),size = 24),
        plot.subtitle = element_text(size=15),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm"),
        panel.border = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  transition_states(fake2, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')


#English ver. data 
p_en <- heat_smooth_en %>% 
  ggplot(aes(rank,group=year(date),fill=as.factor(year(date)),color=as.factor(year(date))))+
  geom_tile(aes(y = heatindex/2,
                height = heatindex,
                width = 0.9), alpha = 0.8, color = NA)+
  geom_text(aes(y = 0, label = paste(year(date), " ")), vjust = 0.2, hjust = 1,family="Segoe UI Semibold") +
  geom_text(aes(y=heatindex,label = str_c(round(heatindex,1),"°C"), hjust=-.1,family="Segoe UI Semibold")) +
  scale_fill_manual(values=ani_col2)+
  scale_color_manual(values=ani_col2)+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title = '{closest_state}',
       subtitle = 'The Hottest Summer was...',
       caption  = "Data Source: 2011-2021 heat index data of Seoul(Korea Meteorological Administration)")+
  theme(text=element_text(family="Segoe UI Semibold"), 
        plot.title = element_text(margin = margin(10, 0, 10, 0),size = 24),
        plot.subtitle = element_text(size=15),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm"),
        panel.border = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  transition_states(fake2, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

#----------------------------------
# make gif
#----------------------------------

animate(p_kor, fps = 25, duration = 25, width = 800, height = 800,
        renderer = gifski_renderer("gganim_kor.gif"))

animate(p_en, fps = 25, duration = 25, width = 800, height = 600,
        renderer = gifski_renderer("gganim_en.gif"))

animate(p_en, 200, fps = 20,  width = 800, height = 800,
        renderer = ffmpeg_renderer()) -> for_mp4anim_save("anim_en.mp4", animation = for_mp4 )
  
options("device" = "RStudioGD")

