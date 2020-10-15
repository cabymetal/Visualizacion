library(readr)
netflix_titles <- read_csv("C:/Users/user/Downloads/archive/netflix_titles.csv")

dist <- sample(c(1, 2, 3, 4), size = 6234, replace = TRUE, prob = c(0.5, 0.2, 0.15, 0.05))
dist <- as.factor(dist)
levels(dist) <- c("TV", "PC", "Mobile", "Other")
netflix_titles$prefered_watch <- dist

write.csv(netflix_titles, "C:/Users/user/Downloads/archive/netflix_titles2.csv")

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

netflix_titles <- netflix_titles %>% mutate(country_fct = ifelse(str_count(country, ',') >= 1, 'Various_countries', country))%>% mutate(country_fct = if_else(is.na(country_fct), "None", country_fct)) 
netflix_titles$country_fct <- as.factor(netflix_titles$country_fct)
df <- netflix_titles %>% group_by(prefered_watch) %>% mutate(total = n())
df$date_added <- as.Date( df$date_added, "%B %d, %Y")
df$type <- as.factor(df$type)


my_theme <- theme(legend.position=c(0.6, 0.6),
                  legend.title = element_blank(),
                  axis.line = element_line(colour = "black", 
                                           size = 2, linetype = "solid"),
                  axis.line.y  = element_blank(),
                  plot.subtitle = element_text(face = "italic"),
                  panel.grid.major = element_line(color = "grey80")
                  
                  #rect = element_rect(margin(t = 20,  r = 20, b = 20, l = 20, unit = "pt"), color = "white")
                  )

ggplot(df, aes(x=prefered_watch, fill=prefered_watch)) + geom_bar(width=0.6) + 
  labs(title="Preferencia de usuarios", subtitle = "Muestra la preferencia de los usuarios a la hora de ver programas de Netflix", 
       xlab="Cantidad personas", ylab="Medio preferido") + scale_fill_grey() + 
  theme_classic() + 
  geom_text(aes(label=total), stat='count', vjust=-1.2) +
  my_theme



#### pendiente seguir tabajando en esta gráfica

df_bydate <- df %>% mutate(anio_mes = format(date_added, "%Y%m")) %>% filter(!is.na(anio_mes)) %>% 
  group_by(anio_mes,type, country_fct) %>% summarise(cant_contenido = n()) %>% 
  filter(country_fct %in% c('United States', 'Various_countries', 'Colombia')) %>% 
  mutate(point_size = if_else(anio_mes <= "201401", 1, 3))

ggplot(df_bydate, aes(x=anio_mes, y=cant_contenido, color=country_fct, size=point_size)) + geom_point(alpha=0.5) +
  scale_x_discrete(breaks = c("200801","201502","201801", "202001"))+
  geom_vline(data=df_bydate, mapping=aes(xintercept=c("201402")), color="black", linetype = "dashed") +
  guides(size=FALSE) +
  geom_text(data=df_bydate, mapping=aes(x=c("201402"), y=0, size=2,label=c("Adopción comercial SmartTV")), colour="black", size=4, angle=90, vjust=-0.4, hjust=-1.5) +
  theme_classic()  + 
  theme(panel.spacing = unit(3, "lines"), legend.position =c(0.8, 0.8),
        legend.title = element_blank(), panel.grid.major = element_line(color = "grey80"))+
  facet_wrap(~type)

#### pendiente seguir tabajando en esta gráfica

df_bydatecomplete <- df %>% group_by(date_added,prefered_watch,type, country_fct) %>% summarise(cant_contenido = mean(n())) %>%  filter(!is.na(date_added)) %>%
  filter(country_fct %in% c('United States', 'Various_countries', 'Colombia'))

ggplot(df_bydatecomplete, aes(x=prefered_watch, fill=prefered_watch)) + geom_bar(alpha=0.9) +
  #scale_x_continuous(breaks = c("200801","201502","201801", "202001"))+
  theme_classic()  + 
  theme(panel.spacing = unit(3, "lines"), legend.position =c(0.8, 0.8),
        legend.title = element_blank(), panel.grid.major = element_line(color = "grey80"))+
  facet_wrap(~type)

#TODO ADD GRAPH BY LINE

by_datemonth <-  df %>% filter(!is.na(date_added)) %>% 
  group_by(date_added, prefered_watch)  %>% summarise(cant_contenido = n()) %>% 
  mutate(anio_mes = format(date_added, "%Y%m")) %>% group_by(anio_mes, prefered_watch)%>%
  summarise(cant_contenido = sum(cant_contenido)) %>% mutate(point_size = if_else(anio_mes <= "201402", 0.7, 2.5))
  #filter(anio_mes!="2020")
by_datemonth%>% filter(prefered_watch == "TV") %>% select( -prefered_watch, -point_size) -> tempdf
  
ggplot(by_datemonth, aes(x=anio_mes, y=cant_contenido, color=prefered_watch)) + 
  geom_line(aes(group=prefered_watch))+
  scale_x_discrete(breaks = c("200801","201402","201502","201801", "202001"))+
  geom_vline(data=df_bydate, mapping=aes(xintercept=c("201402")), color="black", linetype = "dashed") +
  guides(alpha=FALSE) +
  labs(title="Preferencia de usuarios", subtitle = "Relacion de preferencia de usuarios a lo largo del tiempo", 
       xlab="Año Mes", ylab="Cantidad contenido")+
  geom_text(data=df_bydate, mapping=aes(x=c("201402"), y=0, size=2,label=c("Adopción comercial SmartTV")), colour="black", size=4, angle=90, vjust=-0.4, hjust=-1.5) +
  theme_classic()  + 
  theme(legend.title = element_blank(), panel.grid.major = element_line(color = "grey80"))+
  geom_jitter() 