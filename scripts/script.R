library(tidyverse)
library(tidycensus)

stats <- read_csv("data/table_data4a.csv")

stats <- stats %>% 
  filter(!(State == "Alaska" & (Year %in% c(1949:1958)) & Measures == "Highway use of gasoline (thousand gallons)")) %>% 
  filter(!(State == "Hawaii" & (Year %in% c(1949:1958)) & Measures == "Highway use of gasoline (thousand gallons)"))

pop_states <- read_csv("data/pop_states.csv")

pop_states <- pop_states %>% 
  mutate(X1 = str_remove_all(pop_states$X1,"\\.")) %>% 
  pivot_longer(-X1, names_to = "year") %>% 
  rename(State = X1, Year = year, Population = value)

pop_states$Year <- as.numeric(pop_states$Year)

stats_pop <- stats %>% 
  left_join(pop_states) %>% 
  mutate(value_normalized = Values/Population,
         pop_thousands = Population/1000,
         value_normalized_thousands = Values/pop_thousands)

stats_pop %>% 
  filter(Measures == "Licensed drivers") %>% 
  ggplot(aes(x=Year,
             y = fct_reorder(State, desc(State)),
             fill=value_normalized_thousands))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Licensed drivers\nper 1,000 residents"))+
  scale_fill_distiller(palette = "RdYlGn")+
  xlim(1949,2020)+
  theme(text = element_text(family = "SourceSansPro-Light", color = "grey10", lineheight = 0.5),   
        legend.position="right",legend.direction="vertical",
        legend.title = element_text(lineheight = .8),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        plot.title = element_text(family = "SourceSansPro-Regular", size = 20, hjust = 0),
        plot.subtitle = element_text(size = 15, hjust = 0),
        plot.caption = element_text(size = 10, color = "grey50"),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        panel.background = element_blank())+
  labs(x = NULL,
       y = NULL,
       title = "Licensed Drivers",
       subtitle = paste0("1950 - 2018"),
       caption = "Source Data: Bureau of Transportation Statistics, US Census Bureau")

ggsave("output/licensed_drivers.png")

stats_pop %>% 
  filter(Measures == "Highway Fatalities") %>% 
  ggplot(aes(x=Year,
             y = fct_reorder(State, desc(State)),
             fill=value_normalized_thousands))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Highway fatalities\nper 1,000 residents"))+
  scale_fill_distiller(palette = "RdYlGn")+
  xlim(1990,2018)+
  theme(text = element_text(family = "SourceSansPro-Light", color = "grey10", lineheight = 0.5),   
        legend.position="right",legend.direction="vertical",
        legend.title = element_text(lineheight = .8),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        plot.title = element_text(family = "SourceSansPro-Regular", size = 20, hjust = 0),
        plot.subtitle = element_text(size = 15, hjust = 0),
        plot.caption = element_text(size = 10, color = "grey50"),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        panel.background = element_blank())+
  labs(x = NULL,
       y = NULL,
       title = "Highway Fatalities",
       subtitle = paste0("199x - 2018"),
       caption = "Source Data: Bureau of Transportation Statistics, US Census Bureau")

stats_pop %>% 
  filter(Measures == "Highway use of gasoline (thousand gallons)") %>% 
  ggplot(aes(x=Year,
             y = fct_reorder(State, desc(State)),
             fill=value_normalized_thousands))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Gallons of gas\nused on highways\nper resident"))+
  scale_fill_distiller(palette = "RdYlGn")+
  xlim(11949,2020)+
  theme(text = element_text(family = "SourceSansPro-Light", color = "grey10", lineheight = 0.5),   
        legend.position="right",legend.direction="vertical",
        legend.title = element_text(lineheight = .8),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        plot.title = element_text(family = "SourceSansPro-Regular", size = 20, hjust = 0),
        plot.subtitle = element_text(size = 15, hjust = 0),
        plot.caption = element_text(size = 10, color = "grey50"),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        panel.background = element_blank())+
  labs(x = NULL,
       y = NULL,
       title = "Highway Use of Gasoline",
       subtitle = paste0("1950 - 2018"),
       caption = "Source Data: Bureau of Transportation Statistics, US Census Bureau")

