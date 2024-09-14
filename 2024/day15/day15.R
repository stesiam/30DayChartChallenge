library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(ggtext)
library(ggstream)
library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Oswald", "title")
sysfonts::font_add_google("Josefin Slab","js")
sysfonts::font_add_google("Cabin Condensed","cc")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Jost", "jost")

#sysfonts::font_add_google("Gentium Plus", "gp")
sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

url = "https://www.statistics.gr/el/statistics?p_p_id=documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=4&p_p_col_pos=3&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_javax.faces.resource=document&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_ln=downloadResources&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_documentID=116414&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_locale=el"
download.file(url, destfile = "2024/day15/data.xlsx")

## Due to the fact that the dataset is splittled in two parts I have to 
## define each part and combine them

vehicles_in_operation = readxl::read_excel("2024/day15/data.xlsx",
                                           range = "A3:V37")


## Due to the fact that the dataset is splittled in two parts I have to 
## define each part and combine them



dataset_part_1 = vehicles_in_operation[1:16, -1]
dataset_part_2 = vehicles_in_operation[18:34,-1]

#Use the first row of dataset as my variable names and delete the duplicated line

colnames(dataset_part_2) = dataset_part_2[1,]
dataset_part_2 = dataset_part_2[-1,]
dataset_part_1
# Remove duplicated columns

dataset_part_1 = dataset_part_1 %>%
  relocate(`Category and use`, .before = "1985")

dataset_part_2 = dataset_part_2[,-ncol(dataset_part_2)]

# Bind columns along each other

bind_dataset = dplyr::bind_cols(dataset_part_1, dataset_part_2)

## Now it's time to tidy my original dataset

dataset = bind_dataset %>%
  slice(c(1, 2, 6, 9, 12)) %>%
  rename(
    "category" = `Category and use`
  ) %>%
  mutate(category = case_when(
    grepl("Vehicles", category) ~ "Total",
    grepl("cars", category) ~ "Car",
    grepl("Buses", category) ~ "Bus",
    grepl("Motorcycle", category) ~ "Motorcycle",
    grepl("Truck", category) ~ "Truck",
    TRUE ~ category  # Keep other values unchanged
  )) %>%
  dplyr::filter(category != "Total")


tidy_dataset = dataset %>%
  tidyr::pivot_longer(., cols = c(!category), names_to = "Year", values_to = "Obs")


## Viz texts 

title_text = "Vehicles in Greece"
subtitle_text = glue("We observe that in recent years there<br> 
                     has been a <span style = 'font-family:uc; color:red;'>3x</span><span style = 'font-family:fs; color:red;'> &#xe098;</span> of wheeled vehicles. <br>
                     The most notable change is the doubling in <br>
                     the proportion of motorcycles.")
caption_text = "30 Day Chart Challenge, Day 15 (2024) | <b> Data:</b> Hellenic Statistical Authority<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"

title_text_gr = "Οχήματα στην Ελλάδα"
subtitle_text_gr = glue("Παρατηρούμε ότι τα πρόσφατα χρόνια<br> 
                     υπάρχει <span style = 'font-family:uc; color:red;'>3x</span><span style = 'font-family:fs; color:red;'> &#xe098; </span> των συνολικών οχημάτων. <br>
                     Η πιο αξιοσημείωτη αλλαγή είναι ο <br>
                     διπλασιασμός του ποσοστού των μηχανών")
caption_text_gr = "30 Day Chart Challenge, Day 15 (2024) | <b> Δεδομένα:</b> Ελληνική Στατιστική Υπηρεσία<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"


pal=c("#ffa600",
      "#2f4b7c",
      "#f95d6a",
      "#a05195")

total_vehicles_per_year = tidy_dataset %>%
  filter(Year %in% c("1990", "2000", "2010", "2020")) %>%
  group_by(Year) %>%
  summarise(add = sum(Obs)/1e6)


get_yearly_data = function(year_int, category_int){
  tidy_dataset |>
    dplyr::filter(Year == year_int) |>
    mutate(
      Obs = Obs/1e6
    ) |>
    group_by(Year) |>
    mutate(sum = sum(Obs),
           pct = round(Obs/sum*100,2)) %>%
    dplyr::filter(category == category_int)
}


p = ggplot(tidy_dataset, aes(x = Year, y = Obs/1e6, fill = category, group = category, label = category, color = category)) +
  geom_stream(type = "ridge", bw=1) +
  labs(
    y = "Vehicles (in millions)",
    caption = caption_text
  ) +
  annotate("text", x = "2015", y = 6.2,
           label = "Cars",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc")  +
  annotate("text", x = "2015", y = 0.8,
           label = "Truck",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc") +
  annotate("text", x = "2016", y = 2.2,
           label = "Motorcycle",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc") +
  
  # Title text
  
  geom_richtext(aes(x = "1985", y = 13.5,
                    label = "Vehicles in Greece"),
                fill = NA, label.color = NA,
                fontface="bold",family="jost",
                color = "black",size = 9, hjust = 0
                ) +
  
  # Subtitle Text
  
  geom_richtext(aes(x = "1985", y = 11, label = subtitle_text),
                fill = NA, label.color = NA, color = "black",
                hjust = 0,
                size = 3.2,
                family = "jost") +
  
  
  ## Vertical segments
  
  geom_segment(aes(x = "1990", y = 0, xend = "1990", yend = total_vehicles_per_year$add[1] + 0.5),color="black") +
  geom_point(aes(x = "1990", y = total_vehicles_per_year$add[1] + 0.5),color="black") +
  geom_richtext(aes(x = "1990", y = total_vehicles_per_year$add[1] + 1.5,
           label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[1],2)} </span>", " mil.", "<br>",
                          "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("1990", "Car")$pct, "%)" , "<br>",
                          "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("1990", "Motorcycle")$pct, "%)" , "<br>"                          )),
           fill = NA, label.color = NA,
           hjust=0.5,
           size=3,
           lineheight=1.1,
           fontface="bold",family="uc",
           color="black") +
  
  
  geom_segment(aes(x = "2000", y = 0, xend = "2000", yend = total_vehicles_per_year$add[2] + 0.5),color="black") +
  geom_point(aes(x = "2000", y = total_vehicles_per_year$add[2] + 0.5),color="black") +
  geom_richtext(aes(x = "2000", y = total_vehicles_per_year$add[2] + 1.5,
           label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[2],2)} </span>", " mil.", "<br>",
                "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("2000", "Car")$pct, "%)" , "<br>",
                "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("2000", "Motorcycle")$pct, "%)" , "<br>" )),
           fill = NA, label.color = NA,
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family="uc",
           color="black") +
  
  geom_segment(aes(x = "2010", y = 0, xend = "2010", yend = total_vehicles_per_year$add[3] + 0.1),color="black") +
  geom_point(aes(x = "2010", y = total_vehicles_per_year$add[3] + 0.1),color="black") +
  geom_richtext(aes(x = "2010", y = total_vehicles_per_year$add[3] + 1,
                    label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[3],2)} </span>", " mil.", "<br>",
                                 "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("2010", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("2010", "Motorcycle")$pct, "%)" , "<br>" )),
           fill = NA, label.color = NA,
           hjust=0.5,
           size=3,
           lineheight=.8,
           fontface="bold",family="uc",
           color="black") +
  
  geom_segment(aes(x = "2020", y = 0, xend = "2020", yend = total_vehicles_per_year$add[4] + 1),color="black") +
  geom_point(aes(x = "2020", y = total_vehicles_per_year$add[4] + 1),color="black") +
  geom_richtext(aes(x = "2020", y = total_vehicles_per_year$add[4] + 2.4,
                    label = glue("<span style = 'font-family:uc; font-size:20px; color: red;'> {round(total_vehicles_per_year$add[4],2)} </span>", " mil.", "<br>",
                                 "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("2020", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("2020", "Motorcycle")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf0d1; </span> ", " (", get_yearly_data("2020", "Truck")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf207; </span> ", " (", get_yearly_data("2020", "Bus")$pct, "%)" , "<br>" )),
           fill = NA, label.color = NA,
           hjust=0.5,
           size=3,
           lineheight=1.1,
           fontface="bold",family="uc",
           color="black") +
  
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  scale_x_discrete(breaks=c(1990, 2000, 2010, 2020),labels = c("1990","2000","2010","2020")) +
  scale_y_continuous(expand = c(0,0),limits = c(0,15)) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(family = "jost",
                                  margin = margin(t = 10, b = 5), 
                                  hjust = 0.5, 
                                  color = "black",face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "serif",
                                     margin = margin(t = 5, l = 10, r = 10, b = 5),
                                     lineheight = 1.1,
                                     color = "black"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 5, r = 5, b = 4), 
                                    lineheight = 1.4,
                                    color = "black", size = 8,
                                    hjust = 0.5),
    plot.margin = margin(l=8, r=8, t = 5),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_markdown(size = 8, family = "jost"),
    axis.text.y = element_blank(),
    axis.line = element_line(linewidth = 0.2)
  )

p_black = ggplot(tidy_dataset, aes(x = Year, y = Obs/1e6, fill = category, group = category, label = category, color = category)) +
  geom_stream(type = "ridge", bw=1) +
  labs(
    y = "Vehicles (in millions)",
    caption = caption_text
  ) +
  annotate("text", x = "2015", y = 6.2,
           label = "Cars",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc")  +
  annotate("text", x = "2015", y = 0.8,
           label = "Truck",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc") +
  annotate("text", x = "2016", y = 2.2,
           label = "Motorcycle",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc") +
  
  
  #Title Text
  
  geom_richtext(aes(x = "1985", y = 13.5,
                    label = "Vehicles in Greece"),
                fill = NA, label.color = NA,
                fontface="bold",family="jost",
                color = "white",size = 9, hjust = 0
  ) +
  
  #Subtitle Text

  geom_richtext(aes(x = "1985", y = 11, label = subtitle_text),
                fill = NA, label.color = NA, color = "white",
                hjust = 0,
                size = 3.2,
                family = "jost") +
  
  
  ## Vertical segments
  
  geom_segment(aes(x = "1990", y = 0, xend = "1990", yend = total_vehicles_per_year$add[1] + 0.5),color="white") +
  geom_point(aes(x = "1990", y = total_vehicles_per_year$add[1] + 0.5),color="white") +
  geom_richtext(aes(x = "1990", y = total_vehicles_per_year$add[1] + 1.5,
                    label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[1],2)} </span>", " mil.", "<br>",
                                 "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("1990", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("1990", "Motorcycle")$pct, "%)" , "<br>"                          )),
                fill = NA, label.color = NA,
                hjust=0.5,
                size=3,
                lineheight=1.1,
                fontface="bold",family="uc",
                color="white") +
  
  
  geom_segment(aes(x = "2000", y = 0, xend = "2000", yend = total_vehicles_per_year$add[2] + 0.5),color="white") +
  geom_point(aes(x = "2000", y = total_vehicles_per_year$add[2] + 0.5),color="white") +
  geom_richtext(aes(x = "2000", y = total_vehicles_per_year$add[2] + 1.5,
                    label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[2],2)} </span>", " mil.", "<br>",
                                 "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("2000", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("2000", "Motorcycle")$pct, "%)" , "<br>" )),
                fill = NA, label.color = NA,
                hjust=0.5,
                size=3,
                lineheight=.8,
                fontface="bold",family="uc",
                color="white") +
  
  geom_segment(aes(x = "2010", y = 0, xend = "2010", yend = total_vehicles_per_year$add[3] + 0.1),color="white") +
  geom_point(aes(x = "2010", y = total_vehicles_per_year$add[3] + 0.1),color="white") +
  geom_richtext(aes(x = "2010", y = total_vehicles_per_year$add[3] + 1,
                    label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[3],2)} </span>", " mil.", "<br>",
                                 "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("2010", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("2010", "Motorcycle")$pct, "%)" , "<br>" )),
                fill = NA, label.color = NA,
                hjust=0.5,
                size=3,
                lineheight=.8,
                fontface="bold",family="uc",
                color="white") +
  
  geom_segment(aes(x = "2020", y = 0, xend = "2020", yend = total_vehicles_per_year$add[4] + 1),color="white") +
  geom_point(aes(x = "2020", y = total_vehicles_per_year$add[4] + 1),color="white") +
  geom_richtext(aes(x = "2020", y = total_vehicles_per_year$add[4] + 2.4,
                    label = glue("<span style = 'font-family:uc; font-size:20px; color: red;'> {round(total_vehicles_per_year$add[4],2)} </span>", " mil.", "<br>",
                                 "<span style = 'font-family:fs; color:#2f4b7c;'>&#xf1b9; </span> ", " (", get_yearly_data("2020", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs; color:#f95d6a;'>&#xf21c; </span> ", " (", get_yearly_data("2020", "Motorcycle")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs; color:#a05195;'>&#xf0d1; </span> ", " (", get_yearly_data("2020", "Truck")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs; color:#ffa600;'>&#xf207; </span> ", " (", get_yearly_data("2020", "Bus")$pct, "%)" , "<br>" )),
                fill = NA, label.color = NA,
                hjust=0.5,
                size=2.5,
                lineheight=1.3,
                fontface="bold",family="uc",
                color="white") +
  
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  scale_x_discrete(breaks=c(1990, 2000, 2010, 2020),labels = c("1990","2000","2010","2020")) +
  scale_y_continuous(expand = c(0,0),limits = c(0,15)) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(family = "jost",
                                  margin = margin(t = 10, b = 5), 
                                  hjust = 0.5, 
                                  color = "white",face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "serif",
                                     margin = margin(t = 5, l = 10, r = 10, b = 5),
                                     lineheight = 1.1,
                                     color = "white"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 5, r = 5, b = 4), 
                                    lineheight = 1.4,
                                    color = "white", size = 8,
                                    hjust = 0.5),
    plot.margin = margin(l=8, r=8, t = 5),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_markdown(size = 8, family = "jost"),
    axis.text.x = element_markdown(color = "white"),
    axis.text.y = element_blank(),
    axis.line = element_line(linewidth = 0.2)
  )

ggsave(
  filename = "2024/day15/day15-2024-cc.png",
  plot = p,
  device = "png",
  height = 4,
  width = 6)

ggsave(
  filename = "2024/day15/day15_dark-2024-cc.png",
  plot = p_black,
  device = "png",
  height = 4,
  width = 6)


p_black_gr = ggplot(tidy_dataset, aes(x = Year, y = Obs/1e6, fill = category, group = category, label = category, color = category)) +
  geom_stream(type = "ridge", bw=1) +
  labs(
    y = "Αριθμός οχημάτων (σε εκατομμύρια)",
    caption = caption_text_gr
  ) +
  annotate("text", x = "2015", y = 6.2,
           label = "Αυτοκίνητα",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc")  +
  annotate("text", x = "2015", y = 0.8,
           label = "Φορτηγά",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc") +
  annotate("text", x = "2016", y = 2.2,
           label = "Μηχανές",
           hjust=1.1,
           size=3,
           color="white",
           family = "uc") +
  
  
  #Title Text
  
  geom_richtext(aes(x = "1985", y = 13.5,
                    label = "Οχήματα στην Ελλάδα"),
                fill = NA, label.color = NA,
                fontface="bold",family="serif",
                color = "white",size = 9, hjust = 0
  ) +
  
  #Subtitle Text
  
  geom_richtext(aes(x = "1985", y = 11, label = subtitle_text_gr),
                fill = NA, label.color = NA, color = "white",
                hjust = 0,
                size = 3.2,
                family = "serif") +
  
  
  ## Vertical segments
  
  geom_segment(aes(x = "1990", y = 0, xend = "1990", yend = total_vehicles_per_year$add[1] + 0.5),color="white") +
  geom_point(aes(x = "1990", y = total_vehicles_per_year$add[1] + 0.5),color="white") +
  geom_richtext(aes(x = "1990", y = total_vehicles_per_year$add[1] + 1.5,
                    label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[1],2)} </span>", " εκ.", "<br>",
                                 "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("1990", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("1990", "Motorcycle")$pct, "%)" , "<br>"                          )),
                fill = NA, label.color = NA,
                hjust=0.5,
                size=3,
                lineheight=1.1,
                fontface="bold",family="uc",
                color="white") +
  
  
  geom_segment(aes(x = "2000", y = 0, xend = "2000", yend = total_vehicles_per_year$add[2] + 0.5),color="white") +
  geom_point(aes(x = "2000", y = total_vehicles_per_year$add[2] + 0.5),color="white") +
  geom_richtext(aes(x = "2000", y = total_vehicles_per_year$add[2] + 1.5,
                    label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[2],2)} </span>", " εκ.", "<br>",
                                 "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("2000", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("2000", "Motorcycle")$pct, "%)" , "<br>" )),
                fill = NA, label.color = NA,
                hjust=0.5,
                size=3,
                lineheight=.8,
                fontface="bold",family="uc",
                color="white") +
  
  geom_segment(aes(x = "2010", y = 0, xend = "2010", yend = total_vehicles_per_year$add[3] + 0.1),color="white") +
  geom_point(aes(x = "2010", y = total_vehicles_per_year$add[3] + 0.1),color="white") +
  geom_richtext(aes(x = "2010", y = total_vehicles_per_year$add[3] + 1,
                    label = glue("<span style = 'font-family:uc; font-size:20px;'> {round(total_vehicles_per_year$add[3],2)} </span>", " εκ.", "<br>",
                                 "<span style = 'font-family:fs;'>&#xf1b9; </span> ", " (", get_yearly_data("2010", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs;'>&#xf21c; </span> ", " (", get_yearly_data("2010", "Motorcycle")$pct, "%)" , "<br>" )),
                fill = NA, label.color = NA,
                hjust=0.5,
                size=3,
                lineheight=.8,
                fontface="bold",family="uc",
                color="white") +
  
  geom_segment(aes(x = "2020", y = 0, xend = "2020", yend = total_vehicles_per_year$add[4] + 1),color="white") +
  geom_point(aes(x = "2020", y = total_vehicles_per_year$add[4] + 1),color="white") +
  geom_richtext(aes(x = "2020", y = total_vehicles_per_year$add[4] + 2.4,
                    label = glue("<span style = 'font-family:uc; font-size:20px; color: red;'> {round(total_vehicles_per_year$add[4],2)} </span>", " εκ.", "<br>",
                                 "<span style = 'font-family:fs; color:#2f4b7c;'>&#xf1b9; </span> ", " (", get_yearly_data("2020", "Car")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs; color:#f95d6a;'>&#xf21c; </span> ", " (", get_yearly_data("2020", "Motorcycle")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs; color:#a05195;'>&#xf0d1; </span> ", " (", get_yearly_data("2020", "Truck")$pct, "%)" , "<br>",
                                 "<span style = 'font-family:fs; color:#ffa600;'>&#xf207; </span> ", " (", get_yearly_data("2020", "Bus")$pct, "%)" , "<br>" )),
                fill = NA, label.color = NA,
                hjust=0.5,
                size=2.5,
                lineheight=1.3,
                fontface="bold",family="uc",
                color="white") +
  
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  scale_x_discrete(breaks=c(1990, 2000, 2010, 2020),labels = c("1990","2000","2010","2020")) +
  scale_y_continuous(expand = c(0,0),limits = c(0,15)) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(family = "serif",
                                  margin = margin(t = 10, b = 5), 
                                  hjust = 0.5, 
                                  color = "white",face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "serif",
                                     margin = margin(t = 5, l = 10, r = 10, b = 5),
                                     lineheight = 1.1,
                                     color = "white"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    plot.caption = element_markdown(family = "serif", margin = margin(t = 5, r = 5, b = 4), 
                                    lineheight = 1.4,
                                    color = "white", size = 8,
                                    hjust = 0.5),
    plot.margin = margin(l=8, r=8, t = 5),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_markdown(size = 8, family = "jost"),
    axis.text.x = element_markdown(color = "white"),
    axis.text.y = element_blank(),
    axis.line = element_line(linewidth = 0.2)
  )

ggsave(
  filename = "2024/day15/day15_dark-2024-cc-el.png",
  plot = p_black_gr,
  device = "png",
  height = 4,
  width = 6)
