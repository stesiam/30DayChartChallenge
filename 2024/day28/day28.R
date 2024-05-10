library(readr)
library(dplyr)

library(lubridate)
library(ggplot2)
library(glue)
library(ggtext)
library(showtext)
library(sysfonts)
library(ggimage)
library(geomtextpath)


sysfonts::font_add_google("Outfit", "title")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Jost", "jost")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

here::here("2024/day28")


trends <- read_csv("2024/day28/AnalyticsTrends.csv", 
                            skip = 1)

d = trends |>
  setNames(c("Month", "R", "SPSS", "MATLAB")) |>
  mutate(
    Month = lubridate::ym(Month),
    Year = year(Month)
  ) |>
  dplyr::relocate(Year, .before = "Month") |>
  select(-Month) |>
  tidyr::pivot_longer(
    cols = !c("Year")
  ) |>
  group_by(Year, name) |>
  summarise(
    mean = mean(value)
  ) |>
  mutate(
    image = case_when(
      name == "R" ~ "2024/day28/www/Rlang.png",
      name == "SPSS" ~ "2024/day28/www/spss.png",
      TRUE ~ "2024/day28/www/matlab.png"
    )
  )

## Viz texts

title_text = glue("Search Trends in Analytics Tools")
subtitle_text = glue("Investigating people's interest in specific analytics software or programming language,
                     I concluded that <br><b><span style='color:#019b98;'>R</span></b> (also known as <b><span style='color:#019b98;'>Rstats</span></b>) has finally prevailed in analytics field the last decade 
                     over  <b><span style='color:#F18F01;'>MATLAB</span></b> &  <b><span style='color:#dd0025;'>SPSS</span></b><br>
                     Of course we should take into consideration that Google's data are referring to search interest.
                     Other <br> Statistical Software (EViews, JASP, jamovi etc.) have not getting much searches, so
  data are not available.")
caption_text = "30 Day Chart Challenge, Day 28 (2024)<br><b>Data:</b> Google Trends<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"


bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Pillement")[5:6]))



te = ggplot(d ,aes(x = Year, y  = mean, group = name, color = name)) +
  geom_point(
    data = d %>% filter(Year == 2024), size = 10, aes(x = Year, y = mean, group = name,
                                                      color = name)) +
  geom_line(lwd = 3) +
  geom_textline(aes(x = Year, y = round(mean), 
                    color = name, label = name, group = name), size = 5, 
                fontface = 1, hjust = 0.21, vjust = -0.3, family = "title") +
  geom_image( data = d %>% filter(Year == 2024), aes(x=Year,y=mean,image=image, color = NULL), asp=2.2)+
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    y = "Search Interest"
    ) +
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2024)) +
  scale_y_continuous(n.breaks = 5,
                     limits = c(0, 100)) +
  scale_color_manual(
    values = c(
      "R" = "#019b98",
      "MATLAB" = "#F18F01",
      "SPSS" = "#dd0025"
  )) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(color = "white"),
    plot.title = element_markdown(color = "white", family = "jost", face = "bold",
                                  hjust = 0.5),
    plot.subtitle = element_markdown(family = "jost", size = 9.3, lineheight = 1.1),
    plot.caption = element_markdown(lineheight = 1.2, size = 7),
    plot.background = element_rect(fill = bg_gradient),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    legend.position = "none",
    axis.text = element_text(color = "white", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.5)
  )

ggsave(
  here::here("2024/day28/day28-2024-cc.png"), te, width =6, height = 4
)

