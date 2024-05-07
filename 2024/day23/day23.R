library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(ggtext)
library(showtext)
library(sysfonts)
library(here)
library(tabulizer)
library(waffle)
library(tidyr)
library(qpdf)



## Load fonts

sysfonts::font_add_google("Outfit", "title")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Jost", "jost")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)


url = "https://www.unipi.gr/faculty/mbouts/anak/OS_22_23.pdf"
# 
download.file(url,
              destfile = "2024/day23/sg22.pdf",
              method = "wget",
              extra = "--no-check-certificate")

pdf_subset('2024/day23/sg22.pdf',
           pages = 186:190,  output = "2024/day23/subset.pdf")

statistics_tables <- extract_tables(
  file   = "2024/day23/subset.pdf", 
  method = "decide", 
  output = "data.frame")

graduates_data = statistics_tables[[5]] %>%
  setNames(c("Year", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "10Y", "11+","AVGY")) %>%
  .[-c(1:2),] %>%
  pivot_longer(!c("Year", "AVGY")) %>%
  mutate(groupY = case_when(
    name %in% c("4Y", "5Y", "6Y")  ~ "Until6y",
    TRUE ~ "Over6y"
  )) |>
  select(c(-name)) %>%
  mutate(
    Year = ifelse(Year == "", NA_character_, Year)
  ) %>%
  drop_na() %>%
  mutate(
    AVGY = stringr::str_remove(AVGY,"έτη") %>% stringr::str_trim(),
    AVGY = stringr::str_replace(AVGY, "\\,", "\\."),
    AVGY = as.numeric(AVGY)
  ) |>
  dplyr::filter(Year != "2009-2022") |>
  mutate(
    value = as.numeric(value),
    groupY = as.factor(groupY)
  ) %>%
  mutate(
    Year = stringr::str_replace(Year, ".*-", "")
  )

title_text = glue("<b>Number of Statistics' Graduates and <span style='color:#f8766d;'>Late Graduation</span></b>")
subtitle_text = glue("Historically, Statistics' Graduates are struggling to complete our studies.
                     In 2019 the average graduation was<br> at the staggering 7.8 years.
                     A new law imposed limits on our studies <b><span style='color:#00bfc4;'>up to 6 years</span></b>. By the end of the next <br> academic year it is expected to start the deletion of students that have exceeded that limit.
                     From 2016-17, <b><span style='color:#f8766d;'>over <br>the half of graduates have exceeded that limit</span></b>. So, it is expected the following years
                     students that would <br> have taken their degree to not be able to as they will have been deleted from our dept's undergrad register.<br>
                     Over the years several complaints have been done concerning lack of lecture notes, exams' difficulty and <br> profs' behavior
                     but they have been ignoring those till today <span style='font-family:fs; font-size:8px; color:#fed053;'  >&#xf165;</span>")
caption_text = "<b> Data:</b> Study Guide 2022 - Department of Statistics & Insurance Science (Univ. of Piraeus)<br>30 Day Chart Challenge, Day 23 (2024) <br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"

graduates_plot = ggplot(graduates_data, aes(fill = groupY, values = value)) +
  geom_waffle(color = "black", size = .1, n_rows = 8, flip = TRUE) +
  facet_wrap(~Year, nrow = 1, strip.position = "bottom") +
  scale_y_continuous(labels = function(x) x * 8, # make this multiplyer the same as n_rows
                     expand = c(0,0))+
  scale_x_discrete() + 
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  coord_equal()+
  theme_minimal(base_family = "title") +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_markdown(family = "jost", color = "white"),
    plot.subtitle = element_markdown(size = 8.3, family = "jost", color = "white"),
    plot.caption = element_markdown(size = 6, family = "uc", color = "white", 
                                    margin = margin(t = 10),lineheight = 1.1),
    axis.title = element_blank(),
    legend.position = "none",
    legend.background = element_rect(fill = "black"),
    axis.text.x = element_markdown(family = title, size=2, color = "white"),
    panel.grid = element_blank(), 
    strip.text = element_markdown(size = 6, color = "white"))


ggsave("2024/day23/day23-2024-cc.png", graduates_plot, 
       device = "png",
       height = 4,
       width = 6)

