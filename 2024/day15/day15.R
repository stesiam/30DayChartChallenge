library(dplyr)
library(ggplot2)
library(glue)
library(ggtext)
library(ggstream)
library(showtext)
library(sysfonts)
library(readxl)

sysfonts::font_add_google("Jost",             "jost")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Roboto Condensed", "rc")
sysfonts::font_add('fb', '/home/stelios/Documents/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Documents/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

# --- Data ---
url <- "https://www.statistics.gr/el/statistics?p_p_id=documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd&p_p_lifecycle=2&p_p_state=normal&p_p_mode=view&p_p_cacheability=cacheLevelPage&p_p_col_id=column-2&p_p_col_count=4&p_p_col_pos=3&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_javax.faces.resource=document&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_ln=downloadResources&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_documentID=116414&_documents_WAR_publicationsportlet_INSTANCE_Mr0GiQJSgPHd_locale=el"
download.file(url, destfile = "2024/day15/data.xlsx")

vehicles_in_operation <- read_excel("2024/day15/data.xlsx", range = "A3:V37")

dataset_part_1 <- vehicles_in_operation[1:16, -1]
dataset_part_2 <- vehicles_in_operation[18:34, -1]

colnames(dataset_part_2) <- dataset_part_2[1, ]
dataset_part_2 <- dataset_part_2[-1, ]

names(dataset_part_1)[1] <- "Category and use"
dataset_part_2 <- dataset_part_2[, -ncol(dataset_part_2)]

bind_dataset <- dplyr::bind_cols(dataset_part_1, dataset_part_2)

dataset <- bind_dataset %>%
  slice(c(1, 2, 6, 9, 12)) %>%
  rename(category = `Category and use`) %>%
  mutate(category = c("Total", "Car", "Bus", "Motorcycle", "Truck")) %>%
  filter(category != "Total")

tidy_dataset <- dataset %>%
  tidyr::pivot_longer(cols = !category, names_to = "Year", values_to = "Obs") %>%
  mutate(Obs = as.numeric(Obs))

pal <- c("#ffa600", "#2f4b7c", "#f95d6a", "#a05195")

total_vehicles_per_year <- tidy_dataset %>%
  filter(Year %in% c("1990", "2000", "2010", "2020")) %>%
  group_by(Year) %>%
  summarise(add = sum(Obs) / 1e6)

get_yearly_data <- function(year_int, category_int) {
  tidy_dataset %>%
    filter(Year == year_int) %>%
    mutate(Obs = Obs / 1e6) %>%
    group_by(Year) %>%
    mutate(sum = sum(Obs), pct = round(Obs / sum * 100, 2)) %>%
    filter(category == category_int)
}

# --- Pre-compute annotation labels (shared across variants) ---
ann <- function(year, unit) {
  idx <- which(c("1990", "2000", "2010", "2020") == year)
  val <- round(total_vehicles_per_year$add[idx], 2)
  col <- if (year == "2020") "color:red;" else ""
  base <- glue("<span style='font-family:uc; font-size:20px; {col}'> {val} </span> {unit}<br>",
               "<span style='font-family:fs;'>&#xf1b9; </span> ({get_yearly_data(year,'Car')$pct}%)<br>",
               "<span style='font-family:fs;'>&#xf21c; </span> ({get_yearly_data(year,'Motorcycle')$pct}%)<br>")
  if (year == "2020")
    paste0(base,
           "<span style='font-family:fs; color:#a05195;'>&#xf0d1; </span> (", get_yearly_data("2020", "Truck")$pct, "%)<br>",
           "<span style='font-family:fs; color:#ffa600;'>&#xf207; </span> (", get_yearly_data("2020", "Bus")$pct, "%)<br>")
  else
    base
}

# --- Plot function ---
make_plot <- function(title_text, subtitle_text, caption_text,
                      title_font, unit,
                      label_car, label_motorcycle, label_truck,
                      bg, text_color) {

  tvpy <- total_vehicles_per_year

  ggplot(tidy_dataset, aes(x = Year, y = Obs / 1e6, fill = category,
                            group = category, color = category)) +
    geom_stream(type = "ridge", bw = 1) +
    labs(caption = caption_text) +

    # Stream labels
    annotate("text", x = "2015", y = 6.2, label = label_car,        hjust = 1.1, size = 3, color = text_color, family = "uc") +
    annotate("text", x = "2015", y = 0.8, label = label_truck,      hjust = 1.1, size = 3, color = text_color, family = "uc") +
    annotate("text", x = "2016", y = 2.2, label = label_motorcycle, hjust = 1.1, size = 3, color = text_color, family = "uc") +

    # Title + subtitle inside plot
    geom_richtext(
      data = data.frame(x = "1985", y = 13.5, lbl = title_text),
      aes(x = x, y = y, label = lbl),
      inherit.aes = FALSE,
      fill = NA, label.color = NA, fontface = "bold",
      family = title_font, color = text_color, size = 9, hjust = 0
    ) +
    geom_richtext(
      data = data.frame(x = "1985", y = 11, lbl = subtitle_text),
      aes(x = x, y = y, label = lbl),
      inherit.aes = FALSE,
      fill = NA, label.color = NA,
      family = title_font, color = text_color, size = 3.2, hjust = 0
    ) +

    # 1990
    annotate("segment", x = "1990", y = 0, xend = "1990", yend = tvpy$add[1] + 0.5, color = text_color) +
    annotate("point",   x = "1990", y = tvpy$add[1] + 0.5, color = text_color) +
    geom_richtext(
      data = data.frame(x = "1990", y = tvpy$add[1] + 1.5, lbl = ann("1990", unit)),
      aes(x = x, y = y, label = lbl),
      inherit.aes = FALSE,
      fill = NA, label.color = NA, hjust = 0.5, size = 3,
      lineheight = 1.1, fontface = "bold", family = "uc", color = text_color) +

    # 2000
    annotate("segment", x = "2000", y = 0, xend = "2000", yend = tvpy$add[2] + 0.5, color = text_color) +
    annotate("point",   x = "2000", y = tvpy$add[2] + 0.5, color = text_color) +
    geom_richtext(
      data = data.frame(x = "2000", y = tvpy$add[2] + 1.5, lbl = ann("2000", unit)),
      aes(x = x, y = y, label = lbl),
      inherit.aes = FALSE,
      fill = NA, label.color = NA, hjust = 0.5, size = 3,
      lineheight = 0.8, fontface = "bold", family = "uc", color = text_color) +

    # 2010
    annotate("segment", x = "2010", y = 0, xend = "2010", yend = tvpy$add[3] + 0.1, color = text_color) +
    annotate("point",   x = "2010", y = tvpy$add[3] + 0.1, color = text_color) +
    geom_richtext(
      data = data.frame(x = "2010", y = tvpy$add[3] + 1, lbl = ann("2010", unit)),
      aes(x = x, y = y, label = lbl),
      inherit.aes = FALSE,
      fill = NA, label.color = NA, hjust = 0.5, size = 3,
      lineheight = 0.8, fontface = "bold", family = "uc", color = text_color) +

    # 2020
    annotate("segment", x = "2020", y = 0, xend = "2020", yend = tvpy$add[4] + 1, color = text_color) +
    annotate("point",   x = "2020", y = tvpy$add[4] + 1, color = text_color) +
    geom_richtext(
      data = data.frame(x = "2020", y = tvpy$add[4] + 2.4, lbl = ann("2020", unit)),
      aes(x = x, y = y, label = lbl),
      inherit.aes = FALSE,
      fill = NA, label.color = NA, hjust = 0.5, size = 2.5,
      lineheight = 1.3, fontface = "bold", family = "uc", color = text_color) +

    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    scale_x_discrete(breaks = c(1990, 2000, 2010, 2020),
                     labels = c("1990", "2000", "2010", "2020")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
    theme_minimal() +
    theme(
      plot.background  = element_rect(fill = bg, color = bg),
      panel.background = element_rect(fill = bg, color = bg),
      plot.caption     = element_markdown(
        family = title_font, margin = margin(t = 5, r = 5, b = 4),
        lineheight = 1.4, color = text_color, size = 8, hjust = 0.5
      ),
      plot.margin     = margin(l = 8, r = 8, t = 5),
      legend.position = "none",
      panel.grid      = element_blank(),
      axis.title      = element_blank(),
      axis.text       = element_blank(),
      axis.line       = element_blank()
    )
}

# --- Texts ---
subtitle_en <- glue(
  "We observe that in recent years<br>",
  "there has been a ",
  "<span style='font-family:uc; color:red;'>3x</span>",
  "<span style='font-family:fs; color:red;'> &#xe098;</span> of wheeled vehicles.<br>",
  "The most notable change is<br>",
  "the doubling in the proportion of motorcycles."
)
caption_en <- paste0(
  "30 Day Chart Challenge, Day 15 (2024) | <b>Data:</b> Hellenic Statistical Authority<br>",
  "<span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"
)

subtitle_gr <- glue(
  "Παρατηρούμε ότι τα πρόσφατα χρόνια<br>",
  "υπάρχει ",
  "<span style='font-family:uc; color:red;'>3x</span>",
  "<span style='font-family:fs; color:red;'> &#xe098;</span> των συνολικών οχημάτων.<br>",
  "Η πιο αξιοσημείωτη αλλαγή είναι<br>",
  "ο διπλασιασμός του ποσοστού των μηχανών."
)
caption_gr <- paste0(
  "30 Day Chart Challenge, Day 15 (2024) | <b>Δεδομένα:</b> Ελληνική Στατιστική Υπηρεσία<br>",
  "<span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"
)

# --- Generate plots ---
plots <- list(
  list(lang = "en", theme = "light", title_font = "jost", unit = "mil.",
       label_car = "Cars", label_motorcycle = "Motorcycle", label_truck = "Truck",
       bg = "#F9F9F9", text_color = "black",
       title = "Vehicles in Greece", subtitle = subtitle_en, caption = caption_en),
  list(lang = "en", theme = "dark",   title_font = "jost", unit = "mil.",
       label_car = "Cars", label_motorcycle = "Motorcycle", label_truck = "Truck",
       bg = "black", text_color = "white",
       title = "Vehicles in Greece", subtitle = subtitle_en, caption = caption_en),
  list(lang = "el", theme = "light", title_font = "rc",   unit = "εκ.",
       label_car = "Αυτοκίνητα", label_motorcycle = "Μηχανές", label_truck = "Φορτηγά",
       bg = "#F9F9F9", text_color = "black",
       title = "Οχήματα στην Ελλάδα", subtitle = subtitle_gr, caption = caption_gr),
  list(lang = "el", theme = "dark",   title_font = "rc",   unit = "εκ.",
       label_car = "Αυτοκίνητα", label_motorcycle = "Μηχανές", label_truck = "Φορτηγά",
       bg = "black", text_color = "white",
       title = "Οχήματα στην Ελλάδα", subtitle = subtitle_gr, caption = caption_gr)
)

for (p in plots) {
  plt <- make_plot(
    title_text = p$title, subtitle_text = p$subtitle, caption_text = p$caption,
    title_font = p$title_font, unit = p$unit,
    label_car = p$label_car, label_motorcycle = p$label_motorcycle, label_truck = p$label_truck,
    bg = p$bg, text_color = p$text_color
  )
  ggsave(
    filename = glue("2024/day15/day15-2024-{p$theme}-{p$lang}.png"),
    plot     = plt,
    device   = "png",
    height   = 4,
    width    = 6,
    dpi      = 300
  )
}
