library(ggplot2)
library(glue)
library(rvest)
library(dplyr)
library(tidyr)
library(ggimage)
library(ggtext)
library(stringr)
library(showtext)
library(sysfonts)

sysfonts::font_add_google("Oswald",           "oswald")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add('fb', '/home/stelios/Documents/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Documents/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

# --- Logo download helper ---
get_teams_emblem <- function(team_name) {
  wiki_name <- switch(team_name,
    "AEL" = "Athlitiki_Enosi_Larissa",
    "OFI" = "OFI_Crete",
    team_name
  )
  url       <- paste0("https://en.wikipedia.org/wiki/", wiki_name, "_F.C.")
  image_url <- read_html(url) |>
    html_element("body") |>
    html_element(".mw-content-container") |>
    html_element(".infobox") |>
    html_element("img") |>
    html_attr("src")
  download.file(
    paste0("https:", image_url),
    destfile = paste0("2024/day4/team_logos/", wiki_name, ".png")
  )
}

# --- Scrape ---
url <- "https://en.wikipedia.org/wiki/Greek_Football_Cup"

greek_cup_data <- url |>
  read_html() %>%
  html_element("body") %>%
  html_elements(".div-col") %>%
  html_elements("li") %>%
  html_text2() %>%
  as.data.frame() %>%
  setNames("Var1") %>%
  separate(Var1, into = c("Season", "Teams"), sep = ": ") %>%
  separate(Teams, into = c("Teams", "TimesWon"), sep = "\\(", fill = "right") %>%
  mutate(
    TimesWon = str_remove(TimesWon, "\\)"),
    Teams    = str_trim(Teams)
  )

greek_cup_data$Teams[greek_cup_data$Teams == "–"]               <- "None"
greek_cup_data$Teams[greek_cup_data$Teams == "Ethnikos"]         <- "Ethnikos_Piraeus"
greek_cup_data$Teams[greek_cup_data$Teams == "Ethnikos Piraeus"] <- "Ethnikos_Piraeus"
greek_cup_data$Teams[greek_cup_data$Teams == "AEK Athens"]       <- "AEK"

# --- Download missing logos ---
for (team in unique(greek_cup_data$Teams)) {
  if (team == "None") next
  wiki_name <- switch(team,
    "AEL" = "Athlitiki_Enosi_Larissa",
    "OFI" = "OFI_Crete",
    team
  )
  destfile <- paste0("2024/day4/team_logos/", wiki_name, ".png")
  if (file.exists(destfile)) next
  tryCatch(
    get_teams_emblem(team),
    error = function(e) message("Skipped: ", team, " — ", conditionMessage(e))
  )
}

# --- Logos lookup ---
logos <- data.frame(
  Teams = c("AEK", "Ethnikos_Piraeus", "Panathinaikos", "Olympiacos", "None",
            "Aris", "PAOK", "Iraklis", "Panionios", "Kastoria", "AEL", "OFI"),
  Logo  = c("AEK.png", "Ethnikos_Piraeus.png", "Panathinaikos.png", "Olympiacos.png",
            "", "Aris.png", "PAOK.png", "Iraklis.png", "Panionios.png", "Kastoria.png",
            "Athlitiki_Enosi_Larissa.png", "ofi.png")
)
logos$Logo <- paste0("2024/day4/team_logos/", logos$Logo)

# --- Join & compute stats ---
clean_data <- left_join(greek_cup_data, logos, by = "Teams") %>%
  filter(Teams != "None") %>%
  filter(as.integer(str_extract(Season, "^\\d+")) <= 2022)

freq_cups <- clean_data %>%
  group_by(Teams) %>%
  summarise(n = n()) %>%
  arrange(-n)

big5         <- c("Panathinaikos", "Olympiacos", "AEK", "PAOK", "Aris")
n_seasons    <- nrow(clean_data)
n_teams      <- nrow(freq_cups)
non_big5_n   <- freq_cups %>% filter(!Teams %in% big5) %>% pull(n) %>% sum()
non_big5_pct <- round(non_big5_n / n_seasons * 100)

season_range <- paste0(
  str_extract(min(clean_data$Season), "^\\d+"), " – ",
  str_extract(max(clean_data$Season), "^\\d+")
)

# --- Texts ---
en_title <- glue(
  "<b><span style='font-family:fs;'>&#xf091;</span> ",
  "Greek Football Cup</b> ({season_range}) ",
  "<span style='font-family:fs;'>&#xf091;</span>"
)
en_subtitle <- glue(
  "<b><span style='color:#D0061F;'>Olympiacos</span></b> leads with <b>{freq_cups$n[1]}</b> titles, ",
  "followed by <b><span style='color:#007841;'>Panathinaikos</span></b> ({freq_cups$n[2]}) ",
  "and <b><span style='color:#c8a951;'>AEK Athens</span></b> ({freq_cups$n[3]}). ",
  "Across {n_seasons} editions, {n_teams} clubs have lifted the trophy. ",
  "Only <b>{non_big5_pct}% ({non_big5_n} titles)</b> went to clubs outside the Big 5."
)
en_caption <- paste0(
  "30 Day Chart Challenge, Day 4 (2024) | <b>Data:</b> Wikipedia<br>",
  "<span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"
)

gr_title <- glue(
  "<b><span style='font-family:fs;'>&#xf091;</span> ",
  "Κυπελλούχοι Ελλάδας</b> ({season_range}) ",
  "<span style='font-family:fs;'>&#xf091;</span>"
)
gr_subtitle <- glue(
  "Ο <b><span style='color:#D0061F;'>Ολυμπιακός</span></b> πρωτοστατεί με <b>{freq_cups$n[1]}</b> κατακτήσεις, ",
  "ακολουθούμενος από τον <b><span style='color:#007841;'>Παναθηναϊκό</span></b> ({freq_cups$n[2]}) ",
  "και την <b><span style='color:#c8a951;'>ΑΕΚ</span></b> ({freq_cups$n[3]}). ",
  "Σε {n_seasons} διοργανώσεις, {n_teams} ομάδες έχουν κατακτήσει το Κύπελλο. ",
  "Μόνο <b>{non_big5_pct}% ({non_big5_n} κατακτήσεις)</b> ανήκουν σε ομάδες εκτός των 5 μεγάλων."
)
gr_caption <- paste0(
  "30 Day Chart Challenge, Day 4 (2024) | <b>Δεδομένα:</b> Wikipedia<br>",
  "<span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"
)

# --- Plot function ---
make_plot <- function(title_text, subtitle_text, caption_text, font,
                      bg, panel_bg, text_color, subtle_color, strip_color) {
  ggplot(clean_data, aes(0, 1)) +
    geom_image(aes(image = Logo), size = 0.51) +
    facet_wrap(~Season, nrow = 5) +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
    theme_void() +
    theme(
      plot.margin          = margin(l = 10, r = 10, t = 5, b = 5),
      panel.background     = element_rect(fill = panel_bg, color = NA),
      plot.background      = element_rect(fill = bg,       color = NA),
      plot.title.position  = "plot",
      strip.text           = element_markdown(
        size = 6.3, face = "bold", family = font,
        color = strip_color, margin = margin(b = 2)
      ),
      plot.title    = element_markdown(
        family = font, size = 13, hjust = 0.5, color = text_color,
        margin = margin(t = 10, b = 4)
      ),
      plot.subtitle = element_textbox_simple(
        family = font, size = 9, halign = 0.5, color = subtle_color,
        lineheight = 1.35, margin = margin(t = 2, b = 14),
        padding = margin(l = 10, r = 10)
      ),
      plot.caption  = element_markdown(
        family = font, size = 7, hjust = 0.5, color = subtle_color,
        lineheight = 1.4, margin = margin(t = 4, b = 2)
      )
    )
}

plots <- list(
  list(lang = "en", theme = "light", font = "oswald",
       bg = "#F9F9F9", panel_bg = "#F9F9F9", text_color = "grey10",
       subtle_color = "grey40", strip_color = "grey30",
       title = en_title, subtitle = en_subtitle, caption = en_caption),
  list(lang = "en", theme = "dark",   font = "oswald",
       bg = "black",   panel_bg = "black",   text_color = "white",
       subtle_color = "grey70", strip_color = "grey65",
       title = en_title, subtitle = en_subtitle, caption = en_caption),
  list(lang = "el", theme = "light", font = "uc",
       bg = "#F9F9F9", panel_bg = "#F9F9F9", text_color = "grey10",
       subtle_color = "grey40", strip_color = "grey30",
       title = gr_title, subtitle = gr_subtitle, caption = gr_caption),
  list(lang = "el", theme = "dark",   font = "uc",
       bg = "black",   panel_bg = "black",   text_color = "white",
       subtle_color = "grey70", strip_color = "grey65",
       title = gr_title, subtitle = gr_subtitle, caption = gr_caption)
)

for (p in plots) {
  plt <- make_plot(
    p$title, p$subtitle, p$caption, font = p$font,
    bg = p$bg, panel_bg = p$panel_bg, text_color = p$text_color,
    subtle_color = p$subtle_color, strip_color = p$strip_color
  )
  ggsave(
    filename = glue("2024/day4/day4-2024-{p$theme}-{p$lang}.png"),
    plot     = plt,
    device   = "png",
    height   = 4.5,
    width    = 7,
    dpi      = 300
  )
}
