library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(ggtext)
library(showtext)
library(sysfonts)
library(here)
library(tabulapdf)
library(waffle)
library(tidyr)
library(qpdf)

## Load fonts
sysfonts::font_add_google("Noto Serif", "noto_serif")
sysfonts::font_add_google("Noto Sans",  "noto_sans")
sysfonts::font_add('fb', '/home/stelios/Documents/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Documents/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

# --- Data ---
url = "https://www.unipi.gr/faculty/mbouts/anak/OS_22_23.pdf"
download.file(url,
              destfile = "2024/day23/sg22.pdf",
              method = "wget",
              extra = "--no-check-certificate")

pdf_subset('2024/day23/sg22.pdf',
           pages = 186:190, output = "2024/day23/subset.pdf")

statistics_tables <- extract_tables(
  file   = "2024/day23/subset.pdf",
  method = "decide",
  output = "tibble")

graduates_data <- statistics_tables[[5]] %>%
  setNames(c("Year", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "10Y", "11+", "AVGY")) %>%
  .[-c(1:2), ] %>%
  pivot_longer(!c("Year", "AVGY")) %>%
  mutate(groupY = case_when(
    name %in% c("4Y", "5Y", "6Y") ~ "Until6y",
    TRUE ~ "Over6y"
  )) |>
  select(-name) %>%
  mutate(Year = ifelse(Year == "", NA_character_, Year)) %>%
  drop_na() %>%
  mutate(
    AVGY = stringr::str_remove(AVGY, "έτη") %>% stringr::str_trim(),
    AVGY = stringr::str_replace(AVGY, "\\,", "\\."),
    AVGY = as.numeric(AVGY)
  ) |>
  dplyr::filter(Year != "2009-2022") |>
  mutate(
    value  = as.numeric(value),
    groupY = as.factor(groupY)
  ) %>%
  mutate(Year = stringr::str_replace(Year, ".*-", ""))

# --- Texts ---
en_title <- glue("<b>Number of Statistics' Graduates and <span style='color:#f8766d;'>Late Graduation</span></b>")
en_subtitle <- glue(
  "Completing a Statistics degree at the University of Piraeus takes far longer than it should. ",
  "In 2021, the average time-to-degree reached <b>7.9 years</b>, ",
  "nearly double the <b><span style='color:#00bfc4;'>6-year legal limit</span></b> now enforced by law. ",
  "Since 2017, <b><span style='color:#f8766d;'>more than half of graduates have exceeded that limit</span></b>, ",
  "risking expulsion before completing their degree. ",
  "Complaints about the lack of lecture notes, the difficulty of exams, and faculty conduct have gone largely unaddressed."
)
en_caption <- "<b>Data:</b> Study Guide 2022 - Department of Statistics & Insurance Science (Univ. of Piraeus)<br>30 Day Chart Challenge, Day 23 (2024) <br><span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"

el_title <- glue("<b>Αριθμός απόφοιτων Στατιστικής και <span style='color:#f8766d;'>Καθυστερημένη Αποφοίτηση</span></b>")
el_subtitle <- glue(
  "Η αποφοίτηση από το τμήμα Στατιστικής του Παν. Πειραιά διαρκεί πολύ περισσότερο από ό,τι θα έπρεπε. ",
  "Το 2021, η μέση διάρκεια σπουδών έφτασε τα <b>7.9 έτη</b>, ",
  "σχεδόν διπλάσια από το <b><span style='color:#00bfc4;'>νόμιμο όριο των 6 ετών</span></b>. ",
  "Από το 2017, <b><span style='color:#f8766d;'>πάνω από τους μισούς απόφοιτους ξεπερνούν αυτό το όριο</span></b>, ",
  "ρισκάροντας τη διαγραφή πριν λάβουν το πτυχίο τους. ",
  "Παράπονα για την έλλειψη σημειώσεων, τη δυσκολία εξετάσεων και τις συμπεριφορές καθηγητών παραμένουν σε μεγάλο βαθμό αναπάντητα."
)
el_caption <- "<b>Δεδομένα:</b> Οδηγός σπουδών (2022) - Τμήμα Στατιστικής και Ασφαλιστικής Επιστήμης (ΠΑ.ΠΕΙ.)<br>30 Day Chart Challenge, Day 23 (2024) <br><span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"

# --- Plot function ---
make_plot <- function(title_text, subtitle_text, caption_text,
                      title_font, body_font, bg, text_color, subtle_color, strip_color,
                      avgy_suffix = "yrs") {
  plot_data <- graduates_data %>%
    mutate(Year_strip = glue("{Year}<br><span style='font-size:5pt;'>{AVGY} {avgy_suffix}</span>"))

  pct_labels <- plot_data %>%
    group_by(Year_strip) %>%
    summarise(
      pct   = round(sum(value[groupY == "Over6y"]) / sum(value) * 100),
      top_y = ceiling(sum(value) / 8),
      .groups = "drop"
    ) %>%
    mutate(label = paste0(pct, "%"))

  ggplot(plot_data, aes(fill = groupY, values = value)) +
    geom_waffle(color = bg, size = .1, n_rows = 8, flip = TRUE) +
    geom_text(
      data        = pct_labels,
      aes(x = 4.5, y = top_y + 2, label = label),
      hjust       = 0.5,
      vjust       = 0,
      size        = 1.8,
      fontface    = "bold",
      color       = "#f8766d",
      family      = body_font,
      inherit.aes = FALSE
    ) +
    facet_wrap(~Year_strip, nrow = 1, strip.position = "bottom") +
    scale_fill_manual(values = c("Over6y" = "#f8766d", "Until6y" = "#00bfc4")) +
    scale_y_continuous(labels = function(x) x * 8, expand = c(0, 0)) +
    scale_x_discrete() +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
    coord_equal(clip = "off") +
    theme_minimal(base_family = body_font) +
    theme(
      plot.background   = element_rect(fill = bg, color = bg),
      panel.background  = element_rect(fill = bg, color = NA),
      plot.title        = element_markdown(size = 11, family = title_font, color = text_color,
                                           margin = margin(t = 6, b = 6)),
      plot.subtitle     = element_textbox_simple(size = 7, family = body_font, color = subtle_color,
                                                  lineheight = 1.3, margin = margin(t = 4, b = 6)),
      plot.caption      = element_markdown(size = 6, family = body_font, color = subtle_color,
                                           margin = margin(t = 10), lineheight = 1.1),
      axis.title        = element_blank(),
      legend.position   = "none",
      legend.background = element_rect(fill = bg),
      axis.text.x       = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      plot.margin       = margin(l = 10, r = 10, t = 8, b = 8),
      strip.text        = element_markdown(size = 6, family = body_font, color = strip_color,
                                           face = "bold", lineheight = 1.3)
    )
}

# --- Generate all 4 variants ---
plots <- list(
  list(lang = "en", theme = "light", title_font = "noto_serif", body_font = "noto_sans",
       bg = "#F8F4EF", text_color = "grey10", subtle_color = "grey45", strip_color = "grey30",
       avgy_suffix = "yrs",
       title = en_title, subtitle = en_subtitle, caption = en_caption),
  list(lang = "en", theme = "dark",  title_font = "noto_serif", body_font = "noto_sans",
       bg = "#1E1A17", text_color = "white",   subtle_color = "grey70", strip_color = "grey65",
       avgy_suffix = "yrs",
       title = en_title, subtitle = en_subtitle, caption = en_caption),
  list(lang = "el", theme = "light", title_font = "noto_serif", body_font = "noto_sans",
       bg = "#F8F4EF", text_color = "grey10", subtle_color = "grey45", strip_color = "grey30",
       avgy_suffix = "έτη",
       title = el_title, subtitle = el_subtitle, caption = el_caption),
  list(lang = "el", theme = "dark",  title_font = "noto_serif", body_font = "noto_sans",
       bg = "#1E1A17", text_color = "white",   subtle_color = "grey70", strip_color = "grey65",
       avgy_suffix = "έτη",
       title = el_title, subtitle = el_subtitle, caption = el_caption)
)

for (p in plots) {
  plt <- make_plot(
    title_text    = p$title,
    subtitle_text = p$subtitle,
    caption_text  = p$caption,
    title_font    = p$title_font,
    body_font     = p$body_font,
    bg            = p$bg,
    text_color    = p$text_color,
    subtle_color  = p$subtle_color,
    strip_color   = p$strip_color,
    avgy_suffix   = p$avgy_suffix
  )
  ggsave(
    filename = glue("2024/day23/day23-2024-{p$theme}-{p$lang}.png"),
    plot     = plt,
    device   = "png",
    height   = 4,
    width    = 6,
    dpi      = 300
  )
}
