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

## Load fonts
sysfonts::font_add_google("Outfit",        "outfit")
sysfonts::font_add_google("Jost",          "jost")
sysfonts::font_add_google("Noto Sans",     "noto_sans")
sysfonts::font_add_google("Source Sans 3", "source_sans")
sysfonts::font_add('fb', '/home/stelios/Documents/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Documents/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

# --- Data ---
trends <- read_csv("2024/day28/AnalyticsTrends.csv", skip = 1)

d <- trends |>
  setNames(c("Month", "R", "SPSS", "MATLAB")) |>
  mutate(
    Month = lubridate::ym(Month),
    Year  = year(Month)
  ) |>
  select(-Month) |>
  tidyr::pivot_longer(cols = !Year) |>
  group_by(Year, name) |>
  summarise(mean = mean(value), .groups = "drop") |>
  mutate(
    image = case_when(
      name == "R"    ~ "2024/day28/www/Rlang.png",
      name == "SPSS" ~ "2024/day28/www/spss.png",
      TRUE           ~ "2024/day28/www/matlab.png"
    )
  )

d_end <- d |> filter(Year == max(Year))

# --- Texts ---
en_title    <- "Search Trends in Analytics Tools"
en_subtitle <- glue(
  "Investigating people's interest in specific analytics software or programming language, ",
  "I concluded that <b><span style='color:#019b98;'>R</span></b> (also known as <b><span style='color:#019b98;'>Rstats</span></b>) ",
  "has finally prevailed in the analytics field over the last decade, surpassing ",
  "<b><span style='color:#F18F01;'>MATLAB</span></b> & <b><span style='color:#dd0025;'>SPSS</span></b>. ",
  "Note that Google data refer to search interest only. Other statistical software ",
  "(EViews, JASP, jamovi etc.) receive too few searches to appear."
)
en_caption  <- "30 Day Chart Challenge, Day 28 (2024)<br><b>Data:</b> Google Trends<br><span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"

el_title    <- "Τάσεις Αναζήτησης Εργαλείων Ανάλυσης"
el_subtitle <- glue(
  "Ερευνώντας το ενδιαφέρον για διάφορα λογισμικά ή γλώσσες προγραμματισμού ανάλυσης δεδομένων, ",
  "συμπεραίνω ότι η <b><span style='color:#019b98;'>R</span></b> (γνωστή και ως <b><span style='color:#019b98;'>Rstats</span></b>) ",
  "έχει καθιερωθεί στο πεδίο της ανάλυσης την τελευταία δεκαετία έναντι του ",
  "<b><span style='color:#F18F01;'>MATLAB</span></b> και του <b><span style='color:#dd0025;'>SPSS</span></b>. ",
  "Τα δεδομένα της Google αναφέρονται σε ενδιαφέρον αναζήτησης. Άλλα πακέτα ",
  "(EViews, JASP, jamovi κ.ά.) δεν λαμβάνουν αρκετές αναζητήσεις ώστε να εμφανιστούν."
)
el_caption  <- "30 Day Chart Challenge, Day 28 (2024)<br><b>Δεδομένα:</b> Google Trends<br><span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"

# --- Colors ---
tool_colors <- c("R" = "#019b98", "MATLAB" = "#F18F01", "SPSS" = "#dd0025")

# --- Plot function ---
make_plot <- function(title_text, subtitle_text, caption_text,
                      title_font, body_font, subtitle_font = body_font,
                      bg, text_color, subtle_color) {

  ggplot(d, aes(x = Year, y = mean, group = name, color = name)) +
    geom_line(lwd = 3) +
    geom_textline(
      aes(label = name),
      size = 5, fontface = 1, hjust = 0.21, vjust = 0, family = title_font
    ) +
    geom_point(data = d_end, size = 10) +
    geom_image(data = d_end, aes(image = image, color = NULL), asp = 2.2) +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2024)) +
    scale_y_continuous(n.breaks = 5, limits = c(0, 100)) +
    scale_color_manual(values = tool_colors) +
    theme_minimal(base_family = body_font) +
    theme(
      plot.background  = element_rect(fill = bg, color = bg),
      panel.background = element_rect(fill = bg, color = NA),
      plot.title       = element_text(color = text_color, family = title_font,
                                      face = "bold", hjust = 0.5,
                                      margin = margin(t = 6, b = 4), size = 13),
      plot.subtitle    = element_textbox_simple(size = 8.5, color = subtle_color,
                                                family = subtitle_font, lineheight = 1.3,
                                                margin = margin(b = 8)),
      plot.caption     = element_markdown(color = subtle_color, lineheight = 1.2,
                                          size = 7, margin = margin(t = 8)),
      legend.position  = "none",
      axis.text.x      = element_text(color = subtle_color, size = 10),
      axis.text.y      = element_blank(),
      axis.title       = element_blank(),
      panel.grid       = element_blank(),
      axis.line.x      = element_line(color = subtle_color, linewidth = 0.5),
      plot.margin      = margin(l = 10, r = 10, t = 8, b = 8)
    )
}

# --- Generate all 4 variants ---
plots <- list(
  list(
    lang = "en", theme = "light",
    title_font = "outfit", body_font = "jost",
    bg = "#F8F4EF", text_color = "grey10", subtle_color = "grey40",
    title = en_title, subtitle = en_subtitle, caption = en_caption
  ),
  list(
    lang = "en", theme = "dark",
    title_font = "outfit", body_font = "jost",
    bg = "#1E1A17", text_color = "white", subtle_color = "grey70",
    title = en_title, subtitle = en_subtitle, caption = en_caption
  ),
  list(
    lang = "el", theme = "light",
    title_font = "noto_sans", body_font = "noto_sans", subtitle_font = "source_sans",
    bg = "#F8F4EF", text_color = "grey10", subtle_color = "grey40",
    title = el_title, subtitle = el_subtitle, caption = el_caption
  ),
  list(
    lang = "el", theme = "dark",
    title_font = "noto_sans", body_font = "noto_sans", subtitle_font = "source_sans",
    bg = "#1E1A17", text_color = "white", subtle_color = "grey70",
    title = el_title, subtitle = el_subtitle, caption = el_caption
  )
)

for (p in plots) {
  plt <- make_plot(
    title_text    = p$title,
    subtitle_text = p$subtitle,
    caption_text  = p$caption,
    title_font    = p$title_font,
    body_font     = p$body_font,
    subtitle_font = if (!is.null(p$subtitle_font)) p$subtitle_font else p$body_font,
    bg            = p$bg,
    text_color    = p$text_color,
    subtle_color  = p$subtle_color
  )
  ggsave(
    filename = glue("2024/day28/day28-2024-{p$theme}-{p$lang}.png"),
    plot     = plt,
    device   = "png",
    height   = 4,
    width    = 6,
    dpi      = 300
  )
  message("Saved: day28-2024-", p$theme, "-", p$lang)
}
