library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(ggforce)
library(showtext)
library(sysfonts)

sysfonts::font_add_google("Roboto Condensed", "rc")
sysfonts::font_add('fb', '/home/stelios/Documents/otfs/Font Awesome 6 Brands-Regular-400.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

sm_sales <- 14.8

# Professional palette: distinct, non-clashing, brand-inspired
brand_colors <- c(
  "Sklavenitis" = "#C8963E",  # warm gold  (highlight brand)
  "LIDL"        = "#0050AA",  # LIDL blue  (replaces garish yellow)
  "AB"          = "#1F7A52",  # muted teal
  "Metro"       = "#C0392B",  # clean red
  "Masoutis"    = "#1A3A6B",  # dark navy  (distinct from LIDL blue)
  "Kritikos"    = "#8B3A3A",  # dark burgundy (distinct from Metro red)
  "Other"       = "#9EAAB7"   # visible grey (replaces near-invisible grey95)
)

df <- data.frame(
  Brand   = c("Sklavenitis", "LIDL", "AB", "Metro", "Masoutis", "Kritikos", "Other"),
  BrandGR = c("Σκλαβενίτης", "LIDL", "ΑΒ", "Metro", "Μασούτης", "Κρητικός", "Άλλο"),
  Sales   = c(5.35, 2, 1.94, 1.65, 1.15, 0.78,
              sm_sales - 5.35 - 2 - 1.94 - 1.65 - 1.15 - 0.78),
  stringsAsFactors = FALSE
)

# ggforce StatPie starts at π/2 (12 o'clock) and goes clockwise.
# mid_angle converts each slice midpoint to standard math angle
# (0 = right, positive = CCW) so cos/sin give correct Cartesian coords.
df <- df %>%
  mutate(
    pct       = Sales / sum(Sales),
    cum_pct   = cumsum(pct),
    prev_cum  = lag(cum_pct, default = 0),
    mid_pct   = (prev_cum + cum_pct) / 2,
    mid_angle = pi / 2 - mid_pct * 2 * pi,
    lx_in     = 1.04 * cos(mid_angle),
    ly_in     = 1.04 * sin(mid_angle),
    lx_out    = 1.22 * cos(mid_angle),
    ly_out    = 1.22 * sin(mid_angle),
    lx        = 1.30 * cos(mid_angle),
    ly        = 1.30 * sin(mid_angle),
    hjust     = case_when(
      cos(mid_angle) >  0.15 ~ 0,
      cos(mid_angle) < -0.15 ~ 1,
      TRUE                   ~ 0.5
    )
  )

make_plot <- function(title_text,
                      subtitle_text,
                      caption_text,
                      center_text,
                      label_col    = "Brand",
                      bn_suffix    = "bn",
                      bg           = "#F9F9F9",
                      text_color   = "grey15",
                      subtle_color = "grey45") {
  
  accent <- "#C8963E"
  
  # Build per-slice label using the requested name column
  plot_df <- df %>%
    mutate(slice_label = paste0(
      "<span style='color:", brand_colors[Brand], ";'>**", .data[[label_col]], "**</span><br>",
      round(pct * 100, 1), "% - €",
      round(Sales, 2), " ", bn_suffix
    ))
  
  ggplot(plot_df) +
    # Donut slices — r0 = 0.55 gives a wider hole for cleaner center text
    geom_arc_bar(
      aes(x0 = 0, y0 = 0, r0 = 0.55, r = 1, amount = Sales, fill = Brand),
      stat      = "pie",
      color     = bg,
      linewidth = 0.6
    ) +
    # Leader lines: connect slice edge to label anchor
    geom_segment(
      aes(x = lx_in, y = ly_in, xend = lx_out, yend = ly_out),
      color     = "grey65",
      linewidth = 0.3
    ) +
    # Computed-position labels
    geom_richtext(
      aes(x = lx, y = ly, label = slice_label, hjust = hjust),
      vjust       = 0.5,
      size        = 8 / .pt,
      family      = "rc",
      fill        = NA,
      label.color = NA,
      color       = text_color,
      lineheight  = 1.15
    ) +
    # Clean center: total market, no icon
    geom_richtext(
      x           = 0,
      y           = 0,
      label       = center_text,
      size        = 11 / .pt,
      family      = "rc",
      fill        = NA,
      label.color = NA,
      color       = text_color,
      hjust       = 0.5,
      vjust       = 0.5,
      lineheight  = 1.4
    ) +
    scale_fill_manual(values = brand_colors) +
    coord_fixed(xlim = c(-2.1, 2.1), ylim = c(-1.55, 1.55), expand = FALSE) +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text) +
    theme_void() +
    theme(
      plot.title       = element_markdown(
        family = "rc", size = 13, hjust = 0.5,
        margin = margin(t = 12, b = 4), color = text_color
      ),
      plot.subtitle    = element_markdown(
        family = "rc", size = 9, hjust = 0.5,
        margin = margin(b = 8), color = subtle_color, lineheight = 1.3
      ),
      plot.caption     = element_markdown(
        family = "rc", size = 7, hjust = 0.5,
        margin = margin(t = 6, b = 6), color = subtle_color, lineheight = 1.4
      ),
      plot.background  = element_rect(fill = bg, color = NA),
      panel.background = element_rect(fill = bg, color = NA),
      plot.margin      = margin(l = 12, r = 12, t = 5, b = 5),
      legend.position  = "none"
    )
}

accent <- "#C8963E"

# --- English texts ---
en_title    <- "**Supermarkets' Sales in Greece (2022)**"
en_subtitle <- glue("Total market: **€{sm_sales} billion**. ",
                    "<span style='color:{accent};'>**Sklavenitis**</span> ",
                    "leads with **36%** of the market.")
en_caption  <- paste0("30 Day Chart Challenge, Day 1 (2024) | ",
                      "**Data:** Panorama of Greek Supermarkets, selfservice.gr<br>",
                      "<span style='font-family:fb;'>&#xf09b;</span> **stesiam**, 2024")
en_center   <- paste0("**€", sm_sales, " bn**",
                      "<br><span style='font-size:7.5pt'>Total market (2022)</span>")

# --- Greek texts ---
gr_title    <- "**Τζίρος Ελληνικών Σουπερμάρκετ (2022)**"
gr_subtitle <- glue("Συνολικές πωλήσεις: **€{sm_sales} δις**. ",
                    "<span style='color:{accent};'>**Σκλαβενίτης**</span> ",
                    "με **36%** της αγοράς.")
gr_caption  <- paste0("30 Day Chart Challenge, Day 1 (2024) | ",
                      "**Δεδομένα:** Πανόραμα Ελληνικών Σουπερμάρκετ, selfservice.gr<br>",
                      "<span style='font-family:fb;'>&#xf09b;</span> **stesiam**, 2024")
gr_center   <- paste0("**€", sm_sales, " δις**",
                      "<br><span style='font-size:7.5pt'>Σύνολο αγοράς (2022)</span>")

# --- Generate all 4 variants ---
plots <- list(
  list(lang = "en", theme = "light", label_col = "Brand",   bn_suffix = "bn",
       bg = "#F9F9F9", text_color = "grey15", subtle_color = "grey45",
       title = en_title, subtitle = en_subtitle, caption = en_caption, center = en_center),
  list(lang = "en", theme = "dark",   label_col = "Brand",   bn_suffix = "bn",
       bg = "#1C1C1C", text_color = "grey90", subtle_color = "grey60",
       title = en_title, subtitle = en_subtitle, caption = en_caption, center = en_center),
  list(lang = "el", theme = "light", label_col = "BrandGR", bn_suffix = "δις",
       bg = "#F9F9F9", text_color = "grey15", subtle_color = "grey45",
       title = gr_title, subtitle = gr_subtitle, caption = gr_caption, center = gr_center),
  list(lang = "el", theme = "dark",   label_col = "BrandGR", bn_suffix = "δις",
       bg = "#1C1C1C", text_color = "grey90", subtle_color = "grey60",
       title = gr_title, subtitle = gr_subtitle, caption = gr_caption, center = gr_center)
)

for (p in plots) {
  plt <- make_plot(
    title_text    = p$title,
    subtitle_text = p$subtitle,
    caption_text  = p$caption,
    center_text   = p$center,
    label_col     = p$label_col,
    bn_suffix     = p$bn_suffix,
    bg            = p$bg,
    text_color    = p$text_color,
    subtle_color  = p$subtle_color
  )
  ggsave(
    filename = glue("2024/day01/day01-2024-{p$theme}-{p$lang}.png"),
    plot     = plt,
    device   = "png",
    height   = 4.5,
    width    = 6.5,
    dpi      = 300
  )
}

