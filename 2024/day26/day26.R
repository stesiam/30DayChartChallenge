# Data Source: Kaggle Datasets
# Data URL: https://www.kaggle.com/datasets/muhammadtalhaawan/ai-5000-tools-2023/data

library(readr)
library(dplyr)
library(ggplot2)
library(glue)
library(ggtext)
library(showtext)
library(sysfonts)
library(forcats)
library(ggh4x)

## Load fonts
sysfonts::font_add_google("Outfit",       "outfit")
sysfonts::font_add_google("Jost",         "jost")
sysfonts::font_add_google("Noto Serif",   "noto_serif")
sysfonts::font_add_google("Noto Sans",    "noto_sans")
sysfonts::font_add_google("Inter",        "inter")
sysfonts::font_add_google("Roboto",       "roboto")
sysfonts::font_add_google("Ubuntu",       "ubuntu")
sysfonts::font_add_google("Lato",         "lato")
sysfonts::font_add_google("Source Sans 3","source_sans")
sysfonts::font_add_google("Open Sans",    "open_sans")
sysfonts::font_add_google("Crimson Pro",  "crimson")
sysfonts::font_add('fb', '/home/stelios/Documents/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Documents/otfs/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
showtext::showtext_opts(dpi = 300)

# --- Data ---
all_ai_tool <- read_csv("2024/day26/all_ai_tool.csv")

ai_data <- all_ai_tool |>
  setNames(c("Name", "Description", "ChargingType", "usedFor", "Charges",
             "Review", "URL", "Category")) |>
  tidyr::drop_na(Charges) |>
  mutate(
    ChargesPER = case_when(
      grepl("mo", Charges)                        ~ "Monthly",
      grepl("wk", Charges)                        ~ "Weekly",
      grepl("word|request|credit)", Charges)      ~ "Other",
      grepl("hour", Charges)                      ~ "Hourly",
      grepl("year", Charges) & grepl("yr", Charges) ~ "Yearly",
      grepl("second", Charges)                    ~ "second",
      grepl("free", tolower(Charges))             ~ "free",
      TRUE                                        ~ "Other"
    )
  ) |>
  mutate(
    Charges = stringr::str_remove_all(Charges, "\\$"),
    Charges = stringr::str_remove_all(Charges, "\\/.*"),
    Charges = stringr::str_remove_all(Charges, "[a-z, A-Z]"),
    Charges = stringr::str_remove_all(Charges, "-"),
    Charges = stringr::str_trim(Charges),
    Charges = as.numeric(Charges)
  ) |>
  tidyr::drop_na(Charges) |>
  dplyr::filter(ChargesPER == "Monthly", Charges > 0) |>
  group_by(Category) |>
  summarise(
    median = median(Charges),
    n      = n(),
    max    = max(Charges),
    min    = min(Charges),
    .groups = "drop"
  ) |>
  mutate(
    CatCharges = case_when(
      median < 11.6 ~ "$",
      median < 15.5 ~ "$$",
      TRUE          ~ "$$$"
    ),
    Category = stringr::str_to_title(Category),
    Category = ifelse(Category == "3d", "3D", Category)
  ) |>
  dplyr::filter(Category != "Other") |>
  arrange(factor(CatCharges, levels = c("$", "$$", "$$$")), Category) |>
  mutate(Category = factor(Category, levels = Category))

cat_labels_el <- c(
  "3D"       = "3D",
  "Audio"    = "Ήχος",
  "Business" = "Επιχειρήσεις",
  "Code"     = "Προγραμματισμός",
  "Image"    = "Εικόνα",
  "Text"     = "Κείμενο",
  "Video"    = "Βίντεο"
)

# --- Texts ---
en_title    <- glue("<b>Pricing of <span style='color:#FF7F50;'>AI</span> Tools by Purpose</b>")
en_subtitle <- glue(
  "<b>Artificial Intelligence</b> (AI) has already intervened in our lives and helped us make them easier. ",
  "Till today over 5000 AI tools have been recorded for various uses such as Email Preparation, ",
  "Grammar checking, Coding assistance, etc. Almost half of them offer paid plans. Focusing on those ",
  "services, I conclude that AI for <b>coding</b>, <b>video</b> and <b>business</b> purposes ",
  "<b><span style='color:#ff7f50;'>are the most expensive</span></b>."
)
en_caption  <- "<b>NOTE:</b> Pricing is on a monthly basis. Categorization is based on median charges.<br>30 Day Chart Challenge, Day 26 (2024) | <b>Data:</b> Kaggle Datasets<br><span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"

el_title    <- glue("<b>Τιμολόγηση εργαλείων <span style='color:#FF7F50;'>AI</span> ανά τομέα χρήσης</b>")
el_subtitle <- glue(
  "Η <b>Τεχνητή Νοημοσύνη</b> (AI) έχει ήδη παρεισφρήσει στις ζωές μας και έχει βοηθήσει να τις κάνει πιο εύκολες. ",
  "Μέχρι σήμερα έχουν καταγραφεί πάνω από 5000 εργαλεία AI με ποικίλες χρήσεις όπως συγγραφή email, ",
  "γραμματικός έλεγχος, βοήθεια στη συγγραφή κώδικα κτλ. Σχεδόν τα μισά προσφέρουν υπηρεσίες επί πληρωμή. ",
  "Επικεντρώνοντας σε αυτά, συμπεραίνω ότι τα εργαλεία για <b>προγραμματισμό</b>, <b>βίντεο</b> και ",
  "<b>επιχειρηματικούς</b> σκοπούς <b><span style='color:#ff7f50;'>είναι τα πιο κοστοβόρα</span></b>."
)
el_caption  <- "<b>ΣΗΜΕΙΩΣΗ:</b> Η τιμολόγηση είναι σε μηνιαία βάση. Η κατηγοριοποίηση βασίζεται στη διάμεση χρέωση.<br>30 Day Chart Challenge, Day 26 (2024) | <b>Δεδομένα:</b> Kaggle Datasets<br><span style='font-family:fb;'>&#xf09b;</span> <b>stesiam</b>, 2024"

# --- Layout ---
design <- c(
  "AABBCCDD
   #EEFFGG#"
)

# --- Plot function ---
make_plot <- function(plot_data, title_text, subtitle_text, caption_text,
                      title_font, body_font, subtitle_font = body_font,
                      bg, text_color, subtle_color, strip_size = 11,
                      dollar_colors, n_label = "Paid AI Tools", range_label = "Price Range") {

  ggplot(plot_data) +
    geom_text(
      aes(label = CatCharges, y = .07, x = 0, color = CatCharges),
      size = 5.5, hjust = 0.5, family = title_font
    ) +
    geom_richtext(
      aes(
        label = paste0("\\# ", n_label, ": ", n, "<br>", range_label, ": ", min, " – ", max, "$"),
        y = 0.35, x = 0, color = CatCharges
      ),
      fill = NA, label.color = NA, size = 2.7, hjust = 0.5, family = body_font
    ) +
    ggh4x::facet_manual(~Category, design = design) +
    scale_y_continuous(limits = c(0, 0.5)) +
    scale_color_manual(values = dollar_colors) +
    labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "", y = "") +
    theme_minimal(base_family = body_font) +
    theme(
      plot.background  = element_rect(fill = bg, color = bg),
      panel.background = element_rect(fill = bg, color = NA),
      axis.text        = element_blank(),
      panel.grid       = element_blank(),
      legend.position  = "none",
      plot.title       = element_markdown(color = text_color, family = title_font, hjust = 0.5,
                                          margin = margin(t = 5, b = 10), size = 13),
      plot.subtitle    = element_textbox_simple(size = 9, color = subtle_color,
                                                lineheight = 1.3, family = subtitle_font,
                                                margin = margin(b = 10)),
      plot.caption     = element_markdown(color = subtle_color, hjust = 0.5,
                                          lineheight = 1.3, size = 7,
                                          margin = margin(t = 5, b = 5)),
      panel.spacing.y  = unit(1.5, "lines"),
      strip.text       = element_markdown(color = text_color, face = "bold",
                                          size = strip_size, family = title_font)
    )
}

# --- Generate all 4 variants ---
plots <- list(
  list(
    lang = "en", theme = "light",
    plot_data    = ai_data,
    title_font   = "outfit", body_font = "jost",
    bg           = "#F8F4EF", text_color = "grey10",
    subtle_color = "grey40",
    dollar_colors = c("$" = "#2E8B57", "$$" = "#B8860B", "$$$" = "#CC5500"),
    n_label      = "Paid AI Tools", range_label = "Price Range",
    title = en_title, subtitle = en_subtitle, caption = en_caption
  ),
  list(
    lang = "en", theme = "dark",
    plot_data    = ai_data,
    title_font   = "outfit", body_font = "jost",
    bg           = "#1E1A17", text_color = "white",
    subtle_color = "grey70",
    dollar_colors = c("$" = "#61bc84", "$$" = "#ffffa1", "$$$" = "#FF7F50"),
    n_label      = "Paid AI Tools", range_label = "Price Range",
    title = en_title, subtitle = en_subtitle, caption = en_caption
  ),
  list(
    lang = "el", theme = "light",
    plot_data    = ai_data |> mutate(Category = factor(cat_labels_el[as.character(Category)], levels = cat_labels_el[levels(Category)])),
    title_font   = "noto_sans", body_font = "noto_sans", subtitle_font = "source_sans",
    bg           = "#F8F4EF", text_color = "grey10",
    subtle_color = "grey40", strip_size = 10,
    dollar_colors = c("$" = "#2E8B57", "$$" = "#B8860B", "$$$" = "#CC5500"),
    n_label      = "Αριθμός AI", range_label = "Εύρος τιμής",
    title = el_title, subtitle = el_subtitle, caption = el_caption
  ),
  list(
    lang = "el", theme = "dark",
    plot_data    = ai_data |> mutate(Category = factor(cat_labels_el[as.character(Category)], levels = cat_labels_el[levels(Category)])),
    title_font   = "noto_sans", body_font = "noto_sans", subtitle_font = "source_sans",
    bg           = "#1E1A17", text_color = "white",
    subtle_color = "grey70", strip_size = 10,
    dollar_colors = c("$" = "#61bc84", "$$" = "#ffffa1", "$$$" = "#FF7F50"),
    n_label      = "Αριθμός AI", range_label = "Εύρος τιμής",
    title = el_title, subtitle = el_subtitle, caption = el_caption
  )
)

for (p in plots) {
  plt <- make_plot(
    plot_data     = p$plot_data,
    title_text    = p$title,
    subtitle_text = p$subtitle,
    caption_text  = p$caption,
    title_font    = p$title_font,
    body_font     = p$body_font,
    subtitle_font = if (!is.null(p$subtitle_font)) p$subtitle_font else p$body_font,
    strip_size    = if (!is.null(p$strip_size)) p$strip_size else 11,
    bg            = p$bg,
    text_color    = p$text_color,
    subtle_color  = p$subtle_color,
    dollar_colors = p$dollar_colors,
    n_label       = p$n_label,
    range_label   = p$range_label
  )
  ggsave(
    filename = glue("2024/day26/day26-2024-{p$theme}-{p$lang}.png"),
    plot     = plt,
    device   = "png",
    height   = 4,
    width    = 6,
    dpi      = 300
  )
  message("Saved: day26-2024-", p$theme, "-", p$lang)
}
