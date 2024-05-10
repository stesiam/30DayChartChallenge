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

sysfonts::font_add_google("Outfit", "title")
sysfonts::font_add_google("Ubuntu Condensed", "uc")
sysfonts::font_add_google("Jost", "jost")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

# Import dataset

all_ai_tool <- read_csv("2024/day26/all_ai_tool.csv")


e=all_ai_tool |>
  setNames(c("Name", "Description", "ChargingType", "usedFor", "Charges", 
             "Review", "URL", "Category")) |>
  tidyr::drop_na(Charges) |>
  mutate(
    ChargesPER = case_when(
      grepl("mo", Charges) ~ "Monthly",
      grepl("wk", Charges) ~ "Weekly",
      grepl("word|request|credit)", Charges) ~ "Other",
      grepl("hour", Charges) ~ "Hourly",
      grepl("year", Charges) & grepl("yr", Charges) ~ "yearly",
      grepl("second", Charges) ~ "second",
      grepl("free", tolower(Charges)) ~ "free",
      TRUE ~ "Other"
  )) |>
  mutate(
    Charges = stringr::str_remove_all(Charges, "\\$"),
    Charges = stringr::str_remove_all(Charges, "\\/.*"),
    Charges = stringr::str_remove_all(Charges, "[a-z, A-Z]"),
    Charges = stringr::str_remove_all(Charges, "-"),
    Charges = stringr::str_trim(Charges),
    Charges = as.numeric(Charges)
) |>
  tidyr::drop_na(Charges) |>
  dplyr::filter(ChargesPER == "Monthly") |>
  dplyr::filter(
    Charges >0
  ) |>
  mutate(
    Charges = case_when(
      ChargesPER == "Yearly" ~ Charges/12,
      ChargesPER == "Weekly" ~ Charges*4,
      ChargesPER == "Hourly" ~ Charges*24*30,
      ChargesPER == "free" ~ Charges*0,
      ChargesPER == "second" ~ Charges*60*60*24*30,
      TRUE ~ Charges
    )
  )|>
  dplyr::filter(ChargesPER != "other") |>
  group_by(Category) |>
  summarise(median = median(Charges),
            q1 = quantile(Charges, probs = 0.25),
            q3 = quantile(Charges, probs = 0.75),
            n = n(),
            max = max(Charges),
            min = min(Charges)) |>
  mutate(CatCharges = case_when(
    median < 11.6 ~ "$",
    median < 15.5 ~ "$$",
    TRUE ~ "$$$"
  )) |>
  mutate(
    Category = stringr::str_to_title(Category)
  ) |>
  dplyr::filter(Category != "Other")



# Plot texts

title_text = glue("<b>Pricing of <span style='color:black;'>AI</span> Tools by Purpose</b>")
subtitle_text = glue("<b>Artificial Intelligence</b> (AI) has already intervened in our lifes and it has helped us make our lifes <br>easier.
                     Till today over 5000 AI tools have been recorded for various uses such as Email Preparation, <br>Grammar
                     checking, Coding assistance, etc. Almost half of them offer paid plans. Focusing on those <br>services, I conclude that
                     AI for <b>coding</b>, <b>video</b> and <b> business</b> purposes <b><span style='font-family:title; color:#ff7f50;' >are the most expensive </span></b>.")
caption_text = "*NOTE:* The pricing is on a monthly basis. Also the categorization of pricing is based on median charges.<br>30 Day Chart Challenge, Day 26 (2024) | <b> Data:</b> Kaggle Datasets <br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"



bg_gradient <- grid::linearGradient(colours = rev(MetBrewer::met.brewer("Kandinsky")[3:4]))


design <- c(
"AABBCCDD
 #EEFFGG#"
)
e
w = ggplot(e)+
  geom_text(aes(label = CatCharges, y = .07, x = 0, color = CatCharges), size = 5.5, hjust = 0.5, family ="title") +
  geom_richtext(aes(label = glue("\\# Paid AI Tools: {n} <br> Price Range: {min} - {max}$"
                                 ), y = 0.35, x = 0, color = CatCharges),
                fill = NA, label.color = NA,
                 size = 2.7, hjust = 0.5, family ="title") +
  ggh4x::facet_manual(~Category, design = design) +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_color_manual(
    values = c(
      "$" = "#61bc84",
      "$$" = "#ffffa1",
      "$$$" = '#FF7F50'
    )
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = bg_gradient),
    plot.title = element_markdown(color = "white", family = "jost", hjust = 0.5,
                                  margin = margin(t = 5, b = 10), size = 13),
    plot.subtitle = element_markdown(size = 10, color = "white", 
                                     lineheight = 1.2, family = "serif",
                                     margin = margin(b = 10)),
    plot.caption = element_markdown(color = "white", hjust = 0.5,
                                    lineheight = 1.3, size = 8,
                                    margin = margin(t = -5, b = 5)),
    panel.spacing.y = unit(1.5, "lines"),
    strip.text = element_markdown(color = "white", face = "bold", size = 12, family = "title"))

ggsave(
  "2024/day26/day26-2024-cc.png", w, width =6, height = 4
)

