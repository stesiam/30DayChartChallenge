library(dplyr)
library(ggplot2)
library(ggtext)
library(glue)
library(ggforce)

library(showtext)
library(sysfonts)

## Load fonts

sysfonts::font_add_google("Oswald", "title")
sysfonts::font_add_google("Josefin Slab","js")
sysfonts::font_add_google("Cabin Condensed","cc")
sysfonts::font_add_google("Ubuntu Condensed", "uc")

#sysfonts::font_add_google("Gentium Plus", "gp")
sysfonts::font_add('gp',"/home/stelios/Downloads/Gentium_Plus/GentiumPlus-Regular.ttf")

sysfonts::font_add('fb', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fs', '/home/stelios/Downloads/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
showtext::showtext_opts(dpi = 300)

## Data

# Source: https://selfservice.gr/panorama-ton-ellinikon-souper-market-2023-i-chrimatooikonomiki-eikona-42-alysidon-souper-market-to-2022/

sklavenitis_sales_2022 = 3714124000
sm_sales = 11417257000

col  = c("grey90", "#c6a464")

ratio = sklavenitis_sales_2022/sm_sales

data = data.frame(
  "Brand" = c("Sklavenitis", "Other"),
  "Value" = c(ratio, 1-ratio),
  "Sales" = c(sklavenitis_sales_2022, sm_sales - sklavenitis_sales_2022),
  "col"  = c("#c6a464", "grey95")
)

title_text = glue("<b>Supermarkets' Sales in Greece (2022)</b>")
subtitle_text = glue( "According to the latest survey of IELKA, the total sales of Greek 
    supermarkets are <br> amounted to € <b>11.2</b> bil. <b><span style = 'color: #c6a464;'>Sklavenitis</span></b> is by far the most
    prevalent of all its competitors <br> with € <b>3.7</b> bil. in sales (32% of total market)." )
caption_text = "30 Day Chart Challenge, Day 1 (2024) | <b> Data:</b> Panorama of The Greek Supermarkets, selfservice.gr<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"

title_text_gr = glue("<b>Τζίρος Ελληνικών Σουπερμάρκετ (2022)</b>")
subtitle_text_gr = glue( "Σύμφωνα με τη τελευταία έκθεση του ΙΕΛΚΑ, οι συνολικές πωλήσεις ανέρχονται <br> στα € <b>11.2</b> δις. Η αλυσίδα σουπερμάρκετ <b><span style = 'color: #c6a464;'>Σκλαβενίτης</span></b> είναι αυτή
                         με το μεγαλύτερο <br> τζίρο μεταξύ των ανταγωνιστών της με € <b>3.7</b> δις πωλήσεις (32% της αγοράς)." )
caption_text_gr = "30 Day Chart Challenge, Day 1 (2024) <br> <b> Δεδομένα:</b> Πανόραμα ελληνικών σουπερμάρκετ, selfservice.gr<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"


get_plot = function(custom_title, custom_subtitle, custom_caption,
                    custom_font_title = "serif",
                    custom_font_subtitle = "serif",
                    custom_font_caption = "serif",
                    custom_text = "Total Sales",
                    custom_metric = "Bn"){
  ggplot(data) +
    geom_arc_bar(aes(
      x0 = 0,
      y0 = 0,
      r0 = 0.6,
      r = 1,
      amount = Value,
      fill = Brand
    ),
    stat = "pie",
    size = 1,
    color = "#FFFFFF"
    ) +
    geom_richtext(
      x = c(1.25, -1.25),
      y = c(-0.3, 0.3),
      aes(label = paste0(
        glue("<b>{Brand}</b><br>"),
        "<strong>EUR€ ",
        round(Sales/1e9, 2),
        " ",
        custom_metric,
        "</strong>", "<br>",
        round(100 * Value / sum(Value), 2),
        "%"
      )),
      size = 10 / .pt,
      fill = NA,
      label.color = NA
    ) +
    geom_richtext(
      x = 0,
      y = 0,
      label = paste0(
        "<span style='font-family:fs; color:#222021; font-size:30pt;'  >&#xf07a;</span><br>",
        "<br><strong>EUR€ ",
        round(sum(data$Sales) / 1e9, 2),
        "<br>",
        "Billions",
        "</strong>"
      ),
      size = 14 / .pt,
      fill = NA,
      label.color = NA
    ) +
    labs(
      title = custom_title,
      subtitle = custom_subtitle,
      caption = custom_caption
    ) +
    scale_x_continuous(expand = expansion(c(0.3, 0.5))) +
    scale_fill_manual(values = c("Sklavenitis" = "#c6a464",
                                 "Other" = "grey80")) +
    theme_void(base_size = 11.5)  +
    theme(
      plot.title = element_markdown(family = custom_font_title,
                                    margin = margin(t = 10, b = 5), 
                                    hjust = 0.5, 
                                    color = "black",face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = element_markdown(family = custom_font_subtitle,
                                       margin = margin(t = 5, l = 10, r = 10, b = 5),
                                       lineheight = 1.1,
                                       color = "black"),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.caption = element_markdown(family = custom_font_caption, margin = margin(t = 5, r = 5, b = 4), 
                                      lineheight = 1.4,
                                      color = "black", size = 8,
                                      hjust = 0.5),
      plot.margin = margin(l=8, r=8),
      legend.position = "none"
    )
}

greek_viz = get_plot(custom_title = title_text_gr,
                     custom_subtitle = subtitle_text_gr,
                     custom_caption = caption_text_gr,
                     custom_text = "Πωλήσεις",
                     custom_metric = "δις")

eng_viz = get_plot(custom_title = title_text,
                               custom_subtitle = subtitle_text,
                               custom_caption = caption_text,
                   "js", "serif", "title")


ggsave(
  filename = "2024/day1/day1-2024-cc-en.png",
  plot = eng_viz,
  device = "png",
  height = 4,
  width = 6)


ggsave(
  filename = "2024/day1/day1-2024-cc-el.png",
  plot = greek_viz,
  device = "png",
  height = 4,
  width = 6)

