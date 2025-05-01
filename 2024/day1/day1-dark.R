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

sm_sales = 14.8

data = data.frame(
  "Brand" = c("Sklavenitis", "LIDL", "AB", "Metro", "Masoutis", "Kritikos","Other"),
  "BrandGR" = c("Σκλαβενίτης", "LIDL", "ΑΒ", "Metro", "Μασούτης", "Κρητικός","Άλλο"),
  "Sales" = c(5.35, 2, 1.94, 1.65, 1.15, 0.78, sm_sales - 5.35 - 2 - 1.94 - 1.65 - 1.15 - 0.78),
  "col"  = c("#c6a464", "yellow4", "cyan4", "red", "blue4", "red4","grey95"),
  "Image" = c("https://www.sklavenitis.gr/favicon.ico", 
              "https://www.lidl-hellas.gr/static/assets/lidl-onlineshop-hellas-271731.svg", 
              "https://static.ab.gr/static/next/icons/favicon.png",
              "https://www.metrocashandcarry.gr/assets/img/logo_metro.svg",
              NA, NA, NA)
)

title_text = glue("<b>Supermarkets' Sales in Greece (2022)</b>")
subtitle_text = glue( "According to the latest survey of IELKA, the total sales of Greek 
    supermarkets are <br> amounted to € <b>14.8</b> bil. <b><span style = 'color: #c6a464;'>Sklavenitis</span></b> is by far the most
    prevalent of all its competitors <br> with € <b>5.35</b> bil. in sales (36% of total market)." )
caption_text = "30 Day Chart Challenge, Day 1 (2024) | <b> Data:</b> Panorama of The Greek Supermarkets, selfservice.gr<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"

title_text_gr = glue("<b>Τζίρος Ελληνικών Σουπερμάρκετ (2022)</b>")
subtitle_text_gr = glue( "Σύμφωνα με τη τελευταία έκθεση του ΙΕΛΚΑ, οι συνολικές πωλήσεις ανέρχονται <br> στα € <b>11.2</b> δις. Η αλυσίδα σουπερμάρκετ <b><span style = 'color: #c6a464;'>Σκλαβενίτης</span></b> είναι αυτή
                         με το μεγαλύτερο <br> τζίρο μεταξύ των ανταγωνιστών της με € <b>5.35</b> δις πωλήσεις (36% της αγοράς)." )
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
      amount = Sales,
      fill = Brand
    ),
    stat = "pie",
    color = "black"
    ) +
    geom_richtext(
      x = c(1.25, 0.3, -0.3, -0.75, -0.78, -0.65,-0.3),
      y = c(0, -0.72, -0.75, -0.3, 0.1, 0.43, 0.7),
      angle = c(rep(0, 5), -30, 0),
      color = c("white", rep("black",2), rep("grey", 3), "black"),
      aes(label = paste0(
        glue("<b>{Brand}</b><br>"),
        round(100 * Sales / sum(Sales), 1),
        "%"
      )),
      size = 9 / .pt,
      fill = NA,
      label.color = NA
    ) +
    geom_richtext(
      x = 0,
      y = 0,
      label = paste0(
        "<span style='font-family:fs; font-size:30pt;'  >&#xf07a;</span><br>",
        "<br><strong>EUR€ ",
        round(sum(data$Sales), 2),
        "<br>",
        "Billions",
        "</strong>"
      ),
      size = 14 / .pt,
      fill = NA,
      label.color = NA,
      color = "white"
      ) +
    geom_image(
      x = c(0.75, 0.35, -0.25, -1, -1.2, NA, NA),
      y = c(0.2, -1, -1, -0.5, 0.2, NA, NA),
      size = c(rep(0.1,3), 0.2, 0.2, 0.2, 0.2),
      aes(image = Image)
    ) +
    labs(
      title = custom_title,
      subtitle = custom_subtitle,
      caption = custom_caption
    ) +
    scale_x_continuous(expand = expansion(c(0.3, 0.5))) +
    scale_fill_manual(values = c("Sklavenitis" = "#c6a464", "LIDL" = "yellow3", "AB" ="cyan4", 
                                 "Metro" = "red3", "Masoutis" = "blue4", "Kritikos" = "red4",
                                 "Other" = "grey95")) +
    theme_void(base_size = 11.5)  +
    theme(
      plot.title = element_markdown(family = custom_font_title,
                                    margin = margin(t = 10, b = 5), 
                                    hjust = 0.5, 
                                    color = "white",face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = element_markdown(family = custom_font_subtitle,
                                       margin = margin(t = 5, l = 10, r = 10, b = 5),
                                       lineheight = 1.1,
                                       color = "white"),
      panel.background = element_rect(fill = "black", color = "black"),
      plot.background = element_rect(fill = "black"),
      plot.caption = element_markdown(family = custom_font_caption, margin = margin(t = 5, r = 5, b = 4), 
                                      lineheight = 1.4,
                                      color = "grey90", size = 8,
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
  filename = "2024/day1/day1-2024-cc-en-drk.png",
  plot = eng_viz,
  device = "png",
  height = 4,
  width = 6)


ggsave(
  filename = "2024/day1/day1-2024-cc-el-drk.png",
  plot = greek_viz,
  device = "png",
  height = 4,
  width = 6)

