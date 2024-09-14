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


# Custom function to get club emblems
get_teams_emblem = function(team_name){
  if (team_name == "AEL"){
    team_name = "Athlitiki_Enosi_Larissa"
  } else if (team_name == "OFI"){
    team_name = "OFI_Crete" 
  }
  url = paste0("https://en.wikipedia.org/wiki/", team_name, "_F.C.")
  
  image_url = read_html(url) |>
    html_element("body") |>
    html_element(".mw-content-container") |>
    html_element(".infobox") |>
    html_element("img") |>
    html_attr("src")
  
  x = paste0("https:", image_url)
  download.file(x, destfile = paste0("2024/day2/team_logos/", team_name, ".png"))
}


url = "https://en.wikipedia.org/wiki/Greek_Football_Cup"


read_html(url)


greek_cup_data = url |>
  read_html() %>%
  html_element("body") %>%
  html_elements(".div-col") %>%
  html_elements("li") %>%
  html_text2() %>%
  as.data.frame() %>%
  setNames(c("Var1")) %>%
  separate(Var1, into = c("Season", "Teams"), sep = ": ") %>%
  separate(Teams, into = c("Teams", "TimesWon"), sep = "\\(") %>%
  mutate(
    TimesWon = stringr::str_remove(TimesWon, "\\)"),
    Teams = stringr::str_trim(Teams)
  ) %>%
  filter(Season != "2023–24")
greek_cup_data$Teams[greek_cup_data$Teams == "–"] <- "None"
greek_cup_data$Teams[greek_cup_data$Teams == "Ethnikos"] <- "Ethnikos_Piraeus"
  

teams = greek_cup_data$Teams %>% unique()
for (team in teams){
  if(team == "None"){
    next
  }
  get_teams_emblem(team)
}

# Make data frame with emblems

logos = data.frame(
  "Teams" = c("AEK", "Ethnikos_Piraeus", "Panathinaikos", "Olympiacos", "None",
              "Aris", "PAOK", "Iraklis", "Panionios", "Kastoria","AEL","OFI"),
  "Logo" = c("AEK.png", "Ethnikos_Piraeus.png", "Panathinaikos.png", "Olympiacos.png",
             "", "Aris.png", "PAOK.png", "Iraklis.png", "Panionios.png", "Kastoria.png",
             "Athlitiki_Enosi_Larissa.png", "ofi.png")
)
dest = "2024/day4/team_logos/"
logos$Logo = paste0(dest, logos$Logo)


## Join Logo data frame with data 

clean_data = left_join(x = greek_cup_data, y = logos, by = "Teams") %>%
 dplyr::filter(Teams != "None")
clean_data$id = rep(20:1, 4)
## Viz

freq_cups_won_by_team = clean_data %>%
  group_by(Teams) %>%
  summarise(n = n()) %>%
  arrange(-n) 

freq_cups_won_by_team_big5 = freq_cups_won_by_team %>%
  mutate(
    Big5 = ifelse(Teams %in% c("Panathinaikos", "Olympiacos", "AEK",
                               "PAOK", "Aris"), TRUE, FALSE),
    total_cups = sum(n)
  ) %>%
  group_by(Big5) %>%
  summarise(pct = sum(n)/total_cups)

freq_cups_won_by_team$n[1]

freq_cups_won_by_team %>% nrow()
str_remove(clean_data$Season, "-.*")

# Problem: I had to set x and y values. I set them as constants (0 and 1)
clean_data

title_text = "<b> <span style='font-family:fs;'  >&#xf091;</span> Greek Football Cup</b> (1931 - 2022) <span style='font-family:fs;'  >&#xf091;</span>"
subtitle_text =  glue("<b><span style = 'color:#D0061F;'>Olympiacos</span></b> is the team that has won <b>GFC</b> most times ({freq_cups_won_by_team$n[1]}) followed by <b><span style = 'color:#007841;'>Panathinaikos</span><br></b>
    and <b><span style = 'color:#c8a951;'>AEK Athens</span></b> with {freq_cups_won_by_team$n[2]} and {freq_cups_won_by_team$n[3]} cups, respectively. In total, {freq_cups_won_by_team %>% nrow()} teams have succeeded to <br>
    win the cup the last 80 cup seasons of which only <b>10% (8)</b> are from teams outside the<br> so-called Big5 (Olympiacos, Panathinaikos, AEK Athens, PAOK, Aris)")
caption_text = "30 Day Chart Challenge, Day 4 (2024) | <b> Data:</b> Wikipedia<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"

title_text_gr = "<b> <span style='font-family:fs;'  >&#xf091;</span> Κυπελλούχοι Ελλάδας</b> (1931 - 2022) <span style='font-family:fs;'  >&#xf091;</span>"
subtitle_text_gr = glue("O <b><span style = 'color:#D0061F;'>Ολυμπιακός</span></b> είναι η ομάδα που έχει στεφθεί περισσότερες φορές ({freq_cups_won_by_team$n[1]} κατακτήσεις) ως <br>
                         κυπελούχος Ελλάδας ακολουθούμενος από τον <b><span style = 'color:#007841;'>Παναθηναϊκό</span></b>
    και την <b><span style = 'color:#c8a951;'>ΑΕΚ</span></b> με {freq_cups_won_by_team$n[2]} και {freq_cups_won_by_team$n[3]}<br> κύπελλα, αντίστοιχα. Συνολικά, {freq_cups_won_by_team %>% nrow()} διαφορετικές ομάδες έχουν
    κατακτήσει το κύπελλο τις <br>τελευταίες 80 σεζόν. Αξίζει να σημειωθεί ότι μόλις <b>8 κατακτήσεις</b> είναι από ομάδες εκτός<br> των λεγόμενων 5 μεγάλων ελληνικών ομάδων (ΟΣΦΠ, ΠΑΟ, ΑΕΚ, ΠΑΟΚ, Άρης).")
caption_text_gr = "30 Day Chart Challenge, Day 4 (2024) | <b> Δεδομένα:</b> Wikipedia<br><span style='font-family:fb;'  >&#xf09b;</span> <b>stesiam</b>, 2024"


p = ggplot(data = clean_data, aes(0,1))+
  geom_image(aes(image=Logo),size=.6) +
  facet_wrap(~Season, nrow = 5) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_void() +
  theme(
    plot.margin = margin(l = 10, r = 10),
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "white"),
    strip.text = element_markdown(size = 4.5,face = "bold", family = "serif"),
    plot.caption = element_markdown(family = "title", margin = margin(t = 5, r = 5, b = 4), 
                                    lineheight = 1.4,
                                    color = "white", size = 8,
                                    hjust = 0.5),
    plot.title = element_markdown(family = "js",
                                  margin = margin(t = 10, b = 5), 
                                  hjust = 0.5, 
                                  color = "white",face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "js",
                                     margin = margin(t = 5, l = 10, r = 10, b = 5),
                                     lineheight = 1.1,
                                     color = "white")
)

ggsave(
  filename = "2024/day4/day4-2024-cc-en.png",
  plot = p,
  device = "png",
  height = 4,
  width = 6)

p_gr = ggplot(data = clean_data, aes(0,1))+
  geom_image(aes(image=Logo),size=.6) +
  facet_wrap(~Season, nrow = 5) +
  labs(
    title = title_text_gr,
    subtitle = subtitle_text_gr,
    caption = caption_text_gr
  ) +
  theme_void() +
  theme(
    plot.margin = margin(l = 10, r = 10),
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "white"),
    strip.text = element_markdown(size = 4.5,face = "bold", family = "serif"),
    plot.caption = element_markdown(family = "serif", margin = margin(t = 5, r = 5, b = 4), 
                                    lineheight = 1.4,
                                    color = "white", size = 8,
                                    hjust = 0.5),
    plot.title = element_markdown(family = "serif",
                                  margin = margin(t = 10, b = 5), 
                                  hjust = 0.5, 
                                  color = "white",face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "gp",
                                     margin = margin(t = 5, l = 10, r = 10, b = 5),
                                     lineheight = 1.1,
                                     color = "white")
  )


ggsave(
  filename = "2024/day4/day4-2024-cc-el.png",
  plot = p_gr,
  device = "png",
  height = 4,
  width = 6)
