if (!require(magick)) install.packages("magick")
library(magick)
if (!require(bunny)) remotes::install_github("dmi3kno/bunny")
library(bunny)

hex <- image_read(here::here("man/figures/ignyt-hex.png")) %>%
  image_scale("400x400")

gh_logo <- bunny::github %>% image_scale("40x40")

gh_card <- image_canvas_ghcard("#ffffff") %>%
  image_compose(hex, gravity = "East", offset = "+100+0") %>%
  image_annotate(
    "Open-source Radiology",
    gravity = "West",
    location = "+100-60",
    color = "#333333",
    size = 40,
    font = "Helvetica"
  ) %>%
  image_annotate(
    "Workflow Manager",
    gravity = "West",
    location = "+100-0",
    color = "#333333",
    size = 40,
    font = "Helvetica"
  ) %>%
  image_compose(gh_logo, gravity = "West", offset = "+100+60") %>%
  image_annotate(
    "ignyt/ignyt_public",
    gravity = "West",
    location = "+150+60",
    size = 40,
    font = "Ubuntu Mono"
  ) %>%
  image_border_ghcard("#8b9196")

gh_card

gh_card %>%
  image_write(here::here("man/figures/ignyt-github-card.png"))
