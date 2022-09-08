library(hexSticker)
library(showtext)
source(here::here("inst/hex/ignyt_colors.R"))

showtext_auto()

sticker(here::here("inst/hex/ignyt_symbol.png"),
        # Image
        s_x = 1,
        s_y = 1.2,
        s_width = 0.8,
        s_height = 0.8,
        asp = 0.86,
        # Package name
        package = "ignyt",
        p_size = 8,
        p_y = 0.45,
        p_color = col.ignyt[3],
        p_family = "sans",
        # URL
        url = "http://ign.yt/",
        u_x = 1,
        u_y = 0.1,
        u_color = col.ignyt[3],
        u_family = "Aller_Rg",
        u_size = 1.5,
        u_angle = 30,
        # Hex
        h_fill = col.ignyt[1],
        h_color = col.ignyt[3],
        # Output
        filename = here::here("man/figures/ignyt-hex.png"),
        dpi = 300
)
