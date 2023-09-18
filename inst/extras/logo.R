# load the necessary packages
library(hexSticker) # hexSticker generator
library(magick)     # Advanced image processing
library(sysfonts)   # font selection
library(tidyverse)

# Sticker function---------------------------

sticker(
  subplot, #              * image/ggplot object
  s_x = 0.8, #            * subplot x-position (left/right position)
  s_y = 0.75, #           * subplot y-position (up/down position)
  s_width = 0.4, #        * subplot width
  s_height = 0.5, #       * subplot height
  package, #              * package name to be displayed in hexSticker
  p_x = 1, #              * package name x-position
  p_y = 1.4, #            * package name y-position
  p_color = "#FFFFFF", #  * package name color
  p_family = "Aller_Rg", #* package name font family
  p_fontface = "plain", # * package name font face
  p_size = 8, #           * package name font size
  h_size = 1.2, #         * hexSticker size
  h_fill = "#1881C2", #   * hexSticker background color
  h_color = "#87B13F", #  * hexsticker border color
  spotlight = FALSE, #    * add spotlight effect to hexSticker
  l_x = 1, #              * spotlight effect x-position
  l_y = 0.5, #            * spotlight effect y-position
  l_width = 3, #          * spotlight effect width
  l_height = 3, #         * spotlight effect height
  l_alpha = 0.4, #        * spotlight effect level
  url = "", #             * url to add to the hexSticker
  u_x = 1, #              * url x-position
  u_y = 0.08, #           * url y-position
  u_color = "black", #    * url font color
  u_family = "Aller_Rg", #* url font family
  u_size = 1.5, #         * url font size
  u_angle = 30, #         * url angle
  white_around_sticker = FALSE, # * add white blocks around sticker
  filename = paste0(package, ".png"), # * save hexSticker to file
  asp = 1, # * adjust aspect ratio
  dpi = 300 # * Dots Per Inch resolution
)

# Create your first sticker------------------
dna_img <- image_read('./doc/biology_5404863.png')
rna_img <- image_read('./doc/rna_3352654.png')
loop_img <- image_read('./doc/statistics_2920323.png')
#complete_img <- image_read('./doc/package_icon_rna_3-modified.png')
complete_img <- image_read('./doc/package_rna_matrix-modified_1.png')
loop_img <- image_scale(loop_img)
composite_img <- image_composite(dna_img, loop_img,operator = "Over", offset = "+400+300") %>% print()

# Stack images vertically
stacked_img <- image_montage(c(dna_img, loop_img)) %>% print()
fonts_dataset <- font_files()

font_add("Consolas", "consola.TTF")

sticker(
  subplot = complete_img,
  package = 'GeneSelectR',
  s_width = 3.5,
  s_height = 3.5,
  s_x = 1.1,
  s_y = 0.5,
  p_family = "Consolas",
  p_size = 18,
  p_x = 1,
  p_y = 1,
  p_fontface = "bold",
  p_color = 'black',
  h_fill = 'darkolivegreen1',
  h_color = 'black',
  u_size = 4.5,
  url = "github/dzhakparov/GeneSelectR",
  h_size = 2,
  spotlight = F
) %>% print()

sticker(
  subplot = complete_img,
  package = 'GeneSelectR',
  s_width = 6.5,
  s_height = 6.5,
  s_x = 1.15,
  s_y = 1.,
  p_family = "Consolas",
  p_size = 16,
  p_x = 1,
  p_y = 0.7,
  p_fontface = "bold",
  p_color = 'black',
  h_fill = 'cornflowerblue',
  h_color = 'black',
  u_size = 4.5,
  url = "github/dzhakparov/GeneSelectR",
  h_size = 2,
  spotlight = F
) %>% print()
