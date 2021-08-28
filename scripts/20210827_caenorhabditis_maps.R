library(tidyverse)
#library(tmap)
#library(sp)
#library(rnaturalearth)
#library(rnaturalearthdata)
#library(urbnmapr)
library(spDataLarge)
#data(countriesHigh)

setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# Erik says:
#I need a figure with Chile, New Zealand, and Taiwan showing all C.b., C.e., and C.t. collections.
#Countries can be panels A, B, and C. The size has to be 6.5 inches wide max. We don't need writing on the maps.

# set color palette
col_pal <- c("Caenorhabditis elegans" = "#BE0032", # red
             "Caenorhabditis tropicalis" = "#F38400", # orange
             "Caenorhabditis briggsae" = "#A1CAF1") #blue

# load species sheets
ss_cb <- googlesheets4::read_sheet("1IJHMLwuaxS_sEO31TyK5NLxPX7_qSd0bHNKverAv8-0", col_types = "cccccccddccccdddcdccDccclcll")

ss_ce <- googlesheets4::read_sheet("10x-CcKNCl80F9hMcrGWC4fhP_cbekSzi5_IYBY2UqCc", col_types = "cccccccddccccdddcdccDccclcll")

ss_ct <- googlesheets4::read_sheet("1mqXOlUX7UeiPBe8jfAwFZnqlzhb7X-eKGK_TydT7Gx4", col_types = "cccccccddccccdddcdccDccclcll")

# filter to distinct species collections
ss_full <- bind_rows(ss_cb, ss_ce, ss_ct) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::distinct(species, latitude, longitude, .keep_all = T) %>%
  dplyr::mutate(species = factor(species, levels = c("Caenorhabditis elegans", "Caenorhabditis tropicalis", "Caenorhabditis briggsae")))

# convert to sf 
ss_full_sf <- st_as_sf(ss_full, coords = c("longitude", "latitude"), crs = 4326)

# load shape files downloaded from the internet https://www.diva-gis.org/gdata
c_shp <- sf::st_read('data/country_shape_files/chile/CHL_adm/CHL_adm1.shp')
t_shp <- sf::st_read("/Users/tim/Dropbox/AndersenLab/LabFolders/Tim/writing/Erik_grants/data/country_shape_files/TWN_adm/TWN_adm2.shp")
nz_shp <- sf::st_read("/Users/tim/Dropbox/AndersenLab/LabFolders/Tim/writing/Erik_grants/data/country_shape_files/NZL_adm/NZL_adm2.shp")

# find collections in these areas
# Chile
c_cols_m <- sf::st_intersects(ss_full_sf, c_shp, sparse = FALSE) 
c_cols_m2 <- which(c_cols_m, arr.ind = TRUE) # find indicies of TRUE values for intersections
c_cols_sf <- tibble::rownames_to_column(ss_full_sf) %>%
  dplyr::filter(rowname %in% as.character(c_cols_m2[,1])) # filter to collections that intersect New Zealand shape file

# Tiawan
t_cols_m <- sf::st_intersects(ss_full_sf, t_shp, sparse = FALSE) 
t_cols_m2 <- which(t_cols_m, arr.ind = TRUE) # find indicies of TRUE values for intersections
t_cols_sf <- tibble::rownames_to_column(ss_full_sf) %>%
  dplyr::filter(rowname %in% as.character(t_cols_m2[,1])) # filter to collections that intersect New Zealand shape file

# New Zealand
nz_cols_m <- sf::st_intersects(ss_full_sf, nz_shp, sparse = FALSE) 
nz_cols_m2 <- which(nz_cols_m, arr.ind = TRUE) # find indicies of TRUE values for intersections
nz_cols_sf <- tibble::rownames_to_column(ss_full_sf) %>%
  dplyr::filter(rowname %in% as.character(nz_cols_m2[,1])) # filter to collections that intersect New Zealand shape file
  
# get bounding boxes http://bboxfinder.com/#0.000000,0.000000,0.000000,0.000000
loc_bb <- list("new_zealand" = c(151.837835,-47.715768,193.695745,-33.841334),
               "chile" = c(-76.209068,-56.079891,-66.101646,-17.32212),
               "tiawan" = c(119.975982,21.842340,122.068877,25.340264))

# chile
c_bor <- sf::st_sfc(as.matrix(tibble::tibble(long = c(loc_bb$chile[1], loc_bb$chile[3], loc_bb$chile[3], loc_bb$chile[1]), # 1, 3, 3, 1
                                             lat = c(loc_bb$chile[2], loc_bb$chile[2],loc_bb$chile[4],loc_bb$chile[4]))) %>% # 2, 2, 4, 4
                      sf::st_multipoint() %>%
                      sf::st_cast("POLYGON"))
sf::st_crs(c_bor) <- 4326  
c_crop <- sf::st_crop(c_shp, c_bor)
# tiawan
t_bor <- sf::st_sfc(as.matrix(tibble::tibble(long = c(loc_bb$tiawan[1], loc_bb$tiawan[3], loc_bb$tiawan[3], loc_bb$tiawan[1]), # 1, 3, 3, 1
                                             lat = c(loc_bb$tiawan[2], loc_bb$tiawan[2],loc_bb$tiawan[4],loc_bb$tiawan[4]))) %>% # 2, 2, 4, 4
                      sf::st_multipoint() %>%
                      sf::st_cast("POLYGON"))
sf::st_crs(t_bor) <- 4326  
t_crop <- sf::st_crop(t_shp, t_bor)
# new zealand
#loc_bb$new_zealand[1]
nz_bor <- sf::st_sfc(as.matrix(tibble::tibble(long = c(loc_bb$new_zealand[1], loc_bb$new_zealand[3], loc_bb$new_zealand[3], loc_bb$new_zealand[1]), # 1, 3, 3, 1
                                             lat = c(loc_bb$new_zealand[2], loc_bb$new_zealand[2],loc_bb$new_zealand[4],loc_bb$new_zealand[4]))) %>% # 2, 2, 4, 4
                      sf::st_multipoint() %>%
                      sf::st_cast("POLYGON"))
sf::st_crs(nz_bor) <- 4326  
nz_crop <- sf::st_crop(nz_shp, nz_bor)

# make ggplots
c_plot <- ggplot() +
  geom_sf(data = c_crop, size = 0.1) +
  #ggnewscale::new_scale_fill() +
  #ggnewscale::new_scale_color() +
  geom_sf(data = c_cols_sf %>% dplyr::filter(species == "Caenorhabditis briggsae") %>%
            dplyr::arrange(species),
          aes(fill = species),
          size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  geom_sf(data = c_cols_sf %>% dplyr::filter(species == "Caenorhabditis tropicalis") %>%
            dplyr::arrange(species),
          aes(fill = species),
          size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  geom_sf(data = c_cols_sf %>% dplyr::filter(species == "Caenorhabditis elegans") %>%
            dplyr::arrange(species),
          aes(fill = species),
          size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  scale_fill_manual(values = col_pal) +
  theme_map() +
  theme(legend.position = "none") +
  ggsn::scalebar(c_crop, dist = 100, dist_unit = "km", st.color = "black",
                 transform = TRUE, model = "WGS84", st.size = 0, border.size = 0.25, height = 0.0,  location = "topleft") #bottomright
c_plot

t_plot <- ggplot() +
  geom_sf(data = t_crop, size = 0.1) +
  geom_sf(data = t_cols_sf %>% dplyr::filter(species == "Caenorhabditis briggsae") %>%
            dplyr::arrange(species),
          aes(fill = species),
          size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  geom_sf(data = t_cols_sf %>% dplyr::filter(species == "Caenorhabditis tropicalis") %>%
            dplyr::arrange(species),
          aes(fill = species),
          size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  geom_sf(data = t_cols_sf %>% dplyr::filter(species == "Caenorhabditis elegans") %>%
           dplyr::arrange(species),
         aes(fill = species),
         size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  scale_fill_manual(values = col_pal) +
  theme_map() +
  theme(legend.position = "none") +
  ggsn::scalebar(t_crop, dist = 25, dist_unit = "km", st.color = "black",
                 transform = TRUE, model = "WGS84", st.size = 0, border.size = 0.25, height = 0,  location = "topleft")
t_plot


nz_plot <- ggplot() +
  geom_sf(data = nz_crop, size = 0.1) +
  geom_sf(data = nz_cols_sf %>% dplyr::filter(species == "Caenorhabditis briggsae") %>%
            dplyr::arrange(species),
          aes(fill = species),
          size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  geom_sf(data = nz_cols_sf %>% dplyr::filter(species == "Caenorhabditis tropicalis") %>%
            dplyr::arrange(species),
          aes(fill = species),
          size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  geom_sf(data = nz_cols_sf %>% dplyr::filter(species == "Caenorhabditis elegans") %>%
            dplyr::arrange(species),
          aes(fill = species),
          size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
  scale_fill_manual(values = col_pal) +
  theme_map() +
  theme(legend.position = "none") +
  ggsn::scalebar(nz_crop, dist = 100, dist_unit = "km", st.color = "black",
                 transform = TRUE, model = "WGS84", st.size = 0, border.size = 0.25, height = 0.0,  location = "topleft")
nz_plot

# leg <- ggplot() +
#   geom_sf(data = t_cols_sf %>%
#             dplyr::arrange(species),
#           aes(fill = species),
#           size = 3, stroke = 0.25, shape = 21, color = "grey8", alpha = 0.75) +
#   scale_fill_manual(values = col_pal,labels = c("Ce", "Cb", "Ct")) +
#   theme_map() +
#   labs(fill = "")
# leg_only <- cowplot::get_legend(leg)

ab <- cowplot::plot_grid(t_plot, nz_plot, ncol = 1, labels = c("A", "B"))
# c_leg <- cowplot::plot_grid(c_plot, leg_only, labels = c("C", ""))
# abc <- cowplot::plot_grid(ab, c_leg, ncol = 2, rel_widths = c(1,0.8), align = "b")
abc <- cowplot::plot_grid(ab, c_plot, ncol = 2, labels = c("", "C"), rel_widths = c(1,1), align = "vh", axis = "bl")

cowplot::ggsave2(abc, filename = "plots/20210828_caeno_maps3.png", width = 4.25, height = 6.5, dpi = 300)
cowplot::ggsave2(abc, filename = "plots/20210828_caeno_maps3.pdf", width = 4.25, height = 6.5)


#### other stuff
tmap::tm_shape(tawaian_shp) +
  tm_borders()                
tmap::tm_shape(chile_shp) +
  tm_borders()                
