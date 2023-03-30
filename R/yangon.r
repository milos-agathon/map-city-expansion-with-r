################################################################################
#                 Map expansion of cities with R like a PRO
#                 Milos Popovic
#                 2023/03/30
################################################################################
# libraries we need
libs <- c(
    "tidyverse", "sf", "osmdata",
    "terra", "httr", "XML"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. FETCH ALL RASTER LINKS
#--------------------------
# website
url <-
    "https://glad.umd.edu/users/Potapov/GLCLUC2020/Built-up_change_2000_2020/"

get_raster_links <- function() {
    res <- httr::GET(url) # make http request
    parse <- XML::htmlParse(res) # parse data to html format
    links <- XML::xpathSApply( # scrape all the href tags
        parse,
        path = "//a", XML::xmlGetAttr, "href"
    )
    lnks <- links[-c(1:5)] # grab links
    for (l in lnks) { # make all links and store in a list
        rlinks <- paste0(url, lnks)
    }

    return(rlinks)
}

rlinks <- get_raster_links()

# 1. GET BUILT-UP DATA FOR yangon lat ~ 16.8N, long ~ 96.1E
#-------------------------------------------------------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

load_builtup_data <- function() {
    l <- rlinks[grepl("20N_090E", rlinks, rlinks)]
    builtup_data <- terra::rast(l)
    terra::crs(builtup_data) <- crsLONGLAT

    return(builtup_data)
}

builtup_data <- load_builtup_data()


# 2. GET yangon BOUNDARIES FROM OSM DATA
#--------------------------------------
city <- "Rangoon, Myanmar"
# define longlat projection

yangon_border <- osmdata::getbb(
    city,
    format_out = "sf_polygon"
) |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crsLONGLAT)

plot(yangon_border)

terra::plot(builtup_data)
plot(yangon_border, add = T)

# 3. CROP YANGON RASTER
# METHOD 1: POLYGON
#-------------------
crop_builtup_data_with_polygon <- function() {
    yangon_vect <- terra::vect(yangon_border)
    yangon_raster <- terra::crop(builtup_data, yangon_vect)
    yangon_raster_cropped <- terra::mask(
        yangon_raster, yangon_vect
    )
    return(yangon_raster_cropped)
}

yangon_raster_cropped <- crop_builtup_data_with_polygon()
terra::plot(yangon_raster_cropped)

# 3. CROP yangon RASTER
# METHOD 2: BOUNDING BOX
#-----------------------
bbox <- sf::st_bbox(yangon_border)
bbox_poly <- sf::st_sfc(
    sf::st_polygon(list(cbind(
        c(
            bbox["xmin"], bbox["xmax"],
            bbox["xmax"], bbox["xmin"], bbox["xmin"]
        ),
        c(
            bbox["ymin"], bbox["ymin"],
            bbox["ymax"], bbox["ymax"], bbox["ymin"]
        )
    ))),
    crs = crsLONGLAT
)

crop_builtup_data_with_bbox <- function() {
    yangon_vect <- terra::vect(bbox_poly)
    yangon_raster <- terra::crop(builtup_data, yangon_vect)
    yangon_raster_cropped <- terra::mask(
        yangon_raster, yangon_vect
    )
    return(yangon_raster_cropped)
}

yangon_raster_cropped <- crop_builtup_data_with_bbox()
terra::plot(yangon_raster_cropped)

# 3. MAKE BUFFER AROUND YANGON
# METHOD 3: BUFFER
#----------------------------
get_buffer <- function() {
    yangon_cents <- sf::st_centroid(yangon_border)
    yangon_circle <- sf::st_buffer(
        yangon_cents,
        dist = units::set_units(28, km)
    ) |>
        sf::st_set_crs(crsLONGLAT) |>
        sf::st_transform(crs = crsLONGLAT)

    return(yangon_circle)
}

yangon_circle <- get_buffer()

# plot
ggplot() +
    geom_sf(
        data = yangon_border, color = "#3036ff",
        fill = "transparent", size = 1.5,
        inherit.aes = FALSE
    ) +
    geom_sf(
        data = yangon_circle, color = "#e83778",
        fill = "transparent", size = 1.5,
        inherit.aes = FALSE
    ) +
    theme_void() +
    theme(panel.grid.major = element_line("transparent"))


crop_builtup_data <- function() {
    yangon_vect <- terra::vect(yangon_circle)
    yangon_raster <- terra::crop(builtup_data, yangon_vect)
    yangon_raster_cropped <- terra::mask(
        yangon_raster, yangon_vect
    )
    return(yangon_raster_cropped)
}

yangon_raster_cropped <- crop_builtup_data()
terra::plot(yangon_raster_cropped)


# 4. IMAGE TO DATA.FRAME
#-----------------------

raster_to_df <- function() {
    yangon_df <- terra::as.data.frame(
        yangon_raster_cropped,
        xy = T
    )

    return(yangon_df)
}

yangon_df <- raster_to_df()
head(yangon_df)
names(yangon_df)[3] <- "value"

# define categorical values
yangon_df$cat <- round(yangon_df$value, 0)
yangon_df$cat <- factor(yangon_df$cat,
    labels = c("no built-up", "new", "existing")
)

# 5. GET yangon ROADS FROM OSM DATA
#---------------------------------
road_tags <- c(
    "motorway", "trunk", "primary", "secondary",
    "tertiary", "motorway_link", "trunk_link", 
    "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
    bbox <- sf::st_bbox(yangon_border)
    roads <- bbox |>
        opq() |>
        add_osm_feature(
            key = "highway",
            value = road_tags
        ) |>
        osmdata::osmdata_sf()

    return(roads)
}

roads <- get_osm_roads()
yangon_roads <- roads$osm_lines |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crs = crsLONGLAT)

ggplot() +
    geom_sf(
        data = yangon_circle, fill = "transparent",
        color = "#3036ff", size = 1.2,
        inherit.aes = FALSE
    ) +
    geom_sf(
        data = yangon_roads,
        color = "#e83778", inherit.aes = FALSE
    ) +
    theme_void() +
    theme(panel.grid.major = element_line("transparent"))

# 6. CROP yangon ROADS WITH BUFFER
#--------------------------------
yangon_roads_cropped <- sf::st_intersection(
    yangon_roads, yangon_circle
)

ggplot() +
    geom_sf(
        data = yangon_circle,
        color = "#3036ff", fill = NA,
        size = 1.2, inherit.aes = FALSE
    ) +
    geom_sf(
        data = yangon_roads_cropped, fill = "transparent",
        color = "#e83778", inherit.aes = FALSE
    ) +
    theme_void() +
    theme(panel.grid.major = element_line("transparent"))

# 7. MAP
#-------
colrs <- c(
    "black", "#FCDD0F", "#287DFC"
)

p <- ggplot() +
    geom_raster(
        data = yangon_df,
        aes(x = x, y = y, fill = cat),
        alpha = 1
    ) +
    geom_sf(
        data = yangon_roads_cropped,
        color = "black",
        size = .1,
        alpha = 1,
        fill = "transparent"
    ) +
    scale_fill_manual(
        name = "",
        values = colrs,
        drop = F
    ) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.5, units = "mm"),
            keywidth = unit(35, units = "mm"),
            title.position = "top",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "top"
        )
    ) +
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(.5, 1.05),
        legend.text = element_text(size = 12, color = "white"),
        legend.title = element_text(size = 14, color = "white"),
        legend.spacing.y = unit(0.25, "cm"),
        panel.grid.major = element_line(color = "black", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            size = 20, color = "grey80", hjust = .5, vjust = 2
        ),
        plot.caption = element_text(
            size = 9, color = "grey90", hjust = .5, vjust = 5
        ),
        plot.margin = unit(
            c(t = 1, r = 0, b = 0, l = 0), "lines"
        ),
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(colour = "white"),
        panel.border = element_blank()
    ) +
    labs(
        x = "",
        y = NULL,
        title = "YANGON, MYANMAR",
        subtitle = "",
        caption = "©2023 Milos Popovic (https://milospopovic.net)\nData: GLAD Built-up Change Data & ©OpenStreetMap contributors"
    )

ggsave(
    filename = "yangon_built_up.png",
    width = 6, height = 6, dpi = 600,
    device = "png", p
)

