################################################################################
#                 Map expansion of cities with R like a PRO
#                 Milos Popovic
#                 2023/03/26
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

# 1. GET BUILT-UP DATA FOR DELHI lat ~ 28N, long ~ 77E
#-------------------------------------------------------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

load_builtup_data <- function() {
    l <- rlinks[grepl("30N_070E", rlinks, rlinks)]
    builtup_data <- terra::rast(l)
    terra::crs(builtup_data) <- crsLONGLAT

    return(builtup_data)
}

builtup_data <- load_builtup_data()


# 2. GET DELHI BOUNDARIES FROM OSM DATA
#--------------------------------------
city <- "Delhi, India"
# define longlat projection

delhi_border <- osmdata::getbb(
    city,
    format_out = "sf_polygon"
) |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crsLONGLAT)

terra::plot(builtup_data)
plot(delhi_border, add = T)

# 3. CROP DELHI RASTER
# METHOD 1: POLYGON
#-------------------
crop_builtup_data_with_polygon <- function() {
    delhi_vect <- terra::vect(delhi_border)
    delhi_raster <- terra::crop(builtup_data, delhi_vect)
    delhi_raster_cropped <- terra::mask(
        delhi_raster, delhi_vect
    )
    return(delhi_raster_cropped)
}

delhi_raster_cropped <- crop_builtup_data_with_polygon()
terra::plot(delhi_raster_cropped)

# 3. CROP DELHI RASTER
# METHOD 2: BOUNDING BOX
#-----------------------
bbox <- sf::st_bbox(delhi_border)
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
    delhi_vect <- terra::vect(bbox_poly)
    delhi_raster <- terra::crop(builtup_data, delhi_vect)
    delhi_raster_cropped <- terra::mask(
        delhi_raster, delhi_vect
    )
    return(delhi_raster_cropped)
}

delhi_raster_cropped <- crop_builtup_data_with_bbox()
terra::plot(delhi_raster_cropped)

# 3. MAKE BUFFER AROUND DELHI
# METHOD 3: BUFFER
#----------------------------
get_buffer <- function() {
    delhi_cents <- sf::st_centroid(delhi_border)
    delhi_circle <- sf::st_buffer(
        delhi_cents,
        dist = units::set_units(28, km)
    ) |>
        sf::st_set_crs(crsLONGLAT) |>
        sf::st_transform(crs = crsLONGLAT)

    return(delhi_circle)
}

delhi_circle <- get_buffer()

# plot
ggplot() +
    geom_sf(
        data = delhi_border, color = "#3036ff",
        fill = "transparent", size = 1.5,
        inherit.aes = FALSE
    ) +
    geom_sf(
        data = delhi_circle, color = "#e83778",
        fill = "transparent", size = 1.5,
        inherit.aes = FALSE
    ) +
    theme_void() +
    theme(panel.grid.major = element_line("transparent"))


crop_builtup_data <- function() {
    delhi_vect <- terra::vect(delhi_circle)
    delhi_raster <- terra::crop(builtup_data, delhi_vect)
    delhi_raster_cropped <- terra::mask(
        delhi_raster, delhi_vect
    )
    return(delhi_raster_cropped)
}

delhi_raster_cropped <- crop_builtup_data()
terra::plot(delhi_raster_cropped)


# 4. IMAGE TO DATA.FRAME
#-----------------------

raster_to_df <- function() {
    delhi_df <- terra::as.data.frame(
        delhi_raster_cropped,
        xy = T
    )

    return(delhi_df)
}

delhi_df <- raster_to_df()
head(delhi_df)
names(delhi_df)[3] <- "value"

# define categorical values
delhi_df$cat <- round(delhi_df$value, 0)
delhi_df$cat <- factor(delhi_df$cat,
    labels = c("no built-up", "new", "existing")
)

# 5. GET DELHI ROADS FROM OSM DATA
#---------------------------------
road_tags <- c(
    "motorway", "trunk", "primary", "secondary",
    "tertiary", "motorway_link", "trunk_link", 
    "primary_link", "secondary_link", "tertiary_link"
)

get_osm_roads <- function() {
    bbox <- sf::st_bbox(delhi_border)
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
delhi_roads <- roads$osm_lines |>
    sf::st_set_crs(crsLONGLAT) |>
    sf::st_transform(crs = crsLONGLAT)

ggplot() +
    geom_sf(
        data = delhi_circle, fill = "transparent",
        color = "#3036ff", size = 1.2,
        inherit.aes = FALSE
    ) +
    geom_sf(
        data = delhi_roads,
        color = "#e83778", inherit.aes = FALSE
    ) +
    theme_void() +
    theme(panel.grid.major = element_line("transparent"))

# 6. CROP DELHI ROADS WITH BUFFER
#--------------------------------
delhi_roads_cropped <- sf::st_intersection(
    delhi_roads, delhi_circle
)

ggplot() +
    geom_sf(
        data = delhi_circle,
        color = "#3036ff", fill = NA,
        size = 1.2, inherit.aes = FALSE
    ) +
    geom_sf(
        data = delhi_roads_cropped, fill = "transparent",
        color = "#e83778", inherit.aes = FALSE
    ) +
    theme_void() +
    theme(panel.grid.major = element_line("transparent"))

# 7. MAP
#-------
colrs <- c(
    "grey20", "#FCDD0F", "#287DFC"
)

p <- ggplot() +
    geom_raster(
        data = delhi_df,
        aes(x = x, y = y, fill = cat),
        alpha = 1
    ) +
    geom_sf(
        data = delhi_roads_cropped,
        color = "grey20",
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
        panel.grid.major = element_line(color = "grey20", size = 0.2),
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
        plot.background = element_rect(fill = "grey20", color = NA),
        panel.background = element_rect(fill = "grey20", color = NA),
        legend.background = element_rect(fill = "grey20", color = NA),
        legend.key = element_rect(colour = "white"),
        panel.border = element_blank()
    ) +
    labs(
        x = "",
        y = NULL,
        title = "Delhi",
        subtitle = "",
        caption = "©2023 Milos Popovic (https://milospopovic.net)\nData: GLAD Built-up Change Data & ©OpenStreetMap contributors"
    )

ggsave(
    filename = "delhi_built_up.png",
    width = 6, height = 6, dpi = 600,
    device = "png", p
)
