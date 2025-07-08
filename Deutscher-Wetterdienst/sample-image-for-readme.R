library(ggplot2)
library(dplyr)
library(gganimate)
library(stringr)
library(data.table)
library(rvest)
library(here)
library(zoo)
library(duckdb)

library(sf)
library(ggplot2)
library(scales)
library(tidyterra)
library(ggthemes)
library(rgeoboundaries)
library(h3jsr)
library(maptiles)
library(gifski)


setwd(here("Deutscher-Wetterdienst"))

col_names <- c(
  "STATIONS_ID", "von_datum", "bis_datum", "Stationshoehe", "lat",
  "lon", "Stationsname", "Bundesland", "Abgabe"
)

stations <- read.fwf(
  here(
    "Deutscher-Wetterdienst", "data",
    "N_Terminwerte_Beschreibung_Stationen.txt"
  ),
  widths = c(6, 9, 9, 15, 12, 10, 41, 41, 5), skip = 2,
  fileEncoding = "Windows-1252", col.names = col_names
) |> as.data.table()


stations[, von_datum := as.Date(str_trim(von_datum), format = "%Y%m%d")]
stations[, bis_datum := as.Date(str_trim(bis_datum), format = "%Y%m%d")]
stations[, lon := as.numeric(lon)]
stations[, lat := as.numeric(lat)]

con <- dbConnect(duckdb(),
  dbdir = here("Deutscher-Wetterdienst", "db", "weather.duckdb")
)

observations <- dbGetQuery(con, "SELECT * FROM cloudiness") |> as.data.table()

dbDisconnect(con)

observations <- observations[, .(
  .N,
  cloud_coverage = mean(N_TER, na.rm = TRUE),
  cloud_density = mean(CD_TER, na.rm = TRUE)
), by = c("STATIONS_ID", "MESS_DATUM")]

points <- stations[, .(lon, lat)] |> unique()

points[, h3_address := point_to_cell(points, res = 4)]

stations <- stations[, c(
  "STATIONS_ID", "von_datum", "bis_datum", "Stationshoehe",
  "lat", "lon", "Stationsname", "Bundesland", "Abgabe"
)]

stations[points, on = .(lon, lat), h3_address := h3_address]

stations <- stations[, geometry := cell_to_polygon(h3_address, simple = F)[2]]

boundaries <- geoboundaries("Germany", release_type = "gbOpen", adm_lvl = "adm1")

observations <- observations[stations, on = "STATIONS_ID"]
observations[, coud_coverage := mean(cloud_coverage, na.rm = TRUE), by = h3_address]
observations <- observations |> unique(by = c("h3_address", "MESS_DATUM"))

min_date <- observations[, MESS_DATUM] |> min(na.rm = TRUE)
max_date <- observations[, MESS_DATUM] |> max(na.rm = TRUE)

min_date <- max(c(min_date, as.Date("2024-06-01")))
max_date <- min(c(max_date, as.Date("2024-06-30")))

max_coverage <- observations[(MESS_DATUM >= min_date) &
  (MESS_DATUM <= max_date), cloud_coverage] |> max(na.rm = TRUE)
min_coverage <- observations[(MESS_DATUM >= min_date) &
  (MESS_DATUM <= max_date), cloud_coverage] |> min(na.rm = TRUE)

table_dates <- seq(min_date - 7, max_date + 7, by = 1) |>
  as.data.table() |>
  rename(MESS_DATUM = V1)
observations <- table_dates[observations, on = "MESS_DATUM"]

observations[, cloud_coverage_r7 := rollapply(cloud_coverage,
  width = 7, FUN = mean, align = "center", partial = TRUE
),
by = STATIONS_ID
]

dates <- seq(min_date, max_date, by = 1)

for (d in as.character(dates)) {
  p <- ggplot(aes(fill = cloud_coverage_r7),
    data = observations[MESS_DATUM == d] |> as.data.frame()
  ) +
    geom_sf(data = boundaries, fill = "gray78", color = "gray54") +
    geom_sf(aes(geometry = geometry), color = "gray78") +
    scale_fill_whitebox_c(
      palette = "deep",
      direction = 1,
      limits = c(min_coverage, max_coverage)
    ) +
    coord_sf(default_crs = sf::st_crs(4326)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(3, "pt"),
      legend.key.width = unit(15, "pt"),
      legend.title.position = "top",
      legend.text = element_text(size = 8, color = "gray35"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      title = element_text(size = 8, color = "gray35")
    ) +
    labs(title = NULL, fill = "Cloud Coverage")

  ggsave(
    here(
      "Deutscher-Wetterdienst", "figures",
      paste0("sample-", d, ".png")
    ),
    plot = p,
    units = "px",
    width = 1200,
    height = 1200,
    dpi = 300
  )
}

png_files <- list.files(
  here("Deutscher-Wetterdienst", "figures"),
  full.names = TRUE, pattern = "sample.+\\.png"
) |>
  sort() |>
  as.character()

gif_file <- here(
  "Deutscher-Wetterdienst", "sample.gif"
)

gifski(png_files, gif_file, width = 1200, height = 1200, delay = 0.5, loop = TRUE)

unlink(here("Deutscher-Wetterdienst", "figures", "sample*"))
