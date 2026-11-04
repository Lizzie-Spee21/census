library(dplyr)
library(readr)

download_sarasota_data <- function() {
  data_dir <- "data"
  output_file <- file.path(data_dir, "sarasota_tracts_acs2023.csv")
  source_url <- paste0(
    "https://www2.census.gov/programs-surveys/acs/summary_file/",
    "2023/table-based-SF/data/5YRData/acsdt5y2023-b03002.dat"
  )

  dir.create(data_dir, showWarnings = FALSE)
  source_file <- tempfile(fileext = ".dat")
  on.exit(unlink(source_file))

  download.file(source_url, source_file, mode = "wb")

  needed_columns <- c(
    "GEO_ID",
    "B03002_E001", "B03002_M001",
    "B03002_E003", "B03002_M003",
    "B03002_E004", "B03002_M004",
    "B03002_E006", "B03002_M006",
    "B03002_E012", "B03002_M012"
  )

  sarasota_tracts <- read_delim(
    source_file,
    delim = "|",
    col_select = all_of(needed_columns),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    filter(startsWith(GEO_ID, "1400000US12115")) |>
    mutate(across(-GEO_ID, parse_double)) |>
    rename(
      total = B03002_E001,
      total_moe = B03002_M001,
      white_nh = B03002_E003,
      white_nh_moe = B03002_M003,
      black_nh = B03002_E004,
      black_nh_moe = B03002_M004,
      asian_nh = B03002_E006,
      asian_nh_moe = B03002_M006,
      hispanic = B03002_E012,
      hispanic_moe = B03002_M012
    )

  if (nrow(sarasota_tracts) == 0) {
    stop("No Sarasota County census tracts were found in the downloaded file.")
  }

  write_csv(sarasota_tracts, output_file)
  message("Saved ", nrow(sarasota_tracts), " tracts to ", output_file)
}

download_sarasota_data()
