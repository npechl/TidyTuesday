# Shared by build_site.R (Quarto) and build_zensical.R: scans R/<year>/<week>/
# and extracts the metadata both site generators need from what's already there
# (README.md + existing plot image) -- no code execution.

library(data.table)
library(stringr)

yaml_escape <- function(x) x |> str_replace_all("\\\\", "\\\\\\\\") |> str_replace_all('"', '\\\\"')

read_weeks <- function() {

    week_dirs <- list.dirs("R", recursive = FALSE) |>
        lapply(list.dirs, recursive = FALSE) |>
        unlist()

    rbindlist(lapply(week_dirs, function(week_dir) {

        folder <- basename(week_dir)
        year   <- basename(dirname(week_dir))
        date   <- str_extract(folder, "^\\d{4}-\\d{2}-\\d{2}")

        readme <- readLines(file.path(week_dir, "README.md"), warn = FALSE)
        title  <- readme[1] |> str_remove("^#\\s*") |> str_trim()

        body <- readme[-1] |> str_trim()
        body <- body[nzchar(body)]
        description <- if (length(body)) body[1] else title

        images <- list.files(week_dir, pattern = "\\.(png|jpe?g|gif)$", ignore.case = TRUE)
        image  <- c(images[str_starts(images, "Rplot")], images) |> sort() |> head(1)

        script <- list.files(week_dir, pattern = "\\.[Rr]$")[1]

        data.table(
            week_dir = week_dir, folder = folder, year = year, date = date,
            title = title, description = description, image = image, script = script
        )
    }))
}

readme_body <- function(week_dir) readLines(file.path(week_dir, "README.md"), warn = FALSE)[-1]

script_url <- function(week_dir, script) {
    paste0("https://github.com/npechl/TidyTuesday/blob/main/", week_dir, "/", script)
}
