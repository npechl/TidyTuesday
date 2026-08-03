# Regenerates docs/ (Zensical content) from R/<year>/<week>/README.md + existing
# plot images. Unlike Quarto, Zensical only serves files that physically live
# inside docs_dir, so images are copied (not linked) into docs/assets/.
# Run this after adding a new TidyTuesday week, then `zensical serve`.
# CI (.github/workflows/publish-zensical.yml) also runs this before every deploy.

source("weeks.R")
weeks <- read_weeks()
weeks <- weeks[order(-date)]

unlink("docs", recursive = TRUE)
dir.create("docs/assets", recursive = TRUE)
dir.create("docs/posts", recursive = TRUE)

card <- function(w, asset) sprintf(
    "-   ![%s](assets/%s)\n\n    __%s__ (%s)\n\n    %s\n\n    [Open →](posts/%s.md)\n",
    w$title, asset, w$title, w$year, w$description, w$folder
)

index <- c("# TidyTuesday Gallery", "")

for (yr in unique(weeks$year)) {

    index <- c(index, sprintf("## %s", yr), "", '<div class="grid cards" markdown>', "")

    for (i in which(weeks$year == yr)) {
        w <- weeks[i]

        asset <- paste0(w$folder, ".", tools::file_ext(w$image))
        file.copy(file.path(w$week_dir, w$image), file.path("docs/assets", asset))

        page <- c(
            sprintf("# %s", w$title),
            "",
            sprintf("![%s](../assets/%s)", w$title, asset),
            "",
            readme_body(w$week_dir),
            "",
            sprintf("[View R script on GitHub](%s)", script_url(w$week_dir, w$script))
        )
        writeLines(page, file.path("docs/posts", paste0(w$folder, ".md")))

        index <- c(index, card(w, asset))
    }

    index <- c(index, "</div>", "")
}

writeLines(index, "docs/index.md")

cat(nrow(weeks), "page(s) written to docs/\n")
