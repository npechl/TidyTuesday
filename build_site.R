# Regenerates posts/*.qmd from R/<year>/<week>/README.md + existing plot images.
# Run this after adding a new TidyTuesday week, then `quarto render` / `quarto preview`.
# CI (.github/workflows/publish.yml) also runs this before every deploy.

source("weeks.R")
weeks <- read_weeks()

unlink("posts", recursive = TRUE)
dir.create("posts")

for (i in seq_len(nrow(weeks))) {
    w <- weeks[i]

    rel_image <- file.path("..", w$week_dir, w$image)

    qmd <- c(
        "---",
        sprintf('title: "%s"', yaml_escape(w$title)),
        sprintf("date: %s", w$date),
        sprintf('categories: ["%s"]', w$year),
        sprintf('image: "%s"', rel_image),
        sprintf('description: "%s"', yaml_escape(w$description)),
        "---",
        "",
        sprintf("![](%s)", rel_image),
        "",
        readme_body(w$week_dir),
        "",
        sprintf("[View R script on GitHub](%s)", script_url(w$week_dir, w$script))
    )

    writeLines(qmd, file.path("posts", paste0(w$folder, ".qmd")))
}

cat(nrow(weeks), "post(s) written to posts/\n")
