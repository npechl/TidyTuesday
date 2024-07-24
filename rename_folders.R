


fls <- list.files("R/", recursive = TRUE, full.names = TRUE, pattern = "\\.R")

fls = fls[-c(2, 3)]

library(data.table)
library(stringr)

df = data.table(
    "year" = fls |> str_split_i("\\/", 2),
    "folder" = fls |> str_split_i("\\/", 3),
    "title" = fls |> basename() |> str_split("_", 2) |> lapply(function(x) x[2]) |> unlist()
)


df$title = df$title |> 
    str_sub(1, -3) |> 
    str_split("_") |> 
    lapply(str_to_title) |>
    lapply(paste, collapse = "_") |>
    unlist()

df$from = paste("R", df$year, df$folder, sep = "/")
df$to   = paste("R", df$year, paste0(df$folder, "_", df$title), sep = "/")

for(i in seq_len(nrow(df))) {
    
    
    file.rename(
        from = df[i]$from,
        to   = df[i]$to
    )
    
    
}
