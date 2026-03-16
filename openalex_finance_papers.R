# Fetch Journal of Finance papers from 2025 using OpenAlex API
# OpenAlex source ID for Journal of Finance: S5353659

library(httr2)
library(jsonlite)

fetch_jof_papers_2025 <- function() {
  base_url <- "https://api.openalex.org/works"
  source_id <- "S5353659"

  # Fields to retrieve
  select_fields <- paste(c(
    "id", "doi", "title", "publication_date",
    "authorships", "cited_by_count", "abstract_inverted_index"
  ), collapse = ",")

  all_papers <- list()
  page <- 1
  per_page <- 50

  repeat {
    cat(sprintf("Fetching page %d...\n", page))

    resp <- request(base_url) |>
      req_url_query(
        filter = sprintf("publication_year:2025,primary_location.source.id:%s", source_id),
        per_page = per_page,
        page = page,
        select = select_fields
      ) |>
      req_retry(max_tries = 4, backoff = ~ 2 * (2 ^ (.x - 1))) |>
      req_perform()

    data <- resp |> resp_body_json()

    total <- data$meta$count
    results <- data$results

    if (length(results) == 0) break

    all_papers <- c(all_papers, results)
    cat(sprintf("  Retrieved %d / %d papers\n", length(all_papers), total))

    if (length(all_papers) >= total) break
    page <- page + 1
    Sys.sleep(0.1)  # Be polite to the API
  }

  all_papers
}

# Helper: reconstruct abstract from inverted index
reconstruct_abstract <- function(inverted_index) {
  if (is.null(inverted_index) || length(inverted_index) == 0) return(NA_character_)

  # inverted_index is a list: word -> list of positions
  positions <- unlist(lapply(names(inverted_index), function(word) {
    pos <- unlist(inverted_index[[word]])
    setNames(rep(word, length(pos)), pos)
  }))

  # Sort by position
  positions <- positions[order(as.integer(names(positions)))]
  paste(positions, collapse = " ")
}

# Helper: extract authors as a single string
extract_authors <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) return(NA_character_)
  authors <- sapply(authorships, function(a) a$author$display_name)
  paste(authors, collapse = "; ")
}

# Main
papers_raw <- fetch_jof_papers_2025()

cat(sprintf("\nTotal papers fetched: %d\n", length(papers_raw)))

# Convert to data frame
papers_df <- data.frame(
  openalex_id    = sapply(papers_raw, function(p) p$id),
  doi            = sapply(papers_raw, function(p) if (!is.null(p$doi)) p$doi else NA_character_),
  title          = sapply(papers_raw, function(p) p$title),
  publication_date = sapply(papers_raw, function(p) p$publication_date),
  authors        = sapply(papers_raw, function(p) extract_authors(p$authorships)),
  cited_by_count = sapply(papers_raw, function(p) if (!is.null(p$cited_by_count)) p$cited_by_count else 0L),
  abstract       = sapply(papers_raw, function(p) reconstruct_abstract(p$abstract_inverted_index)),
  stringsAsFactors = FALSE
)

# Sort by publication date
papers_df <- papers_df[order(papers_df$publication_date), ]

cat("\nSample of papers:\n")
print(papers_df[, c("title", "publication_date", "cited_by_count")], n = 10)

# Save to CSV
write.csv(papers_df, "jof_papers_2025.csv", row.names = FALSE)
cat("\nSaved to jof_papers_2025.csv\n")

papers_df
