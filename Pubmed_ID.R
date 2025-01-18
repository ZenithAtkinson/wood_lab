install.packages("rentrez")
install.packages("XML")
library(XML)
library(rentrez)

# List of IDs
ids <- c("PRJNA1033318", "PRJNA874893", "PRJNA605913", "PRJNA605911", 
         "PRJNA605904", "PRJNA506363", "PRJNA403806", "PRJNA347834", "PRJNA347829")

# Function to fetch summary for each ID
fetch_summary <- function(id) {
  result <- entrez_summary(db = "sra", id = id)
  title <- result$title
  accession <- result$accession
  list(title = title, accession = accession)
}

# Fetch summaries for all IDs
summaries <- lapply(ids, fetch_summary)

# Print summaries
print(summaries)
