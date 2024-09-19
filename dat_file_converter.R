# Install required packages (only run this once if not already installed)

install.packages("DBI")
install.packages("RSQLite")

# Load necessary libraries
library(DBI)
library(RSQLite)

# Set your working directory (optional)
# setwd("path/to/your/folder")

# Specify the .dat file path
dat_file <- "Great Salt Lake Phragmites_Flux_AmeriFluxFormat.dat"

# Read the .dat file into an R data frame
# Modify the 'sep' argument based on the delimiter in your .dat file.
# If the file is space-delimited, keep sep="". For comma-delimited, use sep=",
print(readLines(dat_file, n = 10))  # Print the first 10 lines to inspect the file structure.
data <- read.table(dat_file, header = TRUE, sep = ",", skip = 1, na.strings = "NAN")


# Preview the first few rows of the data
print(head(data))

# Create a connection to a new SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = "my_database.sqlite")

# Write the data frame into the SQLite database as a table
# The table will be named 'my_table', but you can change the name if needed
dbWriteTable(con, "my_table", data, overwrite = TRUE)

# Verify that the table was created successfully by listing all tables in the database
print(dbListTables(con))

# Optionally, query some data from the table to confirm it's stored properly
print(dbReadTable(con, "my_table"))

# Close the database connection
dbDisconnect(con)

# The data is now stored in the 'my_database.sqlite' file as a table 'my_table'.
