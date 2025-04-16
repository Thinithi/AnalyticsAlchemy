# Specify the path to the CSV file
csv_file_path <- "Location data.csv"  # Change to your file path

# Read the CSV file into a data frame
df <- read.csv(csv_file_path)
head(df)

library(httr)
library(jsonlite)


get_state_nominatim <- function(lat, lon) {
  url <- paste0("https://nominatim.openstreetmap.org/reverse?format=jsonv2&lat=",
                lat, "&lon=", lon, "&zoom=10&addressdetails=1")
  
  Sys.sleep(1)  # Respect Nominatim's rate limit (1 request/sec)
  
  response <- GET(url, user_agent("R geocoding script (your_email@example.com)"))
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content))
    address <- content$address
    return(address$state)  # Only return the 'state' field
  } else {
    return(NA)
  }
}


# Test the function on first 10 rows of df
df_subset <- head(df, 10)

# Get states for first 10 lat/lon pairs
df_subset$State <- mapply(get_state_nominatim, df_subset$Latitude, df_subset$Longitude)

# View the results
print(df_subset)

df$State <- mapply(get_state_nominatim, df$Latitude, df$Longitude)
write.csv(df, "Location_data_with_state.csv", row.names = FALSE)