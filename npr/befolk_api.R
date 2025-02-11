library(httr2)
library(jsonlite)

file_json <- "~/Git-fhi/analysis/npr/ssbapi_table_07459.R"
source(file_json)

# Define the URL for the API endpoint
url <- "https://data.ssb.no/api/v0/no/table/07459/"

# Create a request object
req <- request(url) %>%
  req_body_raw(befolkJSON, type = "application/json") %>%
  req_headers("Content-Type" = "application/json")

# Send the request and receive the response
resp <- req_perform(req)

# Check the status of the response
print(resp_status(resp))

# Parse and display the response content
data <- resp_body_json(resp)

# Print the data
print(data)

str(data)
names(data)


## With httr and rjstat approach ---
library(httr)
library(rjstat)

file_json <- "~/Git-fhi/analysis/npr/ssbapi_table_07459.R"
source(file_json)

d.tmp <- httr::POST(url, body = befolkJSON, encode = "json", verbose())
# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
sbtabell <- rjstat::fromJSONstat(content(d.tmp, "text"))
sbtabell_kode <- rjstat::fromJSONstat(content(d.tmp, "text"), , naming = "id")

str(sbtabell)
str(sbtabell_kode)

## med versjon 1 av json-stat kommer sbtabell som en liste
ds <- sbtabell[[1]]
head(ds)
tail(ds)

ds_kode <- sbtabell_kode[[1]]
head(ds_kode)
tail(ds_kode)

## Using SSB recommended approach ---
library(PxWebApiData)
library(norgeo)
library(data.table)

# Define the URL for the API endpoint
url <- "https://data.ssb.no/api/v0/no/table/07459/"

komm <- get_code("kom", 2023)
age <- c(1:150)

dt <- ApiData2(url, Region = komm[["code"]], Kjonn = 1:2, Alder = age )
setDT(dt)
dt

dt[, .N, keyby = Alder]


### Try to use httr2 -----------
# Define the URL and JSON body (assuming befolkJSON is already defined)
url <- "https://data.ssb.no/api/v0/no/table/07459/"

# Convert the JSON body if it's in a different format
if (!is.character(befolkJSON)) {
  befolkJSON <- toJSON(befolkJSON, auto_unbox = TRUE, pretty = TRUE)
}

# Create and send the POST request
req <- request(url) %>%
  req_method("POST") %>%
  req_body_raw(befolkJSON, type = "application/json") %>%
  req_headers("Content-Type" = "application/json")

resp <- tryCatch(req_perform(req), error = function(e) {
  cat("Error in req_perform:", e$message, "\n")
  NULL
})

# Check if response is NULL
if (is.null(resp)) {
  cat("No response received due to error\n")
} else {
  # Print the status of the response
  status <- resp_status(resp)
  cat("Response Status:", status, "\n")

  # Parse the response content if status is 200
  if (status == 200) {
    # Extract content as text
    resp_content <- resp_body_string(resp)

    # Process the content with rjstat::fromJSONstat
    sbtabell <- rjstat::fromJSONstat(resp_content)

    # Print the processed table
    print(sbtabell)
  } else {
    cat("Failed to retrieve the data. Status:", status, "\n")
  }
}
