# Run the API
library(plumber)

r <- plumb("API.R")

# run it on the port
r$run(port=8000)
