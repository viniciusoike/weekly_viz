library(tidygeocoder)
library(rvest)
library(tidyverse)

geo("Avenida das Nações Unidas 12995 Sao Paulo SP Brazil")



# Webscrape ---------------------------------------------------------------

# Base url for table
base_url <- "https://skyscraperpage.com/cities/?cityID=909&offset=%i&statusID=%i"
# Creates links to navigate over table
urls <- sprintf(base_url, seq(0, 600, 100), 1)

# Parse HTML
parse <- read_html(urls[1], encoding = "UTF-8")

clean_webscrape <- function(url) {
  # Read entire webpage
  html <- read_html(url, encoding = "UTF-8")
  
  # Get individual links for buildings (class = bhu, attr = href)
  x <- html_nodes(html, ".bhu")
  sublink <- html_attr(x, "href")
  sublink <- sublink[str_detect(sublink, "buildingID")]
  # Build links
  building_link <- paste0("https://skyscraperpage.com/cities", sublink)
  
  # Get all table objects and select
  tbl <- html_table(html)
  test <- tbl[[22]]
  # Clean data
  df <- test %>%
    filter(!(X2 == "" | X2 == "Name")) %>%
    select(name_building = X2, floors = X4, status = X6, year = X8)
  
  return(list(df = df, building = building_link))
}

data <- map(urls, clean_webscrape)

# Get only summary table
sumtbl <- bind_rows(lapply(data, "[[", 1))

# Get all links for individual buildings
building_links <- unlist(lapply(data, "[[", 2))

# Set progress bar
pb <- txtProgressBar(min = 0, max = length(building_links), style = 3)
# Pre-allocate an empty list with length equal to builiding_links length
building <- vector("list", length(building_links))
# This takes some time to run
for(i in seq_along(building_links)) {
  
  html <- read_html(building_links[i], encoding = "UTF-8")
  
  # Find building name
  name <- html %>%
    html_nodes(".lrgb") %>%
    html_text()
  
  # Find building uses
  type <- html %>%
    html_nodes(xpath = "//table[2]/tr/td[1]/table/tr/td/table[2]/tr/td[3]") %>%
    html_table()
  
  # Use xpath to find address
  address <- html %>%
    html_nodes(xpath = "//table[2]/tr/td[1]/table/tr/td/table[1]/tr/td") %>%
    html_text2() %>%
    str_split("\n")
  
  # Use xpath to find height
  height1 <- html %>%
    html_nodes(xpath = "//b/span[@id = 'height_0']") %>%
    html_text()
  
  if(length(height1) == 0) {height1 <- NA_character_}
  # Small check to verify if building has multiple heights listed
  height2 <- html %>%
    html_nodes(xpath = "//b/span[@id = 'height_1']") %>%
    html_text()
  
  if(length(height2) == 0) {height2 <- NA_integer_}
  
  # Small check to verify if building has multiple heights listed
  height3 <- html %>%
    html_nodes(xpath = "//b/span[@id = 'height_2']") %>%
    html_text()
  
  if(length(height3) == 0) {height3 <- NA_integer_}

  # Convert to data.frame
  df <- as.data.frame(t(unlist(address)))
  # Adds as a column 
  df$height <- height
  df$name <- name
  df$type <- type
  df$link <- building_links[i]
  df$h1 <- height1
  df$h2 <- height2
  df$h3 <- height3
  # Output to list
  building[[i]] <- df
  setTxtProgressBar(pb, i)
  Sys.sleep(time = 1 + runif(n = 1, min = 0, max = 1))
}

# Bind rows 
df_building <- bind_rows(building)
df_building <- data.table::rbindlist(building, fill = T)

# Simplify type

building$type_concat <- sapply(building$type, paste, collapse = ",")

building <- building %>%
  mutate(
    
    h1 = as.numeric(str_remove(h1, "ft")),
    h2 = as.numeric(str_remove(h2, "ft")),
    h3 = as.numeric(str_remove(h3, "ft")),
    
    height = if_else(h1 > 1000, h2, h1),
    height = height / 3.281,
    
    # Dummies for usage
    residential = if_else(str_detect(type_concat, "residential"), 1, 0),
    office = if_else(str_detect(type_concat, "office"), 1, 0),
    hotel = if_else(str_detect(type_concat, "hotel"), 1, 0),
    other = if_else(residential + office + hotel == 0, 1, 0),
    
    type_simplified = case_when(
      residential + office + hotel >= 2 ~ "Mixed Use",
      residential == 1 & (office + hotel) == 0 ~ "Residential",
      office == 1 & (residential + hotel) == 0 ~ "Office",
      hotel == 1 & (residential + office) == 0 ~ "Hotel",
      TRUE ~ "Other"
    ))


data <- full_join(sumtbl, building, by = c("name_building" = "name"))

data <- data %>%
  rename(
    address = V1,
    address_2 = V2,
    link_2 = V3,
    drop = V4
  ) %>%
  select(-drop)

# Try to geolocate based on address
geodata <- data %>%
  filter(!str_detect(address, "Sao Paulo SP Brazil")) %>%
  mutate(
    address_adj = str_remove(address, "BR.*")
  ) %>%
  geocode(address = address_adj)
# Lots of errors
errors <- filter(geodata, is.na(lat))
# Try to adjust and geolocate errors
errors <- errors %>%
  mutate(
    end = str_replace(address_adj, "Av\\.", "Avenida"),
    end = str_replace(end, "R\\.", "Rua"),
    end = ifelse(str_detect(end, "-"),
                 unlist(str_split(end, "-"))[1],
                 end),
    end = str_sub(end, 1, str_locate(end, str_extract(end, "([0-9]+)"))[, 2])) %>%
  select(-lat, -long)

geodata2 <- geocode(errors, address = end)
# Bind together ()
geodata_full <- geodata %>%
  filter(!is.na(lat)) %>%
  rbind(geodata2 %>% filter(!is.na(lat)) %>% select(-end))

write_csv(select(data, -type), here("data/2021_10/skyscrapper_webscrape.csv"))
write_csv(select(geodata_full, -type), here("data/2021_10/skyscrapper_webscrape_geolocated.csv"))