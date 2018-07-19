# Load packages 

library(tidyverse)
library(rvest)
library(stringr)
library(robotstxt)

# Check bot permission

paths_allowed("https://www.dogfoodadvisor.com/dog-food-reviews/dry/1-star/")

#Create links of dry dog food

for(i in 1:5){
  base_url <- "https://www.dogfoodadvisor.com"
  pg_num <- paste0("dog-food-reviews/dry/",i,"-star/")
  pages <- read_html(file.path(base_url,pg_num)) %>%
    html_nodes(".more-space a , .nashville-close") %>%
    html_attr("href") #%>%
  
  
  #pages <- head(pages, 150) #for testing code
  
  #Save food pages locally
  
  # Create a directory to store downloaded pages
  data_dir <- paste0("R/",i)
  dir.create(data_dir, showWarnings = FALSE)
  
  # Create progress bar
  p <- progress_estimated(length(pages))
  
  # Download each food page
  walk(pages, function(url){
    download.file(url, destfile = file.path(data_dir,basename(url)), quiet = TRUE)
    p$tick()$print()
  })
  
  #Process all food info into df 
  
  # Create a character vector of names of all files in directory
  files <- dir(data_dir,  full.names = TRUE)
  
  # Function: get_food_details, to be applied to each food page
  get_food_details <- function(file) {
    #print(file)
    
    page <- read_html(file)
    
    # Grab the details of the Food (Main_ingredient, Protein, Fat, Carbohydrate)
    details <- page %>% 
      html_nodes(".dog-food-dashboard+ p strong , .dog-food-dashboard") %>% 
      html_text() 
    
    
    # Create data frame with the parsed values
    
    temp <- data_frame(Name = page %>% html_nodes("#content h4") %>% html_text(),
                       
                       # Use index to get Main ingredient from details
                       Main_Ingredient = details[2],
                       
                       # match string with Protein = and remove Protein using Grouping
                       Protein = details %>% str_match("Protein = ([0-9][0-9])") %>% .[,-1] %>% .[1],
                       
                       # match string with Fat = and remove Fat using Grouping
                       Fat = details %>% str_match("Fat = ([0-9][0-9])") %>% .[,-1] %>% .[1],
                       
                       # match string with 'Carbs =' and remove Carbs using Grouping
                       Carbohydrate = details %>% str_match("Carbs = ([0-9][0-9])") %>% .[,-1] %>% .[1],
                       
                       # match string with 'Fiber (estimated dry matter content) = ' and remove Fiber using Grouping
                       #Fiber = details %>% str_match("\Westimated dry matter content\W = ([0-9][.]?[0-9])/g") %>% .[,-1] %>% .[1]
    )
  } 
  
  # Apply the get_food_details function to each element of files
  lq <- map_df(files, get_food_details)
  
  
  # Save as csv
  write_csv(lq, path = paste0("R/dogfood-",i,".csv"))
}

#Add rating
five <- read_csv("dogfood-5.csv")
five <- five %>% mutate(Rating = 5)

four <- read_csv("dogfood-4.csv")
four <- four %>% mutate(Rating = 4)

three <- read_csv("dogfood-3.csv")
three <- three %>% mutate(Rating = 3)

two <- read_csv("dogfood-2.csv")
two <- two %>% mutate(Rating = 2)

one <- read_csv("dogfood-1.csv")
one <- one %>% mutate(Rating = 1)

dogfoodadvisor <- rbind(five, four, three, two, one)
#checknull <- is.na(dogfoodadvisor)
#str(checknull)

#Write ratings added df to file
write.csv(dogfoodadvisor, file="dogfoodadvisor_ratingadded.csv")

