# Load and put the first row of file as a column header
library(readr)
library(dplyr)
library(tidyr)
refine_original<- read.csv("refine_original.csv", header = TRUE, sep = ",", fill=TRUE)
# Understand the data
str(refine_original)
# 1- All lowercase
refine_lowercase <- mutate(refine_original, company = tolower(company))
# 1- Clean up brand names [misspelling..] -Replace Rows's Value

refine_clean <- refine_lowercase %>% 
  mutate(company = replace(company, company== "unilver", "unilever")) %>%
  mutate(company = replace(company, company== "unilever", "unilever")) %>%
  mutate(company = replace(company, company== "fillips" , "philips")) %>%
  mutate(company = replace(company, company== "philips" , "philips")) %>%
  mutate(company = replace(company, company=="phlips", "philips")) %>% 
  mutate(company = replace(company, company=="phillips", "philips")) %>% 
  mutate(company = replace(company, company=="ak zo", "akzo")) %>%
  mutate(company = replace(company, company=="akzo", "akzo")) %>%
  mutate(company = replace(company, company=="akz0", "akzo")) 

# 2- Separate the product code and product number into separate columns , remove the input column
refine_clean <- refine_clean %>%
  separate(Product.code...number, c( "product_code" , "product_number"), sep = '-', remove = TRUE)

# 3- Add product categories based on product code column
refine_clean <- refine_clean %>%
  mutate(product_category = product_code)  %>% 
  mutate(
    product_category = replace(product_category, product_code == "p", "Smartphone"), 
    product_category = replace(product_category, product_code == "x", "Laptop"),
    product_category = replace(product_category, product_code == "v", "TV"),
    product_category = replace(product_category, product_code== "q", "Tablet")
  )

# 4- Add full address for geocoding -  Create a new column full_address that concatenates the three address fields (address, city, country), separated by commas
#First split the address column so we can reverse the number and street name later
refine_clean <- refine_clean %>%
  separate(address, c( "street" , "number"), sep = ' ', remove = TRUE)
# Unite the street and number again
refine_clean <- refine_clean %>%
  unite(address, c( number, street ), sep = " ", remove = TRUE) 
# Concatenate number, street, city, country
refine_clean <- refine_clean %>%
  unite(full_address, c( address , city, country), sep = ",", remove = TRUE) 

# 5: Create dummy variables for company and product category

refine_clean <- refine_clean %>%
  mutate(company, company_philips = ifelse(company == "philips", "1","0")) %>%
  mutate(company, company_akzo = ifelse(company == "akzo", "1" , "0")) %>%
  mutate(company, company_van_houten = ifelse(company == "van houten", "1" , "0")) %>%
  mutate(company, company_van_houten = ifelse(company == "van houten", "1" , "0")) %>%
  mutate(company, company_unilever = ifelse(company == "unilever", "1" , "0")) %>%
  mutate(product_category, product_smartphone = ifelse(product_category == "Smartphone", "1" , "0")) %>%
  mutate(product_category, product_tv = ifelse(product_category == "TV", "1" , "0")) %>%
  mutate(product_category, product_laptop = ifelse(product_category == "Laptop", "1" , "0")) %>%
  mutate(product_category, product_tablet = ifelse(product_category == "Tablet", "1" , "0")) 

View(refine_clean)
write.csv(refine_clean,file="refine_clean.csv")