# This script scrapes municipal clerk contact info from a elections.wi.gov pdf and saves it as a csv

library(tidyverse)
library(magrittr)
library(pdftools)

text <- pdf_text("https://elections.wi.gov/sites/elections.wi.gov/files/2020-01/WI%20Municipal%20Clerks%20%20no%20emails%20Updated%201-24-2020.pdf")

## to test on one page 
# text <- text[1]

# remove header and footer
text %<>% str_remove_all(".*\n.*\n.*\n.*Last Updated|Printed.*")

# retain town/city/village info from the first line
line1 <- str_extract_all(text, "(TOWN OF|CITY OF|VILLAGE OF ).*") %>% unlist()

# split entries
entries <- text %>% str_split("(TOWN OF|CITY OF|VILLAGE OF )") %>% unlist()

# drop any entries less than 100 characters
entries  %<>% .[nchar(entries) > 100]

# split all lines per entry
lines <- str_split(entries, "\n") 

# put info into a dataframe
d <- tibble(Municipality = str_c(line1 %>% str_remove(" -.*| [0-9].*"),# first line before HINDI
                                 map_chr(lines, ~{.[[2]]}), # second line 
                                 sep = " "), 
            HINDI = line1 %>% str_extract("[0-9]+"), # the first set of numbers
            Website = line1 %>% str_extract("htt.*? "),
            Updated = line1 %>% str_extract("[0-9]+/[0-9]+/[0-9]+"),
            Clerk = line1 %>% str_extract("CLERK: .*") %>% 
              str_remove_all("CLERK: |[0-9].*|   .*"),
            Deputy_Clerk = entries %>% str_extract("DEPUTY CLERK: .*") %>% 
              str_remove_all("DEPUTY CLERK: |[0-9].*|   .*"),
            Address = entries %>% str_extract("Municipal Address :.*\n.*") %>% 
              str_remove("Municipal Address :"), # \n is line break, so this returns two lines
            Address_Mailing = entries %>% str_extract("Mailing Address :.*\n.*") %>% 
              str_remove("Mailing Address :"),
            Phone_1 = entries %>% str_extract("Phone 1: .*") %>% 
              str_remove("Phone 1: "),
            Fax = entries %>% str_extract("Fax: .*") %>% 
              str_remove("Fax: "))

# remove extra white space from all columns
d %<>% mutate_all(str_squish)

# save in data folder
write_csv(d, path = here::here("data", "WI_Municipal_Clerks.csv"))
