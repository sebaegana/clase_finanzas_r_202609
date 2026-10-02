#install.packages("pdftools")
#install.packages("stringr")

library(pdftools)
library(stringr)
# Using poppler version 22.04.0

border_patrol <- pdf_text("usbpstatsfy2017sectorprofile.pdf")

head(border_patrol)

length(border_patrol)

border_patrol[1]

strsplit("criminology", split = "n")

sector_profile <- border_patrol[1]
sector_profile <- strsplit(sector_profile, "\n")
sector_profile <- sector_profile[[1]]

head(sector_profile)

sector_profile

sector_profile <- trimws(sector_profile)

sector_profile

grep("Miami", sector_profile)

grep("Nationwide Total", sector_profile)

sector_profile <- sector_profile[grep("Miami", sector_profile):
                                   grep("Nationwide Total", sector_profile)]

head(sector_profile)

sector_profile

sector_profile <- str_split_fixed(sector_profile, " {2,}", 10)

head(sector_profile)

sector_profile <- data.frame(sector_profile)
names(sector_profile) <- c("sector",
                           "agent_staffing",
                           "apprehensions",
                           "other_than_mexican_apprehensions", 
                           "marijuana_pounds",
                           "cocaine_pounds",
                           "accepted_prosecutions",
                           "assaults",
                           "rescues",
                           "deaths")