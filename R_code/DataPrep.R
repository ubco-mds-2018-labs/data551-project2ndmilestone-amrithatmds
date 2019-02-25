# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd("C:/551/presentation/R_code")
loadhistory(file = ".RHistory")

pkgs <- c('dplyr', 'tidyr', 'chron', 'lubridate')
lapply(pkgs, require, character.only = T)

# read the data file
dat <- read.csv('../data/crime.data.csv')

# clean the data
# Each Precinct can be viewed as a factor
dat$Precinct <- as.factor(dat$Precinct)

# Fix column names
# names(dat)
names(dat)[1] <- 'PublicAddress'
names(dat)[names(dat) == "Time.of.Offense"] <- "OffenseTime"

# Split the timestamp so that we can view crime by year, month, day of the week, time etc.
dat <- dat %>%
  separate(ReportedDate, c("ReportedDate","ReportedTime"), "T", fill = "right")

# Set the data type for date and time values
dat$ReportedDate <- as.Date(dat$ReportedDate)
head(as.POSIXct(dat$ReportedTime, format = "%H:%M:%S", tz="CST6CDT"))
dat$OffenseTime <- chron(times=dat$OffenseTime)


# The dataset is mostly good with not many missing values. The some missing values are due to the 
# absene of Reported time. As we don't loose much data by removing these we can remove these rows
# for better results
dat <- na.omit(dat)

# The latitubes and longitudes that are not coded correctly can be removed
dat <- dat[dat$Latitude != 0 | dat$Longitude != 0, ]

# After looking up in the plot I see some data points outside the city of Minneapolist, they are coded wrongly and can be removed
#dat <- dat[dat$Latitude >  | dat$Longitude > , ]


# Group the Neighborhoods by community for a more regional view of the crime 
# This is based on the Neighborhoods info provided in the website:https://en.wikipedia.org/wiki/Neighborhoods_of_Minneapolis
dat$Community <- factor(ifelse(dat$Neighborhood %in% c("Bryn - Mawr", 
                                                       "CARAG",
                                                       "Cedar - Isles - Dean",
                                                       "East Calhoun",
                                                       "East Isles",
                                                       "Kenwood",
                                                       "Lowry Hill",
                                                       "Lowry Hill East",
                                                       "West Calhoun",
                                                       "ECCO"),
                                                      "Calhoun-Isles",
                               ifelse(dat$Neighborhood %in% c("Cleveland",
                                                              "Folwell",
                                                              "Lind - Bohanon",
                                                              "McKinley",
                                                              "Shingle Creek",
                                                              "Victory",
                                                              "Webber - Camden",
                                                              "Camden Industrial",
                                                              "Humboldt Industrial Area"),
                                      "Camden",
                                      ifelse(dat$Neighborhood %in% c("Downtown East",
                                                                     "Downtown West",
                                                                     "Elliot Park",
                                                                     "Loring Park",
                                                                     "North Loop",
                                                                     "Steven's Square - Loring Heights"),
                                             "Central",
                                             ifelse(dat$Neighborhood %in% c("Cooper",
                                                                            "Hiawatha",
                                                                            "Howe",
                                                                            "Longfellow",
                                                                            "Seward"),
                                                    "Longfellow",
                                                    ifelse(dat$Neighborhood %in% c("Harrison",
                                                                                   "Hawthorne",
                                                                                   "Jordan",
                                                                                   "Near - North",
                                                                                   "Sumner - Glenwood",
                                                                                   "Willard - Hay"),
                                                           "Near North",
                                                           ifelse(dat$Neighborhood %in% c("Diamond Lake",
                                                                                          "Ericsson",
                                                                                          "Field",
                                                                                          "Hale",
                                                                                          "Keewaydin",
                                                                                          "Minnehaha",
                                                                                          "Morris Park",
                                                                                          "Northrop",
                                                                                          "Page",
                                                                                          "Regina",
                                                                                          "Wenonah"),
                                                                  "Nokomis",
                                                                  ifelse(dat$Neighborhood %in% c(
                                                                    "Audubon Park",
                                                                    "Beltrami",
                                                                    "Bottineau",
                                                                    "Columbia Park",
                                                                    "Holland",
                                                                    "Logan Park",
                                                                    "Marshall Terrace",
                                                                    "Northeast Park",
                                                                    "Sheridan",
                                                                    "St. Anthony East",
                                                                    "St. Anthony West",
                                                                    "Waite Park",
                                                                    "Windom Park"),
                                                                    "Northeast", 
                                                                    ifelse(dat$Neighborhood %in% c(
                                                                      "East Phillips",
                                                                      "Midtown Phillips",
                                                                      "Phillips West",
                                                                      "Ventura Village"),
                                                                      "Phillips", 
                                                                      ifelse(dat$Neighborhood %in% c(
                                                                        "Bancroft",
                                                                        "Bryant",
                                                                        "Central",
                                                                        "Corcoran",
                                                                        "Lyndale",
                                                                        "Powderhorn Park",
                                                                        "Standish",
                                                                        "Whittier"),
                                                                        "Powderhorn", 
                                                                        ifelse(dat$Neighborhood %in% c(
                                                                          "Armatage",
                                                                          "East Harriet",
                                                                          "Fulton",
                                                                          "Kenny",
                                                                          "King Field",
                                                                          "Linden Hills",
                                                                          "Lynnhurst",
                                                                          "Tangletown",
                                                                          "Windom"),
                                                                          "Southwest", 
                                                                          ifelse(dat$Neighborhood %in% c(
                                                                            "Cedar Riverside",
                                                                            "Como",
                                                                            "Marcy Holmes",
                                                                            "Nicollet Island - East Bank",
                                                                            "Prospect Park",
                                                                            "Prospect Park - East River Road",
                                                                            "University of Minnesota",
                                                                            "Mid - City Industrial"),
                                                                            "University", 
                                                                            dat$Neighborhood))))))))))))



# Group the crime types so that they can be viewed based on their severity
dat$OffenseType <- factor(
  ifelse(dat$Offense %in% c("ROBBIZ", "ROBPAG", "ROBPER"),
         "Robbery",
         ifelse(dat$Offense %in% c("AUTOTH", "BIKETF", "COINOP", "COMPUT", "LOOT", "MVTHFT", "NOPAY", "ONLTHT", "POCKET", "SCRAP", "SHOPLF", "TBLDG", "TFMV", "TFPER", "THEFT", "THFTSW", "TMVP"),
                "Theft",
                ifelse(dat$Offense %in% c("BURGB", "BURGD"),
                       "Burglary",
                       ifelse(dat$Offense %in% c("ADLTTN"),
                              "Aggravated Assault",
                              ifelse(dat$Offense %in% c("DISARM"),
                                     "Attack on Police",
                                     ifelse(dat$Offense %in% c("ARSON"),
                                            "Arson", dat$Offense)))))))

dat$Severity <- factor(ifelse(dat$OffenseType %in% c("Robbery","Aggravated Assault", "Attack on Police"),
                              "Crimes on Individual",
                              ifelse(dat$OffenseType %in% c("Burglary","Theft", "Arson"),
                                     "Crimes Not on Individual", "")))



# Create attributes that tell the year, month and day of the week so that we can view crime by these factors
# Create new columns for the EstOffense weekday, month, and year
dat$YearOfCrime <- factor(year(dat$ReportedDate))
dat$MonthOfCrimee <- factor(months(dat$ReportedDate, abbreviate= FALSE))
dat$WeekdayOfCrime <- factor(weekdays(dat$ReportedDate, abbreviate= FALSE))


# Crime time viewed based on the time of the day
dat <- transform(dat, OffenseTimeOfDay = cut(OffenseTime,
                                                   breaks = times(c("00:00:00", "06:00:00",
                                                                    "18:00:00", "23:59:00")),
                                                   labels = c("Night", "Day", "Evening"),
                                                   include.lowest = TRUE))

str(dat)

# Some fixes to the data after viewing the plots
dat$Neighborhood <- str_replace(string = dat$Neighborhood, pattern = "King Field Field", replacement = "King Field")
dat$Neighborhood <- str_replace(string = dat$Neighborhood, pattern = "Ceder - Isles - Dean", replacement = "Cedar - Isles - Dean")

# Remove the observations for 2016 as the data is available only until July
dat <- dat[dat$YearOfCrime != 2016, ]

# Write the prepared data file
# write.csv(dat, "../prepared_data/minnCrime.csv")



