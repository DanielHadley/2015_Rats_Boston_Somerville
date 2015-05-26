#### Created by DH to analyze rodent call data vis a vis Boston ####

setwd("C:/Users/dhadley/Documents/GitHub/2015_Rats_Boston_Somerville/")

# https://google.github.io/CausalImpact/
library(dplyr)
library(devtools)
library(CausalImpact)
library(ggplot2)
library(scales) # for changing from scientific notation
library(reshape2)

d <- read.csv("./data/Somerville_rats.csv")
dB <- read.csv("./data/Boston_rats.csv")
dNYC <- read.csv("./data/NYC_rats.csv")



#### This is how I retreived the data ####

## Boston
# todayText <- format(today, "%b %d")
# lastWeekText <- format(lastWeek, "%b %d")
# Year <- format(today, "%Y")
# 
# 
# api <- paste("http://data.cityofboston.gov/resource/awu8-dc52.csv?$where=type=%27Rodent%20Activity%27AND%20open_dt%20%3E%20%27", lastWeek, "%27", sep="")

# OR

### Now Boston data ###
# Import data from Socrata
# Using their api
# dBRaw <- read.csv(url("http://data.cityofboston.gov/resource/awu8-dc52.csv?$limit=20000&type=Rodent%20Activity"))


# Duplicate the dataset so I don't have to keep importing from Socrata
# dB <- dBRaw
# write.csv(dBRaw, file = "./data/bos.csv")


## Somerville
# d <- read.csv("../2015_Constituent_Services/data/Daily.csv")
# 
# RatData <- d %>%
#   filter(Service.Type == "Health-Rodents-Rats")
# 
# write.csv(RatData, "./data/Somerville_rats.csv")


# # Somerville
# d <- read.csv("../2015_Constituent_Services/data/Daily.csv")
# 
# RatData <- d %>%
#   filter(Service.Type == "Health-Rodents-Rats")
# 
# write.csv(RatData, "./data/Somerville_rats.csv")
# 
# ## Finally NYC data ##
# # Import data from Socrata
# # Using their api
# # This will not get all of the calls, 
# dBRaw <- read.csv(url("http://data.cityofnewyork.us/resource/erm2-nwe9.csv?$limit=20000&descriptor=Rat%20Sighting"))
# write.csv(dBRaw, file = "./data/nyc.csv")



#### Clean data ####

# I usually make a "1" column to make tabulations easier
d$Tab <- 1


# Date
d$Date <- as.Date(d$Date,"%m/%d/%Y") # Tell R it's a date
d$Month <- format(d$Date, format='%m') # Break it into month, day, year...
d$Day <- format(d$Date, format='%d')
d$Year <- format(d$Date, format='%Y')
d$Month <- as.numeric(as.character(d$Month)) # Transform month to numeric for ifelse
# There are more nuanced ways of dividing seasons, but I prefer 4 even periods:
d$Season <- ifelse((d$Month >= 3) & (d$Month <= 5), "Spring", 
                   ifelse((d$Month >= 6) & (d$Month <= 8), "Summer",
                          ifelse((d$Month >= 9) & (d$Month <= 11), "Fall", "Winter")))

d$MonthYear <-  format(d$Date, "%y %m")

d$MonthTwo <- as.Date(cut(d$Date,
                          breaks = "month"))

# Take out 2007 if neccessary 
# d <- d[ which(d$Year != 2007), ]


# Makes seperate columns for the month, day, and year
dB$Date <- as.Date(dB$OPEN_DT, format='%m/%d/%Y %H:%M:%S')
dB$Month <- format(dB$Date, format='%m') # Break it into month, day, year...
dB$Day <- format(dB$Date, format='%d')
dB$Year <- format(dB$Date, format='%Y')
dB$Month <- as.numeric(as.character(dB$Month)) # Transform month to numeric for ifelse
# There are more nuanced ways of dividing seasons, but I prefer 4 even periods:
dB$Season <- ifelse((dB$Month >= 3) & (dB$Month <= 5), "Spring", 
                    ifelse((dB$Month >= 6) & (dB$Month <= 8), "Summer",
                           ifelse((dB$Month >= 9) & (dB$Month <= 11), "Fall", "Winter")))

dB$MonthYear <-  format(dB$Date, "%y %m")

dB$MonthTwo <- as.Date(cut(dB$Date,
                           breaks = "month"))

# Tab
dB$Tab <- 1

# Makes seperate columns for the month, day, and year
dNYC$Date <- as.Date(dNYC$Created.Date, format='%m/%d/%Y %H:%M:%S')
dNYC$Month <- format(dNYC$Date, format='%m') # Break it into month, day, year...
dNYC$Day <- format(dNYC$Date, format='%d')
dNYC$Year <- format(dNYC$Date, format='%Y')
dNYC$Month <- as.numeric(as.character(dNYC$Month)) # Transform month to numeric for ifelse
# There are more nuanced ways of dividing seasons, but I prefer 4 even periods:
dNYC$Season <- ifelse((dNYC$Month >= 3) & (dNYC$Month <= 5), "Spring", 
                    ifelse((dNYC$Month >= 6) & (dNYC$Month <= 8), "Summer",
                           ifelse((dNYC$Month >= 9) & (dNYC$Month <= 11), "Fall", "Winter")))

dNYC$MonthYear <-  format(dNYC$Date, "%y %m")

dNYC$MonthTwo <- as.Date(cut(dNYC$Date,
                           breaks = "month"))

# Tab
dNYC$Tab <- 1


#### Prepare for the charts ####

som <- aggregate(Tab ~ MonthTwo, d, sum ) # makes a two-way table
bos <- aggregate(Tab ~ MonthTwo, dB, sum ) # makes a two-way table
nyc <- aggregate(Tab ~ MonthTwo, dNYC, sum ) # makes a two-way table


bosom <- merge(som, bos, by="MonthTwo")
bosom <- rename(bosom, Somerville = Tab.x, Boston = Tab.y, Date = MonthTwo)
bosom$Difference <- (bosom$Somerville - bosom$Boston) / bosom$Boston


# Z Scores
bosom$BostonZ <- (bosom$Boston-mean(bosom$Boston))/sd(bosom$Boston)
bosom$SomervilleZ <- (bosom$Somerville-mean(bosom$Somerville))/sd(bosom$Somerville)

# Take out incomplete May
bosom <- bosom %>%
  filter(Date != "2015-05-01")


####  Visualize ####



lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"

my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )


# Weather chart: to show how calls were affected by the mild winter
somFig1 <- som[26:61,]
  
ggplot(somFig1, aes(x=MonthTwo)) + geom_line(aes(y = Tab)) + 
  my.theme + ggtitle("Calls to 311 About Rats") + xlab("Date")+ylab("Monthly Calls") + 
  guides(color=guide_legend(title="City")) +theme(legend.key=element_rect(fill=NA)) +
  annotate("errorbar", x=as.Date("2012-01-01"), ymin=0, ymax=25, width=15, size=.4) + 
  annotate("text", x=as.Date("2011-10-04"), y=15, label="mild winter", size=3) +
  annotate("text", x=as.Date("2012-05-01"), y=125, label="new peak", size=3) +
  scale_x_date(breaks = "1 year",labels=date_format("%Y"))

ggsave("./plots/Fig1_Somerville_Calls.png", dpi=300, width=5, height=3)


ggplot(bosom, aes(x=Date)) + geom_line(aes(y = Boston, colour = "Boston")) + 
  geom_line(aes(y = Somerville, colour = "Somerville")) +
  my.theme + ggtitle("Calls to 311 About Rats") + xlab("Date")+ylab("Monthly Calls") + 
  guides(color=guide_legend(title="City")) +theme(legend.key=element_rect(fill=NA)) +
  scale_colour_manual(name='', values=c('Boston'=teele, 'Somerville'=pinkish_red)) + 
  geom_ribbon(aes(ymin=Somerville, ymax=Boston), fill="slategray3", alpha=0.25) + 
  annotate("errorbarh", y=350, x=as.Date("2013-11-05"), xmin=as.Date("2013-11-15"), xmax=as.Date("2015-04-01"), height=15, size=.4) + 
  annotate("text", x=as.Date("2014-08-15"), y=365, label="new initiatives", size=3)

ggsave("./plots/Fig2_Somerville_v_Boston_Calls.png", dpi=300, width=5, height=3)


#### Analysis of the causal impact ####


# Create data in the same format as the example
data <- bosom[,2:3]
data <- rename(data, c("Somerville" = "y", "Boston" = "x1"))

pre.period <- c(1, 29)
post.period <- c(30, 46)

impact <- CausalImpact(data, pre.period, post.period)


impact.plot <- plot(impact) + theme_bw(base_size = 8)
plot(impact.plot)

ggsave("./plots/Fig3_CausalImpact.png", dpi=250, width=4, height=4)


summary(impact)
summary(impact, "report")
