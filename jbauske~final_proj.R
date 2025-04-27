# Janelle Bauske
# Final Project Code

# load data from FDIC
banklist <- read_csv("IST 719/week 6/hw6/banklist.csv")
bfb_all_data <- read_csv("IST 719/week 6/hw6/bfb-all-data.csv")
bank_data <- read_csv("IST 719/week 6/hw6/bank-data.csv")

# convert date to date format
banklist$`Closing Date `[1:10] # D-MMM-YY
#?strptime() # %e-%b-%y
my.date <- as.Date(strptime(banklist$`Closing Date `, "%e-%b-%y"))
my.date[1:10]
banklist$closing_date <- my.date

# fix var name for merging
banklist$bank_name <- banklist$`Bank Name `
banklist <- subset(banklist, select = -c(`Bank Name `, `Closing Date `))
str(banklist)

# split/process column for merging
library(stringr)
bfb_all_data[c('bank_name', 'city', 'state')] <- 
  str_split_fixed(bfb_all_data$`Bank Name, City, State`, ', ', 3)
bfb_all_data <- subset(bfb_all_data, select = -c(`Bank Name, City, State`))
str(bfb_all_data)

# merge datasets b/c the FDIC is annoying and didn't put all the variables in
# one file
bank_failure <- merge(banklist, bfb_all_data, by = "bank_name")
bank_failure <- subset(bank_failure, 
                       select = -c(city, state, `Closing Date`, 
                                   `Acquirer & Transaction`))
bank_failure <- merge(bank_failure, bank_data, by.x = "Cert ", by.y = "CERT")
bank_failure <- subset(bank_failure, select = -c(CHCLASS1, CITYST, FAILDATE, FIN,
                                                 ID, NAME, QBFASSET, QBFDEP,
                                                 RESTYPE1, SAVR))

# fix up numeric vars for use
bank_failure$`Approx. Asset (Millions)` = gsub("\\$", "", 
                                               bank_failure$`Approx. Asset (Millions)`)
bank_failure$`Approx. Deposit (Millions)` = gsub("\\$", "", 
                                                 bank_failure$`Approx. Deposit (Millions)`)

bank_failure$`Approx. Asset (Millions)` <- as.numeric(bank_failure$`Approx. Asset (Millions)`)
bank_failure$`Approx. Deposit (Millions)` <- as.numeric(bank_failure$`Approx. Deposit (Millions)`)
# the FDIC can literally fight me b/c wdym you put the wrong scale in the df??
bank_failure$`Approx. Asset (Thousands)` <- bank_failure$`Approx. Asset (Millions)`
bank_failure$`Approx. Deposit (Thousands)` <- bank_failure$`Approx. Deposit (Millions)`

bank_failure <- subset(bank_failure, select = -c(`Approx. Asset (Millions)`,
                                                 `Approx. Deposit (Millions)`))

# double-check that dataset is usable 
str(bank_failure)
nrow(bank_failure)
ncol(bank_failure)

(ncol(bank_failure) * 4) * (nrow(bank_failure)/100) >= 100

#par(mar = c(7,4,4,2))

# plot 1 (old)
boxplot(bank_failure$`Approx. Asset (Thousands)`, col = "darkorchid4", 
        main = "Distribution of Approximate Assets (in Thousands of Dollars)", 
        sub = "For Banks that Failed from 2001-2023",
        ylab = "Approximate Assets (in Thousands of Dollars)")

# plot 2 (old)
hist(bank_failure$`Approx. Deposit (Thousands)`, col = "cadetblue", 
     main = "Distribution of Approximate Deposits (in Thousands of Dollars)", 
     sub = "For Banks that Failed from 2001-2023",
     xlab = "Approximate Deposits (in Thousands of Dollars)", 
     ylab = "Frequency of Values")

# plot 3 (old)
barplot(table(bank_failure$`State `), col = "darkseagreen4", 
        main = "Bank Closures per State", xlab = "State", 
        ylab = "Number of Bank Closures", 
        sub = "For Banks that Failed from 2001-2023")

# list number of bank closures per day
df <- aggregate(bank_failure$bank_name, list(bank_failure$closing_date), length)
head(df)
colnames(df) <- c("date", "bank_closures.count")

# plot 4 (old)
plot(df$date, df$bank_closures.count, type = "l", col = "coral3", 
     xlab = "Date of Bank Closure", 
     ylab = "Number of Bank Closures Per Day", 
     main = "Daily Bank Closures Over Time", 
     sub = "For Banks that Failed from 2001-2023")

#install.packages("maps")
library(maps)
#install.packages("mapproj")
library(mapproj)
#install.packages("dplyr")
library(dplyr)
library(RColorBrewer)
library(ggplot2)

df2 <- aggregate(bank_failure$`Approx. Asset (Thousands)`, 
                 list(`Closing Date` = bank_failure$closing_date), sum)
head(df2)
colnames(df2)[2] <- c("Total Approx. Asset (Thousands)")

df3 <- aggregate(bank_failure$`Approx. Asset (Thousands)`, 
                 list(`Closing Date` = bank_failure$closing_date), mean)
head(df3)
colnames(df3)[2] <- c("mean_assets")

df2$mean_assets <- df3$mean_assets

bank_failure$date_year <- year(bank_failure$closing_date)

df4 <- aggregate(bank_failure$`Approx. Asset (Thousands)`, 
                 list(`Closing Year` = bank_failure$date_year), mean)
head(df4)
colnames(df4)[2] <- c("Mean Approx. Asset (Thousands) per Year")

df2$closing_year <- year(df2$`Closing Date`)

df2_new <- merge(df2, df4, by.x = "closing_year", by.y = "Closing Year")
df2_new$closing_year <- as.factor(df2_new$closing_year)


# Plot 1 Final
ggplot(df2_new) + aes(x = `Total Approx. Asset (Thousands)`, y = `Closing Date`) + 
  geom_violin(fill = "darkorchid4", color = "darkorchid4") + coord_flip() +
  theme_minimal() +
  labs(title = "Total Approximate Assets (in Thousands of Dollars) Over Time", 
       subtitle = "For Banks that Failed from 2001-2023",
       x = "Total Approximate Assets (in Thousands of Dollars)")


# Plot 2 Final
ggplot(bank_failure) + aes(x = `Approx. Deposit (Thousands)`) + 
  geom_histogram(fill = "cadetblue", color = "cadetblue3") + theme_minimal() +
  labs(title = "Distribution of Approximate Deposits (in Thousands of Dollars)", 
       subtitle = "For Banks that Failed from 2001-2023",
       x = "Approximate Deposits (in Thousands of Dollars)",
       y = "Frequency of Values")


bank_failure_counts <- as.data.frame(table(bank_failure$`State `))
names(bank_failure_counts) <- c("State", "Number of Bank Closures")

# Plot 3 Final
ggplot(bank_failure_counts) + aes(x = State, y = `Number of Bank Closures`) +
  geom_bar(stat = "identity", fill = "darkseagreen4", color = "darkseagreen3") +
  theme_minimal() +
  labs(title = "Bank Closures per State",
       subtitle = "For Banks that Failed from 2001-2023",
       x = "State",
       y = "Number of Bank Closures")


# Plot 4 Final
ggplot(df) + aes(x = bank_closures.count, y = date) + 
  geom_violin(fill = "coral3", color = "coral3") + theme_minimal() + coord_flip() +
  labs(title = "Total Bank Closures Over Time",
       subtitle = "For Banks that Failed from 2001-2023",
       y = "Date of Bank Closure",
       x = "Number of Bank Closures")


library(ggmap)
#install.packages("osmdata")
#library(osmdata)

# don't do any of this it did not do I wanted it to do >>>
api_key <- "AIzaSyDExGx4xm0qmSuMK4Dr7MCDQ_2HvgUdCKE"
register_google(key = api_key)
cities <- bank_failure[, c("City ", "State ")]  
# Geocode the city and state information using ggmap
geocoded <- geocode(paste(cities$`City `, cities$`State `, sep = ", "), 
                    source = "google", api_key = api_key)
# Combine the geocoded coordinates with the original data frame
bank_failure <- cbind(bank_failure, geocoded)
# Filter out rows with missing coordinates
bank_failure <- bank_failure[complete.cases(bank_failure[, c("lon", "lat")]), ]


# trying this a different way AND IT WORKED YESSSSSSSS
bank_failure$region <- tolower(state.name[match(bank_failure$`State `, state.abb)])
df6 <- aggregate(bank_failure$bank_name, list(bank_failure$region), length)
df6
colnames(df6) <- c('region', 'Number of Banks Closed')

m <- map("state")
m$names

# lots of aggregating and merging
new_map_data <- left_join(map_data("state"), df6, by="region")
new_map_data <- left_join(new_map_data, bank_failure, by = "region")
new_map_data <- new_map_data[order(new_map_data$order),]
#View(new_map_data)

# like, lots and lots
df7 <- aggregate(bank_failure$ESTLOSS, list(bank_failure$region), mean)
head(df7)
colnames(df7) <- c('region', 'Average Loss Per State (in Thousands of Dollars)')
df7$`Average Loss Per State (in Thousands of Dollars)` <- 
  df7$`Average Loss Per State (in Thousands of Dollars)`/1000

new_map_data <- left_join(new_map_data, df7, by="region")
new_map_data <- new_map_data[order(new_map_data$order),]
#View(new_map_data)

# summarizing est loss per state for plotting
summarized_data <- new_map_data %>%
  group_by(region) %>%
  summarize(mean_long = mean(long),
            mean_lat = mean(lat),
            `Average Loss (in Thousands of Dollars)` = 
              mean(`Average Loss Per State (in Thousands of Dollars)`))

my.pal <- colorRampPalette(c("darkseagreen4","cadetblue","darkorchid4","coral3"))

# Plot 5 Final: The Big Cheese, Wow That's A Lot Of Code, Now Don't Touch It Or It Will Break
ggplot(new_map_data) + 
  geom_polygon(aes(long, lat, group = group, fill = `Number of Banks Closed`), color = "gray") + 
  scale_fill_gradientn(colors = my.pal(10), na.value = "aliceblue") +
  xlab("") + ylab("") + theme(legend.position = "right", 
                              axis.text = element_blank(), 
                              axis.ticks = element_blank(),
                              panel.background = element_blank()) +
  coord_map("albers", at0 = 45.5, lat1 = 29.5) +
  geom_point(data = summarized_data, aes(x = mean_long, y = mean_lat, 
                             size = `Average Loss (in Thousands of Dollars)`),
                             shape = 21, fill = "darkgoldenrod1", color = "gray") +
  labs(title = "Aggregation Per State",
       subtitle = "For Banks that Failed from 2001-2023")
  











