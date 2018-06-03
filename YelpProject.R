# ===========================
# SET UP R
# ===========================
Rpt <- "/Users/namsan/Desktop/Spring\ 2018/Projects/Rpt"

# Shortcuts to folders of interest
CleanData <- paste0(Rpt,"/CleanData")
Dictionaries <- paste0(Rpt,"/Dictionaries")
RawData <- paste0(Rpt,"/RawData")
RCode <- paste0(Rpt,"/RCode")
RData <- paste0(Rpt,"/RData")
Output <- paste0(Rpt,"/Output")

# ===========================
# INSTALL PACKAGES 
# AND CREATE A THEME FOR GGPLOT
# ===========================
library(stringr)
library(ggplot2)
#installed.packages("ggcorrplot")
#install.packages("GGally")
library(GGally)
library(data.table)
#install.packages(c("RColorBrewer","scales"))
library(scales); library(grid); library(RColorBrewer)

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# install and set up jsonlite 
install.packages("jsonlite")
library(jsonlite)


# ===========================
# IMPORT DATA 
# ===========================

# import yelp data 
YelpBusiness <- stream_in(file(paste0(RawData,"/business.json")))
head(YelpBusiness)
str(YelpBusiness)

# flatten JSON data 
YelpBusiness_flat <- flatten(YelpBusiness)
str(YelpBusiness_flat)

# import Las Vegas Inspection data 
rawIns <- paste0(RawData,"/Restaurant_Inspections.csv")
rawIns <- data.table(read.csv(rawIns))

# 
goodIns <- rawIns[, Restaurant.Name := as.character(Restaurant.Name)]
goodIns <- rawIns[, Address := as.character(Address)]
str(goodIns)

# ===========================
# PREPARE DATA TO MERGE
# ===========================

# use data.table install.packages("data.table")
library(data.table)

raw.Data <- data.frame(YelpBusiness_flat)
class(raw.Data)
str(raw.Data)

# drop columns whose start with "hours" and "attributes"
names(raw.Data)
raw.Data1 <- raw.Data[, -grep("^hours",names(raw.Data))]
raw.Data1 <- raw.Data1[, -grep("^attributes",names(raw.Data))]

# convert data frame to data table and examine the variables 
raw.Data1 <- as.data.table(raw.Data1)

# take all the businesses in Las Vegas 
raw.Data2 <- raw.Data1[grep("Las Vegas", raw.Data1$city),]

# examine categories variable
raw.Data2 <- raw.Data2[, categories := as.character(categories)]
str(raw.Data2)

str(raw.Data2[,categories])
raw.Data1[2,categories]
table(raw.Data2[,categories])

# =============================================================================
# MERGE 2 DATA SETS
# =============================================================================

raw.Data2$name <- sapply(raw.Data2$name, str_to_title)
goodIns$Restaurant.Name <- sapply(goodIns$Restaurant.Name, str_to_title)

raw.Data3 <- setnames(raw.Data2, old = "name", new = "Restaurant.Name")
names(raw.Data3)
setkey(raw.Data3, Restaurant.Name)
setkey(goodIns, Restaurant.Name)

# perform the join, eliminating not matched rows from Right
Result <- merge(raw.Data3, goodIns, by ="Restaurant.Name")

Result1 <- Result[!duplicated(Result$Restaurant.Name)]

Result <- raw.Data3[goodIns, nomatch = 0]

length(unique(Result1))

summary(Result1)
str(Result1)

# =================
# CLEANING 
# =================
names(Result1)

goodData <- Result1[, c("Restaurant.Name", "neighborhood", "stars", "review_count", "Category.Name", "Current.Demerits","Current.Grade","Inspection.Demerits", "Inspection.Grade","Address","Zip", "longitude", "latitude","Location.1" )]


#write.csv(goodData, file = "restaurantproject1.csv")
str(goodData)
summary(goodData)


# actual cleaning
goodData$neighborhood <- as.factor(goodData$neighborhood)

# fix current grade and inspection grade 
goodData$Current.Grade <- tolower(as.character(goodData$Current.Grade))
table(goodData$Current.Grade)

goodData$Inspection.Grade <- tolower(as.character(goodData$Inspection.Grade))
table(goodData$Inspection.Grade)

# remove o and p 
goodData <- goodData[!goodData$Current.Grade == c("o"), ]
goodData <- goodData[!goodData$Inspection.Grade == c("p"), ]
goodData <- goodData[!goodData$Inspection.Grade == c(""), ]

# convert it back to factor 
goodData$Inspection.Grade <- factor(goodData$Inspection.Grade
                                    , levels = c("a", "b", "c", "x"))
levels(goodData$Inspection.Grade)

goodData$Current.Grade <- factor(goodData$Current.Grade, levels = c("a", "b", "c", "x"))
str(goodData)

# fill blank space wth NA 
is.na(goodData) <- goodData==''
setnames(goodData, c("Current.Demerits", "Inspection.Demerits"),c("Current.Violations", "Inspection.Violations"))

summary(goodData)

goodData.reg <- copy(goodData)

#==================
# VISUALIZATION 
#==================
library(stargazer)

qplot(data=goodData, stars) + theme_bw()

qplot(data=goodData, stars, review_count) + theme_bw()


# Average Star Rating by Category
stars.category <- aggregate(stars ~ Category.Name, goodData, mean)

ggplot(data=stars.category, aes(reorder(Category.Name, stars), stars)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(stars, 2)), hjust=2, size=2, color="white") + fte_theme() + labs(y="Average Star Rating by Category", x="Category", title="Average Yelp Review Star Ratings by Category")


# Average Star Rating by Neighborhood 
stars.neighborhood <- aggregate(stars ~ neighborhood, goodData, mean)

ggplot(data=stars.neighborhood, aes(reorder(neighborhood, stars), stars)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(stars, 2)), hjust=2, size=2, color="white") + fte_theme() + labs(y="Average Star Rating by Neighborhood ", x="Category", title="Average Yelp Review Star Ratings by Neighborhood") 


#correlation plot
ggcorr(goodData[, 2:10], label = TRUE)


# scatter plot stars and inspections demerits
ggplot(goodData, aes(x= stars, y=Current.Violations)) +
  geom_point(color = "darkred") +
  geom_smooth(method=lm) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Scatter plots cumulative violations and stars ") + fte_theme()


ggplot(goodData, aes(x= stars, y=Inspection.Violations)) +
  geom_point(color = "red") +
  geom_smooth(method=lm) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Scatter plots most recent violations and stars")+ fte_theme()


# multiple columns graph
dt1 <- setDT(data.frame(table(goodData$stars, goodData$Inspection.Grade)))

setnames(dt1, c("Var1", "Var2", "Freq"), c("stars", "Inspection.Grade","count"))
dt1[, stars := as.factor(stars)]
dt1[ , sumcount := sum(count), by = stars]
dt1[, percentage := (count/sumcount)*100]


ggplot(dt1,aes(x=stars,y=percentage,fill=factor(Inspection.Grade)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Inspection.Grade",
                      labels=c("a", "b", "c", "x")) +
  xlab("Stars")+ylab("Percentage") + theme_bw()


dt2 <- setDT(data.frame(table(goodData$stars, goodData$Current.Grade)))

setnames(dt2, c("Var1", "Var2", "Freq"), c("stars", "Current.Grade","count"))
dt2[, stars := as.factor(stars)]
dt2[ , sumcount := sum(count), by = stars]
dt2[, percentage := (count/sumcount)*100]

ggplot(dt2,aes(x=stars,y=percentage,fill=Current.Grade))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Stars")+ylab("Percentage") +
  scale_fill_brewer(palette="Dark2") + theme_bw()

# see the different among a grade group and the rest 
require("car")    
goodData2 <- copy(goodData)
goodData2$Inspection.Grade <- recode(goodData2$Inspection.Grade, "c('c','x')='b'")
goodData2$Current.Grade <- recode(goodData2$Current.Grade, "c('c','x')='b'")
names(goodData2)
levels(goodData2$Inspection.Grade)
goodData2$stars <- as.numeric(as.character(goodData2$stars))
str(goodData2)

# Overlaid histograms
ggplot(goodData2, aes(x=stars, fill=Inspection.Grade)) +
  geom_histogram(binwidth=.5, position="dodge")

# A basic box with the conditions colored
ggplot(goodData2, aes(x=Inspection.Grade, y = stars, fill=Inspection.Grade)) + geom_boxplot() + theme_bw()


table(goodData2$Inspection.Grade)

#==================
# REGRESSION
#==================
# ordered logit 
#install.packages("MASS")
library(MASS)
table(goodData.reg$stars)
goodData.regstars <- as.character((goodData.reg$stars))
#class(goodData$stars)
goodData.reg$stars <- factor(goodData.reg$stars, 
       levels = c("1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5"), #labels stars 
       ordered = TRUE)

levels(goodData.reg$stars)
class(goodData.reg$stars)
levels(goodData.reg$Inspection.Grade)
levels(goodData.reg$Current.Grade)

m1 <- polr(stars ~ . -Location.1-longitude-latitude-Zip-Address-Restaurant.Name, data=goodData.reg, Hess=TRUE)
summary(m1)

m1.coef <- data.frame(coef(summary(m1)))
m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE) * 2),2)
class(m1.coef)

significant <- m1.coef[m1.coef$pval < 0.05,]

m1.or=exp(coef(m1))
m1.or

significant

library(gridExtra)
grid.table(significant)
