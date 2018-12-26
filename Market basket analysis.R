library(arules)
library(arulesViz)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(RColorBrewer)


setwd("D:\\Programming\\R")

df <- read.csv("Online Retail.csv")
head(df)

df <- df[complete.cases(df), ] # Drop missing values
df %>% mutate(Description = as.factor(Description),
              Country = as.factor(Country)) # Change Description and Country columns to factors

df$Date <- as.Date(df$InvoiceDate) # Change InvoiceDate to Date datatype

df$InvoiceDate <- as.Date(df$InvoiceDate)
TransTime<- format(as.POSIXct(df$InvoiceDate),"%H:%M:%S") # Extract time from the InvoiceDate column

InvoiceNo <- as.numeric(as.character(df$InvoiceNo)) # Convert InvoiceNo into numeric

cbind(df, TransTime, InvoiceNo) # Add new columns to original dataframe
glimpse(df)

# Group by invoice number and combine order item strings with a comma
transactionData <- ddply(df,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,collapse = ","))
transactionData$InvoiceNo <- NULL # Don't need these columns
transactionData$Date <- NULL

colnames(transactionData) <- c("items")
head(transactionData)

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = TRUE)


# MBA analysis 
# From package arules
tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')

summary(tr)

itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

# Generate the a priori rules
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
summary(association.rules)
inspect(association.rules[1:10]) # Top 10 association rules

# Select rules which are subsets of larger rules -> Remove rows where the sums of the subsets are > 1
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector

# What did customers buy before buying "METAL"
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="METAL"))
inspect(head(metal.association.rules))

# What did customers buy after buying "METAL"
metal.association.rules2 <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))
inspect(head(metal.association.rules2))

# Plotting
# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)

# Top 10 rules viz
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Filter top 20 rules with highest lift
# Paralell Coordinates plot - visualize which products along with which items cause what kind of sales.
# Closer arrows re bought together
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")
