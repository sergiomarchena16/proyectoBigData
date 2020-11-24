# UNIVERSIDAD DEL VALLE DE GUATEMALA
# DATA SCIENCE

# PROYECTO

# SERGIO MARCHENA - 16387

#Libreria para sentimientos
install.packages("sentimentr")
library(sentimentr)

dataReviews<-read.csv("reviews.csv")
dataItems<-read.csv("items.csv")

View(head(dataReviews,100))
View(head(dataItems,10))

# LIMPIEZA DE DATOS

# all words to lowercase
dataReviews$title <- tolower(dataReviews$title)
dataReviews$body <- tolower(dataReviews$body)

# reemplazo de caracteres
dataReviews$title <- gsub("@", "", dataReviews$title)
dataReviews$title <- gsub("!", "", dataReviews$title)
dataReviews$title <- gsub(",", "", dataReviews$title)
dataReviews$title <- gsub("-", "", dataReviews$title)
dataReviews$title <- gsub("?", "", dataReviews$title)
dataReviews$title <- gsub("/", "", dataReviews$title)

dataReviews$body <- gsub("@", "", dataReviews$body)
dataReviews$body <- gsub("!", "", dataReviews$body)
dataReviews$body <- gsub(",", "", dataReviews$body)
dataReviews$body <- gsub("-", "", dataReviews$body)
dataReviews$body <- gsub("?", "", dataReviews$body)
dataReviews$body <- gsub("/", "", dataReviews$body)
dataReviews$body <- gsub("()", "", dataReviews$body)
dataReviews$body <- gsub(")", "", dataReviews$body)
dataReviews$body <- gsub(":", "", dataReviews$body)

# Quitar stopwords, signos de puntuacion
install.packages("quanteda")
library(quanteda)

stopwords <- stopwords("en")
head(stopwords, 100)

install.packages("tm")
library(tm)

dataReviews$title <- removeWords(dataReviews$title, stopwords)
dataReviews$title <- removePunctuation(dataReviews$title)
dataReviews$title <- removeNumbers(dataReviews$title)

dataReviews$body <- removeWords(dataReviews$body, stopwords)
dataReviews$body <- removePunctuation(dataReviews$body)
dataReviews$body <- removeNumbers(dataReviews$body)

# nuevo dataset con columnas importantes

testdata<-subset(dataReviews[c(1,6,7)])
testdata$title <- as.character(testdata$title)
testdata$body <- as.character(testdata$body)
View(testdata)
#----------------- UTILIZACION de sentimentr -----------------#

testdata$sentimentTitle <- ""
testdata$positiveWordsTitle <- ""
testdata$negativeWordsTitle <- ""

var <- sentiment(testdata$title)
testdata$sentimentTitle <- var$sentiment
var2 <- extract_sentiment_terms(testdata$title)
testdata$positiveWordsTitle <- var2$positive
testdata$negativeWordsTitle <- var2$negative

testdata$sentimentBody <- ""
testdata$positiveWordsBody <- ""
testdata$negativeWordsBody <- ""

var <- sentiment(testdata$body)
testdata$sentimentBody <- var$sentiment
var2 <- extract_sentiment_terms(testdata$body)
testdata$positiveWordsBody <- var2$positive
testdata$negativeWordsBody <- var2$negative

#----------------- final de sentimentr -----------------#

# calcular promedios de aceptacion
testdata$promedioAceptacion <- ""
testdata$promedioAceptacion <- rowMeans(testdata[c(4,7)])

###
install.packages("dplyr")
library(dplyr)


prueba <- testdata
View(prueba)
prueba <- data.frame(prueba)
prueba2<- prueba %>%  group_by(asin)

prueba2 <- prueba2 %>% summarise(mean = mean(promedioAceptacion), n=n())

View(prueba2)
View(dataItems)

resultados <- prueba2
View(resultados)
resultados50 <- filter(resultados, n>50)
resultados50$brand <- ""     
View(resultados50)

casi <- merge(resultados50, dataItems, by = "asin")
marcas <- casi$brand.y

resultados50$brand <- marcas

copiares <- resultados50
View(copiares)

copiares <- data.frame(copiares)
copiares <- na.omit(copiares)

# Rename column where names is "Sepal.Length"
names(copiares)[names(copiares) == "mean"] <- "promedio"
names(copiares)[names(copiares) == "n"] <- "cant"

copiares<- copiares %>%  group_by(brand)
copiares <- copiares %>% summarise(mean = mean(promedio), n=n())
View(copiares)
plotdata<-subset(copiares[c(1,2)])
View(plotdata)
asinRes<- c("ROG", "Apple", "ASUS" ,"Google", "HUAWEI", "Motorola" ,"Nokia", "OnePlus", "Samsung" ,"Sony", "Xiaomi")

barplot(copiares$mean, names.arg = asinRes, ylab = "PromedioAceptacion")
install.packages("ggoplot2")
library(ggplot2)
plotdata <- data.frame(plotdata)
ggplot(data = plotdata, aes(x=brand, y=mean, color = asinRes))+
  geom_bar(stat="identity", width=0.5, fill="white") +
  geom_text(aes(label=mean), vjust=-0.3, size=3.3)


##########3




View(casi)
apple <- casi[casi$brand.y =="Apple", ]
View(apple)
apple <- apple[c(1,2,5)]
View(dataReviews)
summary(dataReviews)
View(as.Date.factor(dataReviews$date))

###########################33



plot(casi$brand.y, casi$rating, main = "Ratings", xlab="brand")
