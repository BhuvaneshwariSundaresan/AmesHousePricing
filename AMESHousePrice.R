#########workingdirectory##################
setwd("C:/Users/Bhuvaneshwari/OneDrive - Queen's University Belfast/Semester1/StatisticsforBusiness/Assignment1Data")
getwd()

install.packages("readxl")
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("ggpubr")
install.packages("lmtest")
install.packages("car") 
install.packages("stargazer")


library(readxl) #readexcel
library(dplyr)
library(ggplot2)
library(caret)
library(ggpubr) #corin graph
library(corrplot) #cormatrix
library(lmtest) #dwtest
library(car)
library(stargazer)


#############importing#########
ames_rawdata <- read_excel("ames.xlsx")

summary(ames_rawdata)
nrow(ames_rawdata)
ncol(ames_rawdata)
str(ames_rawdata)

is_numeric <- sapply(ames_rawdata, is.numeric)
numeric_columns <- names(ames_rawdata)[is_numeric]
character_columns <- names(ames_rawdata)[!is_numeric]
print("Numeric columns:")
print(numeric_columns)
print("Character columns:")
print(character_columns)
############DQ##################
boxplot(ames_rawdata$sale_price, main = "Outliers in sale_price")
scatter.smooth(ames_rawdata$rooms_tot, ylab = "Total Rooms", main = "Outliers in rooms_tot")
boxplot(ames_rawdata$lot_area, main = "Outliers in lot_area")
ggplot(data = ames_rawdata, aes(x = house_quality)) +
  geom_bar() +
  labs(x = "House Quality", y = "Frequency", title = "Distribution of House Quality")

ames_cleandata <- ames_rawdata %>% 
  mutate(
    zone = ifelse(zone == "A (agr)", "A", ifelse(zone == "C (all)", "C", ifelse(zone == "I (all)", "I", zone))),
    building = ifelse(building == 'Twnhs', 'TwnhsI', building)
  ) %>% 
  filter(
    !(rooms_tot < 1),
    !(house_quality > 10),
    !(sale_price > 800000),
    !(lot_area < 15 | lot_area > 100000),
    !is.na(garage_area)  # Add this line to filter out rows with NA in garage_area
  )

ames_cleandata$neighbourhood[ames_cleandata$neighbourhood == "Landmrk"]<-NA
ames_cleandata$roof_material[ames_cleandata$roof_material == "ClyTile"]<-NA
ames_cleandata$roof_material[ames_cleandata$roof_material == "Roll"]<-NA
ames_cleandata <- ames_cleandata %>% filter(!is.na(neighbourhood), 
                                            !is.na(roof_material))

nrow(ames_cleandata)
ncol(ames_cleandata)

ames_cleandata <- ames_cleandata %>%
  mutate(across(c(neighbourhood, roof_material, house_quality, kitchen_qual, stories, building), as.factor))
str(ames_cleandata)

#######correlation##########


cor(ames_cleandata$sale_price, ames_cleandata$garage_area, method = "pearson")
cor(ames_cleandata$sale_price, ames_cleandata$year_built, method = "pearson")
cor(ames_cleandata$sale_price, ames_cleandata$lot_area, method = "pearson")
cor(ames_cleandata$sale_price, ames_cleandata$rooms_tot, method = "pearson")
cor(ames_cleandata$sale_price, ames_cleandata$full_bath, method = "pearson")

# Calculate the correlation matrix
correlation_variables <- ames_cleandata[c("sale_price", "garage_area", "year_built", "lot_area", "rooms_tot", "full_bath")]
correlation_matrix <- cor(correlation_variables)
corrplot.mixed(correlation_matrix, tl.col = "black", tl.pos = "lt") 



#########visualisations###########

ggplot(ames_cleandata, aes(x = garage_area, y = sale_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(method = "pearson") +
  theme_minimal() +
  labs(title = "Sale Price by Garage Area", 
       x = "Garage Area (sq. ft.)", 
       y = "Sale Price ($)")


ggplot(ames_cleandata, aes(x = year_built, y = sale_price)) + 
  geom_point(aes(color = year_built)) + 
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Sale Price by Year Built", 
       x = "Year Built", 
       y = "Sale Price ($)")
theme_minimal()

ggplot(ames_cleandata, aes(x = lot_area, y = sale_price)) +
  geom_point(aes(color = lot_area)) + 
  geom_smooth(method = "lm", color = "blue") + 
  stat_cor(method = "pearson") +
  scale_color_viridis_c() + 
  labs(title = "Sale Price by Lot Area", 
       x = "Lot Area (sq. ft.)", 
       y = "Sale Price ($)")
  theme_gray() +
  theme(legend.position = "none")

ggplot(ames_cleandata, aes(x = as.numeric(lot_area), y = sale_price)) + 
  geom_smooth(method = 'loess', span = 0.3, se = FALSE) +
  labs(title = "Sale Price by Lot Area", x = "Lot Area (sq. ft.)", y = "Sale Price ($)") +
  theme_gray() +
  theme(legend.position = "none")

ggplot(ames_cleandata, aes(x = rooms_tot, y = sale_price)) +
  geom_point(aes(color = rooms_tot), alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "pearson") +
  labs(title = "Average Sale Price by Number of Total rooms",
       x = "Number of Total rooms",
       y = "Average Sale Price") +
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(ames_cleandata, aes(x = full_bath, y = sale_price)) +
  geom_point(aes(color = full_bath), alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  stat_cor(method = "pearson") +
  labs(title = "Average Sale Price by Number of Full Bathrooms",
       x = "Number of Full Bathrooms",
       y = "Average Sale Price") +
  theme_minimal() +
  theme(legend.position = "none") 


###########Linear regression################

##Data partitioning
set.seed(40424126)
index<- createDataPartition(ames_cleandata$sale_price, p=0.7, list=FALSE) #list should be set to false other wise it will return a list
train<-ames_cleandata[index,] 
test <- ames_cleandata[-index,] 

#model1
formula1<- sale_price ~ garage_area + year_built + lot_area + rooms_tot + full_bath

model1 <- lm(formula = formula1, data= train) 
summary(model1)

#model2

formula2<- sale_price ~ garage_area + year_built + lot_area + rooms_tot + full_bath + fireplace + year_remod
model2 <- lm(formula = formula2, data= train) 
summary(model2)

#model3
formula3<- sale_price ~ garage_area + year_built + lot_area + rooms_tot + full_bath + fireplace + year_remod + neighbourhood + roof_material +
  house_quality + kitchen_qual + stories + building 
model3 <- lm(formula = formula3, data= train) 
summary(model3)

#Predicting price for test data

predictions <- predict(model3, newdata =  test)
test$predictedsale_price <- predictions

postResample(predictions, test$sale_price)

#assumption test
#cooks distance
cooks.distance(model3)
sum(cooks.distance(model3)<1)

#multicollinearity

vif(model3)

#assumptions of independent errors

dwtest(model3)


#assumptions on residual
plot(model3)

stargazer(model1, model2, model3, type = "html", out = "modelcomparisontable.html")

ggplot(test, aes(x = sale_price, y = predictedsale_price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Actual Price vs Predicted Price",
    x = "Actual Price",
    y = "Predicted Price"
  )

write.xlsx(test, "test.xlsx")
