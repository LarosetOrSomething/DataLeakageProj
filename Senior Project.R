ds <- State_Data

ds$lit_Rate <- 100-ds$Percent_Lacking

histogram(ds$lit_Rate,breaks = 15)

#(x <= 85, 85 < x < 90, x => 90)
ds$Lit_Rate <- cut(ds$lit_Rate,
                  breaks = c(0,85,90,100))

ds$Lit_Rate <- as.factor(ds$Lit_Rate)

ds$Lit_Rate <- revalue(ds$Lit_Rate, c("(0,85]"="low", "(85,90]" = "mid", "(90,100]" = "high"))

table(ds$Lit_Rate)

View(ds$Lit_Rate)

d <- ds
d$Location <- NULL
d$Percent_Lacking <- NULL
d$lit_Rate <- NULL

#Building and showing the first cart model, the one of the entire set
Cart01 <- rpart(Lit_Rate ~ ., data = d, method = "class")
rpart.plot(Cart01, type = 4, extra = 102)

#Determining said model's accuracy
pred.cart01 <-predict(object = Cart01, newdata = d,
                      type = "class")
table(d$Lit_Rate, pred.cart01)

StateTrain <-createDataPartition(
  y = d$Lit_Rate, p= .80, list = FALSE
)
d.train <- d[StateTrain,]
d.test <- d[-StateTrain,]

#labe the training and testing data sets based on the partiton
d.train$trainortest <- 
  rep("train", nrow(d.train))
d.test$trainortest <- 
  rep("test", nrow(d.test))
d.all <- rbind(d.train, d.test)

#validate the partition on every variable, 
kruskal.test(d$Population ~ as.factor(trainortest), data = d.all)$p.value
kruskal.test(d$Pop_per_sqMi ~ as.factor(trainortest), data = d.all)$p.value
kruskal.test(d$Poverty_Percent ~ as.factor(trainortest), data = d.all)$p.value
kruskal.test(d$AVG.F ~ as.factor(trainortest), data = d.all)$p.value
kruskal.test(d$Lit_Rate ~ as.factor(trainortest), data = d.all)$p.value

#Building and showing the second cart model, the one of the training set
Cart02 <- rpart(Lit_Rate ~ ., data = d.train, method = "class")
rpart.plot(Cart02, type = 4, extra = 102)

#Determining said model's accuracy
pred.cart02 <-predict(object = Cart02, newdata = d.train,
                      type = "class")
table(d.train$Lit_Rate, pred.cart02)

Cart03 <- rpart(Lit_Rate ~ ., data = d.test, method = "class")
rpart.plot(Cart03, type = 4, extra = 102, box.palette="Blues")


#Setting up the k-fold cross-validation stuff
train.control <- trainControl(method = "cv", number = 10)
train.cca <- na.omit(d.train)

#Building the first 10-fold model
trainMod <- train(Lit_Rate ~ ., data = train.cca,
                  method = "rpart", trControl = train.control)

#making said model presentable
fancyRpartPlot(trainMod$finalModel, cex = 1.12)

#Finding the accuracy of the 10-fold model
pred.10 <- rpart.predict(object = trainMod, newdata = d.train)
table(d.train$Lit_Rate, pred.10)
