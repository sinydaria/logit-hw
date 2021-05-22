install.packages('MASS')
library('MASS')
View(GAGurine)
set.seed(20)
install.packages('neuralnet')
library('neuralnet')
base <- GAGurine
max_base <- apply(base, 2, max)
min_base <- apply(base, 2, min)
base_scal <- scale(base, center = min_base, scale = max_base - min_base)
index <- sample(1:nrow(base), round(0.80*nrow(base)))
train_base <- as.data.frame(base_scal[index,])
test_base <- as.data.frame(base_scal[-index,])
n <- colnames(base)
f <- as.formula(paste('Age ~', paste(n[!n %in% 'Age'], collapse = '+')))

n1 <- neuralnet(f,data = train_base, hidden = c(9, 5, 3), linear.output = F)
plot(n1)

pr <- compute(n1, test_base[1:2])
print(pr$net.result)

pr$net.result <- sapply(pr$net.result, round, digits = 0)

test1 <- table(test_base$age, pr$net.result)
test1

sum(test1[1,])
sum(test1[2,])

Accuracy1 <- (test1[1,1] + test1[2, 2])/sum(test1)
Accuracy1
Точность модели 0,002




install.packages('RSNNS')
library(RSNNS)
set.seed(20)


base2 <- GAGurine[sample(1:nrow(GAGurine), length(1:nrow(GAGurine))), 
                 1:ncol(GAGurine)]


base2_Values <- base2[,1]
base2_Target <- base2[, 2]


base2 <- splitForTrainingAndTest(base2_Values, base2_Target, ratio = 0.2)
base2 <- normTrainingAndTestSet(base2)


model <- mlp(base2$inputsTrain,
             base2$targetsTrain,
             size = 5,
             maxit = 50,
             inputsTest = base2$inputsTest,
             targetsTest = base2$targetsTest)

test2 <- confusionMatrix(base2$targetsTrain, encodeClassLabels(fitted.values(model), 
                                                               method = "402040", l = 0.5, h = 0.51))
test2

sum(test2[1,])
sum(test2[2,])

Accuracy2 <- (test2[1,1] + test2[2, 2])/sum(test2)
Accuracy2



install.packages("kohonen")
library('kohonen')
set.seed(20)


base3 <- base[1:2]
base3_1 <- GAGurine[1:1]
table(base3_1)


train <- sample(nrow(base3), 151)
X_train <- scale(data3[train,])
X_test <- scale(data3[-train,],
                center = attr(X_train, "scaled:center"),
                scale = attr(X_train, "scaled:center"))
train_base <- list(measurements = X_train,
                   base3_1 = base3_1[train,])
test_base <- list(measurements = X_test,
                  base3_1 = base3_1[-train,])


mygrid <- somgrid(5, 5, 'Age')
som.base3 <- supersom(train_base, grid = mygrid)             
som.predict <- predict(som.base3, newdata = test_base)


test3 <- table(base3_1[-train,], som.predict$predictions$base3_1)


sum(test3[1,])
sum(test3[2,])

Accuracy3 <- (test3[1,1] + test3[2, 2])/sum(test3)
Accuracy3



Accuracy1
Accuracy2
Accuracy3


