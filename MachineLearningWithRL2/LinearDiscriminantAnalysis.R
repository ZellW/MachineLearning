#-----------------Linear Discriminant Analysis----------------------------
library(MASS)
View(fgl)


# Perform the LDA on fgl dataset
data.lda <- lda(formula = type ~ ., 
                data = fgl)
data.lda
# perform prediction
data.lda.pred <-predict(data.lda, newdata=fgl[,c(1:9)])$class
data.lda.pred
### Determine how well the model fits.
table(data.lda.pred,fgl[,10])

