#------------------------------- Knn Algorithm-------------------------------
library(class)
# Read data 
wdbc<- read.csv("D:/elementary learners/machine learning/MODULES/Module-3 Classification/Dataset/cancer.csv")
View(wdbc)

wdbc <- wdbc[-1]
View(wdbc)


wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c('B', 'M'),
                         labels = c('Benign', 'Malignant'))

View(wdbc)

wdbc <- wdbc[sample(nrow(wdbc)), ]
View(wdbc)
lapply(wdbc[2:11], function(x) { max(x) - min(x) })


wdbcNormalized <- as.data.frame(scale(wdbc[-1]))
View(wdbcNormalized)


summary(wdbcNormalized[c('radius_mean', 'area_worst', 'symmetry_se')])


wdbcTraining <- wdbcNormalized[1:427, ]
wdbcTest <- wdbcNormalized[428:569, ]

 
wdbcTrainingLabels <- wdbc[1:427, 1]
wdbcTestLabels <- wdbc[428:569, 1]
View(wdbcTestLabels)


wdbcPredictedLabels <- knn(train = wdbcTraining,
                           test = wdbcTest,
                           cl = wdbcTrainingLabels,
                           k=21)
wdbcPredictedLabels


library(gmodels)
CrossTable(x = wdbcTestLabels, y = wdbcPredictedLabels,
           prop.chisq = F, dnn = c('actual', 'predicted'))

