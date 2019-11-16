#--------------------- Hierarchical Clustering------------------

data(iris)
View(iris)
# Sample dataset
sampleiris <- iris[sample(1:150, 40),]
# Calculate distance
 H.distance <- dist(sampleiris[,-5], method="euclidean")
 # perform Hierarchical clustering
H.fit <- hclust(H.distance, method="average")
 
 # plot  dendograph
 plot(H.fit, hang=-1, label=sampleiris$Species)# hang is used to put the labels of the leaf in same level
 groups <- cutree(H.fit, k=3) # cut tree into 3 clusters
 
 # draw dendogram with red borders around the 3 clusters
 rect.hclust(H.fit, k=3, border="red") 
 