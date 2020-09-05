### Understanding and reading the data####

install.packages("ISLR")
library(ISLR)
data(package="ISLR")                 

compysale<-read.csv(file.choose())
View(compysale)
ncol(compysale)#no. of columns 
nrow(compysale)#no. of rows

install.packages("tree")
library(tree)
require(tree)

names(compysale)#reading the names of the features/inputs
#let us take sales as the root node 
# Root: sales
barplot(compysale$Sales)
hist(compysale$Sales)

# sales has two levels eithr yes and no
#if sales is greter then 8 the it is High so yes either no
High = ifelse(compysale$Sales<=8, "No", "Yes")
compysale_1 = data.frame(compysale, High)#framing a new column to represent the levels of sales

View(High)
View(compysale_1)
ncol(compysale_1)# 1 extra column have been included

tree.compysale_1 = tree(High~.-Sales, data=compysale_1)
View(tree.compysale_1)

summary(tree.compysale_1)
#plotting the tree
plot(tree.compysale_1)
text(tree.compysale_1, pretty = 0)
?text#text draws the strings given in the vector labels at the coordinates given by x and y.

#as there are so many variables its making the tree so conjusted for visualization so splitting is a choise
tree.compysale_1

####**splitting the compysale_1 dataframe into training and test dataset**#####

set.seed(101)
training_1=sample(1:nrow(compysale_1), 250)#splitting into 250 samples for  training dataset
?sample
View(training_1)

#now refitting the same model with "tree" in training_1

tree.compysale_1 = tree(High~.-Sales, compysale_1, subset=training_1)
plot(tree.compysale_1)
text(tree.compysale_1, pretty=0)

###** using "predict" function we will predict on the test dataset with label "class"
?class
?predict

tree.pred = predict(tree.compysale_1, compysale_1[-training_1,], type="class")
View(tree.pred)

##evaulating the error by using a misclassification table##
with(compysale_1[-training_1,], table(tree.pred, High))
###to find the error just take sum of 2 diagonals and divide it by total test observations
###(72+39)/150
###on the diagonals are the correct classification and of the  diagonal are th incorrect classification
#            High
#    tree.pred No Yes
#          No  72  18
#          Yes 21  39
#(72+39)/150=0.74


## reducing the extent of tree by using cv.tree we will use the misclassification error 
cv.compysale_1=cv.tree(tree.compysale_1,FUN = prune.misclass)
?prune.misclass
cv.compysale_1
# shows the details of the path of the cross-validation,sizes of the trees as they were pruned back,the deviances as the pruning proceeded, as well as the cost complexity parameter used in the process.

plot(cv.compysale_1)

#prune the tree to a size of 12(best=12) to identify that tree.
#evaluating in the testing data
prune.compysale_1 = prune.misclass(tree.compysale_1, best = 12)#12 branches can be seen
plot(prune.compysale_1)
text(prune.compysale_1, pretty=0)

#evaluting in the test data
tree.pred = predict(prune.compysale_1, compysale_1[-training_1,], type="class")
View(tree.pred)
#evaluating the eror with a misclassification table
with(compysale_1[-training_1,], table(tree.pred, High))

#         High
#tree.pred No Yes
#      No  72  19
#      Yes 21  38

# errorr=(72+38)/150=[1] 0.7333333
(72+38)/150

#trees dont give a good prediction so we can use random forest and boosting as per as tree is concerned it performed good as compared to that
# random forest will outperform better as per prediction and misclassification is concerned
install.packages("randomForest")
