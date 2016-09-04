library(Boruta)

#http://www.r-bloggers.com/faster-higher-stonger-a-guide-to-speeding-up-r-code-for-busy-people/
library(doParallel)
cl <- makeCluster(4)  
registerDoParallel(cl)

# test dataset
data(iris)
Bor.iris <- Boruta(Species~.,data=iris, doTrace=2)
# summary of assessment
print(Bor.iris)
# print the important ones
print(getSelectedAttributes(Bor.iris))
