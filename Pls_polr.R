##########################################
#           Wine Analytics               #
#       by René Angeles Martinez         #
#
# GLM: 
# - pls-glm-gaussian
# - pls-glm-polr (kfold adjusted)
# - plsr
##########################################

library(plsRglm)
library(caret)
library(dplyr)
library(plsdof)
set.seed(123)


#Load data
red <-read.csv(file.path('data/', 'winequality-red.csv'), sep=';')
white <- read.csv(file.path('data/','winequality-white.csv'), sep=';')

add_type <- function(data, w_type){
  data <- add_column(data, wine.type = w_type, .before = 'quality')
}


##############################################
#      Functions to treat outlies            #
# Do not improve accuracy, but leave them    #
# here for the future                        #
##############################################

#Distribution with heavy tails. 
heavy_tails = c('fixed.acidity',
                'volatile.acidity',
                'citric.acid',
                'residual.sugar',
                'chlorides',
                'total.sulfur.dioxide',
                'free.sulfur.dioxide',
                'density',
                'sulphates')


#red[heavy_tails] <- log1p(red[heavy_tails])
#white[heavy_tails] <- log1p(white[heavy_tails])


#Function to remove outliers / not active at the moment
rm_outliers<-function(data, range = 2.2){
  x<-data
  for (i in heavy_tails ){
    aux<- boxplot((data[[i]]), range, plot=FALSE)$out
    x<- x[-which(data[[i]] %in% aux),]
  }
  return(x)
}

#red<-rm_outliers(red)
#white<-rm_outliers(white)


##############################################
#      Functions to treat outlies            #
# Do not improve accuracy, but leave them    #
# here for the future                        #
##############################################

#red_tmp <- red$quality
#red<-as.data.frame(scale(red[,1:len]))
#red$quality<-red_tmp

#white_tmp <- white$quality
#white<-as.data.frame(scale(white[,1:len]))
#white$quality<-white_tmp

# Uncoment two lines below to scaling
#colMeans(red)   
#apply(red , 2, sd)


##############################################
#  Single pls-glm-polr model                 #
#  Number of compoents to keep adjusted with #
#  kfold valudation                          #
##############################################


#number of features
len <-length(red)-1


##############################################
#  RED WINE                                  #
##############################################

partition <- createDataPartition(red$quality, p = 0.80)[[1]]
red_train <- red[partition,]
red_test <- red[-partition,]

stopifnot(nrow(red_train) +nrow(red_test) ==nrow(red))
Xred<-red_train[,1:len]
yred<-red_train$quality

#>>>>>>>>> Kfold paramert tunning, run once 3 lines below<<<<<<<###
#determining right number of components to keep
#cv.modpls<-cv.plsRglm(yred,Xred,nt=len,modele="pls-glm-polr",NK=50)
#res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
#plot(res.cv.modpls)

Xred_test<-red_test[,1:len]
yred_test<-red_test$quality

#Check data conservation
stopifnot(nrow(Xred)==nrow(red_train))
stopifnot(length(Xred) + 1 ==length(red_train))

res<-plsRglm(yred,  Xred,4,modele="pls-glm-polr")
res_num<- predict(res, newdata = Xred_test, type="class") %>% 
  as.character() %>% as.numeric()

print('MAE of pls-glm-polr, kfold adjusted, apply to RED is:')
MAE(res_num,yred_test)

##############################################
#  WHITE WINE                                #
##############################################


#white<-rm_outliers(white) #remove outlier does not seem to help

partition <- createDataPartition(white$quality, p = 0.80)[[1]]
white_train <- white[partition,]
white_test <- white[-partition,]
Xwhite<-white_train[,1:len]
ywhite<-white_train$quality
Xwhite_test<-white_test[,1:len]
ywhite_test<-white_test$quality


#>>>>>>>>> Kfold paramert tunning, run once 3 lines below<<<<<<<###
#determining right number of components to keep (takes some time)
#cv.modpls<-cv.plsRglm(ywhite,Xwhite,nt=len,modele="pls-glm-polr",NK=50)
#res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
#plot(res.cv.modpls)

res_w_polr<-plsRglm(ywhite,Xwhite,4,modele="pls-glm-polr")
res_w_num<- predict(res_w_polr, newdata = Xwhite_test, type="class") %>% 
  as.character() %>% as.numeric()

print('MAE of pls-glm-polr, kfold adjusted, apply to WHITE is:')
MAE(res_w_num, ywhite_test)

#number of rows in red/white data sets
nred <- nrow(red_test)
nwhite <- nrow(white_test)

print('MAE of pls-glm-polr, kfold adjusted, apply to combined dataset is:')
(nred * MAE(res_num,yred_test) +nwhite * MAE(res_w_num, ywhite_test) )/(nred + nwhite)


res_num_<- predict(res, newdata = Xred, type="class") %>% 
  as.character() %>% as.numeric()
res_w_num_<- predict(res_w_polr, newdata = Xwhite, type="class") %>% 
  as.character() %>% as.numeric()



print('Validation MAE of pls-glm-polr, kfold adjusted, apply to combined dataset is:')
(nred * MAE(res_num_,yred) +nwhite * MAE(res_w_num_, ywhite) )/(nred + nwhite)


####### Finf out which parameters are relevant
#red.bootYX1<-bootplsglm(res,typeboot="plsmodel",sim="balanced",R=20)
#red.bootYX1
#res

#boxplots.bootpls(red.bootYX1)


##########################################
#           pls-glm-gaussian             #
##########################################

#############################################################################
#TODO: apply the following three lines to adjust nt (components to keep)
#cv.modpls<-cv.plsRglm(ywhite, Xwhite, nt=len,modele="pls-glm-gaussian",NK=20)
#res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
#plot(res.cv.modpls)
#############################################################################

res_w_gaussian<-plsRglm(ywhite,  Xwhite,6,modele="pls-glm-gaussian")
res_num_w_gaussian <-  predict(res_w_gaussian, newdata = Xwhite_test)  
print('Todo: adjust using kfold')
print('MAE of pls-glm-gaussian on white wine is')
MAE(res_num_w_gaussian,ywhite_test)


##########################################
#            pls                         #
##########################################

#############################################################################
#TODO: apply the following three lines to adjust nt (components to keep)
#cv.modpls<-cv.plsRglm(ywhite, Xwhite, nt=len,modele="pls",NK=20)
#res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
#plot(res.cv.modpls)
#############################################################################

res_w_plsR<-plsR(ywhite,  Xwhite,6)
res_num_plsR <-  predict(res_w_plsR, newdata = Xwhite_test)
print('Todo: adjust using kfold')
print('The Mae for pls on white wine is')
MAE(res_num_plsR,ywhite_test)












####################################################################################
###>>>>>>>>>>>>>>>>> 
###>>>>>>>>>>>>>>>>> Don't look below it is only for future dev
###>>>>>>>>>>>>>>>>>



#res<-plsRglm(yred,  Xred,6,modele="pls")
#res_num <- predict(res, newdata = Xred_test, type = 'response')
#res_num 
#res_num
#print('The Mae for the red wine is')
#MAE(res_num,yred_test)


#temp.ci=confints.bootpls(red.bootYX1)
#plots.confints.bootpls(temp.ci,typeIC="BCa",colIC=c("blue","blue","blue","blue"),legendpos="topright")


##### Don't look below, only for future deve
#data(bordeaux)
# red$quality<-factor(red$quality,ordered=TRUE)
# modpls1<-plsRglm(quality~.,data=red,11,modele="pls-glm-polr",pvals.expli=TRUE)
# 
# modpls1
# 
# Xred<-red[,1:11]
# yred<-red$quality
# modpls2<-plsRglm(yred,Xred,11,modele="pls-glm-polr",pvals.expli=TRUE)
# 
# modpls2
# all(modpls1$InfCrit==modpls2$InfCrit)
# 
# colSums(modpls2$pvalstep)
# 
# set.seed(123)
# cv.modpls<-cv.plsRglm(yred,Xred,nt=11,modele="pls-glm-polr",NK=20)
# res.cv.modpls=cvtable(summary(cv.modpls,MClassed=TRUE))
# plot(res.cv.modpls)
# res.cv.modpls
# cv.modpls
###
#Retained model according to cross validated missclassed criterion.