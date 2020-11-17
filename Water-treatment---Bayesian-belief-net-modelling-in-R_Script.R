

###################### libaray ###################################
library("RColorBrewer")
library(VIM)
library(mice)

library(Rgraphviz)
library(bnlearn)

library(ggplot2)

library(RBGL)
library(gRain)
library(gRbase)

#################### read data from csv file with na.string###########
## explore its properties and NA
data = read.csv("MS986 Assignment 2 BBN Modelling Data Set v2.csv", na.string = "")
summary(data)
sapply(data, function(x) sum(is.na(x)))


par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(2,2))
hist(data$"wtw_coliform", main = "Coliform Count in Water Treatment Works", cex.main = 0.7, cex.lab = 0.5, xlab = "No. of Coliform", ylim = c(0,30000), col = "thistle")
hist(data$wtw_ecoli,main = "E.Coli Count in Water Treatment Works", cex.main = 0.7, cex.lab = 0.5, xlab = "No. of E.Coli", ylim = c(0,30000), col = "turquoise")
hist(data$srs_coliform, main = "Coliform Count in Water Treatment Works", cex.main = 0.7, cex.lab = 0.5, xlab = "No. of Coliform", ylim = c(0,30000), col = "skyblue1" )
hist(data$srs_ecoli, main = "E.coli Count in Water Treatment Works", cex.main = 0.7, cex.lab = 0.5, xlab = "No. of E.coli" , ylim = c(0,30000), col = "slategray2" )

par(mfrow = c(2,2))
boxplot(data$wtw_coliform, main = "Coliform Count in Water Treatment Works",cex.main = 0.7, col = "thistle" )
boxplot(data$wtw_ecoli,main = "E.Coli Count in Water Treatment Works", cex.main = 0.7, col = "turquoise")
boxplot(data$srs_coliform, main = "Coliform Count in Water Treatment Works", cex.main = 0.7,  col = "skyblue1" )
boxplot(data$srs_ecoli, main = "E.coli Count in Water Treatment Works", cex.main = 0.7, col = "slategray2" )

par(mfrow = c(1,1))
boxplot(data[c("srs_aow","srs_free.chlorine","srs_icc", "srs_stor", "srs_tcc", "srs_total.chlorine","wtw_free.chlorine", "wtw_total.chlorine")], main = "Distribution of data", xlab = c("srs_aow","srs_free.chlorine","srs_icc", "srs_stor", "srs_tcc", "srs_total.chlorine","wtw_free.chlorine", "wtw_total.chlorine"))

boxplot(data[c("srs_aow","srs_free.chlorine", "srs_stor",  "srs_total.chlorine","wtw_free.chlorine", "wtw_total.chlorine")], main = "Distribution of data", xlab = c("srs_aow","srs_free.chlorine", "srs_stor",  "srs_total.chlorine","wtw_free.chlorine", "wtw_total.chlorine"))
#################### subsetting data only those related###############

select = c("srs_aow", "srs_coliform", "srs_condition.risk.factor", "srs_ecoli", "srs_free.chlorine", "srs_icc", "srs_secondary.disinfection.delivery", "srs_secondary.disinfection.risk", "srs_stor", "srs_tank.construction", "srs_tcc", "srs_total.chlorine", "wtw_coliform", "wtw_ecoli", "wtw_free.chlorine", "wtw_ozonation", "wtw_sourcetype", "wtw_sw", "wtw_total.chlorine")

length(select) 

data_select = data[select]
dim(data_select)

################### Explore more of missing value ###################

##  missing data pattern
miss_data = is.na(data_select)
miss_data
cor_miss_data =cor(miss_data)
cor_miss_data[which(is.na(cor_miss_data))] = 0

par(mfrow = c(1,1))

heatmap(cor_miss_data, Rowv = NA, Colv = NA, scale = "column", main="Correrlation of missing variables", col=brewer.pal(9,"RdPu"))


############### Cleaning##################################


rem = c("srs_aow", "srs_condition.risk.factor", "srs_free.chlorine", "srs_secondary.disinfection.delivery", "srs_secondary.disinfection.risk", "srs_stor", "wtw_coliform", "wtw_ecoli", "wtw_free.chlorine", "wtw_ozonation", "wtw_sourcetype", "wtw_sw", "wtw_total.chlorine")

dim(data_select)
data_sclean1 = data_select[complete.cases(data_select[rem]),]
dim(data_sclean1)

summary(data_sclean1)


# still some is na., will have to look more closer.

missing2 = md.pattern(data_sclean1)
dim(missing2)

par(mar=c(9.5, 4.1, 4.1, 2.1))
barplot(missing2[5, 1:19], , col = "thistle3", las=2, ylab ="Count", cex.axis = 0.7, ylim=c(0, 200), main = "Number of missing value after first cleaning", cex.lab = 1, cex.names=0.6, cex.main = 1, names.arg = c(colnames(missing2[1:19])))
par(mar=c(5.1, 4.1, 4.1, 2.1))

summary(data_sclean1$srs_tank.construction)

## categorical, can't just replace with med or min, so remove all.

data_sclean1 = data_sclean1[complete.cases(data_sclean1[c("srs_tank.construction", "srs_total.chlorine", "srs_icc")]),]

dim(data_sclean1)
dim(data_select)
summary(data_sclean1)

############ Change type of data ##################################

num = c("srs_aow", "srs_coliform", "srs_ecoli", "srs_free.chlorine","srs_icc", "srs_stor", "srs_tcc", "srs_total.chlorine", "wtw_coliform", "wtw_ecoli", "wtw_free.chlorine", "wtw_total.chlorine")
length(num)

fac = c("srs_condition.risk.factor", "srs_secondary.disinfection.delivery", "srs_secondary.disinfection.risk", "srs_tank.construction", "wtw_ozonation", "wtw_sourcetype", "wtw_sw")
length(fac)

str(data_sclean1[num])
str(data_sclean1[fac])


fchange = c("srs_condition.risk.factor", "srs_secondary.disinfection.risk", "wtw_ozonation", "wtw_sw")

data_sclean2 = data_sclean1
data_sclean2[fchange]= lapply(data_sclean2[fchange],as.factor)

str(data_sclean2[fac])

nchange = c("srs_tcc", "wtw_ecoli")
data_sclean2[nchange]= lapply(data_sclean2[nchange],as.numeric)

str(data_sclean2)

################## Strucutre learning #############################



learn_dis = data_sclean2


colnames(learn_dis) = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s")
colnames(learn_dis)


numnum = c("a", "b", "d", "e","f", "i", "k","l", "m", "n", "o", "s")
learn_dis[numnum] = discretize(learn_dis[numnum], method = "interval", breaks = 3)



############################### BIC #########################################
net1 = hc(learn_dis, score = "bic")
graphviz.plot(net1)

cv.net1 = bn.cv(learn_dis, bn = "hc", algorithm.args = list(score = "bic"), k=10, runs =10) 
cv.net1
plot(cv.net1 , xlab = "BIC")


############################### BDE #########################################

net2 = hc(learn_dis, score = "bde")
graphviz.plot(net2)



cv.net2 = bn.cv(learn_dis, bn = "hc", algorithm.args = list(score = "bde"), k=10, runs =10) 
cv.net2
plot(cv.net2 , xlab = "BDE")

############################### BDS ######################################

net3 = hc(learn_dis, score = "bds")
graphviz.plot(net3)



cv.net3 = bn.cv(learn_dis, bn = "hc", algorithm.args = list(score = "bds"), k=10, runs =10) 
cv.net3
plot(cv.net3 , xlab = "BDS")


##########################Comparison ##################################### 

plot(cv.net1, cv.net2, cv.net3 , xlab = c("BIC", "BDE", "BDS"))
plot(cv.net2, cv.net3 , xlab = c("BDE", "BDS"))


###################### Prediction  ####################################

cv.net1.pred = bn.cv(learn_dis, bn = "hc", algorithm.args = list(score = "bds"),loss ="pred-lw",loss.args = list(target = "m"), k=10, runs =10)

observed.net1 = cv.net1.pred[[1]][[1]][["observed"]]
predicted.net1 = cv.net1.pred[[1]][[1]][["predicted"]]
table(predicted.net1, observed.net1)

cv.net2.pred = bn.cv(learn_dis, bn = "hc", algorithm.args = list(score = "bds"),loss ="pred-lw",loss.args = list(target = "b"), k=10, runs =10)

observed.net2 = cv.net2.pred[[1]][[1]][["observed"]]
predicted.net2 = cv.net2.pred[[1]][[1]][["predicted"]]
table(predicted.net2, observed.net2)

cv.net3.pred = bn.cv(learn_dis, bn = "hc", algorithm.args = list(score = "bds"),loss ="pred-lw",loss.args = list(target = "d"), k=10, runs =10)

observed.net3 = cv.net3.pred[[1]][[1]][["observed"]]
predicted.net3 = cv.net3.pred[[1]][[1]][["predicted"]]
table(predicted.net3, observed.net3)

cv.net4.pred = bn.cv(learn_dis, bn = "hc", algorithm.args = list(score = "bds"),loss ="pred-lw",loss.args = list(target = "n"), k=10, runs =10)

observed.net4 = cv.net4.pred[[1]][[1]][["observed"]]
predicted.net4 = cv.net4.pred[[1]][[1]][["predicted"]]
table(predicted.net4, observed.net4)

###############Parameter learning ###################
fit3 = bn.fit(net3, learn_dis, method ="bayes")

fit3$b
fit3$d
fit3$m
fit3$n


############ exact inference #########################
exact3 = compile(as.grain(fit3))
summary(exact3)


evid3 = setFinding(exact3, nodes = "f", states = "1000")
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = "f", states = "5")
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = c("f","j"), states =c( "20000","standard concrete"))
querygrain(evid3, nodes = "b", type = "marginal")


evid3 = setFinding(exact3, nodes = "b", states = "60")
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = "b", states = "100000")
querygrain(evid3, nodes = "d", type = "marginal")


evid3 = setFinding(exact3, nodes = "b", states = "200")
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = "b", states = "0")
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = "h", states = "8")
querygrain(evid3, nodes = "n", type = "marginal")

evid3 = setFinding(exact3, nodes = "h", states = "0")
querygrain(evid3, nodes = "n", type = "marginal")

evid3 = setFinding(exact3, nodes = c("b", "g"), states = c("500", "liquid doser"))
querygrain(evid3, nodes = "m", type = "marginal")


evid3 = setFinding(exact3, nodes = "m", states = "500")
querygrain(evid3, nodes = "h", type = "marginal")


evid3 = setFinding(exact3, nodes = "m", states = "100")
querygrain(evid3, nodes = "p", type = "marginal")



evid3 = setFinding(exact3, nodes = "d", states = "100")
querygrain(evid3, nodes = "m", type = "marginal")


evid3 = setFinding(exact3, nodes = "b", states = "1500")
querygrain(evid3, nodes = "j", type = "marginal")


evid3 = setFinding(exact3, nodes = "b", states = "10000")
querygrain(evid3, nodes = "k", type = "marginal")


evid3 = setFinding(exact3, nodes = c("b","m"), states = c("100", "50"))
querygrain(evid3, nodes = "g", type = "marginal")


evid3 = setFinding(exact3, nodes = c("b","m"), states = c("100", "50"))
querygrain(evid3, nodes = "o", type = "marginal")

evid3 = setFinding(exact3, nodes = "b", states = "100")
querygrain(evid3, nodes = "p", type = "marginal")

evid3 = setFinding(exact3, nodes = "g", states = "none")
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = "g", states = "floating dispenser")
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = "g", states = "sock dosing")
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = "g", states = "chlorine gas")
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = "g", states = "codyne/vortex")
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = "l", states = "2")
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = "l", states = "0")
querygrain(evid3, nodes = "b", type = "marginal")

##########################################################

evid3 = setFinding(exact3, nodes = "g", states = "codyne/vortex")
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = "l", states = "2")
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = "o", states = "1")
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = "p", states = "0")
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = "p", states = "1")
querygrain(evid3, nodes = "d", type = "marginal")

#########################################################
evid3 = setFinding(exact3, nodes = "g", states = "codyne/vortex")
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = "l", states = "2")
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = "o", states = "1")
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = "p", states = "0")
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = "p", states = "1")
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = "s", states = "2")
querygrain(evid3, nodes = "m", type = "marginal")

#########################################################
evid3 = setFinding(exact3, nodes = "g", states = "codyne/vortex")
querygrain(evid3, nodes = "n", type = "marginal")


evid3 = setFinding(exact3, nodes = "p", states = "0")
querygrain(evid3, nodes = "n", type = "marginal")

evid3 = setFinding(exact3, nodes = "p", states = "1")
querygrain(evid3, nodes = "n", type = "marginal")

evid3 = setFinding(exact3, nodes = "s", states = "9")
querygrain(evid3, nodes = "n", type = "marginal")


evid3 = setFinding(exact3, nodes = "s", states = "1")
querygrain(evid3, nodes = "n", type = "marginal")

#######################################################

evid3 = setFinding(exact3, nodes = c("o", "a"), states = c("0.5", "8.7"))
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = c("o", "a"), states = c("0.5", "8.7"))
querygrain(evid3, nodes = "n", type = "marginal")

evid3 = setFinding(exact3, nodes = c("o", "a"), states = c("0.5", "8.7"))
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = c("o", "a"), states = c("0.5", "8.7"))
querygrain(evid3, nodes = "d", type = "marginal")


evid3 = setFinding(exact3, nodes = c("g", "o", "a"), states = c("0.5", "1.4", "tube tablet dosing"))
querygrain(evid3, nodes = "b", type = "marginal")


evid3 = setFinding(exact3, nodes = c("g", "o", "a"), states = c("3", "10", "tube tablet dosing"))
querygrain(evid3, nodes = "m", type = "marginal")


evid3 = setFinding(exact3, nodes = c("p", "e", "q"), states = c("0", "5", "sw"))
querygrain(evid3, nodes = "m", type = "marginal")


evid3 = setFinding(exact3, nodes = c("p", "e", "q"), states = c("0", "5", "mw"))
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q"), states = c("1", "10", "mw"))
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q"), states = c("1", "10", "mw"))
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o"), states = c("1", "10", "mw", "none", "6", "10"))
querygrain(evid3, nodes = "b", type = "marginal")


evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o", "r", "h", "f"), states = c("1", "10", "mw", "none", "6", "10", "1", "8", "20000"))
querygrain(evid3, nodes = "b", type = "marginal")


evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o", "r", "h", "f"), states = c("1", "10", "mw", "none", "6", "10", "0", "10", "20000"))
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o", "r", "h", "f"), states = c("1", "10", "mw", "none", "6", "10", "0", "10", "100"))
querygrain(evid3, nodes = "b", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o", "r", "h", "f"), states = c("1", "10", "mw", "none", "6", "10", "0", "10", "100"))
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o", "r", "h", "f"), states = c("1", "10", "mw", "none", "6", "10", "0", "10", "100"))
querygrain(evid3, nodes = "n", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o", "r", "h", "f"), states = c("1", "10", "mw", "none", "6", "10", "0", "10", "100"))
querygrain(evid3, nodes = "m", type = "marginal")


evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o", "r", "h", "f"), states = c("1", "10", "mw", "none", "6", "10", "1", "8", "20000"))
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o"), states = c("1", "10", "mw", "none", "6", "10"))
querygrain(evid3, nodes = "d", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o"), states = c("1", "10", "mw", "none", "6", "10"))
querygrain(evid3, nodes = "m", type = "marginal")

evid3 = setFinding(exact3, nodes = c("p", "e", "q", "g", "i", "o"), states = c("1", "10", "mw", "none", "6", "10"))
querygrain(evid3, nodes = "n", type = "marginal")
