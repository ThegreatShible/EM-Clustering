replace_categ_na <- function(var) {
  tmp=sort(table(var),decreasing=TRUE)
  var[is.na(var)]=names(tmp)[1]
  factor(var)
}

replace_conti_na <- function(var) {
  if (sum(is.na(var)) > 0)
    var[is.na(var)] = mean(var, na.rm=TRUE)
  as.numeric(var)
}

is_constant <- function(var) {
  if (length(var) == 0) return(TRUE)
  else return(all(var == var[1]))
}

data=read.table("./data/VisaPremier.txt",header=TRUE,na.strings =".")

# Second individual is too different
data = data[-2,]

# index of categorical features
ind_categ_feature=c(1,2,3,4,6,8,9,45,46,47)
# extraction of only the categ. features
data_categ=data[,ind_categ_feature]

# extraction of only the continuous features
data_continuous=data[,-ind_categ_feature]

# first have a look on the categorical feature
summary(data_categ)

for (j in 1:ncol(data_categ)) {
  data_categ[,j] = replace_categ_na(data_categ[,j])
}

# the variable sexer and cartevpr are recoding of the variable cartevp and sexe,
# we don't need them
data_categ$sexer=NULL
data_categ$cartevpr=NULL
# we remove also the variable corresponding to the id of the customer
data_categ$matricul=NULL

summary(data_categ)

# for the continous variables, we automatically replace the NA
summary(data_continuous)
for (j in ncol(data_continuous):1){
  d = data_continuous[,j]
  data_continuous[,j] = replace_conti_na(d)
  if (is_constant(d)) {
    print(names(data_continuous)[j])
    data_continuous[,j] = NULL
  }
}
summary(data_continuous)

#res=PCAmix(data_continuous,data_categ,rename.level=TRUE,ndim=50)
# we can also plot the individual with different color following
# they have bought or not the VisaPremier credit card
#plot(res,choice="ind",coloring.ind=data_categ$cartevp,posleg="bottomright")
#plot(res,choice="cor")

source("./src/em.R")

# Clustering with cartevp
V = cbind(data_categ, data_continuous)
dep = V[,"departem"]
dep[is.na(dep)] = 31

V[,"departem"] = as.factor(dep)
V[, "ptvente"] = as.factor(V[, "ptvente"])

# Clustering without cartevp variable
V$engageml = NULL


V <-  V[-2,]
V <- V[!V$avtscpte == 6009132,]
V <- V[!V$avtscpte == 9299466,]
V <- V[!V$avtscpte == 3530191,]
V <-  V[!V$avtscpte == 1945469,]
V <- V[!V$avtscpte == 1879699,]
V <- V[!V$avtscpte == 2871625,]
V <- V[!V$avtscpte == 2297333,]
V <- V[!V$avtscpte == 2139492 ,]
num = unlist(lapply(V, is.numeric))
Vq = as.data.frame(V[,num])



V$cartevp = NULL
V$aveparfi = NULL
V$nbeparlo = NULL
V$mteparlo = NULL
V$nbeparte = NULL
V$codeqlt = NULL
V$mtrejet = NULL
V$endette = NULL
V$mteparmo = NULL
#V$mtvie = NULL
V[, "mteparte"] = NULL
#V$mteparlt = NULL
#V$moycred3 = NULL
#V$nbcbptar = NULL
#V$nbcb = NULL
#V$nbeparlt = NULL
#V$nbcptvue = NULL


#V$mtfactur = NULL
#V$nbvie  = NULL
#V$nbeparmo = NULL
#V$nblivret = NULL
#V$engagemc = NULL

#V$engagemt  = NULL
#V$agemvt = NULL
#V$moycredi = NULL
#V$age= NULL



Vq = as.data.frame(V[,num])
clusty = clust(V, 2:6, 4, "kmeans", 0.1)
plot_result(clusty)

library(mixtClust)
mixmodCluster(V, nbClust=2:7)




