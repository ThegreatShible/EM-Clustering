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

data=read.table("../data/VisaPremier.txt",header=TRUE,na.strings =".")

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

res=PCAmix(data_continuous,data_categ,rename.level=TRUE,ndim=50)
# we can also plot the individual with different color following
# they have bought or not the VisaPremier credit card
plot(res,choice="ind",coloring.ind=data_categ$cartevp,posleg="bottomright")
plot(res,choice="cor")

source("../src/em.R")

# Clustering with cartevp
X = cbind(data_categ, data_continuous)

X$aveparfi = NULL
X$nbeparlo = NULL
X$mteparlo = NULL
X$nbeparte = NULL

clusty = clust(X, 2:5, "VVV", 1, "random", 0.1)

install.packages("Rmixmod")
library(Rmixmod)
mixmodCluster(data=X, nbCluster=1:4, dataType="composite")

coord <- function(n, x) {
  l = floor((n-1)/x)
  c(l + 1, n - l*x)
}

# Clustering without cartevp variable
X$cartevp = NULL
