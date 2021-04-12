# Testing survival analysis in R
#dev.off()[dev.list()]["RStudioGD"]

library(cmprsk) #, foreign, survival)
library(readxl)
source("/home/andrep/workspace/bubbleCAN/CumIncidence.R")

loc = "/home/andrep/workspace/bubbleCAN/survivalBubbleCAN.xlsx"

data = read_excel(loc)#read.csv(loc, header=TRUE, sep=",")
print(data)

#res = cuminc(data$`Day of euthanisia`, data$dod_bc_other_2015, data$GRAD)
for (i in 1:7){
  res = cuminc(data$`Day of euthanisia`, as.integer(data$`Group number` == i), data$`Group number`)
  #fit = CumIncidence(data$`Day of euthanisia`, as.integer(data$`Group number` == i), data$`Group number`, cencode=0, xlab="Months")
}

  # {4,5,7} vs {2,3,6}
group1 = c(4,5,7)
group2 = c(2,3,6)
tmp = data$`Group number`
for (i in 1:length(tmp)){
  a = tmp[i]
  if (a %in% group1){
    tmp[i] = -1
  }
  if (a %in% group2){
    tmp[i] = -2
  }
}
data$`Group number` = data$`Group number`
res = cuminc(data$`Day of euthanisia`, as.integer(tmp == -1), tmp)




