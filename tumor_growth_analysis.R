library(readxl)
library(MVN)
library(profvis)
library(multcomp)
library(lsmeans)
library(DescTools)
library(tinytex)
library(openxlsx)
library(stargazer)
library(Hmisc)

loc = "/home/andrep/workspace/bubbleCAN/TumorgrowthBubbleCAN.xlsx"
data = read_excel(loc)#read.csv(loc, header=TRUE, sep=",")

save_path = "/home/andrep/workspace/bubbleCAN/"

# have to swap group 4 and 6
tmp = data[,1]
data[tmp == 4, 1] = 6
data[tmp == 6,1] = 4

# groups = 2:7 #c(2,3,4,5,7)
groups = 1:7 # <- compare all!

times = 3:18 #12

pvals.t.test = matrix(rep(0, 21 * length(times)), nrow=21)
pvals.wilcox = pvals.t.test*1

x_names = names(data)[3:dim(data)[2]][1:10]
y_names = c("12", "13", "14", "15", "16", "17", "23", "24", "25", "26", "27", "34", "35", "36", "37", "45", "46", "47", "56", "57", "67")

normal = rep(FALSE, length(times))
normal_matrix = matrix(c(rep(NA, length(groups)*length(times))), ncol=length(times))

cnt = 1
for (j in times){ # could only do it up to t_12, as for t_13 only one sample
  print("----------------------------------------------------------------------------------------------------------------------")
  print(paste("Starting with timepoint: ", toString(names(data)[j])))
  
  data_tmp = data[, c(1, j)]
  
  # only keep groups that has more than three samples left
  tmp_groups = c()
  counter = 1
  for (i in groups){
    a = data_tmp[data_tmp[, 1] == i, 2]
    a = sum(!is.na(a))
    if (a > 3) {
      tmp_groups[counter] = i
      counter = counter + 1
    }
  }
  
  if (length(tmp_groups) == 1) {
    break
  }
  
  a = t(combn(tmp_groups, 2))
  y_names_list = c()
  for (ii in 1:dim(a)[1]) {
    b = a[ii, ]
    c = paste(toString(b[1]), toString(b[2]), sep="")
    y_names_list[ii] = c
  }
  
  val = 0
  for (i in tmp_groups){
    tmp_val = sum(data_tmp[,1] == i)
    if (tmp_val > val){
      val = tmp_val
    }
  }
  
  print(y_names_list)
  
  tmps = matrix(rep(NA, val*length(tmp_groups)), ncol=length(tmp_groups))
  tmps2 = matrix(rep(NA, 2*val*length(tmp_groups)), ncol=2)
  count = 1
  for (i in 1:length(tmp_groups)){
    curr = as.numeric(as.matrix(data_tmp[data_tmp[,1] == tmp_groups[i], 2]))
    for (k in 1:length(curr)){
      tmps[k, i] = curr[k]
      tmps2[count, ] = c(tmp_groups[i], curr[k])
      count = count + 1
    }
  }
  
  a = matrix(as.numeric(na.omit(tmps2)), ncol=2)
  c = matrix(rep(NA, dim(a)[1]*length(unique(a[, 1]))), nrow=dim(a)[1])
  uniques = unique(a[, 1])
  for (i in 1:length(uniques)) {
    tt = a[a[, 1] == uniques[i], 2]
    c[1:length(tt), i] = tt
  }
             
  print(b)
  print(":)")
  
  # Cannot do mardia test with group 1, as there are too few samples...
  #
  #result<-mvn(data=matrix(as.numeric(na.omit(tmps2)), ncol=2), mvnTest="mardia", scale=FALSE, tol=1e-25, multivariatePlot="qq", univariateTest="SW")
  result<-mvn(c, mvnTest="mardia", scale=FALSE, tol=1e-25, univariateTest="SW")
  #print(result$multivariateNormality)
  print(result$univariateNormality)
  #print(result)
  #result = shapiro.test(matrix(as.numeric(na.omit(tmps2))))
  print(result)
  #print("----")
  
  #aa = as.numeric(result$univariateNormality$`p value`)
  
  # add shapiro test results pvs in normal_matrix
  #for (t in 1:length(y_names_list)) {
  #  a = y_names_list[t]
  #  normal_matrix[y_names == a, cnt] = aa[t]
  #}
  
  # normal assumption NOT OK for quite a few... let's do nonparametric testing instead!
  A.wilcox = pairwise.wilcox.test(tmps2[,2], tmps2[,1], p.adjust.method = "BH", alternative="two.sided")
  #pvals.wilcox[y_names == y_names_list, cnt] = A.wilcox$p.value[lower.tri(A.wilcox$p.value) | diag(length(tmp_groups)-1) == 1]
  tmp_pvs = A.wilcox$p.value[lower.tri(A.wilcox$p.value) | diag(length(tmp_groups)-1) == 1]
  tmps = c()
  for (t in 1:length(y_names_list)) {
    a = y_names_list[t]
    pvals.wilcox[y_names == a, cnt] = tmp_pvs[t]
  }
  
  # test anova
  #res = aov(as.numeric(as.matrix(data_tmp[, 2])) ~ data_tmp$`Group number`)
  #print(res)
  #print(summary(res))
  #print(pairwise.t.test(as.numeric(as.matrix(data_tmp[, 2])), data_tmp$`Group number`))
  #print(pairwise.t.test(as.numeric(as.matrix(data_tmp[, 2])), data_tmp$`Group number`, p.adj="bonf"))
  
  # test homogeneity of variance
  #print(bartlett.test(tmps2[,2], tmps2[,1]))
  # H0: Variance is homogeneous -> pv > 5% => variance is homogeneity => VARIANCES ARE NOT EQUAL!
  # Thus, cannot do ANOVA, however ANOVA is more interesting if one want to compare the effect of multiple factors,
  # an not multiple contrasts, which is the case in multiple comparisons (now, only 1 factor variable, TREATMENT)
  #res = anova(aov(tmps2[,2] ~ factor(tmps2[,1])))
  #print(res)
  
  #print(pairwise.t.test(tmps2[,2], tmps2[,1], p.adj="none", pool.sd=FALSE))
  #print(pairwise.t.test(tmps2[,2], tmps2[,1], p.adj="bonf", pool.sd=FALSE))
  print(pairwise.t.test(tmps2[,2], tmps2[,1], p.adj="BH", pool.sd=FALSE)) # <- BH = Benjamini & Hochberg => FDR (!)
  #print(pairwise.t.test(tmps2[,2], tmps2[,1], p.adj="BY", pool.sd=FALSE)) # BY : solution to BH given dependency, BH assumed independent tests
  
  A.t.test = pairwise.t.test(tmps2[,2], tmps2[,1], p.adj="BH", pool.sd=FALSE)
  tmp_pvs = A.t.test$p.value[lower.tri(A.t.test$p.value) | diag(length(tmp_groups)-1) == 1]
  tmps = c()
  for (t in 1:length(y_names_list)) {
    a = y_names_list[t]
    pvals.t.test[y_names == a, cnt] = tmp_pvs[t]
  }
  #pvals.t.test[y_names == y_names_list, cnt] = tmp_pvs
  cnt = cnt + 1
  
  # <- Cannot do Tukey as it requires equally many samples in each category
  #print(TukeyHSD())
  
  # test Sheffe's method => good given many comparisons
  #print(ScheffeTest(aov(tmps2[,2] ~ factor(tmps2[,1]))))
  
  
  # now compare groups {1}, {2,3,4}, {5,6,7} against each other (control, no US, US) at current time point:
  growth_var = tmps2[,2]
  factor_var = tmps2[,1]
  for (ii in 2:4){
    factor_var[factor_var == ii] = -1
  }
  for (ii in 5:7){
    factor_var[factor_var == ii] = -2
  }
  print("Test groups: (control, no US, US): ")
  print(pairwise.t.test(tmps2[,2], factor_var, p.adj="BH", pool.sd=FALSE))
    
  print(paste("finished with ", toString(j)))
  
  print("----------------------------------------------------------------------------------------------------------------------")
  print("----------------------------------------------------------------------------------------------------------------------")
  
}
  
x_names = names(data)[3:dim(data)[2]] #[1:10]
y_names = c("12", "13", "14", "15", "16", "17", "23", "24", "25", "26", "27", "34", "35", "36", "37", "45", "46", "47", "56", "57", "67")

write.table(pvals.t.test, file = "/home/andrep/workspace/bubbleCAN/pvals_t_tests.csv",
          sep="\t", col.names=x_names, row.names=y_names)
write.table(pvals.wilcox, file = "/home/andrep/workspace/bubbleCAN/pvals_wilcox.csv",
            sep="\t", col.names=x_names, row.names=y_names)
#write.table(normal_matrix, file = "/home/andrep/workspace/bubbleCAN/shapiro.csv",
#            sep="\t", col.names=x_names, row.names=y_names)

