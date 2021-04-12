import numpy as np 
import pandas as pd
from lifelines import KaplanMeierFitter, NelsonAalenFitter, AalenAdditiveFitter, AalenJohansenFitter
from lifelines.statistics import StatisticalResult, logrank_test, multivariate_logrank_test, pairwise_logrank_test
import matplotlib.pyplot as plt
np.set_printoptions(suppress=True)
pd.set_option('display.float_format', lambda x: '%.6f' % x)

from matplotlib import rc


data_path = "/home/andrep/workspace/bubbleCAN/survivalBubbleCAN.xlsx"

data = pd.read_excel(data_path)

# do logrank hypothesis test
res = multivariate_logrank_test(data["Day of euthanisia"], data["Group number"])
print(res.print_summary())
pv = res.p_value
print(pv)

# 4,5,7 ultralyd, 1 control, rest uten

res2 = pairwise_logrank_test(data["Day of euthanisia"], data["Group number"])

print(res2.p_value)
print(res2.summary)


ax = plt.subplot(111)
# initialize a kmf object
kmf = KaplanMeierFitter()

for i in np.unique(data["Group number"]):

	# fit data to model
	kmf.fit(data["Day of euthanisia"][data["Group number"] == i], data["Group number"][data["Group number"] == i], label=str(i))

	# create an estimate
	ax = kmf.plot(ax=ax, ci_show=True, linewidth=2) #ci_show=False) # ci_show : CIs

#ax.text(np.amax(survival)*0.7, 0.7, "pv = %1.6f" % pv)
#ax.set_title("Kaplan Meier, pv12 = %1.4f, pv13 = %1.4f, pv23 = %1.4f" % tuple(np.round(p_values, 4)))

plt.show()


### {4,5,7} vs {2,3,6} ###

tmp = data["Group number"]
group2 = [4, 5, 7]
group1 = [2, 3, 6]
for i, t in enumerate(tmp):
	if t in group1:
		tmp[i] = -1
	elif t in group2:
		tmp[i] = -2

res2 = pairwise_logrank_test(data["Day of euthanisia"], tmp)

groups = ["control", "no US", "US"] #["1", "4,5,7", "2,3,6"]

print(res2.p_value)
print(res2.summary)

ax = plt.subplot(111)
# initialize a kmf object
kmf = KaplanMeierFitter()

#rc('font', family='sans-serif')

print(np.unique(tmp))

for j, i in enumerate(np.unique(tmp)[::-1]):

	# fit data to model
	kmf.fit(data["Day of euthanisia"][tmp == i], data["Group number"][tmp == i], label=groups[j])

	# create an estimate
	ax = kmf.plot(ax=ax, ci_show=True, linewidth=2) #ci_show=False) # ci_show : CIs

#ax.text(np.amax(survival)*0.7, 0.7, "pv = %1.6f" % pv)
#ax.set_title("Kaplan Meier, pv12 = %1.4f, pv13 = %1.4f, pv23 = %1.4f" % tuple(np.round(p_values, 4)))

plt.show()


exit()













exit()
ax = plt.subplot(122)

# initialize a kmf object
ajf = AalenJohansenFitter()

tmps = []
for i in np.unique(data["Group number"]):

	surv_tmp = survival[data["Group number"] == i]
	grades_tmp = grades[data["Group number"] == i]
	events_tmp = events[data["Group number"] == i]
	print(grades_tmp)
	grades_tmp = (grades_tmp > 0).astype(int)
	print(events_tmp)
	events_tmp[events_tmp != 1] = 2
	print(events_tmp)

	# fit data to model
	ajf.fit(surv_tmp, events_tmp, event_of_interest=1, label=str(i))
	tmps.append(np.array(ajf.cumulative_density_)[-1])

	# create an estimate
	ax = ajf.plot(ax=ax, ci_show=False) #ci_show=False) # ci_show : CIs

ax.set_title("Cumulative Incidence Function (CIF)")

plt.show()


exit()


#data = pd.read_stata(data_path, convert_categoricals=False)
#data.to_csv("/home/andrep/workspace/phd/stata/Samlefil_med_prognosedata2.csv")

names = list(data.keys())
print(data)
print(names)
print(data.dod_bc_other_2015)
print(data.GRAD)
tmp = data.dod_bc_other_2015
print(np.unique(tmp))
for i in range(len(np.unique(tmp))):
	print(len(tmp[tmp == i]))

cond1 = np.invert(np.isnan(data._t))
cond2 = data.dod_bc_other_2015 == 1 # <- 1 for death because of breast cancer?
ids = np.array(data.ID_deltaker[cond1 & cond2]).astype(int)
#grades = data["GRAD"]
grades = np.array(data.GRAD[cond1 & cond2]).astype(int)
survival = np.array(data._t[cond1 & cond2])
print(names)

plt.figure()

for i in np.unique(grades):
	print(np.sum(grades[grades == i]))

# initialize figure/axis
ax = plt.subplot(131)

# do logrank hypothesis test
res = multivariate_logrank_test(survival, grades)
print(res.print_summary())
pv = res.p_value
print(pv)

res2 = pairwise_logrank_test(survival, grades)
p_values = res2.p_value
print(res2.print_summary())

# initialize a kmf object
kmf = KaplanMeierFitter()

for i in np.unique(grades):

	# fit data to model
	kmf.fit(survival[grades == i], grades[grades == i], label=str(i))

	# create an estimate
	ax = kmf.plot(ax=ax, ci_show=False) #ci_show=False) # ci_show : CIs

ax.text(np.amax(survival)*0.7, 0.7, "pv = %1.6f" % pv)
ax.set_title("Kaplan Meier, pv12 = %1.4f, pv13 = %1.4f, pv23 = %1.4f" % tuple(np.round(p_values, 4)))


# initialize figure/axis
ax = plt.subplot(132)

# initialize a kmf object
naf = NelsonAalenFitter()

for i in np.unique(grades):

	# fit data to model
	naf.fit(survival[grades == i], grades[grades == i], label=str(i))

	# create an estimate
	ax = naf.plot(ax=ax, ci_show=False) #ci_show=False) # ci_show : CIs

ax.set_title("Nelson Aalen")


### Cumulative Incidence Function (CIF)
#cond2 = data.dod_bc_other_2015 == 1 # <- 1 for death because of breast cancer?
#grades = np.array([data.GRAD[cond2]]).astype(int)



cond1 = np.invert(np.isnan(data._t))
data = data[cond1]
cond2 = data.dod_bc_other_2015
events = np.array((data.dod_bc_other_2015 == 1).astype(int))
grades = np.asarray(data.GRAD).astype(int)
survival = np.array(data._t)
#print(grades)
#print(events)
#events += grades
#print(events)
#exit()
print(grades)
print(events)


# do weighted logrank hypothesis test (Gray's test)
#from cmprsk import cmprsk

#tmp = pd.read_csv('cmprsk/cmprsk/tests/test_set.csv')
#exit()

#cuminc_res = cmprsk.cuminc(survival, )

# initialize figure/axis
ax = plt.subplot(133)

ufig, ax = plt.subplots()

# initialize a kmf object
ajf = AalenJohansenFitter()

print(np.unique(grades))

tmps = []
for i in np.unique(grades):
	print()
	print(i)

	surv_tmp = survival[grades == i]
	grades_tmp = grades[grades == i]
	events_tmp = events[grades == i]
	print(grades_tmp)
	grades_tmp = (grades_tmp > 0).astype(int)
	print(events_tmp)
	events_tmp[events_tmp != 1] = 2
	print(events_tmp)

	# fit data to model
	ajf.fit(surv_tmp, events_tmp, event_of_interest=1, label=str(i))
	tmps.append(np.array(ajf.cumulative_density_)[-1])

	# create an estimate
	ax = ajf.plot(ax=ax, ci_show=False) #ci_show=False) # ci_show : CIs

ax.set_title("Cumulative Incidence Function (CIF)")

print(tmps)
print(sum(tmps))



'''
# initialize figure/axis
ax = plt.subplot(133)

# initialize a kmf object
aaf = AalenAdditiveFitter()

for i in np.unique(grades):

	# fit data to model
	aaf.fit(survival[grades == i], grades[grades == i], label=str(i))

	# create an estimate
	ax = naf.plot(ax=ax, ci_show=False) #ci_show=False) # ci_show : CIs

ax.set_title("Aalen's additive method")
'''

plt.suptitle("Survival analysis estimates")

plt.show()