log using "C:\Users\ogmcd\Dropbox\00_2019_Class\Fall\BST213\Homework\Homework1.smcl", replace
import excel "C:\Users\ogmcd\Dropbox\00_2019_Class\Fall\BST213\Dataset\lbw.xls", sheet("lbw") firstrow
tab race
tab smoke
tab ptl
/*Decided to collapse history of premature labor to binomial variable*/
gen ptl_yes_no=0 if ptl==0
replace ptl_yes_no=1 if ptl>=1
tab ptl_yes_no
tab ht
tab ui
tab ftv
/*Decided to collapse history of premature labor to binomial variable*/
gen ftv_yes_no=0 if ftv==0
replace ftv_yes_no=1 if ftv>=1
tab ftv_yes_no
/*no missing value in race, smoke, ptl, ht, ui, ftv*/
/*ptl and ftv are collapsed into binomial variable ptl_yes_no and ftv_yes_no*/
sum bwt, detail
sum age, detail
sum lwt, detail
/*no missing value in bwt, age, lwt*/
/*bwt and age are approximately normally distributed*/
/*lwt is relatively skew and kurtotic*/
bys race : sum bwt , detail
bys smoke : sum bwt , detail
bys ptl_yes_no : sum bwt , detail
bys ht : sum bwt , detail
sdtest bwt, by(ht)
bys ui : sum bwt , detail
bys ftv_yes_no : sum bwt , detail
ttest bwt, by(smoke)
ttest bwt, by(ht)
ttest bwt, by(ui)
ttest bwt, by(ptl_yes_no)
ttest bwt, by(ftv_yes_no)
oneway bwt race, tab
graph twoway scatter bwt ftv
graph twoway scatter bwt age
corr bwt age, mean
pwcorr bwt age, sig
corrci bwt age, fisher
graph twoway scatter bwt lwt
corr bwt lwt, mean
pwcorr bwt lwt, sig
corrci bwt lwt, fisher
pwcorrs bwt lwt, spearman sig
