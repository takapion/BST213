log using "C:\Users\ogmcd\Dropbox\00_2019_Class\Fall\BST213\Homework\HW2\HM2.log", replace
import excel "C:\Users\ogmcd\Dropbox\00_2019_Class\Fall\BST213\Dataset\lbw.xls", sheet("lbw") firstrow
/*ageとbwtの関連*/
graph twoway (scatter bwt age) (lfit bwt age)
corr bwt age, mean
pwcorr bwt age, sig
corrci bwt age, fisher
regress bwt age
glm bwt age
/*lwtとbwtの関連*/
graph twoway (scatter bwt lwt) (lfit bwt lwt)
corr bwt lwt, mean
pwcorr bwt lwt, sig
corrci bwt lwt, fisher
regress bwt lwt
glm bwt lwt
/*raceとbwtの関連*/
graph twoway scatter bwt race
oneway bwt race, tab
xi: regress bwt i.race
xi: glm bwt i.race
