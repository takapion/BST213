# homework 2

#Section.1 Import excel data ####
#Sectionで区切るとsection毎にまとまって解析が可能
# "# Section名 #4つ"でSectionの始まりを認識します
library(readxl)
lbw <- read_excel("C:/Users/ogmcd/Dropbox/00_2019_Class/Fall/BST213/Dataset/lbw.xls", sheet = "lbw")
View(lbw)


#Section.2 Descriptive analysis ####
#finalfitというライブラリを使うとtable 1が超絶楽です
#今回はアウトカムが2値ではないので使えません
#library(finalfit)
library(tidyverse)
lbw %>% summary

#数値をfactorに変換して変数を作成(Characterではないことに注意)
lbw$factor_low <-factor(lbw$low)
lbw$factor_smoke <-factor(lbw$smoke)
lbw$factor_ht <-factor(lbw$ht)
lbw$factor_ui <-factor(lbw$ui)
lbw$factor_ptl <-factor(lbw$ptl)
lbw$factor_ftv <-factor(lbw$ftv)
lbw$factor_race <-factor(lbw$race)

#Tableコマンドで症例数を確認し、作戦を練る
table(lbw$factor_low)
table(lbw$factor_smoke)
table(lbw$factor_ht)
table(lbw$factor_ui)
table(lbw$factor_ptl)
table(lbw$factor_ftv)
#パイプ（後ろの引数を先に書く）を使って以下のようにも書ける
#lbw$factor_low %>% table
#lbw$factor_smoke %>% table
#lbw$factor_ht %>% table
#lbw$factor_ui %>% table
#lbw$factor_ptl %>% table
#lbw$factor_ftv %>% table

#Yes or Noなど明らかにfactorでわかる変数として作成
lbw <-
  mutate(lbw, low_y_n = if_else(low == 1, true = "Yes", false = "No"))
         
lbw <-
  mutate(lbw, smoke_y_n = if_else(smoke == 1, true = "Yes", false = "No"))
lbw <-
  mutate(lbw, ht_y_n = if_else(ht == 1, true = "Yes", false = "No"))
lbw <-
  mutate(lbw, ui_y_n = if_else(ui == 1, true = "Yes", false = "No"))
lbw <-
  mutate(lbw, ptl_y_n = if_else(ptl == 0, true = "No", false = "Yes"))


#if式の適切な書き方は下の感じになるようです。
#lbw <-
#  mutate(lbw, low_y_n = if(low == 1) {
#    "Yes"
#  } else {
#    "No"
#  }

#3つ以上の変数を作る時はif elif elseを使うか、case whenを使います

lbw <-
  mutate(lbw, race_WBO = case_when(race == 1 ~ "White",
                                   race == 2 ~ "Black",
                                   race == 3 ~ "Others"))

lbw <-
  mutate(lbw, ftv_3_cat = case_when(ftv == 6 ~ "Many",
                                    ftv == 4 ~ "Many",
                                    ftv == 3 ~ "Many",
                                    ftv == 2 ~ "Moderate",
                                    ftv == 1 ~ "Moderate",
                                    ftv == 0 ~ "None"))

#実はこのままではfactorになっておらずcharacterなので、factorに変換
lbw$low_y_n <-factor(lbw$low_y_n)
lbw$smoke_y_n <-factor(lbw$smoke_y_n)
lbw$ht_y_n <-factor(lbw$ht_y_n)
lbw$ui_y_n <-factor(lbw$ui_y_n)
lbw$ptl_y_n <-factor(lbw$ptl_y_n)
lbw$ftv_3_cat <-factor(lbw$ftv_3_cat)
lbw$race_WBO <-factor(lbw$race_WBO)


#変数を作った時は必ずうまくいってるか確認
lbw$low_y_n %>% table
lbw$smoke_y_n %>% table
lbw$ht_y_n %>% table
lbw$ui_y_n %>% table
lbw$ptl_y_n %>% table
lbw$race_WBO %>% table
lbw$ftv_3_cat %>% table


#Histogramでbwtのnormalityを確認
hist(lbw$bwt)

#ggplotで書くとこんな感じ(binwidthは1つのヒストグラムの幅)
library(ggplot2)
g1 <- ggplot(lbw, aes(x = bwt))
g2 <- g1 + geom_histogram(binwidth = 500)
plot(g2)

#group別にも書ける(alphaは図の透明度)
g1 <- ggplot(lbw, aes(x = bwt, fill = smoke_y_n))
g2 <- g1 + geom_histogram(position = "identity", binwidth = 300, alpha = 0.5)
plot(g2)

#色とか変えてみたり
g1 <- ggplot(lbw, aes(x = bwt, fill = smoke_y_n))
g2 <- g1 + geom_histogram(position = "identity", binwidth = 300, alpha = 0.5)
g3 <- g2 + scale_fill_manual(values = c("blue", "pink"))
plot(g3)

#Boxplotもggplotで
g1 <- ggplot(lbw, aes(y = bwt, x = smoke_y_n))
g2 <- g1 + geom_boxplot()
g3 <- g2 + labs(title = "Birth Weight in Smoking Category", x = "Smoking Status", y = "Birth Weight")
plot(g3)

#他の変数は省略

#Section.3 Linear regression ####
linearMod1 <- lm(bwt ~ lwt, data=lbw)
linearMod1 %>% summary
confint(linearMod1)

glmMod1 <- glm(bwt ~ lwt, data=lbw, family=gaussian())
glmMod1 %>% summary
confint(glmMod1)


linearMod2 <- lm(bwt ~ age, data=lbw)
linearMod2 %>% summary
confint(linearMod2)

glmMod2 <- glm(bwt ~ lwt, data=lbw, family=gaussian())
glmMod2 %>% summary
confint(glmMod2)


contrasts(lbw$race_WBO) <- contr.treatment(3, base = 3)
linearMod3 <- lm(bwt ~ race_WBO, data=lbw)
linearMod3 %>% summary
linearMod3 %>% confint

glmMod3 <- glm(bwt ~ race_WBO, data=lbw, family=gaussian())
glmMod3 %>% summary
glmMod3 %>% confint