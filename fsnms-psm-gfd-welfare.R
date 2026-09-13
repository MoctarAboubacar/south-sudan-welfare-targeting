# using the propensity score matching technique we try to estimate a causal coefficient to understand the 'effect' of general food distribution on certain indicators of welfare among refugee populations in south sudan.

# remove all
rm(list=ls())

## packages
require(haven)
require(tidyverse)
require(data.table)
require(DataExplorer)
require(MatchIt)
require(Hmisc)
require(survey)


## data
dat <- read_sav(here::here("data", "fsnms.23.sav")) # for data privacy reasons, we aren't sharing any of the actual data here in this repository, just the code to produce results.
glimpse(dat)
sum <- introduce(dat)

# na table
na.count <- function(a){
sum.missing <- a %>% 
  #select_if(is.numeric) %>% 
  map_dbl(function(x) sum(is.na(x)))
percent.missing <- a %>%
  #select_if(is.numeric) %>% 
  map_dbl(function(x) round(sum(is.na(x))/nrow(a)*100,1))
sum.missing <- as.data.frame(sum.missing)
percent.missing <- as.data.frame(percent.missing)
na.table <<- setDT(cbind(sum.missing, percent.missing), keep.rownames = "variable")[] 
na.table.ggplot <<- na.table %>%
  arrange(percent.missing) %>%
  ggplot(aes(x = reorder(variable, -percent.missing), y = percent.missing, color = percent.missing))+
  scale_color_gradientn(colours = rainbow(4))+ 
  xlab("Variables")+
  geom_bar(stat = 'identity')+
  ggtitle("Percent missing")
return(na.table)
} 
na.count(dat)

# column names to position numbers function
numcol <- function(x, y){
  which(colnames(x) == y)
}

# process data (all variables in right types)
# A. General information
dat$psu <- factor(dat$A09)

# B. Demographics
dat$sex_hh <- factor(dat$B01)
dat$age_hh <- dat$B02
dat$edu_hh <- factor(dat$B03)
dat$male_under5 <- dat$B04M
dat$female_under5 <- dat$B05F
dat$male5to15 <- dat$B05M
dat$female5to15 <- dat$B05F
dat$male16to60 <- dat$B06M
dat$female16to60 <- dat$B06F
dat$maleover60 <- dat$B07M
dat$femaleover60 <- dat$B07F
dat$members_hh <- as.integer(dat$B0_total_family_members)
dat$dis_phys <- dat$B16
dat$dis_mental <- dat$B17
dat$dis_ill <- dat$B18
dat$dis_injured <- dat$B19
dat$house_type <- factor(dat$B21)
dat$social_group <- factor(dat$B23)
dat$recent_training <- factor(dat$B24)

# C. Migration
dat$recent_migrants <- factor(dat$C01)
dat$C02[dat$C02==""] <- 0
dat$migration_location <- factor(dat$C02)

# D. WASH
dat$water_source <- factor(dat$D01)
dat$water_time <- factor(dat$D02)
dat$jerrycans <- as.numeric(dat$D03)
dat$defecate_location <- factor(dat$D06)

# E. Livelihoods and assets
dat$livelihood_1 <- factor(dat$E01)
dat$livelihood_2 <-factor(dat$E02)
dat$remittance <- factor(dat$E05)
dat$bed <- factor(dat$E06.1)
dat$mattress <- factor(dat$E06.2)
dat$chair <- factor(dat$E06.3)
dat$table <- factor(dat$E06.4)
dat$radio <- factor(dat$E06.5)
dat$tv <- factor(dat$E06.6)
dat$mobile <- factor(dat$E06.7)
dat$wheelbarrow <- factor(dat$E06.8)
dat$mosquitonet <- factor(dat$E06.9)
dat$motorcycle <- factor(dat$E06.10)
dat$bicycle <- factor(dat$E06.11)
dat$flatiron <- factor(dat$E06.12)
dat$trad_stove <- factor(dat$E06.13)
dat$solarpanel <- factor(dat$E06.14)
dat$fishingtools <- factor(dat$E06.15)
dat$seeds <- factor(dat$E06.16)
dat$graingrinding <- factor(dat$E06.17)
dat$agritools <- factor(dat$E06.18)
dat$sold_productiveassets <- factor(ifelse(dat$E08.8 == 1 | dat$E08.10 == 1 | dat$E08.15 == 1 | dat$E08.16 == 1 | dat$E08.17 == 1 | dat$E08.18 == 1, 1, 0))
dat$cashexp_cereal = dat$H01B
dat$cashexp_cereal[is.na(dat$cashexp_cereal)] <- 0
dat$credit_cereal = dat$H01C
dat$credit_cereal[is.na(dat$credit_cereal)] <- 0
dat$ownproduction_cereal = dat$H01D
dat$ownproduction_cereal[is.na(dat$ownproduction_cereal)] <- 0
dat$cashexp_tubers = dat$H02B
dat$cashexp_tubers[is.na(dat$cashexp_tubers)] <- 0
dat$credit_tubers = dat$H02C
dat$credit_tubers[is.na(dat$credit_tubers)] <- 0
dat$ownproduction_tubers = dat$H02D
dat$ownproduction_tubers[is.na(dat$ownproduction_tubers)] <- 0
dat$cashexp_pulses = dat$H03B
dat$cashexp_pulses[is.na(dat$cashexp_pulses)] <- 0
dat$credit_pulses = dat$H03C
dat$credit_pulses[is.na(dat$credit_pulses)] <- 0
dat$ownproduction_pulses = dat$H03D
dat$ownproduction_pulses[is.na(dat$ownproduction_pulses)] <- 0
dat$cashexp_vegfruit = dat$H04B
dat$cashexp_vegfruit[is.na(dat$cashexp_vegfruit)] <- 0
dat$credit_vegfruit = dat$H04C
dat$credit_vegfruit[is.na(dat$credit_vegfruit)] <- 0
dat$ownproduction_vegfruit = dat$H04D
dat$ownproduction_vegfruit[is.na(dat$ownproduction_vegfruit)] <- 0
dat$cashexp_meats = dat$H05B
dat$cashexp_meats[is.na(dat$cashexp_meats)] <- 0
dat$credit_meats = dat$H05C
dat$credit_meats[is.na(dat$credit_meats)] <- 0
dat$ownproduction_meats = dat$H05D
dat$ownproduction_meats[is.na(dat$ownproduction_meats)] <- 0
dat$cashexp_oilfat = dat$H06B
dat$cashexp_oilfat[is.na(dat$cashexp_oilfat)] <- 0
dat$credit_oilfat = dat$H06C
dat$credit_oilfat[is.na(dat$credit_oilfat)] <- 0
dat$ownproduction_oilfat = dat$H06D
dat$ownproduction_oilfat[is.na(dat$ownproduction_oilfat)] <- 0
dat$cashexp_dairy = dat$H07B
dat$cashexp_dairy[is.na(dat$cashexp_dairy)] <- 0
dat$credit_dairy = dat$H07C
dat$credit_dairy[is.na(dat$credit_dairy)] <- 0
dat$ownproduction_dairy = dat$H07D
dat$ownproduction_dairy[is.na(dat$ownproduction_dairy)] <- 0
dat$cashexp_sugarsalt = dat$H8B
dat$cashexp_sugarsalt[is.na(dat$cashexp_sugarsalt)] <- 0
dat$credit_sugarsalt = dat$H8C
dat$credit_sugarsalt[is.na(dat$credit_sugarsalt)] <- 0
dat$ownproduction_sugarsalt = dat$H8D
dat$ownproduction_sugarsalt[is.na(dat$ownproduction_sugarsalt)] <- 0
dat$cashexp_teacoffee = dat$H9B
dat$cashexp_teacoffee[is.na(dat$cashexp_teacoffee)] <- 0
dat$credit_teacoffee = dat$H9C
dat$credit_teacoffee[is.na(dat$credit_teacoffee)] <- 0
dat$ownproduction_teacoffee = dat$H9D
dat$ownproduction_teacoffee[is.na(dat$ownproduction_teacoffee)] <- 0
dat$cashexp_milling = dat$H11B
dat$cashexp_milling[is.na(dat$cashexp_milling)] <- 0
dat$credit_milling = dat$H11C
dat$credit_milling[is.na(dat$credit_milling)] <- 0
dat$ownproduction_milling = dat$H11D
dat$ownproduction_milling[is.na(dat$ownproduction_milling)] <- 0
dat$cashexp_firefuel = dat$H14B
dat$cashexp_firefuel[is.na(dat$cashexp_firefuel)] <- 0
dat$credit_firefuel = dat$H14C
dat$credit_firefuel[is.na(dat$credit_firefuel)] <- 0
dat$ownproduction_firefuel = dat$H14D
dat$ownproduction_firefuel[is.na(dat$ownproduction_firefuel)] <- 0
dat$cashexp_soap = dat$H15B
dat$cashexp_soap[is.na(dat$cashexp_soap)] <- 0
dat$cashexp_construction = as.numeric(dat$H18A)
dat$cashexp_construction[is.na(dat$cashexp_construction)] <- 0
dat$credit_construction = dat$H18B
dat$credit_construction[is.na(dat$credit_construction)] <- 0
dat$cashexp_taxfines = as.numeric(dat$H19A)
dat$cashexp_taxfines[is.na(dat$cashexp_taxfines)] <- 0
dat$credit_taxfines = dat$H19B
dat$credit_taxfines[is.na(dat$credit_taxfines)] <- 0
dat$cashexp_agritoolseed = as.numeric(dat$H20A)
dat$cashexp_agritoolseed[is.na(dat$cashexp_agritoolseed)] <- 0
dat$credit_agritoolseed = dat$H20B
dat$credit_agritoolseed[is.na(dat$credit_agritoolseed)] <- 0
dat$cashexp_hirelabor = as.numeric(dat$H21A)
dat$cashexp_hirelabor[is.na(dat$cashexp_hirelabor)] <- 0
dat$credit_hirelabor = dat$H21B
dat$credit_hirelabor[is.na(dat$credit_hirelabor)] <- 0
dat$cashexp_hhassets = as.numeric(dat$H22A)
dat$cashexp_hhassets[is.na(dat$cashexp_hhassets)] <- 0
dat$credit_hhassets = dat$H22B
dat$credit_hhassets[is.na(dat$credit_hhassets)] <- 0
dat$cashexp_livestock = as.numeric(dat$H23A)
dat$cashexp_livestock[is.na(dat$cashexp_livestock)] <- 0
dat$credit_livestock = dat$H23B
dat$credit_livestock[is.na(dat$credit_livestock)] <- 0
dat$cashexp_medical = as.numeric(dat$H24A)
dat$cashexp_medical[is.na(dat$cashexp_medical)] <- 0
dat$credit_medical = dat$H24B
dat$credit_medical[is.na(dat$credit_medical)] <- 0
dat$cashexp_educ = as.numeric(dat$H25A)
dat$cashexp_educ[is.na(dat$cashexp_educ)] <- 0
dat$credit_educ = dat$H25B
dat$credit_educ[is.na(dat$credit_educ)] <- 0
dat$cashexp_celebration = as.numeric(dat$H26A)
dat$cashexp_celebration[is.na(dat$cashexp_celebration)] <- 0
dat$credit_celebration = dat$H26B
dat$credit_celebration[is.na(dat$credit_celebration)] <- 0
dat$cashexp_rent = as.numeric(dat$H27A)
dat$cashexp_rent[is.na(dat$cashexp_rent)] <- 0
dat$credit_rent = dat$H27B
dat$credit_rent[is.na(dat$credit_rent)] <- 0
dat$cashexp_other = as.numeric(dat$H28A)
dat$cashexp_other[is.na(dat$cashexp_other)] <- 0
dat$credit_other = dat$H28B
dat$credit_other[is.na(dat$credit_other)] <- 0

# K. Agriculture
dat$land_access <- factor(dat$K01)
dat$land_cultivated <- factor(dat$K02)
dat$cultivated_cereals <- factor(dat$K03.1)
dat$cultivated_cereals[is.na(dat$cultivated_cereals)] <- 0
dat$cultivated_nutslegumes <- factor(dat$K03.2)
dat$cultivated_nutslegumes[is.na(dat$cultivated_nutslegumes)] <- 0
dat$cultivated_cashcrops <- factor(dat$K03.3)
dat$cultivated_cashcrops[is.na(dat$cultivated_cashcrops)] <- 0
dat$cultivated_vegetables <- factor(dat$K03.4)
dat$cultivated_vegetables[is.na(dat$cultivated_vegetables)] <- 0
dat$cultivated_tubers <- factor(dat$K03.5)
dat$cultivated_tubers[is.na(dat$cultivated_tubers)] <- 0

# L. Livestock
dat$own_livestock <- factor(dat$L01)
dat$num_cattle = dat$L02
dat$num_cattle[is.na(dat$num_cattle)] <- 0
dat$num_sheep =dat$L03
dat$num_sheep[is.na(dat$num_sheep)] <- 0
dat$num_goats = dat$L04
dat$num_goats[is.na(dat$num_goats)] <- 0
dat$num_pigs = dat$L05
dat$num_pigs[is.na(dat$num_pigs)] <- 0
dat$num_poultry = dat$L06
dat$num_poultry[is.na(dat$num_poultry)] <- 0

# M. Assistance received
dat$gfd <- factor(dat$M02.1)
dat$morethanmonth_aid <- factor(ifelse(dat$M03 == 4, 1, 0))
dat$morethanmonth_aid[is.na(dat$morethanmonth_aid)] <- 0

# select variables and subset dataset
# identify existing variables we want to keep
keepvars <- c("Total_NonFood_exp", "Total_Cash_Expe", "Total_Credit_Expe", "Total_nopurch_Expe", "Total_Expe", "rCSI", "FCS")

# subset to include newly created variables
dat.1 <- dat[,which(colnames(dat) == "psu") : which(colnames(dat) == "morethanmonth_aid")]

# subset to include existing variables and combine
col.location <- function(x){
  v <- numeric(length(x))
  for (i in 1:length(x)){
    v[i] <- which(colnames(dat) == x[i])
  }
  return(v)
}
v <- col.location(keepvars)
dat.1.1 <- dat[v]
dat.2 <- cbind(dat.1, dat.1.1)
dat.2 <- dat.2[,-c(19, 23)]
glimpse(dat.2)

## PSM modelling
# 1. Preliminary analysis
# covariate balance test (continuous variables)

cov.cont <- dat.2 %>%
  dplyr::select_if(is.numeric)
treat.cont <- dat.2$gfd == 1
st.d.cont <- apply(cov.cont, 2, function(x){
  100 *(mean(x[treat.cont]) - mean(x[!treat.cont])) / (sqrt(0.5*(var(x[treat.cont]) + var(x[!treat.cont]))))
})
st.d.cont.table <- abs(st.d.cont)
st.d.cont.table # good balance on gfd receipt, low imbalance (only one variable over 25)


# covariate balance test (binary variables) 
dat.test.bin <- dat.2[,c(4, 14:21, 36, 39, 40, 42, 50, 53, 54, 59)]
dat.test.bin <- apply(dat.test.bin, 2, function(x) as.numeric(x))
treat.bin <- (dat.2$gfd == 1)

st.d.bin <- apply(dat.test.bin, 2, function(x){
  100 *(mean(x[treat.bin]) - mean(x[!treat.bin])) / sqrt(0.5*((mean(x[treat.bin]) * (1-mean(x[!treat.bin]))) + (mean(x[!treat.bin]) * (1-mean(x[treat.bin])))))
})
st.d.bin.table <- abs(st.d.bin)
st.d.bin.table # likewise good balance on 'gfd' variable

# cleanup; no NAs, no detailed expenditures, (keep collinear vars for additional modelling as needed), add food expenditure share and location fixed effects, eliminate morethanmonth_aid 
summary(dat.2)
dat.psm <- dat.2[,-c(47:102)]

dat.psm <- cbind(dat.psm, FoodExp_share = dat$FoodExp_share)
dat.psm <- cbind(dat.psm, state = factor(dat$A05)) # state
dat.psm <- cbind(dat.psm, county = factor(dat$A06)) # county
dat.psm <- cbind(dat.psm, payam = factor(dat$A07)) # payam
dat.psm <- cbind(dat.psm, village = factor(dat$A08)) # village
dat.psm <- dat.psm[,-(which(colnames(dat.psm) == "morethanmonth_aid"))]
dat.2 <- na.omit(dat.2)


# dat.psm$gfd <- as.integer(dat.psm$gfd)
# attach survey weights, eliminate cases with missing weights
#dat.psm  <- cbind(dat.psm, wts = dat$Weights_FSNMS_R23_IJ)
dat.psm <- na.omit(dat.psm)

# alternative is set NA weights to 1 :: dat.psm[is.na(dat.psm$wts)] <- 1
svy.1 <- svydesign(data = dat.psm,
                  id = ~psu,
                  nest = TRUE,
                  strata = ~county,
                  weights = ~wts)

# naive fit
fit.naive <- lm(FCS ~ gfd,
                data = dat.psm)
summary(fit.naive) # naive fit shows that receiving gfd is associated with only a ~.7 point lower food consumption score

# test regression with survey weights
fit.reg <- lm(FCS ~ .,
              data = dat.psm)
summary(fit.reg) # negative, but non-significant association between gfd and FCS

# add higher order polinomials
dat.psm$age_hh2 <- dat.psm$age_hh^2
dat.psm$Total_nopurch_Expe2 <- dat.psm$Total_nopurch_Expe^2
dat.psm$Total_NonFood_exp2 <- dat.psm$Total_NonFood_exp^2
dat.psm$Total_Cash_Expe2 <- dat.psm$Total_Cash_Expe^2
dat.psm$Total_Credit_Expe2 <- dat.psm$Total_Credit_Expe^2
dat.psm$sex_hh_f_num <- ifelse(dat.psm$sex_hh == "f", 1, 0)
dat.psm$land_x_sex <- as.numeric(dat.psm$sex_hh_f_num) * as.numeric(dat.psm$land_access)
# second test (convert gfd to numeric)
dat.psm$gfd <- as.numeric(dat.psm$gfd)
fit.reg.2 <- lm(gfd ~ .,
                data = dat.psm)
summary(fit.reg.2)

# 2. Propensity score estimation # without weights for now, algorithm won't converge with weights....
dat.psm$gfd <- factor(dat.psm$gfd)

model.1 <- glm(gfd ~ edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + members_hh +  recent_migrants +  livelihood_1 + livelihood_2 + sold_productiveassets + land_access + sex_hh + age_hh + water_time + jerrycans + defecate_location + remittance  + graingrinding + Total_NonFood_exp + Total_Cash_Expe + Total_Credit_Expe + Total_nopurch_Expe + num_cattle + num_goats + num_poultry + bicycle + trad_stove + seeds + FoodExp_share + FCS,
               data = dat.psm,
               family = "binomial")
summary(model.1)

# model with polinomials and interactions... a *slightly* better fit

model.2 <- glm(gfd ~ edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + members_hh +  recent_migrants +  livelihood_1 + livelihood_2 + sold_productiveassets + land_access + sex_hh + age_hh + water_time + jerrycans + defecate_location + remittance  + graingrinding + Total_NonFood_exp + Total_Cash_Expe + Total_Credit_Expe + Total_nopurch_Expe + num_cattle + num_goats + num_poultry + bicycle + trad_stove + seeds + FoodExp_share + land_x_sex + age_hh2 + Total_nopurch_Expe2 + FCS,
               data = dat.psm,
               family = "binomial")
summary(model.2)

model.3 <- glm(gfd ~ edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + members_hh +  recent_migrants +  livelihood_1 + livelihood_2 + sold_productiveassets + land_access + sex_hh + age_hh + water_time + jerrycans + defecate_location + remittance  + graingrinding + Total_NonFood_exp + Total_Cash_Expe + Total_Credit_Expe + Total_nopurch_Expe + num_cattle + num_goats + num_poultry + bicycle + trad_stove + seeds + FoodExp_share + land_x_sex + age_hh2 + Total_nopurch_Expe2 + FCS + psu,
               data = dat.psm,
               family = "binomial")
summary(model.3)

# Chi^2 test
anova(model.1, model.2, test = "Chisq")
anova(model.2, model.3, test = "Chisq") # we reject the null hypothesis, so are justified to go with the more 'complete' model.3

# select model.3 
formula <- gfd ~ edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + members_hh +  recent_migrants +  livelihood_1 + livelihood_2 + sold_productiveassets + land_access + sex_hh + age_hh + water_time + jerrycans + defecate_location + remittance  + graingrinding + Total_NonFood_exp + Total_Cash_Expe + Total_Credit_Expe + Total_nopurch_Expe + num_cattle + num_goats + num_poultry + bicycle + trad_stove + seeds + FoodExp_share + land_x_sex + age_hh2 + Total_nopurch_Expe2 + FCS + psu

# testing the linearity assumption visually
# draw probabilities for model 3
probabilities <- predict(model.3, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# select only numeric predictors
dat.x <- dat.psm %>% 
  dplyr::select_if(is.numeric)
predictors <- colnames(dat.x)

# binding the logit and tidying up data for plot
dat.x <- dat.x %>% 
  mutate(logit = log(probabilities/ (1-probabilities))) %>% 
  gather(key = "predictors", value = "predictor.value", -logit)
plots <- ggplot(dat.x, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") # this is fine. Most of the nonlinearity in the loess curves are due to outliers.

## 3. Propensity score matching
# attach predicted propensity scores to dat.psm
dat.psm$psvalue <- predict(model.3, type = "response")

# observe group balance before matching
histbackback(split(dat.psm$psvalue, dat.psm$gfd), main = "Propensity score before matching", xlab = c("Non-assistance", "Assistance")) # largely doable.

# nearest neighbor matching with calipers, with replacement
# dat.psm$gfd <- ifelse(dat.psm$gfd == 2, 1, 0)
m.nn <- matchit(formula,
        data = dat.psm,
        method = "nearest",
        caliper = 0.25,
        replace = TRUE)
summary(m.nn)
match.data <- match.data(m.nn)

# visuals
plot(m.nn, type = "jitter")
plot(m.nn, type = "hist")

# post matching back to back histogram (better balance)
histbackback(split(match.data$psvalue, match.data$gfd), main = "Propensity Scores After Matching", xlab = c("Non-benefs", "Beneficiaries"))

# post-matching covariate balance checks (//just for doubles and integers here, complement as you wish)
dat.match.cont <- match.data %>%
  dplyr::select_if(is.numeric)
treat.match.cont <- (match.data$gfd == 1)
cov.match.cont <- dat.match.cont

st.d.cont.match <- apply(cov.match.cont, 2, function(x){
  100 *(mean(x[treat.match.cont]) - mean(x[!treat.match.cont])) / (sqrt(0.5*(var(x[treat.match.cont]) + var(x[!treat.match.cont]))))
})
abs(st.d.cont.match) # very low distances indicate good covariate balance post matching

## 4. Outcomes analysis
# save matched data
matches <- data.frame(m.nn$match.matrix)

# find the matches for group 1 and 2
group1 <- match(row.names(matches), row.names(match.data))
group2 <- match(matches$X1, row.names(match.data))

# extract the outcome value for the matches
yT <- match.data$FCS[group1]
yC <- match.data$FCS[group2]

# binding
matched.cases <- cbind(matches, yT, yC)

# paired t-test

t.test(matched.cases$yT, matched.cases$yC,
       paired = TRUE) 

# No significant differences in food consumption score, total expenditure, or cash expenditure were found, after propensity score matching. 
# Substantively this means that there is either a serious endogeneity issue, or in actual fact gfd does not greatly contribute to improvements in the food consumption score of households (i.e. at the same levels of all covariates, households have similar food consumption regardless of status as beneficiaries. If households were not given gfd, they would still ensure to have a similar food consumption score)
