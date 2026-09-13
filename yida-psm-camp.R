## packages
require(tidyverse)
require(haven)
require(data.table)
require(Hmisc)
require(MatchIt)
require(CBPS)

## load data
dat <- read_sav(here::here("data", "yida.sav"))

## explore data
na.count(dat)

## Find vars to keep and subset
# Payam name
dat$A05
# village name
dat$A06
# sex hhh
dat$B01
# age hhh
dat$B02
# education level hhh
dat$B03
# male, female under 5
dat$B04M
dat$B04F
# male, female 5 to 15
dat$B05M
dat$B05F
# male, female 16 to 60
dat$B06M
dat$B06F
# male, female, above 60
dat$B07M
dat$B07F
# total family members
dat$B0_total_family_members
# resident/refugee
dat$B08
# disability (physical, mental, chronic ill, injured)
dat$B13
dat$B14
dat$B15
dat$B16
# source of drinking water
dat$D01
# time to collect water
dat$D02
# defecation location
dat$D06
# sickness in past 2 months
dat$D10
# household livelihood (primary, secondary, tertiary)
dat$E01
dat$E02
dat$E03
# access to land
dat$K01
# land size
dat$K02
# plant crops in the current season?
dat$K03
# livestock ownership yes/no
dat$L01
# number of livestock owned (cattle, sheep, goats, pigs, poultry)
dat$L02
dat$L03
dat$L04
dat$L05
dat$L06

# get list of columns and location
columns <- c("A05", "A06", "B01", "B02", "B03", "B04M", "B04F","B05M", "B05F", "B06M", "B06F", "B07M", "B07F", "B0_total_family_members", "B08",  "D01", "D02", "D06", "D10", "E01", "E02", "E03", "K01", "K02", "K03", "L01", "L02", "L03", "L04", "L05", "L06", "edu_HH", "PhysiclDisability_YN", "MentalDisability_YN", "ChronicallyIll_YN", "Injured_YN", "H01B", "H01C", "H01D", "H02B", "H02C", "H02D", "H03B", "H03C", "H03D", "H04B", "H04C", "H04D", "H05B", "H05C", "H05D", "H06B", "H06C", "H06D", "H07B", "H07C", "H07D", "H8B", "H8C", "H8D", "H9B", "H9C", "H9D", "H11B", "H11C", "H11D", "H12B", "H12C", "H12D", "H13B", "H13C", "H13D", "H14B", "H14C", "H14D", "H15B", "H15C", "H15D", "H16B", "H16C", "H16D", "H17B", "H17C", "H17D", "H18A", "H18B", "H19A", "H19B", "H20A", "H20B", "H21A", "H21B", "H22A", "H22B", "H23A", "H23B", "H24A", "H24B", "H25A", "H25B", "H26A", "H26B", "H27A", "H27B", "H28A", "H28B", "H29", "H30", "H31_food",  "H31_cash", "Total_Cash_Expe", "Total_Credit_Expe", "FCS", "Total_Expe", "FoodExp_share", "Total_nopurch_Expe", "FS_final", "rCSI", "rCSI_cat", "HDDS_cat", "HDDS_scores")

col.location <- function(x){
  v <- numeric(length(x))
  for (i in 1:length(x)){
    v[i] <- which(colnames(dat) == x[i])
  }
  return(v)
}
v <- col.location(columns)

# subset dataset
dat.2 <- dat[-1, v] # row 1 was weird, looked like test row

# rename and label data
dat.3 <- dat.2 %>% 
  mutate(state = factor(A05),
         county = factor(A06),
         female = factor(
           case_when(B01 == 1 ~ 0,
                     B01 == 2 ~ 1),
           levels = c(0, 1),
           labels = c("Male", "Female")),
         age_hh = B02,
         edu_hh = factor(
           case_when(edu_HH == 0 ~ 0,
                     edu_HH == 1 ~ 1,
                     edu_HH == 2 ~ 2),
           levels = c(0, 1, 2),
           labels = c("No formal edu", "Up to primary", "Above primary")),
         male_under5 = B04M,
         female_under5 = B05F,
         male5to15 = B05M,
         female5to15 = B05F,
         male16to60 = B06M,
         female16to60 = B06F,
         maleover60 = B07M,
         femaleover60 = B07F,
         tot_hh = as.integer(B0_total_family_members),
         refugee = ifelse(B08 == 1, 0, 1),
         dis_phys = factor(PhysiclDisability_YN),   
         dis_mental = factor(MentalDisability_YN),
         dis_illness = factor(ChronicallyIll_YN),
         dis_injured = factor(Injured_YN),
         water_source = factor(
           case_when(D01 == 1 ~ 1,
                     D01 == 2 ~ 2,
                     D01 == 5 ~ 3,
                     D01 == 6 ~ 4),
           levels = c(1, 2, 3, 4),
           labels = c("Borehole", "Tap stand", "Swamp", "Stagnant water")),
         water_time = factor(
           case_when(D02 == 1 ~ 1,
                     D02 == 2 ~ 2,
                     D02 == 3 ~ 3,
                     D02 == 4 ~ 4,
                     D02 == 5 ~ 5,
                     D02 == 6 ~ 6,
                     D02 == 7 ~ 7),
           levels = c(1, 2, 3, 4, 5, 6, 7),
           labels = c("inside compound", "Under 30 min", "30 min to 1 hour", "1 hour to half day", "half day", "more than half day", "dont know")),
         defecate_location = factor(
           case_when(D06 == 1 ~ 1,
                     D06 == 2 ~ 2,
                     D06 == 3 ~ 3,
                     D06 == 4 ~ 4,
                     D06 == 5 ~ 5,
                     D06 == 6 ~ 6,
                     D06 == 7 ~ 7),
           levels = c(1, 2, 3, 4, 5, 6, 7),
           labels = c("in the bush", "in the river", "dig hole and fill", "latrine", "dont know", "dont want to answer", "other")),
         recently_sick = factor(
           case_when(D10 == 1 ~ 1,
                     D10 == 2 ~ 2,
                     D10 == 3 ~ 3,
                     D10 == 4 ~ 4),
           levels = c(1, 2, 3, 4),
           labels = c("children", "children and adults", "adults", "no")),
         livelihood_1 = factor(
           case_when(E01 == NA ~ 0,
                     E01 == 1 ~ 1,
                     E01 == 2 ~ 2,
                     E01 == 3 ~ 3,
                     E01 == 4 ~ 4,
                     E01 == 5 ~ 5,
                     E01 == 6 ~ 6,
                     E01 == 7 ~ 7,
                     E01 == 8 ~ 8,
                     E01 == 9 ~ 9,
                     E01 == 10 ~ 10,
                     E01 == 11 ~ 11,
                     E01 == 12 ~ 12,
                     E01 == 13 ~ 13,
                     E01 == 14 ~ 14,
                     E01 == 15 ~ 15),
           levels = c(0, 1, 2,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
           labels = c("None", "agriculture, sale of cereals", "livestock, sale of livestock", "sale of alcohol", "casual labor", "skilled labor", "trade, commerce", "salaried work", "sale of firewood, charcoal, grass etc.", "borrowing", "fishing, sale of fish", "family support, remittances", "begging", "food assistance, sale of food assistance", "gathering of wild food", "other")),
         livelihood_2 = factor(
           case_when(is.na(E02) ~ 0,
                     E02 == 1 ~ 1,
                     E02 == 2 ~ 2,
                     E02 == 3 ~ 3,
                     E02 == 4 ~ 4,
                     E02 == 5 ~ 5,
                     E02 == 6 ~ 6,
                     E02 == 7 ~ 7,
                     E02 == 8 ~ 8,
                     E02 == 9 ~ 9,
                     E02 == 10 ~ 10,
                     E02 == 11 ~ 11,
                     E02 == 12 ~ 12,
                     E02 == 13 ~ 13,
                     E02 == 14 ~ 14,
                     E02 == 15 ~ 15),
           levels = c(0, 1, 2,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
           labels = c("None", "agriculture, sale of cereals", "livestock, sale of livestock", "sale of alcohol", "casual labor", "skilled labor", "trade, commerce", "salaried work", "sale of firewood, charcoal, grass etc.", "borrowing", "fishing, sale of fish", "family support, remittances", "begging", "food assistance, sale of food assistance", "gathering of wild food", "other")),
         livelihood_3 = factor(
           case_when(E03 == NA ~ 0,
                     E03 == 1 ~ 1,
                     E03 == 2 ~ 2,
                     E03 == 3 ~ 3,
                     E03 == 4 ~ 4,
                     E03 == 5 ~ 5,
                     E03 == 6 ~ 6,
                     E03 == 7 ~ 7,
                     E03 == 8 ~ 8,
                     E03 == 9 ~ 9,
                     E03 == 10 ~ 10,
                     E03 == 11 ~ 11,
                     E03 == 12 ~ 12,
                     E03 == 13 ~ 13,
                     E03 == 14 ~ 14,
                     E03 == 15 ~ 15),
           levels = c(0, 1, 2,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
           labels = c("None", "agriculture, sale of cereals", "livestock, sale of livestock", "sale of alcohol", "casual labor", "skilled labor", "trade, commerce", "salaried work", "sale of firewood, charcoal, grass etc.", "borrowing", "fishing, sale of fish", "family support, remittances", "begging", "food assistance, sale of food assistance", "gathering of wild food", "other")),
         land_access = factor(
           case_when(K01 == 0 ~ 0,
                     K01 == 1 ~ 1),
           levels = c(0, 1),
           labels = c("No", "Yes")),
         land_size = factor(
           case_when(K02 == NA ~ 0,
                     K02 == 1 ~ 1,
                     K02 == 2 ~ 2,
                     K02 == 3 ~ 3,
                     K02 == 4 ~ 4),
           levels = c(0, 1, 2, 3, 4),
           labels = c("None", "Less than half feddan", "half to one feddan", "More than one feddan", "Don't know")),
         crop_planted = factor(
           case_when(K03 == 0 ~ 0,
                     K03 == 1 ~ 1),
           levels = c(0, 1),
           labels = c("No", "Yes")),
         livestock_own = factor(
           case_when(L01 == 0 ~ 0,
                     L01 == 1 ~ 1),
           levels = c(0, 1),
           labels = c("No", "Yes")),
         num_cattle = L02,
         num_sheep = L03,
         num_goats = L04,
         num_pigs = L05,
         num_poultry = L06,
         CARI = factor(
           case_when(FS_final == 1 ~ 1,
                     FS_final == 2 ~ 2,
                     FS_final == 3 ~ 3,
                     FS_final == 4 ~ 4),
           levels = c(1, 2, 3, 4),
           labels = c("Food Secure", "Marginally secure", "Moderately insecure", "Severely insecure")),
         cashexp_cereal = H01B,
         credit_cereal = H01C,
         ownproduction_cereal = H01D,
         cashexp_tubers = H02B,
         credit_tubers = H02C,
         ownproduction_tubers = H02D,
         cashexp_pulses = H03B,
         credit_pulses = H03C,
         ownproduction_pulses = H03D,
         cashexp_vegfruit = H04B,
         credit_vegfruit = H04C,
         ownproduction_vegfruit = H04D,
         cashexp_meats = H05B,
         credit_meats = H05C,
         ownproduction_meats = H05D,
         cashexp_oilfat = H06B,
         credit_oilfat = H06C,
         ownproduction_oilfat = H06D,
         cashexp_dairy = H07B,
         credit_dairy = H07C,
         ownproduction_dairy = H07D,
         cashexp_sugarsalt = H8B,
         credit_sugarsalt = H8C,
         ownproduction_sugarsalt = H8D,
         cashexp_teacoffee = H9B,
         credit_teacoffee = H9C,
         ownproduction_teacoffee = H9D,
         cashexp_milling = H11B,
         credit_milling = H11C,
         ownproduction_milling = H11D,
         cashexp_clothes = H12B,
         credit_clothes = H12C,
         ownproduction_clothes = H12D,
         cashexp_gasfuel = H13B,
         credit_gasfuel = H13C,
         ownproduction_gasfuel = H13D,
         cashexp_firefuel = H14B,
         credit_firefuel = H14C,
         ownproduction_firefuel = H14D,
         cashexp_soap = H15B,
         credit_soap = H15C,
         ownproduction_soap = H15D,
         cashexp_alcoholtob = H16B,
         credit_alcoholtob = H16C,
         ownproduction_alcoholtob = H16D,
         cashexp_trans_comm = H17B,
         credit_trans_comm = H17C,
         ownproduction_trans_comm = H17D,
         cashexp_construction = H18A,
         credit_construction = H18B,
         cashexp_taxfines = H19A,
         credit_taxfines = H19B,
         cashexp_agritoolseed = H20A,
         credit_agritoolseed = H20B,
         cashexp_hirelabor = H21A,
         credit_hirelabor = H21B,
         cashexp_hhassets = H22A,
         credit_hhassets = H22B,
         cashexp_livestock = H23A,
         credit_livestock = H23B,
         cashexp_medical = H24A,
         credit_medical = H24B,
         cashexp_educ = H25A,
         credit_educ = H25B,
         cashexp_celebration = H26A,
         credit_celebration = H26B,
         cashexp_rent = H27A,
         credit_rent = H27B,
         cashexp_other = H28A,
         credit_other = H28B,
         freq_borrow = factor(
           case_when(H29 == 1 ~ 1,
                     H29 == 2 ~ 2,
                     H29 == 3 ~ 3,
                     H29 == 4 ~ 4,
                     H29 == 5 ~ 5),
           levels = c(1, 2, 3, 4, 5),
           labels = c("Never", "Once", "Twice", "Three times", "More than three times")),
         reason_borrow = factor(
           case_when(H30 == NA ~ 0,
                     H30 == 1 ~ 1,
                     H30 == 2 ~ 2,
                     H30 == 3 ~ 3,
                     H30 == 5 ~ 4,
                     H30 == 8 ~ 5,
                     H30 == 10 ~ 6,
                     H30 == 12 ~ 7),
           levels = c(0, 1, 2, 3, 4, 5, 6, 7),
           labels = c("Did not borrow", "Food purchase", "tuition fees", "Healthcare", "Agri inputs", "ceremonies", "Land or building", "Other")),
         current_debt_food = ifelse(is.na(H31_food), 0, H31_food),
         current_debt_cash = ifelse(is.na(H31_cash), 0, H31_cash)
         )
         
# remove old columns
dat.4 <- dat.3[,-c(1:which(colnames(dat.3) == "H31_cash"))]
# remove empty columns

## eda
require(skimr)
require(ggvis)
require(ggthemes)

# who lives where?
locations <- dat %>% 
  group_by(A08, B08) %>% 
  summarize(n())

skim(dat.4) # wow. There is very little variation in many key categories, including cash expenditure/credit/own production. This supposes a very homogeneous population. let's look a little further into the distribution of some of these income and asset ownership variables.

hist(dat.4$Total_Cash_Expe) # actually well...roughly 20% of the pop looks like it's spending ~2 dollars a day
hist(dat.4$Total_Expe) # same trend with total value produced/expenditure, but it looks like the bottom of the distribution might be making up for low expenditure by self-production somewhat.
summary(dat.4$Total_Expe) # half the population lives on less than 1.5 dollars a day. 25% live on more than 2.5 dollars a day

hist(dat.4$Total_Credit_Expe) # very few credit purchases, and in significantly smaller amounts.
hist(dat.4$FoodExp_share) # left skewed...
summary(dat.4$FoodExp_share) # 50% spend less than 64% of expenses on food, only 25% of HHs spend less than 50% of expenditure on food

hist(dat.4$FCS)
table(dat.4$CARI) # 14.5% of households are food secure according to CARI methodology

# by-status breakdown
ggplot(dat.4, aes(x = CARI))+
  geom_bar()+
  facet_wrap(~refugee)+
  theme_bw()

ggplot(dat.4, aes(x = Total_Expe, fill = refugee))+
  geom_histogram() # in fact it seems like refugees have a similar variation in total expenditure as the residents, just much fewer people spending at each level on average.

ggplot(dat.4, aes(x = Total_Expe, fill = refugee)) +
  geom_histogram()


ggplot(dat.4, aes(y = FCS, x = log(Total_Expe), color = refugee)) +
  geom_point(alpha = 0.4)+
  geom_smooth(se = F, method = "lm")+
  theme_classic() # tbh pretty similar... both residents and refugees's food security increases at the same rate with more income ++ about ~ 7 points of difference on the FCS scale, on average between the two groups (would need survey weights to see for sure, and would need to consider the design to have SEs we can trust). ALSO note how crazy scattered these points are. Expenditure is a bad predictor of FCS, most likely because expenditure does not consider food assistance received.

ggplot(dat.4, aes(y = FCS, x = FoodExp_share, color = refugee)) +
  geom_point(alpha = 0.4)+
  geom_smooth(se = TRUE, method = "lm") # interesting. Though scattered, the general trend is reversed: the more % refugees are spending on food, the better off they are, while the opposite is true of residents. What explains this? Perhaps refugees have pressing additional expenditures, and those that are well enough off can focus more income on food?

ggplot(dat.4, aes(y = FCS, x = CARI))+
  geom_boxplot()+
  facet_wrap(~refugee) # median FCS is much more similar for the first 2 CARi categories (food secure, marginally secure) than for the 3rd/4th, and the 3rd 4th are clustered together themselves.
  
ggplot(dat.4, aes(y = FoodExp_share, x = CARI))+
  geom_boxplot()+
  facet_wrap(~refugee) # With food expenditure share, there is high variability in CARI categories as well. For refugees, high overlap in Food expenditure share across the 1st, 2nd and 3rd categories.

ggplot(dat.4, aes(y = FCS, x = num_cattle))+
  geom_point(alpha = 0.25, size = 4)+
  geom_jitter()+
  facet_wrap(~refugee) # refugees don't report owning any cattle. Also cattle ownership looks like a mediocre predictor of food consumption

ggplot(dat.4, aes(x = num_cattle))+
  geom_bar()+
  facet_wrap(~CARI) # cow ownership isn't that bad with the CARI categories. But of course...refugees don't own cattle so... I wonder if basic demographics are different?
# also though high similarity across the groups could mean more balanced classes, which would be good for PSM... #

ggplot(dat.4, aes(x = female_under5))+
  geom_bar()+
  facet_wrap(~refugee) # crazy similar

ggplot(dat.4, aes(x = male_under5))+
  geom_bar()+
  facet_wrap(~refugee) # roughly same

ggplot(dat.4, aes(x = female16to60))+
  geom_bar()+
  facet_wrap(~refugee) # same

ggplot(dat.4, aes(x = male16to60))+
  geom_bar()+
  facet_wrap(~refugee) # same

ggplot(dat.4, aes(x = maleover60))+
  geom_bar()+
  facet_wrap(~refugee) # same

ggplot(dat.4, aes(x = log(Total_Cash_Expe)))+
  geom_histogram() #some variation is there

#education
ggplot(dat.4, aes(x = edu_hh))+
  geom_bar()+
  facet_wrap(~refugee) # very similar

# water source
ggplot(dat.4, aes(x = water_source))+
  geom_bar()+
  facet_wrap(~refugee) # more refugees get water from tap stands

ggplot(dat.4, aes(x = water_time))+
  geom_bar()+
  facet_wrap(~refugee) # and it takes them a little longer to get the water

# livelihoods and land access
ggplot(dat.4, aes(x = livelihood_1))+
  geom_bar()+
  facet_wrap(~refugee)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(dat.4, aes(x = land_access))+
  geom_bar()+
  facet_wrap(~refugee) # while uneven, close to 70 refugee households out of ~300 report access to land

ggplot(dat.4, aes(x = land_size))+
  geom_bar()+
  facet_wrap(~refugee) # though refugees have lower land area all things equal

ggplot(dat.4, aes(x = crop_planted))+
  geom_bar()+
  facet_wrap(~refugee) # almost all refugees with land access are planting

table(dat.4$land_access, dat.4$crop_planted) # almost everyone with land access also plants their crops

# correlations tables
require(psych)
pairs.panels(dat.4[42:60])
require(GGally)
ggcorr(dat.4[42:60])
ggcorr(dat.4[20:24])


# keep non-null and reasonable-variation variables, drop CARI and total cash expenditure, which is too highly correlated to total expenditure
keepvars <- c(2:5, 8:18, 20:22, 25:27, 29, 30, 32, 33, 36:38, 40, 42, 44, 57, 69, 78, 81)

# subset and reorder columns
dat.5 <- dat.4[keepvars]
dat.5 <- dat.5[, c(2, 16, 3, 1, 4:15, 17:35)]

# all NAs must be 0
dat.5$num_cattle[is.na(dat.5$num_cattle)] <- 0
dat.5$num_sheep[is.na(dat.5$num_sheep)] <- 0
dat.5$num_goats[is.na(dat.5$num_goats)] <- 0
dat.5$num_poultry[is.na(dat.5$num_poultry)] <- 0
dat.5$land_size[is.na(dat.5$land_size)] <- "None"

## PSM
# 1. Preliminary analysis
# covariate selection
dat.psm <- dat.5[,-c(17, 18)] #take out disabilities, which are being weird
dat.psm <- dat.psm[,-12] # take out perfectly collinear/singular variables *******(also remove CATTLE + SHEEP + water_time)
FCS <- dat.5$FCS # create vector for response variable

# cleanup
dat.psm <- na.omit(dat.psm) # no NAs. very few NAs, looks random, so no imputation necessary

# covariate imbalance test -- doubles and integers
cov.cont <- dat.psm[,c(***)] # redo numbers to capture all the integers/doubles
treat.cont <- dat.psm$refugee == 1
st.d.cont <- apply(cov.cont, 2, function(x){
  100 *(mean(x[treat.cont]) - mean(x[!treat.cont])) / (sqrt(0.5*(var(x[treat.cont]) + var(x[!treat.cont]))))
})
st.d.cont.table <- abs(st.d.cont)
st.d.cont.table # some (~11) areas of high suspect covariates...

fit.reg <- lm(refugee ~ ., data = dat.psm)
summary(fit.reg) #... overall negated by the very small magnitude of difference

fit.naive <- lm(FCS ~ refugee, data = dat.5)
summary(fit.naive) # the naive comparison shows a ~7.8 point FCS score difference (naive 'effect' of being a refugee on food consumption) We consider this the 'effect' of receiving food assistance

# 2. Propensity score estimation
require(MatchIt)

model.1 <- glm(refugee ~ Total_Expe + Total_Credit_Expe +FoodExp_share + female + age_hh + edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + livelihood_1 + livelihood_2 + defecate_location + num_goats + num_poultry + land_access + land_size + cashexp_cereal + ownproduction_cereal + cashexp_oilfat + cashexp_milling + cashexp_firefuel + cashexp_soap + water_source,
               data = dat.psm,
               family = binomial())
summary(model.1)

# remove statistically non-significant variables not known to be associated with food assistance: num_goats, num_poultry
model.2 <- glm(refugee ~ Total_Expe + Total_Credit_Expe +FoodExp_share + female + age_hh + edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + livelihood_1 + livelihood_2 + defecate_location + land_access + land_size + cashexp_cereal + ownproduction_cereal + cashexp_oilfat + cashexp_milling + cashexp_firefuel + cashexp_soap + water_source,
               data = dat.psm,
               family = binomial())
summary(model.2)

# add higher-order covariates/quadratic terms
dat.psm$Total_Expe2 <- dat.psm$Total_Expe^2
dat.psm$age_hh2 <- dat.psm$age_hh^2

model.3 <- glm(refugee ~ Total_Expe + Total_Expe2 +  Total_Credit_Expe + FoodExp_share + female + age_hh + age_hh2 + edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + livelihood_1 + livelihood_2 + defecate_location + land_access + land_size + cashexp_cereal + ownproduction_cereal + cashexp_oilfat + cashexp_milling + cashexp_firefuel + cashexp_soap + water_source,
               data = dat.psm,
               family = binomial())
summary(model.3) # lowest residual deviance and AIC, fit looks good

# attach predicted propensity scores to dat.psm
dat.psm$psvalue <-  predict(model.3, type = "response")

# observe group balance before matching
histbackback(split(dat.psm$psvalue, dat.psm$refugee), main = "Propensity score before matching", xlab = c("Resident", "Refugee")) # a very high imbalance here

# nearest neighbor matching with calipers 
match.model3 <- matchit(refugee ~ Total_Expe + Total_Expe2 +  Total_Credit_Expe + FoodExp_share + female + age_hh + age_hh2 + edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + livelihood_1 + livelihood_2 + defecate_location + land_access + land_size + cashexp_cereal + ownproduction_cereal + cashexp_oilfat + cashexp_milling + cashexp_firefuel + cashexp_soap + water_source,
                        data = dat.psm,
                        method = "nearest",
                        caliper = 0.20)
summary(match.model3) # results in too few matches (13)
plot(match.model3, type = "jitter") 

# alternative version: Automated Covariate Balance in model specification
require(CBPS)
model.cbps <- CBPS(refugee ~ Total_Expe + Total_Expe2 +  Total_Credit_Expe + FoodExp_share + female + age_hh + age_hh2 + edu_hh + male_under5 + female_under5 + male5to15 + male16to60 + female16to60 + maleover60 + femaleover60 + livelihood_1 + livelihood_2 + defecate_location + land_access + land_size + cashexp_cereal + ownproduction_cereal + cashexp_oilfat + cashexp_milling + cashexp_firefuel + cashexp_soap + water_source,
            data = dat.psm,
            ATT = TRUE)

summary(model.cbps)

match.cbps <- matchit(refugee ~ fitted(match.cbps),
                      data = dat.psm,
                      method = "nearest",
                      caliper = 0.25)
summary(match.cbps)
plot(match.cbps, type = "jitter") # still results in too few matches (2)

# conclusion: the groups are too imbalanced to be able to derive a quasi-causal coefficient of FCS, or any other measure.