# first execute "Merging FSNMS datasets.R" file

# packages
require(tidyverse)
require(quantreg)
require(survey)



# no purchase expenditure NAs to 0
dat.all$tot.nopurch.exp[is.na(dat.all$tot.nopurch.exp)] <- 0 

# create modified SDR variable. This one drops consideration of credit and calculated a rate, not a ratio. Tradeoffs on this decision are briefly discussed in the note. Suffice it to say it would be too weird a distribution to try to derive sense out of in original form. (But I think that's also because the shape of the distribution of the modified SDR that I calculated is less skewed than we would think. I'm not too sure)
dat.sdr <- dat.all %>% 
  filter(gfd == 1) %>% 
  mutate(sdr.numerator = (196 * 30 * members_hh),
         sdr.denominator = (tot.cash.exp + tot.nopurch.exp + sdr.numerator),
         SDR = sdr.numerator/sdr.denominator)

ggplot(dat.sdr, aes(x = SDR))+
  geom_density() # much more skewed than the Yida data...! It denotes however that the people receiving food truly highly depend on it

# SDR by round: kernel densities for each round + show the top right quadrant

sdr.20 <- dat.sdr[dat.sdr$round == 20,]
sdr.21 <- dat.sdr[dat.sdr$round == 21,]
sdr.22 <- dat.sdr[dat.sdr$round == 22,]
sdr.23 <- dat.sdr[dat.sdr$round == 23,]

# ROUND-SDR evolution by round
ROUND_SDR <- ggplot(dat.sdr, aes(x = SDR, fill = round))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = nominal)+
  labs(title = "SDR Kernel Density by FSNMS Round",
       y = "Density",
       fill = "FSNMS Round")+
  theme(legend.position = "bottom")

# SDR by FCS for each round
SDR_FCS <- ggplot(dat.sdr, aes(x = fcs, y = SDR))+
  geom_jitter(alpha = 0.25, color = "grey30")+
  geom_smooth(method = 'lm', se = T, color = "grey15")+
  geom_jitter(alpha = 0.7, size = 2)+
  geom_vline(xintercept = 37, linetype = "dashed", color = "darkred")+
  geom_hline(yintercept = 0.70, linetype = "dashed", color = "darkred")+
  annotate("rect", xmin = -Inf, xmax = 37, ymin = 0.7, ymax = Inf, fill = "#ff5252", alpha = 0.2)+
  annotate("rect", xmin = 37, xmax = Inf, ymin = 0.7, ymax = Inf, fill = "#ffc759", alpha = 0.2)+
  annotate("rect", xmin = 37, xmax = Inf, ymin = -Inf, ymax = 0.7, fill = "#b3de62", alpha = 0.2)+
  annotate("rect", xmin = -Inf, xmax = 37, ymin = -Inf, ymax = 0.7, fill = "gray85", alpha = 0.2)+
  facet_wrap(~round)+
  scale_color_manual(values = nominal)+
  labs(title = "SDR vs. Food Consumption Score by FSNMS Round",
       x = "Food Consumption Score")+
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "grey30",
                                  face = "bold",
                                  size = 12, 
                                  family = "Tahoma"),
        legend.position = "none")


# modeling... done by year
dat.sdr$weights.ij <- as.numeric(dat.sdr$weights.ij)

ggplot(sdr.23, aes(x = SDR))+
  geom_density() # round 23 is a _little_ less skewed

#remove single-psu strata
sdr.23 <- sdr.23 %>% 
  filter(county != "Abiemnhom",
         county != "Baliet",
         county != "Panyikang",
         county != "Aweil South",
         county != "Ezo",
         county != "Maridi",
         county != "Lafon",
         county != "Nagero",
         county != "Yei",
         county != "Maiwut",
         county != "Wulu",
         county != "Tonj East",
         county != "Pariang",
         county != "Terekeka")


design.23 <- (svydesign(ids = ~psu, strata = ~county,  weights = ~weights.ij, data = sdr.23, nest = TRUE))

# bootstrap weights design
bootdesign <- as.svrepdesign(design.23, type = 'auto', replicates = 150)

# transform SDR

fit.qr <- withReplicates(bootdesign, (rq(SDR ~ fcs, weights = weights.ij, tau = 0.5, method = "fn", data = sdr.23)))
# Ok this is too bad but for time reasons I have to move on and can't keep on this. the point of doing this analysis is more to show what would be possible with Maban information anyway. the above fit.qr throws a variance-(theta) related error, which I suspect has to do with the nature of the weightsij variable in the dataset. what I'm trying to do here though is the same quantile regression as in the Yida data, but this time accounting for the survey design. Bootstrap weights are meant to give estimates as regular taylor series linearization will not work with quantile regression.
# if troubleshooting fails and you really want to do this for some reason, the minimum would be to factor in the weights into the regression which can be done with "weights = " in the regular 'rq' call. But you won't be able to trust the variance estimates (standard errors etc) that you get, they will most likely all be under-estimated.