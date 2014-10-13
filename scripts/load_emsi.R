
library(dplyr)
library(ggplot2)

# to set up files be sure to put the data cells in excel into number format without commas
# also, remove and ' single quotes in the header row

fname <- "input_data/msa_ht31_withex.csv"
temp <- read.table(fname, sep=",",
                      header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, msa = MSA, msa_name = MSA.Name, 
                ht31_2001_j = X2001.Jobs, 
                ht31_2004_j = X2004.Jobs, 
                ht31_2013_j = X2013.Jobs)
head(temp)
msa_core <- temp


fname <- "input_data/msa_hospair_withex.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, msa = MSA, 
               htair_2001_j = X2001.Jobs, 
               htair_2004_j = X2004.Jobs, 
               htair_2013_j = X2013.Jobs)
head(temp)
msa_core <- merge(msa_core, temp, by = "msa")

fname <- "input_data/msa_alltwodig_withex.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, msa = MSA, 
               tot_2001_j = X2001.Jobs, 
               tot_2004_j = X2004.Jobs, 
               tot_2013_j = X2013.Jobs)
msa_core <- merge(msa_core, temp, by = "msa")

fname <- "input_data/msa_manf_withexprop.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, msa = MSA, 
               manf_2001_j = X2001.Jobs, 
               manf_2004_j = X2004.Jobs,
               manf_2013_j = X2013.Jobs)
msa_core <- merge(msa_core, temp, by = "msa")

fname <- "input_data/msa_by_region.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, msa, 
               region)
msa_core <- merge(msa_core, temp, by = "msa")

fname <- "input_data/msa_pop.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, msa = GeoFips, 
               pop1990 = X1990, 
               pop1994 = X1994,
               pop2004 = X2004,
               pop2000 = X2000, 
               pop2012 = X2012
               ) #msa_name = GeoName, 
msa_core <- merge(msa_core, temp, by = "msa")


fname <- "input_data/msa_jobs_bea.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, msa = GeoFips, 
               jobs1990 = X1990, 
               jobs1994 = X1994,
               jobs2004 = X2004,
               jobs2000 = X2000, 
               jobs2012 = X2012
) 
msa_core <- merge(msa_core, temp, by = "msa")

fname <- "input_data/msa_per_capita_inc.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, msa = GeoFips, 
               percapita_2004 = X2004,
               percapita_2000 = X2000) #msa_name = GeoName, 
msa_core <- merge(msa_core, temp, by = "msa")


head(msa_core)
msa_core1 <- msa_core
msa_core1 <- mutate(msa_core1, 
                    tot_gr = ((tot_2013_j / tot_2004_j)-1),
                    tot_gr_log = log(tot_2013_j / tot_2004_j),
                   # totbea_gr_log = log(jobs2012 / jobs2004),
                    ht_share_log = log(ht31_2004_j / tot_2004_j),
                    ht_share_ratio = (ht31_2004_j / tot_2004_j),
                    ht_per_thous = ((ht31_2004_j / tot_2004_j)*1000),
                    ht_per_thous_log = log((ht31_2004_j / tot_2004_j)*1000),
                     htair_per_thous_log = log((htair_2004_j / tot_2004_j)*1000),
                     pop_gr_log = log(pop2004 / pop1994),
                     pop_log = log(pop2000),
                    percapita_log = log(percapita_2004),
                     manf_share_log = log(manf_2004_j / tot_2004_j)
                    )
# decending order
msa_core1 <- arrange(msa_core1, desc(tot_2001_j))
msa_core1 <- filter(msa_core1, ht_share_log != -Inf)

# select largest msas
msa_core1 <- msa_core1[1:381,]

head(msa_core1)
tail(msa_core1)

p <- ggplot(msa_core1, aes(x=totbea_gr_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p
p <- ggplot(msa_core1, aes(x=ht_share_ratio, y=ht_share_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p
p <- ggplot(msa_core1, aes(x=ht_share_ratio, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p
p <- ggplot(msa_core1, aes(x=ht_share_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p
p <- ggplot(msa_core1, aes(x=ht_per_thous, y=tot_gr)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p
p <- ggplot(msa_core1, aes(x=ht_per_thous_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p

p <- ggplot(msa_core1, aes(x=pop_gr_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p

p <- ggplot(msa_core1, aes(x=percapita_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p

p <- ggplot(msa_core1, aes(x=manf_share_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p


p <- ggplot(msa_core1, aes(x=htair_per_thous_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p

t <- apply(msa_core1[3:ncol(msa_core1)], 2, mean)
t

fit <- lm(tot_gr_log ~ htair_per_thous_log, msa_core1)
fit2 <- lm(tot_gr_log ~ htair_per_thous_log + log(pop2000) + pop_gr_log + manf_share_log, msa_core1)
fit3 <- lm(tot_gr_log ~ htair_per_thous_log + region, msa_core1)

summary(fit)
summary(fit2)
summary(fit3)


coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted <- fitted(fit) # predicted values
plot(x1, fitted)

resid <- residuals(fit) # residuals
plot(x1, resid)
plot(y, resid)
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit2)


write.csv(msa_core1, file="output_data/msa_core1.csv", row.names = TRUE)
