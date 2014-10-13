
library(dplyr)
library(ggplot2)

# to set up files be sure to put the data cells in excel into number format without commas
# also, remove and ' single quotes in the header row

fname <- "input_data/county_ht31_withoutex.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, cty = County, cty_name = County.Name, 
               ht31_2001_j = X2001.Jobs, 
               ht31_2004_j = X2004.Jobs, 
               ht31_2013_j = X2013.Jobs)
head(temp)
cty_core <- temp

fname <- "input_data/county_hospair_withoutex.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, cty = County, 
               htair_2001_j = X2001.Jobs, 
               htair_2004_j = X2004.Jobs, 
               htair_2013_j = X2013.Jobs)
head(temp)
cty_core <- merge(cty_core, temp, by = "cty")


fname <- "input_data/county_alltwodig_withoutex.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, cty = County, 
               tot_2001_j = X2001.Jobs, 
               tot_2004_j = X2004.Jobs, 
               tot_2013_j = X2013.Jobs)
cty_core <- merge(cty_core, temp, by = "cty")

fname <- "input_data/county_manf_withoutex.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, cty = County, 
               manf_2001_j = X2001.Jobs, 
               manf_2004_j = X2004.Jobs,
               manf_2013_j = X2013.Jobs)
cty_core <- merge(cty_core, temp, by = "cty")

fname <- "input_data/cty_pop.csv"
temp <- read.table(fname, sep=",",
                   header = TRUE, stringsAsFactors=FALSE)
temp <- select(temp, cty = GeoFips, city.name.bea = GeoName,
               pop1990 = X1990, 
               pop1994 = X1994,
               pop2004 = X2004,
               pop2000 = X2000, 
               pop2012 = X2012
)  
cty_core <- merge(cty_core, temp, by = "cty")



cty_core1 <- cty_core
cty_core1 <- mutate(cty_core1, 
                    tot_gr = ((tot_2013_j / tot_2004_j)-1),
                    tot_gr_log = log(tot_2013_j / tot_2004_j),
                    ht_share_log = log(ht31_2004_j / tot_2004_j),
                    ht_share_ratio = (ht31_2004_j / tot_2004_j),
                    ht_per_thous = ((ht31_2004_j / tot_2004_j)*1000),
                    ht_per_thous_log = log((ht31_2004_j / tot_2004_j)*1000),
                    htair_per_thous_log = log((htair_2004_j / tot_2004_j)*1000),
                    pop_gr_log = log(pop2004 / pop1994),
                    pop_log = log(pop2000),
                    #percapita_log = log(percapita_2004),
                    manf_share_log = log(manf_2004_j / tot_2004_j)
)
# decending order
cty_core1 <- arrange(cty_core1, desc(tot_2001_j))
cty_core1 <- filter(cty_core1, ht_share_log != -Inf)

# select largest
cty_core1 <- cty_core1[1:100,]

head(cty_core1)
tail(cty_core1)



p <- ggplot(cty_core1, aes(x=ht_per_thous_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p

p <- ggplot(cty_core1, aes(x=htair_per_thous_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p

p <- ggplot(cty_core1, aes(x=pop_gr_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p

p <- ggplot(cty_core1, aes(x=percapita_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p

p <- ggplot(cty_core1, aes(x=manf_share_log, y=tot_gr_log)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
p


fit <- lm(tot_gr_log ~ htair_per_thous_log, cty_core1)
fit2 <- lm(tot_gr_log ~ htair_per_thous_log +  pop_gr_log + manf_share_log, cty_core1)

summary(fit)
summary(fit2)

fitted <- fitted(fit) # predicted values
plot(cty_core1$tot_gr_log, fitted)

write.csv(cty_core1, file="output_data/cty_core1.csv", row.names = TRUE)
