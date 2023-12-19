getwd()

# load tidyverse package
library(tidyverse)

#load dataset 
var <- read.csv("variants_long_table.csv")

#explore dataset
head(var)
view(var)

## Check the dimension of the data
dim(var)

# Display the structure of your R object
str(var)

summary(var)

#transform dataframe into tibble
var_tb <- as_tibble(var)

# Compute a LOG2 transformation on the DP values

var_tb_log <- var_tb %>% mutate(DP_log2 = log2(DP))

select(var_tb_log, SAMPLE, REF, ALT, DP, DP_log2) %>% head()

#calculate mean DP value for each sample
var_tb %>% group_by(SAMPLE) %>% summarise(mean(DP))

var_tb %>% mutate(DP100 = DP/100) %>% select(SAMPLE, DP, DP100)

# Distribution of DP values per chromosome and per sample
ggplot(data = var_tb, aes(x=CHROM, y=DP, fill= SAMPLE)) + 
  geom_boxplot() + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu") + 
  labs(title="DP_per_Chromosome") + facet_grid(. ~ SAMPLE)

# Define a variable with plotting options
p_DP_CHROM <- ggplot(data = var_tb, aes(x=CHROM, y=DP, fill= SAMPLE)) + ylim(0,10000) + scale_fill_brewer(palette="RdYlBu") + labs(title="DP_per_Chromosome") + theme(legend.position="bottom")

# Test boxplots with faceting 
p_DP_CHROM + geom_boxplot() + facet_grid(. ~ SAMPLE)

# Combine violin plots and boxplots with faceting
p_DP_CHROM + geom_violin(trim=FALSE) + facet_grid(. ~ SAMPLE) + geom_boxplot(width=0.1)

# Count number of different effects per sample
p_EFFECT <- ggplot(data = var_tb, aes(y=EFFECT, fill= SAMPLE)) + scale_fill_brewer(palette="RdBu") + labs(title="Effect_per_Sample") + theme(legend.position="bottom")

p_EFFECT + geom_bar()

# Count the number of different effects

var_tb %>% count(EFFECT)

# Count the number of different effects and link them to sample information

var_tb %>% count(EFFECT, SAMPLE, sort = TRUE)

# Counting the effects per gene

var_tb %>% count(EFFECT, GENE, sort = TRUE)

# Filtering option 1 to select for effect on stop

filter(var_tb, EFFECT == "stop_lost" | EFFECT == "stop_gained")

# Filtering option 2 to select for effect on stop

filter(var_tb, EFFECT %in% c("stop_lost", "stop_gained"))

# Filtering on effect and selected columns

filter(var_tb, EFFECT %in% c("stop_lost", "stop_gained")) %>% select(SAMPLE, CHROM, GENE, EFFECT)

# Define your variable and plot

p_DP_POS <- ggplot(data = var_tb, aes(x=POS, y=DP, fill= SAMPLE)) + scale_fill_brewer(palette="RdBu") + labs(title="DP_per_Position") + theme(legend.position="bottom")

p_DP_POS + geom_point(shape = 21, size = 5, alpha = 0.7)

# plot of DP Vs Alt_DP
ggplot(data=var_tb, aes(x=ALT_DP, y=DP, fill=SAMPLE)) + 
  scale_fill_brewer(palette="RdBu") + geom_point(shape = 21, size = 5)

test <- ggplot(data=var_tb, aes(x=ALT_DP, y=DP, fill=SAMPLE)) + 
  scale_fill_brewer(palette="RdBu") + geom_point(shape = 21, size = 5)

#plot
test + labs(title = "DP_per_ALT_DP") + 
  theme(legend.position = "bottom")
