# R Code for Clink et al. Argus calling
# Latest version (V2) updated by DJC 02/03/2020
# First version uploaded to GitHub 02/03/2020

# Load libraries
library(flextable)
library(dplyr)
library(plyr)
library(tidyr)
library(sp)
library(ggplot2)
library(gstat)
library(glmmTMB)
library(ggpubr)
library(jtools)
library(bbmle)
library(DHARMa)

## First load the data
# Load the dataset of all calling events that are not aggregated by time bin
argus.vocal.data.not.aggregated <-
  read.csv('argus.vocal.data.not.aggregated.csv')
head(argus.vocal.data.not.aggregated)

# Load the dataset of calling events that are aggregated by time bin
argus.vocal.data.aggregated <-
  read.csv('argus.vocal.data.aggregated.csv')
head(argus.vocal.data.aggregated)

data.for.call.timing.plot <- read.csv('argus.time.bin.df.csv')
head(data.for.call.timing.plot)

argus.call.type.offset <- read.csv('argus.call.type.df.csv')
head(argus.call.type.offset)

# Load dataset for call and response
call.diff.df <- read.csv('call.and.response.df.csv')

## Part I. Table to summarize calling events

# The following lines of code count the number of hours per recorder
total.hours.summary <-
  paste(
    argus.vocal.data.not.aggregated$date,
    argus.vocal.data.not.aggregated$time.bin.id,
    sep = '_'
  )

total.hours.recorder <-
  paste(
    argus.vocal.data.not.aggregated$recorder,
    argus.vocal.data.not.aggregated$date,
    argus.vocal.data.not.aggregated$time.bin.id,
    sep = '_'
  )

total.hours.summary <-
  cbind.data.frame(
    total.hours.recorder,
    total.hours.summary,
    argus.vocal.data.not.aggregated$recorder
  )

total.hours.summary <-
  distinct(total.hours.summary, total.hours.recorder, .keep_all = TRUE)

total.hours.summary$hours.count <- rep(6, nrow(total.hours.summary))

colnames(total.hours.summary) <-
  c('total.hours.recorder',
    "time.bin.comb",
    "recorder",
    "hours.count")

hours.summar.df <- aggregate(
  total.hours.summary$hours.count,
  by = list(recorder = total.hours.summary$recorder),
  FUN = sum
)

hours.summar.df <- rename(hours.summar.df, c("x" = "total.hours"))
hours.summar.df <- as.data.frame(hours.summar.df)
hours.summar.df$days <- round(hours.summar.df$total.hours / 24)
colnames(hours.summar.df) <-
  c("Recorder", "Total Hours", "Total Days")

hours.summar.df

# The following lines of code tally the different call types by recorder
argus.table <- argus.vocal.data.not.aggregated %>%
  group_by(recorder, call.type) %>%
  tally((n.calls))

argus.table <- as.data.frame(argus.table)

argus.table <- subset(argus.table, call.type != 'none')

argus.table <- argus.table %>% spread(call.type, n)

colnames(argus.table) = c("Recorder", "Long Calls", "Short Calls")

argus.table


# Now we combine both tables
combined.argus.call.table <-
  cbind.data.frame(argus.table, hours.summar.df[,-c(1)])
combined.argus.call.table

# The following lines of code calculate the values for the 'TOTALS' row in the table
total.short.calls <- sum(na.omit(argus.table$`Short Calls`))
total.long.calls <- sum(argus.table$`Long Calls`)
total.calls <-
  (argus.table$`Long Calls`) + ((argus.table$`Short Calls`))
total.all.calls <-
  sum(argus.table$`Long Calls`) + sum(na.omit (argus.table$`Short Calls`))
total.hours <- sum(combined.argus.call.table$`Total Hours`)
total.days <- sum(combined.argus.call.table$`Total Days`)
total.calls[6] <- argus.table$`Long Calls`[6]

# Now we combine the total values with the previous table
combined.argus.call.table <-
  cbind.data.frame(combined.argus.call.table, total.calls)

# Rename column 6
colnames(combined.argus.call.table)[6] <- 'Total Calls'

temp.df <-
  cbind.data.frame(
    "TOTALS",
    total.long.calls,
    total.short.calls,
    total.hours,
    total.days,
    total.all.calls
  )

colnames(temp.df)  <- colnames(combined.argus.call.table)

argus.table.all <-
  rbind.data.frame(combined.argus.call.table, temp.df)

# Replace the NA value with '~'
argus.table.all$`Short Calls`[is.na(argus.table.all$`Short Calls`)] <-
  "~"

argus.table.all <-
  argus.table.all[c("Recorder",
                    "Total Hours",
                    "Total Days",
                    "Short Calls",
                    "Long Calls",
                    "Total Calls")]

argus.table.all <- argus.table.all[, c(1, 3, 2, 6, 4, 5)]

# Use the 'flextable' function to format the table for publication
myft <- flextable((argus.table.all))
myft <- width(myft, width = 1)
myft <- bold(myft, part = "header")
myft

#save_as_docx(myft,path='Table2.docx')

## Part II. Call timing plot
# Check numbers of calls
# Number of short calls
sum(subset(data.for.call.timing.plot, call.type == "short.argus")$n.detections)

# Number of long calls
sum(subset(data.for.call.timing.plot, call.type == "long.argus")$n.detections)

# Create new dataframe with number of calls per time standardized
for.circadian <-
  droplevels(subset(data.for.call.timing.plot, call.type != 'none'))

# Check the total number of detections
sum((for.circadian$n.detections))

# Revalue factor names for table
for.circadian$call.type <-
  revalue(for.circadian$call.type,
          c("short.argus" = "Short Call", "long.argus" = "Long Call"))

# Convert date from Swift recorder format to month
for.circadian$month <-  substr(for.circadian$date, start = 5, stop = 6)


forbarplot <- aggregate(
  for.circadian$n.detections,
  by = list(time = for.circadian$time, call.type =
              for.circadian$call.type),
  FUN = sum
)

forbarplot$total.detections <- forbarplot$x

# Standardize by hours recorded
recording.hours.summary <- aggregate(
  data.for.call.timing.plot$recorder,
  by = list(by.hour = data.for.call.timing.plot$time),
  FUN = sum
)

# This tells R how many hours of recording time there was for each 2 hour bin on 24 hour clock
hours.rec <-
  c(585, 585, 585, 584, 589, 588, 587, 587, 587, 587, 584, 584)
time.vec <- seq(0, 22, 2)

adjusted.for.bar.plot <- data.frame()
for (b in 1:length(time.vec)) {
  subset.1 <- subset(forbarplot, time == time.vec[b])
  c <- time.vec[b] + 1
  subset.2 <-  subset(forbarplot, time == c)
  time.combined <- rbind.data.frame(subset.1, subset.2)
  time.combined$new.vals <-
    time.combined$total.detections / hours.rec[b]
  print(time.combined)
  adjusted.for.bar.plot <-
    rbind.data.frame(adjusted.for.bar.plot, time.combined)
}

# Code to make Figure 3
ggbarplot(
  adjusted.for.bar.plot,
  x = 'time',
  y = 'new.vals',
  lab.col = "white",
  facet.by = c('call.type'),
  nrow = 2,
  fill = 'gray41',
  xlim = c(0, 23)
) +
  theme_bw(base_size = 20) +
  #scale_fill_manual(values= viridis::viridis(2,begin=0.7))+
  xlab('Time (hours)') +
  ylab('Number of great argus calls \n (normalized)') + guides(fill = F) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.background = element_rect(fill = "white"))


## Part III. Model Selection

# Create an offset based on recording effort
argus.vocal.data.aggregated$date <-
  as.numeric(argus.vocal.data.aggregated$date)

argus.aggregate.off.set <- data.frame()

date.index <- unique(argus.vocal.data.aggregated$date)

for (b in 1:length(date.index)) {
  temp.subset <-
    subset(argus.vocal.data.aggregated, date == date.index[b])
  log.recorder.offset <- log(length(unique(temp.subset$recorder)))
  temp.subset <- cbind.data.frame(temp.subset, log.recorder.offset)
  argus.aggregate.off.set <-
    rbind.data.frame(argus.aggregate.off.set, temp.subset)
}

# Check number of argus calls in dataset before running models
argus.aggregate.off.set.check <-
  droplevels(subset(argus.aggregate.off.set, call.type != 'none'))
sum(argus.aggregate.off.set.check$n.calls) # Should be 2738 calls


# Convert presence of rain in calling period to a factor
argus.aggregate.off.set$rain.bin <-
  as.factor(argus.aggregate.off.set$rain.bin)

# Assign to new object
argus.count.cleaned <- argus.aggregate.off.set

# Model selection using count data
m.argusscomb.intercept <-
  glmmTMB(
    n.calls ~ time.bin.id + offset(log.recorder.offset) + (1 |
                                                             recorder / date),
    data = argus.count.cleaned,
    family = "nbinom2"
  )
m.argusscomb.1 <-
  glmmTMB(
    n.calls ~ precip.day.before + time.bin.id + offset(log.recorder.offset) + (1 |
                                                                                 recorder / date),
    data = argus.count.cleaned,
    family = "nbinom2"
  )
m.argusscomb.2 <-
  glmmTMB(
    n.calls ~ temp + time.bin.id + offset(log.recorder.offset) +  (1 |
                                                                     recorder / date),
    data = argus.count.cleaned,
    family = "nbinom2"
  )
m.argusscomb.3 <-
  glmmTMB(
    n.calls ~ precip.day.of + time.bin.id + offset(log.recorder.offset) + (1 |
                                                                             recorder / date),
    data = argus.count.cleaned,
    family = "nbinom2"
  )
m.argusscomb.4 <-
  glmmTMB(
    n.calls ~ tmin + time.bin.id + offset(log.recorder.offset) + (1 |
                                                                    recorder / date),
    data = argus.count.cleaned,
    family = "nbinom2"
  )
m.argusscomb.5 <-
  glmmTMB(
    n.calls ~ rain.bin + time.bin.id + offset(log.recorder.offset) +
      (1 | recorder / date),
    data = argus.count.cleaned,
    family = "nbinom2"
  )

# Compare models using AIC
bbmle::AICctab(
  m.argusscomb.intercept,
  m.argusscomb.1,
  m.argusscomb.2,
  m.argusscomb.3,
  m.argusscomb.4,
  m.argusscomb.5,
  weights = T
)

# Check summary of top model
anova(m.argusscomb.5, m.argusscomb.intercept)

# Evaluate performance using pseudo-R squared
MuMIn::r.squaredGLMM(m.argusscomb.5)

# Check for normality of residuals
testResiduals(m.argusscomb.5)

# Check for collinearity
performance::check_collinearity(m.argusscomb.5)


# Model selection using binary data
argus.binary.cleaned <- argus.count.cleaned

# Convert call counts to binary
argus.binary.cleaned$n.calls <-
  ifelse(argus.binary.cleaned$n.calls == 0 , 0, 1)

# Model selection using count data
m.argussbinary.intercept <-
  glmmTMB(n.calls ~ time.bin.id  + (1 | recorder / date),
          data = argus.binary.cleaned,
          family = "binomial")
m.argussbinary.1 <-
  glmmTMB(
    n.calls ~ precip.day.before + time.bin.id  + (1 | recorder / date),
    data = argus.binary.cleaned,
    family = "binomial"
  )
m.argussbinary.2 <-
  glmmTMB(n.calls ~ temp + time.bin.id +  (1 | recorder / date),
          data = argus.binary.cleaned,
          family = "binomial")
m.argussbinary.3 <-
  glmmTMB(
    n.calls ~ precip.day.of + time.bin.id  + (1 | recorder / date),
    data = argus.binary.cleaned,
    family = "binomial"
  )
m.argussbinary.4 <-
  glmmTMB(n.calls ~ tmin + time.bin.id  + (1 | recorder / date),
          data = argus.binary.cleaned,
          family = "binomial")

m.argussbinary.5 <- glmmTMB(
  n.calls ~ rain.bin + time.bin.id +
    (1 | recorder / date),
  data = argus.binary.cleaned,
  family = "binomial"
)


# Compare models using AIC
bbmle::AICctab(
  m.argussbinary.intercept,
  m.argussbinary.1,
  m.argussbinary.2,
  m.argussbinary.3,
  m.argussbinary.4,
  m.argussbinary.5,
  weights = T
)

# Check summary of top model
summary(m.argussbinary.5)

# Evaluate performance using pseudo-R squared
MuMIn::r.squaredGLMM(m.argussbinary.5)

# Although the KS test is significant then QQ-plot doesn't look too bad so we accept
testResiduals(m.argussbinary.5)

# Create table of model results for manuscript
jtools::export_summs(
  m.argussbinary.5,
  m.argussbinary.intercept,
  m.argusscomb.5,
  m.argusscomb.intercept,
  coefs = c(
    "Morning Calling Period" = "time.bin.idmorning" ,
    "Night Calling Period" = "time.bin.idmadrugada" ,
    "Evening Calling Period" = "time.bin.idevening",
    "Rain (binary)" = "rain.bin1"
  ),
  model.names =  c(
    "Binary Model Top",
    "Binary Model Intercept",
    "Count Model Top",
    "Count Model Intercept"
  ),
  stars = c(`***` = 0.05),
  scale = F,
  error_format = "[{conf.low}, {conf.high}]",
  to.file = "docx",
  file.name = "Table3.docx"
)

# Code to make Figure 4
model1Framebinary <-
  data.frame(
    Variable = rownames(summary(m.argussbinary.5)$coefficients$cond),
    Coefficient = (summary(m.argussbinary.5)$coefficients$cond[, 1]),
    SE = (summary(m.argussbinary.5)$coefficients$cond[, 2]),
    modelName = "Argus Model Binary"
  )
model1Framecount <-
  data.frame(
    Variable = rownames(summary(m.argusscomb.5)$coefficients$cond),
    Coefficient = (summary(m.argusscomb.5)$coefficients$cond[, 1]),
    SE = (summary(m.argusscomb.5)$coefficients$cond[, 2]),
    modelName = "Argus Model Count"
  )

allModelFrame <-
  data.frame(rbind(model1Framebinary[c(-1), ], model1Framecount[c(-1), ]))

# Specify the width of your confidence intervals
interval1 <- -qnorm((1 - 0.9) / 2)  # 90% multiplier
interval2 <- -qnorm((1 - 0.95) / 2)  # 95% multiplier

# Plot using ggplot
argusallcallsplot <- ggplot(allModelFrame, aes(colour = modelName))
argusallcallsplot <-
  argusallcallsplot + geom_hline(yintercept = 0,
                                 colour = gray(1 / 2),
                                 lty = 2)
argusallcallsplot <-
  argusallcallsplot + geom_linerange(
    aes(
      x = Variable,
      ymin = Coefficient - SE * interval1,
      ymax = Coefficient + SE *
        interval1
    ),
    lwd = 1,
    position = position_dodge(width = 1 / 2)
  )
argusallcallsplot <-
  argusallcallsplot + geom_pointrange(
    aes(
      x = Variable,
      y = Coefficient,
      ymin = Coefficient - SE * interval2,
      ymax = Coefficient + SE *
        interval2
    ),
    lwd = 1 / 2,
    position = position_dodge(width = 1 / 2),
    shape = 21,
    fill = "WHITE"
  )
argusallcallsplot <-
  argusallcallsplot + scale_color_manual(values = c("black", "red"))

argusallcallsplot <-
  argusallcallsplot + coord_flip() + theme_bw((base_size = 20))
argusallcallsplot <-
  argusallcallsplot + ggtitle("") + guides(legend = F) + theme(legend.title = element_blank())
argusallcallsplot <- argusallcallsplot  + scale_x_discrete(
  labels = c(
    "Rain during \n calling period",
    "Night \n calling period",
    "Evening \n calling period",
    "Morning \n calling period",
    "Maximum \n Temperature (C)"
  )
) + xlab('')

argusallcallsplot


### Argus lunar plots
argus.lunar.time.count <-
  droplevels(
    subset(
      argus.aggregate.off.set,
      time.bin.id == 'evening' | time.bin.id == 'madrugada'
    )
  )

argus.lunar.time.count$date <-
  as.numeric(argus.lunar.time.count$date)

argus.aggregate.time.count <- argus.lunar.time.count %>%
  group_by(
    recorder,
    date,
    humidity,
    temp,
    lunar.vals,
    tmin,
    rain.bin,
    precip.day.before,
    log.recorder.offset
  ) %>%
  tally(n.calls)


argus.aggregate.time.count$n.calls <- argus.aggregate.time.count$n

argus.aggregate.time.count <- argus.aggregate.time.count %>%
  dplyr::group_by(
    recorder,
    date,
    temp,
    lunar.vals,
    tmin,
    precip.day.before,
    n.calls,
    log.recorder.offset
  ) %>%
  dplyr::summarise(rain.bin = sum(as.numeric(as.character(rain.bin))))

argus.aggregate.time.count$rain.bin <-
  as.factor(argus.aggregate.time.count$rain.bin)

# Model selection code
m.argusslunar.intercept <-
  glmmTMB(
    n.calls ~ offset(log.recorder.offset) + (1 | recorder / date),
    data = argus.aggregate.time.count,
    family = "nbinom1"
  )

m.argusslunar.1 <-
  glmmTMB(
    n.calls ~ precip.day.before  + offset(log.recorder.offset) + (1 |
                                                                    recorder / date),
    data = argus.aggregate.time.count,
    family = "nbinom1"
  )

m.argusslunar.2 <-
  glmmTMB(
    n.calls ~ temp  + offset(log.recorder.offset) + (1 | recorder / date),
    data = argus.aggregate.time.count,
    family = "nbinom1"
  )

m.argusslunar.3 <-
  glmmTMB(
    n.calls ~ lunar.vals +  offset(log.recorder.offset) + (1 |
                                                             recorder / date),
    data = argus.aggregate.time.count,
    family = "nbinom1"
  )

m.argusslunar.4 <-
  glmmTMB(
    n.calls ~ tmin + offset(log.recorder.offset) + (1 | recorder / date),
    data = argus.aggregate.time.count,
    family = "nbinom1"
  )

m.argusslunar.5 <-
  glmmTMB(
    n.calls ~ tmin + lunar.vals + offset(log.recorder.offset) + (1 |
                                                                   recorder / date),
    data = argus.aggregate.time.count,
    family = "nbinom1"
  )

m.argusslunar.6 <-
  glmmTMB(
    n.calls ~  rain.bin + lunar.vals + offset(log.recorder.offset) + (1 |
                                                                        recorder / date),
    data = argus.aggregate.time.count,
    family = "nbinom1"
  )

# Model comparison
bbmle::AICctab(
  m.argusslunar.intercept,
  m.argusslunar.1,
  m.argusslunar.2,
  m.argusslunar.3,
  m.argusslunar.4,
  m.argusslunar.5,
  m.argusslunar.6,
  weights = T
)


# Calculate pseudo R-squared
MuMIn::r.squaredGLMM(m.argusslunar.6)


# Model diagnostics; outlier test significant but others look fine
testResiduals(m.argusslunar.6)

# Check collinearity
performance::check_collinearity(m.argusslunar.6)


## Model selection based on binary calls that happen only at night
argus.lunarbinary.time.bin <-
  droplevels(subset(
    argus.count.cleaned,
    time.bin.id == 'evening' |
      time.bin.id == 'madrugada'
  ))


argus.aggregate.time.bin <- argus.lunarbinary.time.bin %>%
  group_by(recorder,
           date,
           humidity,
           temp,
           lunar.vals,
           tmin,
           rain.bin,
           precip.day.before) %>%
  tally(n.calls)


argus.aggregate.time.bin$n.calls <-
  as.factor(ifelse(argus.aggregate.time.bin$n > 0 , 1, 0))

argus.aggregate.time.bin <- argus.aggregate.time.bin %>%
  dplyr::group_by(recorder,
                  date,
                  temp,
                  lunar.vals,
                  tmin,
                  precip.day.before,
                  n.calls) %>%
  dplyr::summarise(rain.bin = sum(as.numeric(as.character(rain.bin))))

argus.aggregate.time.bin$date <-
  as.numeric(argus.aggregate.time.bin$date)

argus.aggregate.time.bin$rain.bin <-
  as.factor(argus.aggregate.time.bin$rain.bin)


# Binary model selection for calls that happen at night 

m.argusslunarbinary.intercept <-
  glmmTMB(n.calls ~   (1 | recorder / date),
          data = argus.aggregate.time.bin,
          family = "binomial")

m.argusslunarbinary.1 <-
  glmmTMB(n.calls ~ precip.day.before  + (1 | recorder / date),
          data = argus.aggregate.time.bin,
          family = "binomial")

m.argusslunarbinary.2 <- glmmTMB(n.calls ~ temp + (1 | recorder / date),
                                 data = argus.aggregate.time.bin,
                                 family = "binomial")

m.argusslunarbinary.3 <-
  glmmTMB(n.calls ~ lunar.vals + (1 | recorder / date),
          data = argus.aggregate.time.bin,
          family = "binomial")

m.argusslunarbinary.4 <-
  glmmTMB(n.calls ~ tmin +  (1 | recorder / date),
          data = argus.aggregate.time.bin,
          family = "binomial")

m.argusslunarbinary.5 <-
  glmmTMB(n.calls ~ tmin + lunar.vals + (1 | recorder / date),
          data = argus.aggregate.time.bin,
          family = "binomial")

m.argusslunarbinary.6 <-
  glmmTMB(
    n.calls ~ rain.bin  + lunar.vals++(1 | recorder / date),
    data = argus.aggregate.time.bin,
    family = "binomial"
  )

bbmle::AICctab(
  m.argusslunarbinary.intercept,
  m.argusslunarbinary.1,
  m.argusslunarbinary.2,
  m.argusslunarbinary.3,
  m.argusslunarbinary.4,
  m.argusslunarbinary.5,
  m.argusslunarbinary.6,
  weights = T
)


# Calculate pseudo R-squared
MuMIn::r.squaredGLMM(m.argusslunarbinary.6)


# Model diagnostics
testResiduals(m.argusslunarbinary.6)

# Check collinearity
performance::check_collinearity(m.argusslunarbinary.6)


# Code for Figure 5a
argus.aggregate.time.count$rain.bin <-
  plyr::revalue(argus.aggregate.time.count$rain.bin, c('0' = "No", '1' = "Yes"))

night.error.plot <-
  ggerrorplot(
    data = argus.aggregate.time.count,
    x = 'rain.bin',
    y = 'n.calls',
    color = 'lunar.vals',
    shape = 'lunar.vals',
    desc_stat = "mean_se",
    add = 'mean',
    palette = gplots::rich.colors (4),
    size = 1
  ) +
  xlab('Rain during calling period') +
  ylab('Number of calls (mean ± SE)  \n in calling period') +
  theme_bw((base_size = 18)) + theme(legend.title = element_blank())



### Code for Figure 5b
model1Framelunarbinary <-
  data.frame(
    Variable = rownames(summary(m.argusslunarbinary.6)$coefficients$cond),
    Coefficient = (summary(m.argusslunarbinary.6)$coefficients$cond[, 1]),
    SE = (summary(m.argusslunarbinary.6)$coefficients$cond[, 2]),
    modelName = "Night calls \n (binary)"
  )

model1Framelunarcount <-
  data.frame(
    Variable = rownames(summary(m.argusslunar.6)$coefficients$cond),
    Coefficient = (summary(m.argusslunar.6)$coefficients$cond[, 1]),
    SE = (summary(m.argusslunar.6)$coefficients$cond[, 2]),
    modelName = "Night calls \n (count)"
  )

allModelFrame <-
  data.frame(rbind(model1Framelunarcount[c(-1), ], model1Framelunarbinary[c(-1), ]))


zplunar <- ggplot(allModelFrame, aes(colour = modelName))
zplunar <-
  zplunar + geom_hline(yintercept = 0,
                       colour = gray(1 / 2),
                       lty = 2)
zplunar <-
  zplunar + geom_linerange(
    aes(
      x = Variable,
      ymin = Coefficient - SE * interval1,
      ymax = Coefficient + SE * interval1
    ),
    lwd = 1,
    position = position_dodge(width = 1 / 2)
  )

zplunar <-
  zplunar + geom_pointrange(
    aes(
      x = Variable,
      y = Coefficient,
      ymin = Coefficient - SE * interval2,
      ymax = Coefficient + SE * interval2
    ),
    lwd = 1 / 2,
    position = position_dodge(width = 1 / 2),
    shape = 21,
    fill = "WHITE"
  )

zplunar <- zplunar + scale_color_manual(values = c("black", "red"))

zplunar <- zplunar + coord_flip()

zplunar <- zplunar +  guides(legend = F)

zplunar <- zplunar  + scale_x_discrete(labels = c("New \n moon",
                                                  "Waning \n moon",
                                                  "Waxing \n moon",
                                                  "Rain \n (binary)")) +
  xlab('') + theme_bw((base_size = 18)) + theme(legend.title = element_blank())

# Combine both plots to create Figure 5
cowplot::plot_grid(
  night.error.plot,
  zplunar,
  labels = c('A', 'B'),
  label_x = 0.9,
  label_size = 18
)

# Create a summary table
jtools::export_summs(
  m.argusslunarbinary.6,
  m.argusslunar.6,
  coefs = c(
    "Waning Moon" = 'lunar.valsWaning',
    "New Moon" = "lunar.valsNew",
    'Waxing Moon' = 'lunar.valsWaxing',
    "Rain (binary)" = "rain.bin1"
  ),
  model.names = c("Binary Model Top",
                  "Count Model Top"),
  stars = c(`***` = 0.05),
  scale = F,
  error_format = "[{conf.low}, {conf.high}]",
  to.file = "docx",
  file.name = "Table4lunarmodel.docx"
)

## Part V. Model Selection with different call types

# Model selection
m.argusscalltype.null <-
  glmmTMB(
    n.calls ~  time.bin.id   + offset(log.recorder.offset) + (1 |
                                                                recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )

m.argusscalltype.intercept <-
  glmmTMB(
    n.calls ~ call.type + time.bin.id   + offset(log.recorder.offset) + (1 |
                                                                           recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )

m.argusscalltype.1 <-
  glmmTMB(
    n.calls ~ call.type  + time.bin.id + precip.day.before + offset(log.recorder.offset) + (1 |
                                                                                              recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )

m.argusscalltype.2 <-
  glmmTMB(
    n.calls ~ call.type  + time.bin.id + temp + offset(log.recorder.offset) +  (1 |
                                                                                  recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )

m.argusscalltype.3 <-
  glmmTMB(
    n.calls ~ call.type + time.bin.id  + precip.day.of + offset(log.recorder.offset) + (1 |
                                                                                          recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )

m.argusscalltype.4 <-
  glmmTMB(
    n.calls ~ call.type + time.bin.id  + tmin + offset(log.recorder.offset) + (1 |
                                                                                 recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )

m.argusscalltype.5 <-
  glmmTMB(
    n.calls ~ rain.bin +  time.bin.id + offset(log.recorder.offset) + (1 |
                                                                         recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )

m.argusscalltype.6 <-
  glmmTMB(
    n.calls ~  rain.bin + call.type * time.bin.id + offset(log.recorder.offset) + (1 |
                                                                                     recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )

m.argusscalltype.7 <-
  glmmTMB(
    n.calls ~  call.type * time.bin.id + offset(log.recorder.offset) + (1 |
                                                                          recorder),
    data = argus.call.type.offset,
    family = "nbinom1"
  )


bbmle::AICctab(
  m.argusscalltype.null,
  m.argusscalltype.intercept,
  m.argusscalltype.1,
  m.argusscalltype.2,
  m.argusscalltype.3,
  m.argusscalltype.4,
  m.argusscalltype.5,
  m.argusscalltype.6,
  m.argusscalltype.7,
  weights = T
)


# Calculate pseudo R-squared
MuMIn::r.squaredGLMM(m.argusscalltype.7)

# Perform model diagnostics
testResiduals(m.argusscalltype.7)


# Code for Figure 6 (left)
model1Frame <-
  data.frame(
    Variable = rownames(summary(m.argusscalltype.7)$coefficients$cond),
    Coefficient = (summary(m.argusscalltype.7)$coefficients$cond[, 1]),
    SE = (summary(m.argusscalltype.7)$coefficients$cond[, 2]),
    modelName = "Argus Call Type"
  )

allModelFrame <- data.frame(rbind(model1Frame[-c(1), ]))

zpcalltype <- ggplot(allModelFrame, aes(colour = modelName))

zpcalltype <-
  zpcalltype + geom_hline(yintercept = 0,
                          colour = gray(1 / 2),
                          lty = 2)

zpcalltype <-
  zpcalltype + geom_linerange(
    aes(
      x = Variable,
      ymin = Coefficient - SE * interval1,
      ymax = Coefficient + SE *
        interval1
    ),
    lwd = 1,
    position = position_dodge(width = 1 / 2)
  )

zpcalltype <-
  zpcalltype + geom_pointrange(
    aes(
      x = Variable,
      y = Coefficient,
      ymin = Coefficient - SE * interval2,
      ymax = Coefficient + SE *
        interval2
    ),
    lwd = 1 / 2,
    position = position_dodge(width = 1 / 2),
    shape = 21,
    fill = "WHITE"
  )

zpcalltype <-
  zpcalltype + scale_color_manual(values = c("black", "red"))


zpcalltype <- zpcalltype + coord_flip() + theme_bw((base_size = 20))

zpcalltype <-
  zpcalltype + ggtitle("") + guides(colour = F, legend = F) + theme(legend.title = element_blank())

zpcalltype <-
  zpcalltype  + scale_x_discrete(
    labels = c(
      "Short Call",
      "Short Call* \n evening",
      "Short Call* \n night",
      "Short Call* \n morning",
      
      "Evening \n calling period",
      "Night \n calling period",
      "Morning \n calling period"
    )
  ) + xlab('')

print(zpcalltype)

# Code for Figure 6 (right)
myData <- aggregate(
  data.for.call.timing.plot$call.type,
  by = list(
    recorder = data.for.call.timing.plot$recorder,
    call.type = data.for.call.timing.plot$call.type,
    data.for.call.timing.plot$date,
    time.bin.id = data.for.call.timing.plot$time.bin.id
  ),
  FUN = function(x)
    c(n = length(x))
)
std <- function(x)
  sd(x) / sqrt(length(x))

myData <- subset(data.for.call.timing.plot, call.type != 'none')

myDataagg <- aggregate(
  myData$n.detections,
  by = list(
    call.type = myData$call.type,
    time.bin.id = myData$time.bin.id
  ),
  FUN = function(x)
    c(
      mean = mean(x),
      se = std(x),
      n = length(x)
    )
)

time_names <- c(
  'madrugada' = "Night (00:00 - 06:00)",
  'morning' = "Morning (06:00 - 12:00)",
  'afternoon' = "Afternoon (12:00 - 18:00)",
  'evening' = "Evening (18:00 - 23:59)"
)


myDataagg$call.type <-
  revalue(myDataagg$call.type,
          c("short.argus" = "Short Call", "long.argus" = "Long Call"))

call.type.bin.plot <-
  ggplot(myDataagg, aes(x = call.type, y = myDataagg$x[, 1], fill = call.type)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           alpha = 0.85) +
  scale_fill_manual(values = c('black', 'grey')) + # values=c(viridis::plasma(n=2,alpha=0.75))
  facet_wrap(time.bin.id ~ ., labeller = as_labeller(time_names)) +
  geom_errorbar(aes(
    ymin = myDataagg$x[, 1] - myDataagg$x[, 2],
    ymax = myDataagg$x[, 1] + myDataagg$x[, 2]
  ),
  width = 0.2) + theme_bw(base_size = 20) +
  ylab("Mean Number of Calls") + xlab("Call Type") + theme(legend.position = "none") +
  theme(strip.background = element_rect(fill = "white"))


# Combine bot plots fo Figure 6
cowplot::plot_grid(zpcalltype, call.type.bin.plot)


# Create table for AIC comparison

Ncalls <-
  bbmle::AICctab(
    m.argusscomb.intercept,
    m.argusscomb.1,
    m.argusscomb.2,
    m.argusscomb.3,
    m.argusscomb.4,
    m.argusscomb.5,
    weights = T,
    base = TRUE,
    logLik = TRUE
  )
class(Ncalls) <- 'data.frame'

BinCalls <-
  bbmle::AICctab(
    m.argussbinary.intercept,
    m.argussbinary.1,
    m.argussbinary.2,
    m.argussbinary.3,
    m.argussbinary.4,
    m.argussbinary.5,
    weights = T,
    base = TRUE,
    logLik = TRUE
  )
class(BinCalls) <- 'data.frame'

Ncalls.night <-
  bbmle::AICctab(
    m.argusslunar.intercept,
    m.argusslunar.1,
    m.argusslunar.2,
    m.argusslunar.3,
    m.argusslunar.4,
    m.argusslunar.5,
    m.argusslunar.6,
    weights = T,
    base = TRUE,
    logLik = TRUE
  )
class(Ncalls.night) <- 'data.frame'

BinCalls.night <-
  bbmle::AICctab(
    m.argusslunarbinary.intercept,
    m.argusslunarbinary.1,
    m.argusslunarbinary.2,
    m.argusslunarbinary.3,
    m.argusslunarbinary.4,
    m.argusslunarbinary.5,
    m.argusslunarbinary.6,
    weights = T,
    base = TRUE,
    logLik = TRUE
  )
class(BinCalls.night) <- 'data.frame'

CallType <-
  bbmle::AICctab(
    m.argusscalltype.null,
    m.argusscalltype.intercept,
    m.argusscalltype.1,
    m.argusscalltype.2,
    m.argusscalltype.3,
    m.argusscalltype.4,
    m.argusscalltype.5,
    m.argusscalltype.6,
    m.argusscalltype.7,
    weights = T,
    base = TRUE,
    logLik = TRUE
  )
class(CallType) <- 'data.frame'

all.aic.combined <- rbind.data.frame(BinCalls[1:3, ], Ncalls[1:3, ], BinCalls.night[c(1, 2, 7), ], Ncalls.night[c(1, 2, 7), ], CallType[c(1, 2, 5), ])

all.aic.combined <- round(all.aic.combined, 2)
all.aic.combined

all.aic.combined <-
  subset(all.aic.combined, select = -c(dLogLik, logLik))


model.names <- c(
  'Rain (binary) + calling period',
  'Intercept',
  'Rain previous day',
  'Rain (binary) + calling period',
  'Temperature (max) + calling period',
  'Intercept',
  'Rain (binary) + lunar phase',
  'Minimum temperature + lunar phase',
  'Intercept',
  'Rain (binary) + lunar phase',
  'Minimum temperature + lunar phase',
  'Intercept',
  'Call type * calling period',
  'Rain (binary) + lunar phase',
  'Intercept'
)

all.aic.combined <- cbind.data.frame(model.names, all.aic.combined)

# Code to create AIC table
myft <- flextable((all.aic.combined))
myft <- width(myft, width = 1)
myft <- bold(myft, part = "header")
myft <-
  set_header_labels(
    myft,
    model.names = 'Models',
    #logLik = "Log-likelihood",
    dAICc = "∆ AICc",
    df = 'DF',
    weight = 'Weight'
  )
myft

save_as_docx(myft, path = 'AICModelSelection1.docx')

# Part V. Call and Response Analysis

call.response <-
  call.diff.df[which(call.diff.df$call.diff.s < 1 &
                       call.diff.df$call.diff.s > -5), ]

nrow(call.response) /
  nrow(call.diff.df)


call.diff.df$call.response <-
  ifelse(call.diff.df$call.diff.s < 1 &
           call.diff.df$call.diff.s > -5,
         'Yes',
         'No')


call.diff.df <-
  call.diff.df %>% replace_na(list(call.response = 'No'))


rowSums(table(call.diff.df$call.response , call.diff.df$time.hours))


call.diff.df$time.hours <- as.factor(call.diff.df$time.hours)

call.diff.df$call.response.binary <-
  ifelse(call.diff.df$call.response == 'No', '0', '1')


call.diff.df$call.response.binary <-
  as.numeric(call.diff.df$call.response.binary)

call.diff.df$time.hours <- as.factor(call.diff.df$time.hours)

call.diff.df$Call.type <-
  revalue(call.diff.df$Call.type,
          c("short.argus" = "Short Call", "long.argus" = "Long Call"))

call.diff.df$Call.type <-
  factor(call.diff.df$Call.type , levels(call.diff.df$Call.type)[c(2, 1)])

call.response.null <-
  glmmTMB(call.response.binary ~ 1 +  (1 | recorder),
          data = call.diff.df,
          family = "binomial")

call.response.model <-
  glmmTMB(
    call.response.binary ~ 1 + (1 |
                                  recorder) + ar1(time.hours + 0 | recorder),
    data = call.diff.df,
    family = "binomial"
  )

call.response.call.type <-
  glmmTMB(
    call.response.binary ~ Call.type + (1 |
                                          recorder) + ar1(time.hours + 0 | recorder),
    data = call.diff.df,
    family = "binomial"
  )

call.response.call.type.only <-
  glmmTMB(call.response.binary ~ Call.type + (1 | recorder),
          data = call.diff.df,
          family = "binomial")

summary(call.response.call.type)

sjPlot::plot_model(call.response.call.type, type = 'eff')

AICctab(call.response.null, call.response.call.type.only, weights = T)
