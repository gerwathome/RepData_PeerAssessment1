# rep res wk2 proj 1 code
# this is to work things out before putting them in Rmd

rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(xtable)
library(moments)
#---------------80 columns------------------------------------------------------
## Loading and preprocessing the data
act <- read.csv(unz("activity.zip","activity.csv"),header=TRUE,
                stringsAsFactors = FALSE)
names(act)[1] <- "measured_steps"

## What is mean total number of steps taken per day?
act_by_date <-group_by(act,date)
# na.rm=FALSE is so there are not a large number of zero step days in the
#     histogram.  This gives a warning of "Removed 8 rows containing
#     non-finite values (stat_bin)." because there are 8 days with only NAs
steps_per_day <- summarise(act_by_date, sum(measured_steps, na.rm=FALSE))
names(steps_per_day) <- c("date", "measured_steps")

# 1. required histogram
ggh <- ggplot(steps_per_day,aes(measured_steps))
ggh <- ggh + geom_histogram(bins=30,col="green")
ggh


# 2. required mean and median
mean_steps_per_day <- mean(steps_per_day$measured_steps, na.rm = TRUE)
median_steps_per_day <- median(steps_per_day$measured_steps, na.rm = TRUE)


## What is the average daily activity pattern?
act_by_interval <- group_by(act, interval)
# Here the NAs need to be removed, or there are no non NA values
avg_steps_per_interval <- summarise(act_by_interval,
                                    mean(measured_steps,na.rm=TRUE))
names(avg_steps_per_interval) <- c("interval", "average_steps")

# 1. required time series plot
ggt <- ggplot(avg_steps_per_interval, aes(interval, average_steps))
ggt <- ggt + geom_line()
ggt

# required interval with max average steps
find_max <-avg_steps_per_interval[,2] ==
    max(avg_steps_per_interval[,2],na.rm=TRUE)
max_interval <- unlist(avg_steps_per_interval[find_max,1])
# interval 
#      835 
hrs <- max_interval %/% 100
min <- max_interval %% 100
time <- paste0(hrs, ":", min, " AM")

## Imputing missing values
# 1. required count NA's
count_na <- sum(is.na(act$measured_steps))
# [1] 2304

# 2.  required strategy for imputing values
# look at skewness to see if mean or median is the more appropriate statistic
skew_steps_per_interval <- summarise(act_by_interval,
                                     skewness(measured_steps,na.rm=TRUE))
names(skew_steps_per_interval) <- c("interval", "skewness")
min_skew <- min(skew_steps_per_interval[,2],na.rm=TRUE)
find_min_skew <-skew_steps_per_interval[,2] == min_skew
find_min_skew[is.na(find_min_skew)] <-  FALSE
min_interval <- unlist(skew_steps_per_interval[find_min_skew,1])

ggsk <- ggplot(skew_steps_per_interval, aes(interval,skewness))
ggsk <- ggsk + geom_line()
ggsk <- ggsk + geom_smooth()
ggsk <- ggsk + geom_point(x=min_interval,y=min_skew,pch=19,color="red")
ggsk <- ggsk + annotate("text", x=min_interval, y=min_skew, vjust=1,
                        label=paste("Minimum skewness is at the",
                                    "same interval as the maximum activity"))
ggsk

# nudge_y=0.0,

# compare median and mean as an imputed value
med_steps_per_interval <- summarise(act_by_interval,
                                    median(measured_steps,na.rm=TRUE))
names(med_steps_per_interval) <- c("interval", "median_steps")
steps_per_interval <- left_join(avg_steps_per_interval,med_steps_per_interval)

with(steps_per_interval,{
    plot(x = average_steps,
         y = median_steps,
         pch = 19,
         col = "red",
         main = paste("Comparison of Mean and Median",
                      "Steps per Interval")
    )
    abline(0,1)
    fudge <- 0.9
    text(x = fudge * range(median_steps)[2],
         y = fudge * range(median_steps)[2],
         pos = 4,
         labels = "Line where median steps = average steps"
    )
}
)
spi_narrow <- gather(data = steps_per_interval,
                     key = step_type, 
                     value = steps,
                     contains("step"))

ggspi <- ggplot(spi_narrow, aes(x = interval,
                                y = steps,
                                color = step_type))
ggspi <- ggspi + geom_line()
ggspi



ggs <- ggplot(spi_narrow, aes(x = steps,
                                         y = ..density..,
                                         fill=step_type))
ggs <- ggs + geom_histogram(color="black",
                            bins=40)
ggs <- ggs + facet_grid(step_type ~ . )
ggs

# The median values are much less than the average values.  It is unclear
#     which is most appropriate for the imputation of missing values.
#     Both approaches will be carried forward.

# 3. Create a new dataset that is equal to the original dataset but with the
#    missing data filled in.

# match the step value with the appropriate time interval
interval_index <- match(act$interval,steps_per_interval$interval)
act$median_steps <- unlist(steps_per_interval[interval_index,"median_steps"])
act$average_steps <- unlist(steps_per_interval[interval_index,"average_steps"])

# create an imputed step with the median values where step is NA
na_filter <- is.na(act$measured_steps)
act$med_imp_steps <- act$measured_steps
act$med_imp_steps[na_filter] <- act$median_steps[na_filter]

# create an imputed step with the average values where step is NA
act$avg_imp_steps <- act$measured_steps
act$avg_imp_steps[na_filter] <- act$average_steps[na_filter]

act <- select(act,-median_steps, -average_steps)

# did this work correctly?
with(act,{
    plot(x = measured_steps,
         y = med_imp_steps,
         type = "p",
         pch = 19,
         cex = 1.0,
         col = "red")
    points(x = measured_steps,
           y = avg_imp_steps,
           type = "p",
           pch = 19,
           cex = 0.5,
           col = "green")
}
)
summary(act)
# yes, they duplicate the measured steps, but have no NAs
# 4. Make a histogram of the total number of steps taken each day and Calculate
#    and report the mean and median total number of steps taken per day. Do
#    these values differ from the estimates from the first part of the assignment?
#    What is the impact of imputing missing data on the estimates of the total
#    daily number of steps?
act_narrow <- gather(act, key=step_type, value=steps, contains("step"))
act_nar_date_type <- group_by(act_narrow, date, step_type)
act_nar_date_type_sum <- summarise(act_nar_date_type, sum(steps, na.rm=FALSE))
names(act_nar_date_type_sum)[3] <- "steps"

ggp <- ggplot(act_nar_date_type_sum, aes(x = steps,
                                         y = ..density..,
                                         fill=step_type))
ggp <- ggp + geom_histogram(color="black",
                            bins=40)
ggp <- ggp + facet_grid(step_type ~ . )
ggp

mean_meas_spd <- mean(steps_per_day$measured_steps, na.rm = TRUE)
median_meas_spd <- median(steps_per_day$measured_steps, na.rm = TRUE)

act_nar_dts_by_step_type <- group_by(act_nar_date_type_sum,step_type)
act_nar_dts_median <- summarize(act_nar_dts_by_step_type,
                                median(steps, na.rm=TRUE))
act_nar_dts_mean <- summarize(act_nar_dts_by_step_type,
                                mean(steps, na.rm=TRUE))

comp_imp_methods <- left_join(act_nar_dts_median,act_nar_dts_mean)
names(comp_imp_methods) <- c("step_type", "median", "mean")

## Are there differences in activity patterns between weekdays and weekends?
# 1. Create a new factor variable in the dataset with two levels – “weekday”
#    and “weekend” indicating whether a given date is a weekday or weekend
#    day.

act$day_of_week <- weekdays(ymd(act$date))
act$weekdays <- factor("weekday", levels = c("weekday", "weekend"))
weekend_filter <- act$day_of_week == "Saturday" |
    act$day_of_week == "Sumday"
act$weekdays[weekend_filter] <- factor("weekend",
                                       levels = c("weekday", "weekend"))
# summary(act)
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the
#    5-minute interval (x-axis) and the average number of steps taken, averaged
#    across all weekday days or weekend days (y-axis).
act_nar <- gather(act, key=step_type, value=steps, contains("step"))
# summary(act_nar)
act_nar_by_int_stype <- group_by(act_nar,interval,step_type,weekdays)
act_nar_int_stype_avg <- summarize(act_nar_by_int_stype,
                                   mean(steps, na.rm=TRUE))
names(act_nar_int_stype_avg) <- c("interval", "step_type", "weekdays", "mean_steps")

ggts <- ggplot(act_nar_int_stype_avg, aes(x = interval,
                                         y = mean_steps,
                                         color=step_type))
ggts <- ggts + geom_line()
ggts <- ggts + facet_grid(. ~ weekdays)
ggts <- ggts + scale_colour_brewer(palette = "Dark2")
ggts

