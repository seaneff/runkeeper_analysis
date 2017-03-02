#####################################################################
## set working directory and load libraries #########################
#####################################################################

setwd("~/Documents/workspace/runkeeper_analysis")
library(ranger)
library(tm)
library(slam)
library(ggplot2)
library(ggmap)
library(wordcloud)
library(XML)
library(plyr)
library(fpc)
library(scales)
library(dplyr)
library(sqldf)
library(lda_tuning)

#####################################################################
## set plotting parameters ##########################################
#####################################################################

color_scheme <- c("#335DEA", "#1D9BEA", "#6D6DFF", "#5B5BFF")

## save default plotting margins
mar_default <- par()$mar

#####################################################################
## define function to add a footnote to a figure ####################
#####################################################################

## code reference: http://statmodeling.com/best-way-to-add-a-footnote-to-a-plot-created-with-ggplot2.html

# write a simple function to add footnote
makeFootnote <- function(text = format(Sys.time(), "%d %b %Y"),
                         cex = 1, color = "black")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = text ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = cex, col = color))
  popViewport()
}

#####################################################################
## define function to break up dates ################################
## used for plotting dates on continuous color scale in figures below 
#####################################################################

## based on stackoverflow function
## http://stackoverflow.com/questions/5380417/is-there-a-way-of-manipulating-ggplot-scale-breaks-and-labels/5380817#5380817

dateBreaks <- function(x){
  breaks <- c(min(x), median(x), max(x))
  attr(breaks, "labels") <- as.Date(breaks, origin = "1970-01-01")
  names(breaks) <- attr(breaks, "labels")
  return(breaks)
}

####################################################################
## specify time range and location of data #########################
####################################################################

## name of subdirectory (must be in the data directory) containing GPX workout data
## and cardioActivities file
directory_name <- "runkeeper-data-export-41471231-2017-02-20-0155"

## time range is set manually in export, specify which one was used here
time_range <- "3/1/2015 - 2/19/2017"

race_dates <- c("2015-10-31", "2015-11-26", "2016-06-09", "2016-10-08",
                "2016-11-24")

#####################################################################
## load and format runkeeper data (non-gpx) #########################
#####################################################################

workout <- read.csv(paste("data/", directory_name, "/cardioActivities.csv", sep = ""))

## format selected variables
workout$Notes <- as.character(workout$Notes)
workout$GPX.File <- as.character(workout$GPX.File)
workout$Date <- as.POSIXct(workout$Date)

## just look at runs, not walks or hikes
run <- workout[which(workout$Type == "Running"),]

## exclude tough mudder, if it's included in data
if(any(run$Notes == "Tough Mudder in Portland Maine")){
  run <- run[-which(run$Notes == "Tough Mudder in Portland Maine"),]
}

## identify runs that were on the treadmill
## (I write "treadmill" in the comments section)
run$treadmill <- grepl("treadmill", run$Notes, ignore.case = TRUE)

## reformat pace and time of day as time
run$avg_pace <- as.POSIXct(run$Average.Pace, format = "%M:%S")
run$time_of_day <- format(as.POSIXct(run$Date), "%H:%M:%S")

## identify whether a run was in the morning or afternoon/evening
run$morning_afternoon <- factor(ifelse(run$time_of_day < "12:00:00", "morning", "afternoon/evening"),
                                levels = c("morning", "afternoon/evening"))

## identify run month and year
run$date_formatted <-  as.Date(gsub( " .*$", "", run$Date))
run$year <- as.numeric(gsub( "-.*$", "", run$Date))
run$month <- months(run$date_formatted, abbreviate = FALSE)

## identify runs that were races
run$race <- ifelse(as.character(run$date_formatted) %in% race_dates, TRUE, FALSE)

## identify day of the week, treat is as a factor 
run$weekday <- factor(weekdays(run$date_formatted),
                      levels = c("Monday", "Tuesday", "Wednesday",
                                 "Thursday", "Friday", "Saturday", "Sunday"))

## reformat some annoyingly named features
names(run)[which(names(run) == "Distance..mi.")] <- "distance_mi"
names(run)[which(names(run) == "Climb..ft.")] <- "climb_ft"

## number of runs in the past 7, 14, and 30 days
run_counts <- sqldf("select r1.date_formatted as date__Date, 
              count(distinct r2.date_formatted) as run_count_past_7_days,
              sum(case when r2.date_formatted is not null then r2.distance_mi else 0 end) as run_length_past_7_days,
              count(distinct r3.date_formatted) as run_count_past_14_days,
              sum(case when r3.date_formatted is not null then r3.distance_mi else 0 end) as run_length_past_14_days,
              count(distinct r4.date_formatted) as run_count_past_30_days,
              sum(case when r4.date_formatted is not null then r4.distance_mi else 0 end) as run_length_past_30_days
            from run as r1
            left join run as r2
                 on (r1.date_formatted - r2.date_formatted < 7 and
                 r1.date_formatted - r2.date_formatted > 0)
            left join run as r3
                 on (r1.date_formatted - r3.date_formatted < 14 and
                 r1.date_formatted - r3.date_formatted > 0)
            left join run as r4
                 on (r1.date_formatted - r4.date_formatted < 30 and
                 r1.date_formatted - r4.date_formatted > 0)
            group by r1.date_formatted",
            method = "name__class")

run <- merge(run, run_counts,
              all.x = TRUE,
              by.x = "date_formatted", by.y = "date")

#####################################################################
## load and format gpx data #########################################
#####################################################################

## thanks to Daniel Cook and Holger Brandl for providing code on git
## reference: http://www.danielecook.com/how-to-plot-all-of-your-runkeeper-data/
## reference: https://gist.github.com/holgerbrandl/5595165

for (i in 1:nrow(run)) {

  gpx_file <- run$GPX.File[i]
  print(paste("reading in", gpx_file, "(", i, "of", prettyNum(nrow(run), big.mark = ","), ")"))
  
  if(gpx_file != ""){
  curr_route <- xmlParse(paste("data/", directory_name, "/", gpx_file, sep = ""), useInternalNodes = TRUE)
  route_df <- ldply(xpathSApply(curr_route, "//*[local-name()='trkpt']"), 
                    function(x) unlist(xmlToList(x)))
  colnames(route_df) <- c("elevation", "datetime", "lat", "lon")
  trackdata <- data.frame(datetime = route_df$datetime, 
                          colwise(as.numeric, .(elevation, lat, lon))(route_df))
  trackdata$run_id <- i
  if(i == 1){ full_runs <- trackdata }
  if(i > 1) {full_runs <- rbind.data.frame(full_runs, trackdata)}
  }
}

#####################################################################
## Distribution of run length #######################################
#####################################################################

pdf("results/run_length.pdf", height = 4, width = 5)
par(mar = mar_default + c(2, 0, 0, 0))
hist(run$distance_mi,
     ylab = "Number of Runs",
     xlab = "Distance (in miles)",
     col = "#3a33a3",
     main = "Overall Run Distance",
     breaks = 20,
     xlim = c(0, 14),
     ylim = c(0, length(run$distance_mi)/4))
box()
legend("topright",
       legend = c(paste("median:", round(median(run$distance_mi), 1), "miles"),
                  paste("IQR: [",  round(quantile(run$distance_mi, .25), 1),
                        ", ", 
                        round(quantile(run$distance_mi, .75), 1), "] miles",
                        sep = "")),
       bg = "gray90")
makeFootnote(paste("Based on RunKeeper Run Data\nDate Range:", time_range),
             cex = 0.6)
dev.off()

#####################################################################
## Distribution of run length by time of day ########################
#####################################################################

pdf("results/run_length_by_time_of_day.pdf", height = 4, width = 4)
ggplot(run, aes(distance_mi)) + 
  ggtitle("Distribution of Run Length by Time of Day") +
  geom_histogram(fill = "dark blue", col = "gray50",  binwidth = 1) +
  xlab("Distance (in miles)") +
  ylab("Number of Runs") +
  facet_wrap(~ morning_afternoon, nrow = 2, scales = "free_y")
dev.off()

#####################################################################
## Distribution of run length by day of week ########################
#####################################################################

pdf("results/run_length_by_day_of_week.pdf", height = 8, width = 4)
ggplot(run, aes(distance_mi)) + 
  ggtitle("Distribution of Run Length by Time of Day") +
  geom_histogram(fill = "dark blue", col = "gray50",  binwidth = 1) +
  xlab("Distance (in miles)") +
  ylab("Number of Runs") +
  facet_wrap(~ weekday, nrow = 7, scales = "free_y") +
  scale_y_continuous(breaks = pretty_breaks()) ## only integers on y axis
dev.off()
                     
#####################################################################
## Pace vs. Distance ################################################
#####################################################################

pdf("results/pace_vs_distance.pdf", height = 4, width = 6)

## general pace vs. distance
par(mar = mar_default + c(2, 2, 0, 0))
plot(run$distance_mi, run$avg_pace,
     xlab = "Miles per Run",
     ylab = "Average Pace per Run\n",
     main = "Pace vs. Distance",
     col = "#3a33a3", las = 1)
makeFootnote(paste("Based on RunKeeper Run Data\nDate Range:", time_range), cex = 0.6)

## pace vs. distance 
ggplot(run, aes(x = factor(round(distance_mi), levels = 0:max(run$distance_mi)),
                y = avg_pace)) +
  geom_boxplot(alpha = 0.8, col = "dark blue", fill = "light blue") +
  xlab("Miles per Run") + 
  ylab("Average Pace per Run") +
  ggtitle("Pace vs. Distance") + 
  scale_y_datetime(labels = date_format("%M:%S")) +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

## pace vs. distance by treadmill or not treadmill
ggplot(run, aes(distance_mi, avg_pace, col = treadmill)) + 
  geom_point(size = 2, alpha = 0.8) +
  xlab("Miles per Run") + 
  ylab("Average Pace per Run") +
  ggtitle("Pace vs. Distance") + 
  scale_color_manual(name = "Treadmill", values = c("#2b8cbe", "#e7298a")) +
  scale_y_datetime(labels = date_format("%M:%S")) +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

## pace vs. distance by year
ggplot(run, aes(distance_mi, avg_pace, col = as.factor(year))) + 
  geom_point(size = 2, alpha = 0.8) +
  xlab("Miles per Run") + 
  ylab("Average Pace per Run") +
  ggtitle("Pace vs. Distance") + 
  scale_color_discrete(name = "Year") +
  scale_y_datetime(labels = date_format("%M:%S")) +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

## pace vs. distance by year
ggplot(run, aes(x = factor(round(distance_mi), levels = 0:max(run$distance_mi)),
                y = avg_pace)) +
  geom_boxplot(alpha = 0.8, col = "dark blue", fill = "light blue") +
  xlab("Miles per Run") + 
  ylab("Average Pace per Run") +
  ggtitle("Pace vs. Distance") + 
  scale_y_datetime(labels = date_format("%M:%S")) +
  facet_wrap(~ year) + 
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

## pace vs. distance by date
ggplot(run, aes(distance_mi, avg_pace, col = as.integer(date_formatted))) + 
  geom_point(size = 2, alpha = 0.8) +
  xlab("Miles per Run") + 
  ylab("Average Pace per Run") +
  ggtitle("Pace vs. Distance") + 
  scale_color_continuous(name = "Date", breaks = dateBreaks) +
  scale_y_datetime(labels = date_format("%M:%S")) +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

## pace vs. distance by time of day
ggplot(run, aes(distance_mi, avg_pace, col = morning_afternoon)) + 
  geom_point(size = 2, alpha = 0.6) +
  xlab("Miles per Run") + 
  ylab("Average Pace per Run") +
  ggtitle("Pace vs. Distance") + 
  scale_color_discrete(name = "Time of Day") +
  scale_y_datetime(labels = date_format("%M:%S")) +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure
dev.off()

#############################################################################
## identify clusters of runs based on location ##############################
#############################################################################

## code based on code originally written by Daniel Cook
## reference: http://www.danielecook.com/how-to-plot-all-of-your-runkeeper-data/

# Partitioning around medoids 
clusters <- pamk(full_runs[,c("lat", "lon")], 
                 krange = 1:15, 
                 diss = TRUE, usepam = FALSE)$pamobject$medoids

#############################################################################
## plot runs for each primary location identified by clustering (above) #####
#############################################################################

## code based on code originally written by Eric C. Anderson for (NOAA/SWFSC) for a Reproducable Research Course
## http://eriqande.github.io/rep-res-web/
## reference: http://www.danielecook.com/how-to-plot-all-of-your-runkeeper-data/
## reference: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

## save pdf
pdf("results/maps.pdf", height = 5, width = 5)

for(i in 1:nrow(clusters)){

## pull roadmap from API
map_roadmap <- get_map(location = c(clusters[i,2], clusters[i,1]), 
                maptype = "roadmap", source = "google",
                zoom = 11, color = "bw")


## plot map with tracks (roadmap)
r <- ggmap(map_roadmap, extent = "device") +
  theme(axis.line = element_line(color = NA)) + 
  xlab("") + ylab("") 

for( j in unique(full_runs$run_id)) {
  r <- r + geom_path(data = full_runs[which(full_runs$run_id == j),], 
                     col = "dark blue", size = 0.8, lineend = "round", alpha = 0.2,
                     col = "bw")
}

print(r)

}

dev.off()

################################################################################
## predict average pace based on distance, time of day, and elevation/climb ####
################################################################################

set.seed(1202)
cv_index <- sample(1:10, nrow(run), replace = TRUE)
counter <- 0

mtry.range <- 2:5
fold.cv <- 10
min.node.size.range <- 1:4

for(min.node.size in min.node.size.range){
  for(mtry in mtry.range){
    print(paste("fitting model with mtry =", mtry.range, 
                "and minimum node size =", min.node.size))
    for(i in 1:fold.cv){
      ## eventually want to choose parameters based on cross-validation
      pace_by_time_rf <- ranger(Average.Speed..mph. ~ distance_mi + morning_afternoon + 
                            climb_ft + year + month + treadmill + race +
                            run_count_past_7_days +
                            run_count_past_14_days +
                            run_count_past_30_days, 
                            data = run[-which(cv_index == i),
                                       which(names(run) %in% c(
                                         "Average.Speed..mph.",
                                         "distance_mi",
                                         "morning_afternoon",
                                         "climb_ft",
                                         "year",
                                         "month",
                                         "treadmill",
                                         "race",
                                         "run_count_past_7_days",
                                         "run_count_past_14_days",
                                         "run_count_past_30_days"))],
                            num.trees = 500, min.node.size = min.node.size, mtry = mtry, 
                            importance = "impurity",
                            write.forest = TRUE)
  
  if(counter == 0){
    
    results <- data.frame(
                    min.node.size = min.node.size,
                    mtry = mtry,
                    holdout_group = rep(i, sum(cv_index == i)),
                    observed = run[which(cv_index == i),]$Average.Speed..mph.,
                    predicted = predict(pace_by_time_rf, data = run[which(cv_index == i),])$predictions)
    
    } else {
    
    results <- rbind.data.frame(results,
                    data.frame(min.node.size = min.node.size,
                    mtry = mtry,
                    data.frame(holdout_group = rep(i, sum(cv_index == i)),
                    observed = run[which(cv_index == i),]$Average.Speed..mph.,
                    predicted = predict(pace_by_time_rf, data = run[which(cv_index == i),])$predictions)))
    }
      counter <- counter + 1
    }
  }
}

################################################################################
## evaluate parameter selection ################################################
################################################################################

performance_iter <- summarize(group_by(results, min.node.size, mtry, holdout_group),
                              rmse = sqrt(mean((observed - predicted)^2)))

performance <- summarize(group_by(performance_iter, min.node.size, mtry),
                         mean_rmse = mean(rmse),
                         median_rmse = median(rmse))

pdf("results/model_parameter_selection.pdf", height = 4, width = 6)
ggplot(data = performance_iter, aes(x = mtry, y = rmse, 
                          group = interaction(min.node.size, mtry),
                          col = factor(min.node.size))) + 
  geom_boxplot(fill = "white", alpha = 0.8) +
  scale_shape_manual(values = c(22,21)) +
  ylab("Root Mean Squared Error") +
  xlab("mtry") +
  ggtitle("Random Forest Parameter Selection\n(all iterations)") +
  scale_color_discrete(name = "minimum node size") +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

ggplot(data = performance, aes(x = mtry, y = median_rmse, 
                               group = min.node.size,
                               col = factor(min.node.size))) + 
  geom_line(size = 1.5) + 
  geom_point(size = 3, fill = "white") +
  scale_shape_manual(values = c(22,21)) +
  ylab("Median of Root Mean Squared Error") +
  xlab("mtry") +
  ggtitle("Random Forest Parameter Selection\n(median of tenfold CV)") +
  scale_color_discrete(name = "minimum node size") +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

ggplot(data = performance, aes(x = mtry, y = mean_rmse, 
                               group = min.node.size,
                               col = factor(min.node.size))) + 
  geom_line(size = 1.5) + 
  geom_point(size = 3, fill = "white") +
  scale_shape_manual(values = c(22,21)) +
  ylab("Mean of Root Mean Squared Error") +
  xlab("mtry") +
  ggtitle("Random Forest Parameter Selection\n(mean of tenfold CV)") +
  scale_color_discrete(name = "minimum node size") +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

dev.off()

################################################################################
## choose random forest parameters based on cross-validation results ###########
################################################################################

best_performance <- performance[which(performance$median_rmse == min(performance$median_rmse)),]

################################################################################
## selected final model: full ##################################################
################################################################################

pace_by_time_rf <- ranger(Average.Speed..mph. ~ distance_mi + morning_afternoon + 
                            Climb..ft. + year + month + treadmill + race +
                            run_count_past_7_days +
                            run_count_past_14_days +
                            run_count_past_30_days,
                          data = run[-which(cv_index == i),
                                     which(names(run) %in% c(
                                       "Average.Speed..mph.",
                                       "distance_mi",
                                       "morning_afternoon",
                                       "Climb..ft.",
                                       "year",
                                       "month",
                                       "treadmill",
                                       "race",
                                       "run_count_past_7_days",
                                       "run_count_past_14_days",
                                       "run_count_past_30_days"))], 
                          num.trees = 500, 
                          min.node.size = best_performance$min.node.size, 
                          mtry = best_performance$mtry, 
                          importance = "impurity",
                          write.forest = TRUE)

## examine variable importance
sort(importance(pace_by_time_rf))

##############################################################
## calculate residual for each random forest prediction ######
##############################################################

## residual = observed - predicted
## negative residuals = ran slower than expected (prediction > actual)
## positive residuals = ran faster than expected (prediction < actual)

run$residual <- run$Average.Speed..mph. - predict(pace_by_time_rf, run)$predictions 
  
## evaluate model fit
model_fit <- cbind.data.frame(actual = run$Average.Speed..mph.,
                              predicted = predict(pace_by_time_rf, run)$predictions,
                              race = run$race)

pdf("results/model_fit_itself.pdf", height = 3, width = 5)
ggplot(model_fit, aes(actual, predicted)) + 
  geom_point(size = 2, alpha = 0.3, col = "#e7298a") +
  xlab("Actual Miles per Hour") + 
  ylab("Predicted Miles per Hour") +
  ggtitle("Predicted vs. Actual Miles per Hour") +
  xlim(c(3, 10)) +
  ylim(c(3, 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted")

ggplot(model_fit, aes(actual, predicted, col = race)) + 
  geom_point(size = 2, alpha = 0.3) +
  xlab("Actual Miles per Hour") + 
  ylab("Predicted Miles per Hour") +
  ggtitle("Predicted vs. Actual Miles per Hour") +
  xlim(c(3, 10)) +
  ylim(c(3, 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted")
dev.off()

pdf("results/model_fit_cross_validated.pdf", height = 3, width = 5)
ggplot(results[which(results$min.node.size == best_performance$min.node.size &
                     results$mtry == best_performance$mtry),], 
       aes(observed, predicted)) + 
  geom_point(size = 2, alpha = 0.3, col = "#e7298a") +
  xlab("Actual Miles per Hour") + 
  ylab("Predicted Miles per Hour") +
  ggtitle("Predicted vs. Actual Miles per Hour\n(Cross-Validation Results)") +
  xlim(c(3, 10)) +
  ylim(c(3, 10)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted")
dev.off()

################################################
## process free text ###########################
################################################

## format data as corpus
run_text <- Corpus(VectorSource(run$Notes))

## remove SMART stopwords: http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop
run_text_processed <- tm_map(run_text, removeWords, stopwords("SMART"))

## remove punctuation
run_text_processed <- tm_map(run_text_processed, removePunctuation)

## generate document term matrix from processed text data
dt_initial <- as.matrix(DocumentTermMatrix(run_text_processed))

## there is a bug in removeWords that causes stopwords
## not to be removed if they're the first word in a piece of text
dt <- dt_initial[,-which(colnames(dt_initial) %in% stopwords("SMART"))]

################################################
## basic word cloud ############################
################################################

pdf("results/note_wordcloud.pdf", height = 5, width = 6)
set.seed(1203)
wordcloud(colnames(dt),
          col_sums(dt),
          col = "#3a33a3")
dev.off()

################################################
## distance-related word cloud #################
################################################

## median distance of all runs
median_distance <- median(run$distance_mi)

## for each run, identify whether run distance is above or below median distance
run$long_binary <- ifelse(run$distance_mi > median_distance, "longer", "shorter")

## count the number of times each word is used in long runs and in sort runs
## note that a word will only be counted once per post/note, even if it's used twice in that post/note
long_counts <- as.vector(rollup(dt[which(run$long_binary == "longer"),], 1, FUN = function(x) sum(ifelse(x > 0, 1, 0))))
short_counts <- as.vector(rollup(dt[which(run$long_binary == "shorter"),], 1, FUN = function(x) sum(ifelse(x > 0, 1, 0))))

## reformat names to keep track of words associated with each count
names(long_counts) <- colnames(dt); names(short_counts) <- colnames(dt)

## create a dataframe with one row per word in the corpus
## and one column that counts the number of posts for long runs that used that word,
## and one column that counts the number of posts for short runs that used that word
length_level <- cbind.data.frame(long = long_counts, short = short_counts)

## count the total number of posts in which a word was used (in both long and short runs)
length_level$total_count <- apply(length_level, 1, sum)

## calculate probability that each word shows up in a note for a long run and a short run
length_level$p_long <- length_level$long / sum(run$long_binary == "longer")
length_level$p_short <- length_level$short / sum(run$long_binary == "shorter")

## calculate relative risk and log relative risk
## add 0.001 to avoid dividing by zero or taking the log of 0
length_level$rr <- (length_level$p_long + 0.001) / (length_level$p_short + 0.001)
length_level$log_rr <- log((length_level$p_long + 0.001) / (length_level$p_short + 0.001)) 

## calculate x axis position:
## add a bit of a jitter (add random noise along the x axis to avoid overlapping words)
plotting_randomness_factor <- 1.2
set.seed(1202)
length_level$x <- length_level$log_rr + rnorm(mean = 0, sd = plotting_randomness_factor, n = nrow(length_level))

## calculate y axis position:
## randomly
set.seed(0311)
length_level$y <- runif(min = 1, max = 10, n = nrow(length_level))

## specify x plotting limits
plotting_limit <- 1.1*max(c(abs(min(length_level$x)), abs(max(length_level$x)))) 

## calculate alpha based on the total number of times a word was used
## originally used minmax, but then decided to adjust to the log scale to make differences slightly less pronounced
## make sure every word has an alpha of at least 0.1 (see adjustment below)
## since for now, I want every word to be plotted

length_level$alpha <-  (log(length_level$total_count) - min(log(length_level$total_count)))/
  max(log(length_level$total_count))

length_level$alpha_updated <- ifelse(length_level$alpha > 0.1, length_level$alpha, 0.1)

## generate figure
pdf("results/positioned_wordcloud_distance.pdf", height = 5, width = 9)
ggplot(length_level, aes(x = x , y = y)) + 
  geom_text(aes(size = total_count,
                label = row.names(length_level),
                colour = log_rr, alpha = alpha_updated)) +
  scale_size(range = c(3, 5), name = "Number of Runs\nwith Notes\nMentioning Word") +
  scale_color_gradient(low = "#8E0045", high = "#0A4877", guide = "none") +  
  scale_x_continuous(breaks = c(-1*plotting_limit, 0, plotting_limit),
                     limits = c(-1.2*plotting_limit, 1.2*plotting_limit),
                     labels = c(paste("Said More after\nShort Runs\n(runs less than", round(median_distance, 1), "miles)"),
                                "Said Equally after\nShort and Long Runs",
                                paste("Said More after\nLong Runs\n(runs more than",  round(median_distance, 1), "miles)"))) +
  scale_y_continuous(breaks=NULL) + 
  ggtitle("Relaxing vs. Strugglefest:\nWord use After Shorter vs. Longer Runkeeper Runs") +
  guides(alpha = FALSE) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

dev.off()

################################################
## distance and speed word cloud ###############
################################################

## use the residuals of the random forest (fit above) of pace ~ distance + climb + time of day
## to see if a run is faster than expected for its distance or slower than expected for its distance, climb, and time of day
## then count the number of times each word is used in fast runs and in slow runs (adjusted for distance)
## note that a word will only be counted once per post/note, even if it's used twice in that post/note

## residual = observed - predicted
## negative residuals = ran slower than expected (prediction > actual)
## positive residuals = ran faster than expected (prediction < actual)

slow_counts <- as.vector(rollup(dt[which(run$resid <= 0),], 1, FUN = function(x) sum(ifelse(x > 0, 1, 0))))
fast_counts <- as.vector(rollup(dt[which(run$resid > 0),], 1, FUN = function(x) sum(ifelse(x > 0, 1, 0))))

## reformat names to keep track of words associated with each count
names(slow_counts) <- colnames(dt); names(fast_counts) <- colnames(dt)

## create a dataframe with one row per word in the corpus
## and one column that counts the number of posts for rast runs that used that word,
## and one column that counts the number of posts for slow runs that used that word
pace_level <- cbind.data.frame(fast = fast_counts, slow = slow_counts)

## count the total number of posts in which a word was used (in both long and short runs)
pace_level$total_count <- apply(pace_level, 1, sum)

## calculate probability that each word shows up in a note for a long run and a short run
pace_level$p_fast <- pace_level$fast / sum(run$residual > 0)
pace_level$p_slow <- pace_level$slow / sum(run$residual <= 0)

## calculate relative risk and log relative risk
## add 0.001 to avoid dividing by zero or taking the log of 0
pace_level$rr <- (pace_level$p_fast + 0.001) / (pace_level$p_slow + 0.001)
pace_level$log_rr <- log((pace_level$p_fast + 0.001) / (pace_level$p_slow + 0.001)) 

## calculate x axis position based on length-based relative risk:
## add a bit of a jitter (add random noise along the x axis to avoid overlapping words)
plotting_randomness_factor_x <- 1.2
set.seed(1202)
length_level$x <- length_level$log_rr + rnorm(mean = 0, sd = plotting_randomness_factor_x, n = nrow(length_level))

## calculate y axis position based on pace-based relative risk:
## add a bit of a jitter (add random noise along the y axis to avoid overlapping words)
plotting_randomness_factor_y <- 1.2
set.seed(0311)
length_level$y <- pace_level$log_rr + rnorm(mean = 0, sd = plotting_randomness_factor_y, n = nrow(pace_level))

## calculate alpha based on the total number of times a word was used
## see additional notes above 
length_level$alpha <-  (log(length_level$total_count) - min(log(length_level$total_count)))/
                          max(log(length_level$total_count))
length_level$alpha_updated <- ifelse(length_level$alpha > 0.1, length_level$alpha, 0.1)

## specify x plotting limits
plotting_limit_x <- 1.1*max(c(abs(min(length_level$x)), abs(max(length_level$x)))) 

## specify y plotting limits
plotting_limit_y <- 1.1*max(c(abs(min(length_level$y)), abs(length_level$y))) 

min_word_count <- 3

## generate figure
pdf("results/positioned_wordcloud_distance_pace.pdf", height = 5, width = 9)
## only words that were used in at least X (where x = min_word_count) runs
ggplot(length_level[which(length_level$total_count >= min_word_count),], 
       aes(x = x , y = y, alpha = alpha_updated)) + 
  geom_text(aes(size = total_count,
                label = row.names(length_level[which(length_level$total_count >= min_word_count),]),
                colour = log_rr)) +
  scale_size(range = c(3, 5), name = "Number of Runs\nwith Notes\nMentioning Word") +
  scale_alpha(range = c(min(length_level[which(length_level$total_count > 1),]$alpha_updated), 
                        max(length_level[which(length_level$total_count > 1),]$alpha_updated))) +
  scale_color_gradient(low = "#8E0045", high = "#0A4877", guide = "none") +  
  scale_x_continuous(breaks = c(-1*plotting_limit_x, 0, plotting_limit_x),
                     limits = c(-1.2*plotting_limit_x, 1.2*plotting_limit_x),
                     labels = c(paste("Said More after\nShort Runs\n(runs less than", round(median_distance, 1), "miles)"),
                                "Said Equally after\nShort and Long Runs",
                                paste("Said More after\nLong Runs\n(runs more than",  round(median_distance, 1), "miles)"))) +
  scale_y_continuous(breaks = c(-1*plotting_limit_y, 0, plotting_limit_y),
                     limits = c(-1.2*plotting_limit_y, 1.2*plotting_limit_y),
                     labels = c("Said More after\nSlower Runs of Similar Distances",
                                "Said Equally after\nFaster and Slower Runs",
                                "Said More after\nFaster Runs of Similar Distances")) +
  guides(alpha = FALSE) +
  ggtitle("Relaxing vs. Strugglefest:\nWord use After Different Distances and Paces of Runs") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) ## center title of figure

dev.off()

################################################
## number of topics ############################
################################################

## todo
# result <- FindTopicsNumber(
#   dtm,
#   topics = seq(from = 2, to = 15, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   mc.cores = 2L,
#   verbose = TRUE
# )
