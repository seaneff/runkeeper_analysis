#####################################################################
## set working directory and load libraries #########################
#####################################################################

setwd("~/Documents/workspace/runkeeper_analysis")
library(ranger)
library(tm)
library(slam)
library(ggplot2)
library(wordcloud)
library(XML)
library(plyr)
library(fpc)

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
## load and format runkeeper data (non-gpx) #########################
#####################################################################

workout <- read.csv("data/runkeeper-data-export-41471231-2016-08-08-2358/cardioActivities.csv")

## format selected variables
workout$Notes <- as.character(workout$Notes)
workout$notes <- as.POSIXct(workout$Date)

## time range is set manually in export, specify which one was used here
time_range <- "1/1/2015 - 8/8/2016"

## just look at runs (workout type = "running")
run <- workout[which(workout$Type == "Running"),]

## exclude tough mudder
run <- run[-which(run$Notes == "Tough Mudder in Portland Maine"),]

## reformat pace and time of day as time
run$avg_pace <- as.POSIXct(run$Average.Pace, format = "%M:%S")
run$time_of_day <- format(as.POSIXct(run$Date), "%H:%M:%S")
run$morning_afternoon <- factor(ifelse(run$time_of_day < "12:00:00", "morning", "afternoon/evening"),
                                levels = c("morning", "afternoon/evening"))

#####################################################################
## load and format gpx data #########################################
#####################################################################

## thanks to Daniel Cook and Holger Brandl for providing code on git
## reference: http://www.danielecook.com/how-to-plot-all-of-your-runkeeper-data/
## reference: https://gist.github.com/holgerbrandl/5595165

# gpx files in data subdirectory
gpx_files <- paste("data/runkeeper-data-export-41471231-2016-08-08-2358/",
                   dir(path = "data/runkeeper-data-export-41471231-2016-08-08-2358", 
                 pattern = "\\.gpx"), sep = "")

for (i in 1:length(gpx_files)) {
  curr_route <- xmlParse(gpx_files[i], useInternalNodes = TRUE)
  route_df <- ldply(xpathSApply(curr_route, "//*[local-name()='trkpt']"), 
                    function(x) unlist(xmlToList(x)))
  colnames(route_df) <- c("elevation", "datetime", "lat", "lon")
  trackdata <- data.frame(datetime=route_df$datetime, 
                          colwise(as.numeric, .(elevation, lat, lon))(route_df))
  trackdata$run_id <- i
  if(i == 1){ full_runs <- trackdata }
  if(i > 1) {full_runs <- rbind.data.frame(full_runs, trackdata)}
}


#####################################################################
## Distribution of run length #######################################
#####################################################################

pdf("results/run_length.pdf", height = 4, width = 5)
par(mar = mar_default + c(2, 0, 0, 0))
hist(run$Distance..mi,
     ylab = "Number of Runs",
     xlab = "Distance (in miles)",
     col = "#3a33a3",
     main = "Overall Run Distance",
     xlim = c(0, 12),
     ylim = c(0, length(run$Distance..mi)/1.75))
box()
legend("topright",
       legend = c(paste("median:", round(median(run$Distance..mi), 1), "miles"),
                  paste("IQR: [",  round(quantile(run$Distance..mi, .25), 1),
                        ", ", 
                        round(quantile(run$Distance..mi, .75), 1), "] miles",
                        sep = "")),
       bg = "gray90")
makeFootnote(paste("Based on RunKeeper Run Data\nDate Range:", time_range),
             cex = 0.6)
dev.off()

#####################################################################
## Distribution of run length by time of day ########################
#####################################################################

pdf("results/run_length_by_time_of_day.pdf", height = 4, width = 4)
ggplot(run, aes(Distance..mi.)) + 
  ggtitle("Distribution of Run Length by Time of Day") +
  geom_histogram(fill = "dark blue", col = "gray50",  binwidth = 1) +
  xlab("Distance (in miles)") +
  ylab("Number of Runs") +
  facet_wrap(~ morning_afternoon, nrow = 2)
dev.off()
                     
#####################################################################
## Pace vs. Distance ################################################
#####################################################################

pdf("results/pace_vs_distance.pdf", height = 4, width = 6)
par(mar = mar_default + c(2, 2, 0, 0))
plot(run$Distance..mi, run$avg_pace,
     xlab = "Miles per Run",
     ylab = "Average Pace per Run\n",
     main = "Pace vs. Distance",
     col = "#3a33a3", las = 1)
makeFootnote(paste("Based on RunKeeper Run Data\nDate Range:", time_range),
             cex = 0.6)
dev.off()

#############################################################################
## identify clusters of runs based on location ##############################
#############################################################################

## code based on code originally written by Daniel Cook
## reference: http://www.danielecook.com/how-to-plot-all-of-your-runkeeper-data/

# Identify geospatial clusters of run locations
# Partitioning around medoids 

num_locations <- 3
clusters <- pamk(full_runs[,c("lat", "lon")], 
                 krange = num_locations:20, 
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

## pull watercolor map from API
map_watercolor <- get_map(location = c(clusters[i,2], clusters[i,1]), 
                           maptype = "watercolor", source = "stamen",
                           zoom = 12)

## pull watercolor map from API
map_watercolor <- get_map(location = c(clusters[i,2], clusters[i,1]), 
                          maptype = "watercolor", source = "stamen",
                          zoom = 12)

map_roadmap <- get_map(location = c(clusters[i,2], clusters[i,1]), 
                maptype = "roadmap", source = "google",
                zoom = 12, color = "bw")

## plot map with tracks (watercolor)
wc <- ggmap(map_watercolor, extent = "device") +
  theme(axis.line = element_line(color = NA)) + 
  xlab("") + ylab("") 

  for( j in unique(full_runs$run_id)) {
    wc <- wc + geom_path(data = full_runs[which(full_runs$run_id == j),], 
                         col = "dark blue", size = 1, lineend = "round", alpha = 0.4)
  }

print(wc)

## plot map with tracks (watercolor)
r <- ggmap(map_roadmap, extent = "device") +
  theme(axis.line = element_line(color = NA)) + 
  xlab("") + ylab("") 

for( j in unique(full_runs$run_id)) {
  r <- r + geom_path(data = full_runs[which(full_runs$run_id == j),], 
                     col = "dark blue", size = 1, lineend = "round", alpha = 0.4,
                     col = "bw")
}

print(r)

}

dev.off()

#############################################################################
## imputation for missing elevation data ####################################
#############################################################################

## if climb.ft is not known, use imputation
impute_climb <- ranger(Climb..ft. ~ Average.Speed..mph. + Distance..mi.,
                       data = run[which(complete.cases(run$Climb..ft.)),],
                       write.forest = TRUE,
                       num.trees = 500, mtry = 1)

## make predictions for run climb
impute_climb_predictions <- predict(impute_climb, 
                                    data = run[-which(complete.cases(run$Climb..ft.)),])

## add indicator to show if climb data were imputed
run$climb_imputed <- ifelse(is.na(run$Climb..ft.), TRUE, FALSE)

## fill in missing data with imputed precitions
run[-which(complete.cases(run$Climb..ft.)),]$Climb..ft. <- impute_climb_predictions$predictions

################################################################################
## predict average pace based on distance, time of day, and elevation/climb ####
################################################################################

pace_by_time_rf <- ranger(Average.Speed..mph. ~ Distance..mi. + morning_afternoon + Climb..ft., 
                          data = run, num.trees = 500,
                          mtry = 1, importance = "impurity",
                          write.forest = TRUE)

##############################################################
## calculate residual for each random forest prediction ######
##############################################################

## residual = observed - predicted
## negative residuals = ran slower than expected (prediction > actual)
## positive residuals = ran faster than expected (prediction < actual)

run$residual <- run$Average.Speed..mph. - predict(pace_by_time_rf, run)$predictions 

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
median_distance <- median(run$Distance..mi.)

## for each run, identify whether run distance is above or below median distance
run$long_binary <- ifelse(run$Distance..mi. > median_distance, "longer", "shorter")

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
plotting_randomness_factor <- 1.5
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
  theme_bw()
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
plotting_randomness_factor_x <- 1.5
set.seed(1202)
length_level$x <- length_level$log_rr + rnorm(mean = 0, sd = plotting_randomness_factor_x, n = nrow(length_level))

## calculate y axis position based on pace-based relative risk:
## add a bit of a jitter (add random noise along the y axis to avoid overlapping words)
plotting_randomness_factor_y <- 1.5
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

## generate figure
pdf("results/positioned_wordcloud_distance_pace.pdf", height = 5, width = 9)
ggplot(length_level, aes(x = x , y = y, alpha = alpha_updated)) + 
  geom_text(aes(size = total_count,
                label = row.names(length_level),
                colour = log_rr)) +
  scale_size(range = c(3, 5), name = "Number of Runs\nwith Notes\nMentioning Word") +
  scale_alpha(range = c(min(length_level$alpha_updated), max(length_level$alpha_updated))) +
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
  theme_bw()
dev.off()
