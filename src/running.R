#####################################################################
## set working directory and load libraries #########################
#####################################################################

setwd("~/Documents/workspace/runkeeper_analysis")
library(tm)
library(slam)
library(ggplot2)
library(wordcloud)

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
                         size = 1, color = "black")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = text ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = size, col = color))
  popViewport()
}

#####################################################################
## load and format runkeeper data ###################################
#####################################################################

workout <- read.csv("data/runkeeper-data-export-41471231-2016-06-12-2322/cardioActivities.csv")
workout$Notes <- as.character(workout$Notes)
workout$notes <- as.POSIXct(workout$Date)
## time range is set manually in export, specify which one was used here
time_range <- "1/1/2016 - 6/12/2016"

## just look at runs (workout type = "running")
run <- workout[which(workout$Type == "Running"),]
## reformat pace as time
run$avg_pace <- as.POSIXct(run$Average.Pace, format = "%M:%S")

run$time_of_day <- as.POSIXct(run$Average.Pace, format = "%:H:%M:%S")

#####################################################################
## How long are my runs? ############################################
#####################################################################

pdf("results/run_length.pdf", height = 5, width = 6)
par(mar = mar_default + c(2, 0, 0, 0))
hist(run$Distance..mi,
     ylab = "Number of Runs",
     xlab = "Distance (in miles)",
     col = "#3a33a3",
     main = "Overall Run Distance",
     xlim = c(0, 10),
     ylim = c(0, length(run$Distance..mi)/2.5))
box()
legend("topright",
       legend = c(paste("median:", round(median(run$Distance..mi), 1), "miles"),
                  paste("IQR: [",  round(quantile(run$Distance..mi, .25), 1),
                        ", ", 
                        round(quantile(run$Distance..mi, .75), 1), "] miles",
                        sep = "")),
       bg = "gray90")
makeFootnote(paste("Based on RunKeeper Run Data\nDate Range:", time_range))
dev.off()

#####################################################################
## Does my running speed differ by run length? ######################
#####################################################################

pdf("results/pace_vs_distance.pdf", height = 4, width = 6)
par(mar = mar_default + c(2, 2, 0, 0))
plot(run$Distance..mi, run$avg_pace,
     xlab = "Miles per Run",
     ylab = "Average Pace per Run\n",
     main = "Pace vs. Distance",
     col = "#3a33a3", las = 1)
makeFootnote(paste("Based on RunKeeper Run Data\nDate Range:", time_range))
dev.off()

#############################################################################
## basic regression: pace ~ run length  (dont' consider date of run) ########
#############################################################################

## for future plots, this will help to define runs that may be 
## faster than expected given the distance vs. slower than expected
## given the distance

pace_by_time <- lm(run$Average.Speed..mph. ~ run$Distance..mi)

## for runs between 1/1/2016 and 6/12/2016, there is no strong evidence
## than my face differed based on climb (elevation gain)

################################################
## process free text ###########################
################################################

run_text <- Corpus(VectorSource(run$Notes))
## remote SMART stopwords: http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop

run_text_processed <- tm_map(run_text, removeWords, stopwords("SMART"))

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

median_distance <- median(run$Distance..mi.)
run$long_binary <- ifelse(run$Distance..mi. > median_distance, "longer", "shorter")

## for each document, identify if a each word is ever used
long_counts <- as.vector(rollup(dt[which(run$long_binary == "longer"),], 1, FUN = function(x)sum(ifelse(x > 0, 1, 0))))
short_counts <- as.vector(rollup(dt[which(run$long_binary == "shorter"),], 1, FUN = function(x)sum(ifelse(x > 0, 1, 0))))
names(long_counts) <- colnames(dt); names(short_counts) <- colnames(dt)

length_level <- cbind.data.frame(long = long_counts, short = short_counts)
length_level$total_count <- apply(length_level, 1, sum)
length_level$p_long <- length_level$long / sum(run$long_binary == "longer")
length_level$p_short <- length_level$short / sum(run$long_binary == "shorter")
## add 0.001 to avoid dividing by zero or taking the log of 0
length_level$rr <- (length_level$p_long + 0.001) / (length_level$p_short + 0.001)
length_level$log_rr <- log((length_level$p_long + 0.001) / (length_level$p_short + 0.001)) 

## add a bit of a jitter (add random noise along the x axis to avoid overlapping words)
plotting_randomness_factor <- 1.5
set.seed(1202)
length_level$x <- length_level$log_rr + rnorm(mean = 0, sd = plotting_randomness_factor, n = nrow(length_level))
set.seed(0311)
length_level$y <- runif(min = 1, max = 10, n = nrow(length_level))

plotting_limit <- 1.1*max(c(abs(min(length_level$x)),
                    abs(max(length_level$x)))) 


pdf("results/positioned_wordcloud_distance.pdf", height = 5, width = 9)
ggplot(length_level, aes(x = x , y = y)) + 
  geom_text(aes(size = total_count,
                label = row.names(length_level),
                colour = log_rr), alpha = 0.7) +
  scale_size(range = c(3, 5), name = "Number of Runs\nwith Notes\nMentioning Word") +
  scale_color_gradient(low = "#8E0045", high = "#0A4877", guide = "none") +  
  scale_x_continuous(breaks = c(-1*plotting_limit, 0, plotting_limit),
                     limits = c(-1.2*plotting_limit, 1.2*plotting_limit),
                     labels = c(paste("Said More after\nShort Runs\n(runs less than", round(median_distance, 1), "miles)"),
                                "Said Equally after\nShort and Long Runs",
                                paste("Said More after\nLong Runs\n(runs more than",  round(median_distance, 1), "miles)"))) +
  scale_y_continuous(breaks=NULL) + 
  ggtitle("Relaxing vs. Strugglefest:\nWord use After Shorter vs. Longer Runkeeper Runs") +
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()

################################################
## distance and speed word cloud ###############
################################################

## use the residuals of the very simple fitted model (fit above) of pace ~ distance
## to see if a run is faster than expected for its distance or slower than expected
## for its distance
slow_counts <- as.vector(rollup(dt[which(pace_by_time$resid < 0),], 1, FUN = function(x)sum(ifelse(x > 0, 1, 0))))
fast_counts <- as.vector(rollup(dt[which(pace_by_time$resid >= 0),], 1, FUN = function(x)sum(ifelse(x > 0, 1, 0))))
names(slow_counts) <- colnames(dt); names(fast_counts) <- colnames(dt)

pace_level <- cbind.data.frame(fast = fast_counts, slow = slow_counts)

pace_level$total_count <- apply(pace_level, 1, sum)
pace_level$p_fast <- pace_level$fast / sum(pace_by_time$resid >= 0)
pace_level$p_slow <- pace_level$slow / sum(pace_by_time$resid < 0)
## add 0.001 to avoid dividing by zero or taking the log of 0
pace_level$rr <- (pace_level$p_fast + 0.001) / (pace_level$p_slow + 0.001)
pace_level$log_rr <- log((pace_level$p_fast + 0.001) / (pace_level$p_slow + 0.001)) 

## add a bit of a jitter (add random noise along the x axis to avoid overlapping words)
plotting_randomness_factor <- 1.5
set.seed(1202)
length_level$x <- length_level$log_rr + rnorm(mean = 0, sd = plotting_randomness_factor, n = nrow(length_level))
set.seed(0311)
length_level$y <- pace_level$log_rr + rnorm(mean = 0, sd = plotting_randomness_factor, n = nrow(pace_level))

length_level$alpha <- (length_level$total_count - min(length_level$total_count))/
                       max(length_level$total_count)

length_level$alpha[which(length_level$alpha == 0)] <- 0.01

plotting_limit_x <- 1.1*max(c(abs(min(length_level$x)),
                            abs(max(length_level$x)))) 

plotting_limit_y <- 1.1*max(c(abs(min(length_level$y)),
                              abs(length_level$y))) 


pdf("results/positioned_wordcloud_distance_pace.pdf", height = 5, width = 9)
ggplot(length_level, aes(x = x , y = y)) + 
  geom_text(aes(size = total_count,
                label = row.names(length_level),
                colour = log_rr),
            alpha = 0.6) + 
  scale_size(range = c(3, 5), name = "Number of Runs\nwith Notes\nMentioning Word") +
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
  ggtitle("Relaxing vs. Strugglefest:\nWord use After Shorter vs. Longer Runkeeper Runs") +
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()
