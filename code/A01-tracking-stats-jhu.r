# --------------------------------------------------------------------
#  A01-tracking-stats-jhu.r
#  Author: James Robert White, PhD
#  Email: jwhite@respherabio.com
# --------------------------------------------------------------------
# code pulls data from JHU CSSE for analysis and visualization
# runs from command line as
# Rscript A01-tracking-stats-jhu.r
# --------------------------------------------------------------------
rm(list = ls())
library(RColorBrewer)
library(scales)
library(ggplot2)
require(gtools)
library(MASS)
library(utils)
library(reshape2)
library(ggrepel)
library(egg)
# ------------------------------------------------------------------------
# set the working directory as the location of the script ---
setwd("/Users/jwhite/Desktop/resphera-covid-19-global/code")
scriptPrefix = "A01-tracking-stats-jhu"
# ------------------------------------------------------------------------
reportTimeStamp = format(Sys.time(), "%Y-%m-%d (%a) %X")
titleStr        = paste("COVID-19 Deaths by Country/Region ", "[", reportTimeStamp, "]", sep="")
# ------------------------------------------------------------------------
# create output directory
analysisdir = paste("../analysis/", scriptPrefix, sep="")
unlink(analysisdir, recursive=TRUE)
dir.create(analysisdir)
# ------------------------------------------------------------------------
# load data from A00 script
visdat               = read.table(file="../analysis/A00-scrape-daily-reports/A00.covid19-long-form.txt", header=TRUE, sep="\t", quote="\"")
visdat$DateFormatted = as.Date(as.character(visdat$DateFormatted))
visdat$Label         = as.character(visdat$Label)
# sort by most deaths ---
aggres = aggregate(Cumulative.Deaths ~ Country.Region, visdat, FUN=max)
# select those places with >= 50 deaths
aggres = aggres[aggres[,2]>=50,]
visdat = visdat[visdat$Country.Region %in% aggres[,1],]
visdat$Country.Region = as.character(visdat$Country.Region)
visdat$Country.Region = factor(visdat$Country.Region,levels=c(as.character(aggres[order(aggres[,2],decreasing=TRUE),1])))

# adaptive color scheme ---
colscheme    = c("#d72123", "#d87632", "#cac654", "#589A5D", "#4781A7", "#816fa3", "#d368a1", "grey50")
adaptiveCols = colorRampPalette(colscheme)(length(levels(visdat$Country.Region)))

# plot by country by date ---
outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-by-date-log10.png", sep="")
p1 <- ggplot(visdat, aes(x=DateFormatted, y=Cumulative.Deaths, group=Country.Region, label=Label, color=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.9) +
geom_point(aes(color=Country.Region), alpha=0.9, size=1.5) +
geom_text_repel(data          = subset(visdat, LastInSeries=="yes"),
                aes(label     = Label),
                force         = 3,
                xlim          = c(as.Date("2020-04-05"), as.Date("2020-04-30")),
                size          = 2,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=9, colour="black"),
      axis.text.y  = element_text(size=11, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab("Date") +
scale_x_date(date_labels = "%b %d", date_breaks = "1 week", limits=as.Date(c("2020-03-01",NA))) +
expand_limits(x = as.Date("2020-04-30")) +
ylab("Cumulative Deaths") +
ggtitle(titleStr) +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p1, height=6, width=8)

# plot by country post 50th death ---
aggres  = aggres[aggres[,2]>=150,]
visdat2 = visdat[visdat$Country.Region %in% aggres[,1],]
visdat2$Country.Region = as.character(visdat2$Country.Region)
visdat2$Country.Region = factor(visdat2$Country.Region,levels=c(as.character(aggres[order(aggres[,2],decreasing=TRUE),1])))
adaptiveCols2 = colorRampPalette(rep(colscheme,2))(length(levels(visdat2$Country.Region)))

outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-from-50th-death-log10.png", sep="")
p2 <- ggplot(visdat2, aes(x=Days.from.50th.Death, y=Cumulative.Deaths, group=Country.Region, label=Label, color=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.9) +
geom_point(aes(color=Country.Region), alpha=0.9, size=1.5) +
geom_text_repel(data          = subset(visdat2, LastInSeries=="yes"),
                aes(label     = Label),
                nudge_y       = 0,
                nudge_x       = 38 - subset(visdat2, LastInSeries=="yes")$Days.from.50th.Death,
                xlim          = c(24,max(subset(visdat2, LastInSeries=="yes")$Days.from.50th.Death)+4),
                force         = 2,
                direction     = "x",
                angle         = 0,
                size          = 2.25,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols2) +
theme(axis.text.x  = element_text(size=10, colour="black"),
      axis.text.y  = element_text(size=11, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab(bquote("Days from 50"^"th"*" Death")) +
ylab("Cumulative Deaths") +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
coord_cartesian(ylim = c(45, max(visdat2$Cumulative.Deaths))) +
scale_x_continuous(breaks = seq(0, max(na.omit(visdat2$Days.from.50th.Death)+4), by = 4)) +
ggtitle(titleStr) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p2, height=6, width=8)


# plot cumulative deaths and growth rate ---
outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-vs-death-rate-increase.png", sep="")
p2 <- ggplot(visdat[visdat$LastInSeries=="yes",], aes(x=Cumulative.Deaths, y=Prct.Increase.Death.2Day, group=Country.Region, label=Label, color=Country.Region)) +
geom_point(aes(color=Country.Region), alpha=0.75, size=2.5) +
geom_text_repel(data          = subset(visdat, LastInSeries=="yes"),
                aes(label     = Label),
                force         = 5,
                angle         = 0,
                nudge_y       = 1,
                size          = 2,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=10, colour="black"),
      axis.text.y  = element_text(size=11, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab("Cumulative Deaths") +
ylab("% Increase in Cumulative Deaths (Most Recent 2-Day Avg)") +
scale_x_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
coord_cartesian(xlim = c(45, max(visdat$Cumulative.Deaths))) +
ggtitle(titleStr) +
geom_hline(yintercept=0, color="black", linetype="dashed") +
theme(aspect.ratio=1)
ggsave(outfile1, plot=p2, height=6, width=6)


# deaths per day --- peak analysis ---
# sort by most deaths ---
aggres = aggregate(Deaths.per.Day.3DayMA ~ Country.Region, visdat, FUN=max)
# select those places with >= 25 deaths per day
aggres = aggres[aggres[,2]>=35,]
visdat = visdat[visdat$Country.Region %in% aggres[,1],]
visdat$Country.Region = as.character(visdat$Country.Region)
visdat$Country.Region = factor(visdat$Country.Region,levels=c(as.character(aggres[order(aggres[,2],decreasing=TRUE),1])))

# adaptive color scheme ---
colscheme    = c("#d72123", "#d87632", "#cac654", "#589A5D", "#4781A7", "#816fa3", "#d368a1", "grey50")
adaptiveCols = colorRampPalette(rep(colscheme,1))(length(levels(visdat$Country.Region)))

outfile1 = paste(analysisdir, "/covid-19.deaths-per-day-3dma.png", sep="")
p2 <- ggplot(visdat, aes(x=Days.from.50th.Death, y=Deaths.per.Day.3DayMA, group=Country.Region, label=Label, color=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.25) +
geom_point(aes(color=Country.Region), alpha=0.75, size=1.9) +
geom_text_repel(data          = subset(visdat, LastInSeries=="yes"),
                aes(label     = Label),
                nudge_x       = 0,
                force         = 4,
                angle         = 0,
                direction     = "x",
                xlim          = c(44,max(na.omit(visdat$Days.from.50th.Death))+2),
                size          = 2,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=10, colour="black"),
      axis.text.y  = element_text(size=11, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab(bquote("Days from 50"^"th"*" Death")) +
ylab("Deaths per Day (3 Day Avg)") +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
scale_x_continuous(breaks = seq(0, max(na.omit(visdat$Days.from.50th.Death))+4, by = 4)) +
ggtitle(titleStr) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p2, height=6, width=8)
