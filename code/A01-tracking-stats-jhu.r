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
aggres = aggres[aggres[,2]>=500,]
visdat = visdat[visdat$Country.Region %in% aggres[,1],]
visdat$Country.Region = as.character(visdat$Country.Region)
visdat$Country.Region = factor(visdat$Country.Region,levels=c(as.character(aggres[order(aggres[,2],decreasing=TRUE),1])))

# adaptive color scheme ---
colscheme    = c("#d72123", "#d87632", "#cac654", "#589A5D", "#4781A7", "#816fa3", "#d368a1", "grey50")
adaptiveCols = colorRampPalette(colscheme)(length(levels(visdat$Country.Region)))

# plot by country by date ---
outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-by-date-log10.png", sep="")
p1 <- ggplot(visdat, aes(x=DateFormatted, y=Cumulative.Deaths, group=Country.Region, label=Label, color=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.75) +
geom_point(aes(color=Country.Region), alpha=0.75, size=0.8) +
geom_text_repel(data          = subset(visdat, LastInSeries=="yes"),
                aes(label     = Label),
                force         = 3,
                xlim          = c(as.Date("2020-07-01"), as.Date("2020-09-14")),
                size          = 1.75,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=6, colour="black"),
      axis.text.y  = element_text(size=11, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab("Date") +
scale_x_date(date_labels = "%b %d", date_breaks = "1 week", limits=as.Date(c("2020-04-01",NA))) +
expand_limits(x = as.Date("2020-09-14")) +
ylab("Cumulative Deaths") +
ggtitle(titleStr) +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p1, height=6, width=8)

# plot by country post 50th death ---
aggres  = aggres[aggres[,2]>=100,]
visdat2 = visdat[visdat$Country.Region %in% aggres[,1],]
visdat2$Country.Region = as.character(visdat2$Country.Region)
visdat2 = visdat2[grepl("USA",visdat2$Country.Region),]
visdat2$Country.Region = factor(visdat2$Country.Region,levels=c(as.character(aggres[order(aggres[,2],decreasing=TRUE),1])))
adaptiveCols2 = colorRampPalette(rep(colscheme,2))(length(levels(visdat2$Country.Region)))

outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-from-50th-death-log10.png", sep="")
p2 <- ggplot(visdat2, aes(x=Days.from.50th.Death, y=Cumulative.Deaths, group=Country.Region, label=Label, color=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.5) +
geom_point(aes(color=Country.Region), alpha=0.5, size=1.5) +
geom_text_repel(data          = subset(visdat2, LastInSeries=="yes"),
                aes(label     = Label),
                nudge_y       = 0,
                nudge_x       = 20 + 0.25*subset(visdat2, LastInSeries=="yes")$Days.from.50th.Death,
                force         = 1,
                xlim          = c(100,max(subset(visdat2, LastInSeries=="yes")$Days.from.50th.Death)+70),
                direction     = "x",
                angle         = 0,
                size          = 1.85,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols2) +
theme(axis.text.x  = element_text(size=8, colour="black"),
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
scale_x_continuous(breaks = seq(0, max(na.omit(visdat2$Days.from.50th.Death)+70), by = 5), limits=c(0,max(subset(visdat2, LastInSeries=="yes")$Days.from.50th.Death)+70)) +
ggtitle(titleStr) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p2, height=6, width=8)


# plot cumulative deaths and growth rate ---
outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-vs-death-rate-increase.png", sep="")
p2 <- ggplot(visdat[visdat$LastInSeries=="yes" & visdat$Country.Region != "Finland",], aes(x=Cumulative.Deaths, y=Prct.Increase.Death.2Day, group=Country.Region, label=Label, color=Country.Region)) +
geom_point(aes(color=Country.Region), alpha=0.75, size=2.5) +
geom_text_repel(data          = subset(visdat[visdat$Country.Region != "Finland",], LastInSeries=="yes"),
                aes(label     = Label),
                force         = 2,
                angle         = 0,
                nudge_y       = 0.25,
                size          = 1.5,
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
scale_x_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000)) +
coord_cartesian(xlim = c(250, max(visdat$Cumulative.Deaths)), ylim=c(0,5)) +
ggtitle(titleStr) +
geom_hline(yintercept=0, color="black", linetype="dashed") +
theme(aspect.ratio=1)
ggsave(outfile1, plot=p2, height=6, width=6)


# deaths per day --- peak analysis ---
# sort by most deaths ---
aggres = aggregate(Deaths.per.Day.3DayMA ~ Country.Region, visdat, FUN=max)
# select those places with >= 25 deaths per day
aggres = aggres[aggres[,2]>=10,]
aggres = aggregate(Deaths.per.Day.3DayMA ~ Country.Region, visdat[visdat$Country.Region %in% aggres[,1] & visdat$LastInSeries=="yes", ], FUN=max)

visdat = visdat[visdat$Country.Region %in% aggres[,1],]
visdat$Country.Region = as.character(visdat$Country.Region)
visdat$Country.Region = factor(visdat$Country.Region,levels=c(as.character(aggres[order(aggres[,2],decreasing=TRUE),1])))

outfile1   = paste(analysisdir, "/covid-19.deaths-per-day-3dma.png", sep="")
thisvisdat = visdat[grepl("^(China|France|Germany|Brazil|USA|United|Italy|Peru|Canada|Argentina|Chile|Belgium|Mexico|India|Russia)$", visdat$Country.Region),]
thisvisdat$Country.Region = droplevels(thisvisdat$Country.Region)

daydex = thisvisdat[thisvisdat$DateFormatted == "2020-06-22",]

thisvisdat$Country.Region = factor(thisvisdat$Country.Region,levels=c(as.character(daydex[order(daydex$Deaths.per.Day.3DayMA, decreasing=TRUE),"Country.Region"])))

# adaptive color scheme ---
colscheme    = c("#d72123", "#d87632", "#cac654", "#589A5D", "#4781A7", "#816fa3", "#d368a1", "grey50")
adaptiveCols = colorRampPalette(rep(colscheme,1))(length(levels(thisvisdat$Country.Region)))
p2 <- ggplot(thisvisdat[thisvisdat$Deaths.per.Day.3DayMA >=2, ], aes(x=DateFormatted, y=Deaths.per.Day.3DayMA, group=Country.Region, label=Label, color=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.6) +
geom_point(aes(color=Country.Region), alpha=0.6, size=2) +
geom_text_repel(data          = subset(thisvisdat, LastInSeries=="yes"),
                aes(label     = Label),
                nudge_x       = 1,
                force         = 3,
                angle         = 0,
                xlim          = c(as.Date("2020-07-01"), as.Date("2020-07-10")),
                size          = 2.2,
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
ylab("Deaths per Day (3 Day Avg)") +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000), limits=c(2,4000)) +
xlab("Date") +
scale_x_date(date_labels = "%b %d", date_breaks = "1 week", limits=as.Date(c("2020-04-15",NA))) +
expand_limits(x = as.Date("2020-07-10")) +
ggtitle(titleStr) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p2, height=6, width=8)
