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
csvLoc   = "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
# ------------------------------------------------------------------------
csvLocal = "../data/processed/time_series_19-covid-Deaths.csv"
download.file(csvLoc,csvLocal)
# ------------------------------------------------------------------------
rawdat                 = read.csv(csvLocal, header=FALSE, as.is=TRUE, sep=",")
odat                   = rawdat
# ------------------------------------------------------------------------
# correct inconsistent naming scheme ---
for (i in 1:nrow(odat)){
  if (odat[i,1] == odat[i,2]){
    odat[i,1] = ""
  }
}

odat[, 5:ncol(rawdat)] = apply(rawdat[, 5:ncol(rawdat)], 2,
                               function(x) as.numeric(as.character(x)))
odat[, 5:ncol(odat)]   = apply(odat[ ,5:ncol(odat)], 2,
                               function(x) { x[is.na(x)] <- 0; x })

dat                    = odat[odat[,1]=="" | odat[,2]=="US", ]
# china is not listed as a full country, sum by province to the country level --
china                  = odat[odat[,2]=="China", ]
datchina               = c("", "China", "NA", "NA", colSums(china[,5:ncol(china)]))
dat                    = rbind(dat, datchina)
# USA is not listed as a full country, sum by province to the country level --
usa                    = odat[odat[,2]=="US", ]
datusa                 = c("", "United States", "NA", "NA", colSums(usa[,5:ncol(usa)]))
dat                    = rbind(dat, datusa)
dat[ , 5:ncol(dat)]    = apply(dat[ ,5:ncol(dat)], 2,
                               function(x) as.numeric(as.character(x)))

# US -> USA ---
dat[dat[,2]=="US",2] <- "USA"
# remove countries with fewer than 50 deaths ---
dat = dat[do.call(pmax, dat[,5:ncol(dat)]) >= 50,]
# format columns ---
dat[,2] = apply(dat[,1:2],1,paste,collapse=", ")
dat[,2] = gsub("^, ", "", dat[,2])
dat[,2] = gsub("Korea, South", "South Korea", dat[,2])

# format for visualization ---
visdat           = dat[,c(2,5:ncol(dat))]
colnames(visdat) = rawdat[1,c(2,5:ncol(rawdat))]
rownames(visdat) = visdat[,1]
# melt data frame by Country (wide to long form) ---
visdat           = melt(data = visdat, id.vars="Country/Region")
# add to visdat, days from the 100th case, log the point last in the series
from50       = list()
visdat$Daysfrom50thDeath = array(NA,dim=c(nrow(visdat),1))
visdat$LastInSeries      = array(NA,dim=c(nrow(visdat),1))
visdat$Label             = array(NA,dim=c(nrow(visdat),1))
for (i in 1:nrow(visdat)){
  if (is.null(from50[[visdat[i,1]]]) & visdat[i,3] >= 50){
    from50[[visdat[i,1]]] = 1
    visdat[i,4] = from50[[visdat[i,1]]]
  }else if (!is.null(from50[[visdat[i,1]]])){
    from50[[visdat[i,1]]] = from50[[visdat[i,1]]] + 1
    visdat[i,4] = from50[[visdat[i,1]]]
  }
}
seen = list()
for (i in nrow(visdat):1){
  if (is.null(seen[[visdat[i,1]]])){
    seen[[visdat[i,1]]] = 1
    visdat[i,5] = "yes"
    visdat[i,6] = paste(visdat[i,1], " (", visdat[i,3], ")", sep="")
  }else if (!is.null(from50[[visdat[i,1]]])){
    visdat[i,5] = "no"
    visdat[i,6] = NA
  }
}
colnames(visdat)      = c("Country.Region", "Date", "Cumulative.Deaths", "Days.from.50th.Death", "LastInSeries", "Label")

# remove data before feb for vis by country ---
visdat                = visdat[grepl("^(2|3|4|5)",visdat[,2]),]
visdat                = data.frame(visdat)
# sort by most deaths ---
aggres                = aggregate(Cumulative.Deaths ~ Country.Region, visdat, FUN=max)
visdat$Country.Region = factor(visdat$Country.Region,levels=c(aggres[order(aggres[,2],decreasing=TRUE),1]))
# adaptive color scheme ---
colscheme    = c("#d72123", "#d87632", "#cac654", "#589A5D", "#4781A7", "#816fa3", "#d368a1", "grey50")
adaptiveCols = colorRampPalette(colscheme)(length(levels(visdat$Country.Region)))
# plot by country by date ---
outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-by-date.png", sep="")
p1 <- ggplot(visdat, aes(x=Date, y=Cumulative.Deaths, group=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.9) +
geom_point(aes(color=Country.Region), alpha=0.9, size=1.5) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=5, colour="black", angle=45, hjust=1, vjust=1),
      axis.text.y  = element_text(size=10, colour="black"),
      axis.title.x = element_text(size=11, colour="black"),
      axis.title.y = element_text(size=11, colour="black"),
      plot.title   = element_text(size=11, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
xlab("Date") +
ylab("Cumulative Deaths") +
ggtitle(titleStr) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p1, height=4.8, width=8)

# plot by country by date ---
outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-by-date-log10.png", sep="")
p1 <- ggplot(visdat, aes(x=Date, y=Cumulative.Deaths, group=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.9) +
geom_point(aes(color=Country.Region), alpha=0.9, size=1.5) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=5, colour="black", angle=45, hjust=1, vjust=1),
      axis.text.y  = element_text(size=10, colour="black"),
      axis.title.x = element_text(size=11, colour="black"),
      axis.title.y = element_text(size=11, colour="black"),
      plot.title   = element_text(size=9, colour="black"),
      legend.title = element_text(size=9, colour="black"),
      legend.text  = element_text(size=9, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm")) +
xlab("Date") +
ylab("Cumulative Deaths") +
ggtitle(titleStr) +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p1, height=4.8, width=8)


# --------------------------------------------------
# plot by country post 50th death ---
outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-from-50th-death.png", sep="")
p1 <- ggplot(visdat, aes(x=Days.from.50th.Death, y=Cumulative.Deaths, group=Country.Region, label=Label, color=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.9) +
geom_point(aes(color=Country.Region), alpha=0.9, size=1.5) +
geom_text_repel(data          = subset(visdat, LastInSeries=="yes"),
                aes(label     = Label),
                nudge_x       = 35 - subset(visdat, LastInSeries=="yes")$Days.from.50th.Death,
                nudge_y       = 100,
                angle         = 0,
                force         = 100,
                size          = 3,
                segment.size  = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=12, colour="black"),
      axis.text.y  = element_text(size=12, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab("Days from 50th Death") +
ylab("Cumulative Deaths") +
scale_x_continuous(breaks = seq(0, max(na.omit(visdat$Days.from.50th.Death)), by = 4)) +
coord_cartesian(ylim = c(50, max(visdat$Cumulative.Deaths))) +
ggtitle(titleStr) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p1, height=4.8, width=8)

# plot by country post 50th death ---
outfile1 = paste(analysisdir, "/covid-19.cumulative-deaths-from-50th-death-log10.png", sep="")
p1 <- ggplot(visdat, aes(x=Days.from.50th.Death, y=Cumulative.Deaths, group=Country.Region, label=Label, color=Country.Region)) +
geom_path(mapping=aes(group=Country.Region, color=Country.Region), alpha=0.9) +
geom_point(aes(color=Country.Region), alpha=0.9, size=1.5) +
geom_text_repel(data          = subset(visdat, LastInSeries=="yes"),
                aes(label     = Label),
                nudge_x       = 35 - subset(visdat, LastInSeries=="yes")$Days.from.50th.Death,
                nudge_y       = 0,
                direction     = "x",
                angle         = 0,
                size          = 3,
                segment.size  = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=12, colour="black"),
      axis.text.y  = element_text(size=12, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab("Days from 50th Death") +
ylab("Cumulative Deaths") +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
coord_cartesian(ylim = c(45, max(visdat$Cumulative.Deaths))) +
scale_x_continuous(breaks = seq(0, max(na.omit(visdat$Days.from.50th.Death)), by = 4)) +
ggtitle(titleStr) +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p1, height=6, width=8)
