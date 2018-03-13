library(lattice)
library(agsemisc)
library(RColorBrewer)
library(extrafont)
library(ggplot2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#############################################################################

pdf("determinants_fancy.pdf", family="CM Roman")

runs <- read.table("170207-091502-Determinants-all-filterOn.txt", sep='\t', header=TRUE)
runs2 <- subset(runs, NUMSOURCES==2)
runs3 <- subset(runs, NUMSOURCES==3)
runs4 <- subset(runs, NUMSOURCES==4)
runs5 <- subset(runs, NUMSOURCES==5)

newruns <- read.table("170220-094941-BaseValues.txt", sep="\t", header=TRUE, row.names = NULL, na.strings = c("-"))
newruns <- subset(newruns, EVENTS != 0)
newruns2 <- subset(newruns, NUMSOURCES==2)
newruns3 <- subset(newruns, NUMSOURCES==3)
newruns4 <- subset(newruns, NUMSOURCES==4)
newruns5 <- subset(newruns, NUMSOURCES==5)

numsources.f <- factor(runs$NUMSOURCES, levels=c(2,3,4,5))
sensdist.f <- factor(runs$SENSINGDISTANCE, levels=c(30,40,50))
sensdist2.f <- factor(runs2$SENSINGDISTANCE, levels=c(30,40,50))
sensdist3.f <- factor(runs3$SENSINGDISTANCE, levels=c(30,40,50))
sensdist4.f <- factor(runs4$SENSINGDISTANCE, levels=c(30,40,50))
sensdist5.f <- factor(runs5$SENSINGDISTANCE, levels=c(30,40,50))

newnumsources.f <- factor(newruns$NUMSOURCES, levels=c(2,3,4,5))
newsensdist.f <- factor(newruns$SENSINGDISTANCE, levels=c(30,40,50))
newsensdist2.f <- factor(newruns2$SENSINGDISTANCE, levels=c(30,40,50))
newsensdist3.f <- factor(newruns3$SENSINGDISTANCE, levels=c(30,40,50))
newsensdist4.f <- factor(newruns4$SENSINGDISTANCE, levels=c(30,40,50))
newsensdist5.f <- factor(newruns5$SENSINGDISTANCE, levels=c(30,40,50))

#print(bwplot(runs$ACCURACY~sensdist.f|numsources.f, ylab="Accuracy",
                #index.cond = list(c(3,4,1,2)),
                #xlab="Sensing distance", ylim = range(0, 1.0)))


p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$ACCURACY, fill = sensdist2.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$ACCURACY, fill = sensdist3.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$ACCURACY, fill = sensdist4.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$ACCURACY, fill = sensdist5.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

p2 <- ggplot(newruns2, aes(x=newsensdist2.f, y = newruns2$EVENTS, fill = newsensdist2.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(newruns3, aes(x=newsensdist3.f, y = (newruns3$TRUE_POSITIVES + newruns3$TRUE_NEGATIVES) / newruns3$EVENTS, fill = newsensdist3.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(newruns4, aes(x=newsensdist4.f, y = (newruns4$TRUE_POSITIVES + newruns4$TRUE_NEGATIVES) / newruns4$EVENTS, fill = newsensdist4.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(newruns5, aes(x=newsensdist5.f, y = (newruns5$TRUE_POSITIVES + newruns5$TRUE_NEGATIVES) / newruns5$EVENTS, fill = newsensdist5.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "Accuracy") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

#print(bwplot(runs$TRUE_POSITIVE_RATE~sensdist.f|numsources.f, ylab="True positive rate",
                #index.cond = list(c(3,4,1,2)),
                #xlab="Sensing distance", ylim = range(0, 1.0)))

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$TRUE_POSITIVE_RATE, fill = sensdist2.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "True positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$TRUE_POSITIVE_RATE, fill = sensdist3.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "True positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$TRUE_POSITIVE_RATE, fill = sensdist4.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "True positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$TRUE_POSITIVE_RATE, fill = sensdist5.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "True positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

#print(bwplot(runs$FALSE_POSITIVE_RATE~sensdist.f|numsources.f, ylab="False positive rate",
                #index.cond = list(c(3,4,1,2)),
                #xlab="Sensing distance", ylim = range(0, 1.0)))

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$FALSE_POSITIVE_RATE, fill = sensdist2.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "False positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$FALSE_POSITIVE_RATE, fill = sensdist3.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "False positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$FALSE_POSITIVE_RATE, fill = sensdist4.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "False positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$FALSE_POSITIVE_RATE, fill = sensdist5.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "False positive rate") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

#print(bwplot(runs$PRECISION~sensdist.f|numsources.f, ylab="Precision",
                #index.cond = list(c(3,4,1,2)),
                #xlab="Sensing distance", ylim = range(0, 1.0)))

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$PRECISION, fill = sensdist2.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "Precision") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$PRECISION, fill = sensdist3.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "Precision") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$PRECISION, fill = sensdist4.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "Precision") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$PRECISION, fill = sensdist5.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "Precision") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

#print(bwplot(runs$SPECIFICITY~sensdist.f|numsources.f, ylab="Specificity",
                #index.cond = list(c(3,4,1,2)),
                #xlab="Sensing distance", ylim = range(0, 1.0)))

p2 <- ggplot(runs2, aes(x=sensdist2.f, y = runs2$SPECIFICITY, fill = sensdist2.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("2 event sources") +
        labs(x = "Sensing range", y = "Specificity") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p3 <- ggplot(runs3, aes(x=sensdist3.f, y = runs3$SPECIFICITY, fill = sensdist3.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("3 event sources") +
        labs(x = "Sensing range", y = "Specificity") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p4 <- ggplot(runs4, aes(x=sensdist4.f, y = runs4$SPECIFICITY, fill = sensdist4.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("4 event sources") +
        labs(x = "Sensing range", y = "Specificity") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
p5 <- ggplot(runs5, aes(x=sensdist5.f, y = runs5$SPECIFICITY, fill = sensdist5.f)) +
        geom_boxplot() + stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        ggtitle("5 event sources") +
        labs(x = "Sensing range", y = "Specificity") +
        theme(plot.title = element_text(hjust=0.5), legend.position = "none")
multiplot(p2, p4, p3, p5, cols=2)

runs <- read.table("170207-091502-Determinants-all.txt", sep='\t', header=TRUE)

numsources.f <- factor(runs$NUMSOURCES, levels=c(2,3,4,5))
sensdist.f <- factor(runs$SENSINGDISTANCE, levels=c(30,40,50))

#colors <- rainbow(2)

#bwplot(runs$EDR ~ sensdist.f | numsources.f, groups = runs$FILTER, pch = "|",
        #index.cond = list(c(3,4,1,2)),
        #box.width = 1/3,
        #par.settings = list(box.rectangle = list(colors),
                            #box.dot = list(colors),
                            #box.umbrella = list(colors),
                            #superpose.symbol = list(fill = colors, col = colors)
                            #),
        #panel = panel.superpose,
        #panel.groups = function(x, y, ..., group.number) {
            #panel.bwplot(x + (group.number - 1.5) / 3, y, ...)
        #},
        #key = list(text = list(levels(runs$FILTER)), space = "right",
                   #rectangles = list(pch = 1:nlevels(runs$FILTER),
                                     #col = colors), columns = 1),
        #ylab = "Event delivery rate", xlab = "Sensing distance")
        #ylim = range(0, 1.0))

ggplot(runs, aes(x=sensdist.f, y = runs$EDR, fill = runs$FILTER)) +
        geom_boxplot() +
        stat_summary(fun.y = mean, geom = "point") +
        labs(x = "Sensing range", y = "Event delivery rate") +
        scale_fill_discrete("Filter") +
        theme(legend.position = "top")

ggplot(runs, aes(x=sensdist.f, y = runs$EDR, fill = runs$FILTER)) +
        geom_boxplot() +
        stat_summary(fun.y = mean, geom = "point") +
        coord_cartesian(ylim = c(0.0, 1.0)) +
        labs(x = "Sensing range", y = "Event delivery rate") +
        scale_fill_discrete("Filter") +
        theme(legend.position = "top")

dev.off()
embed_fonts("determinants_fancy.pdf", outfile="determinants_fancy.pdf")
