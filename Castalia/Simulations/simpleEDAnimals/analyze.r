myread.cvsdata <- function(file) {

	runs <- read.table(file, sep='\t', header=TRUE)

	return(runs)
}

myanalyze.cvsdata <- function(file) {

	runs <- myread.cvsdata(file)

	yrange <- range(0.0,1.0)

	pdf("determinants.pdf")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("accuracy-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="Accuracy", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$ACCURACY, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("Accuracy")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("accuracy-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="Accuracy", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$ACCURACY, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("Accuracy")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("precision-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="Precision", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$PRECISION, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("Precision")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("precision-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="Precision", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$PRECISION, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("Precision")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("specificity-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="Specificity", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$SPECIFICITY, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("Specificity")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("specificity-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="Specificity", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$SPECIFICITY, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("Specificity")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("f1-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="F1 Score", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$F1, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("F1 Score")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("f1-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="F1 Score", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$F1, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("F1 Score")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("f2-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="F2 Score", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$F2, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("F2 Score")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("f2-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="F2 Score", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$F2, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("F2 Score")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("f05-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="F0.5 Score", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$F05, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("F0.5 Score")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("f05-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="F0.5 Score", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$F05, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("F0.5 Score")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("true-positive-rate-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="True Positive Rate", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$TRUE_POSITIVE_RATE, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("True Positive Rate")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("true-positive-rate-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="True Positive Rate", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$TRUE_POSITIVE_RATE, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("True Positive Rate")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("false-positive-rate-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="False Positive Rate", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$FALSE_POSITIVE_RATE, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("False Positive Rate")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("false-positive-rate-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="False Positive Rate", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$FALSE_POSITIVE_RATE, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("False Positive Rate")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")

	###########################

	xrange <- range(2,5)

	nruns <- 3

	#pdf("youden-numSources.pdf")
	plot(xrange, yrange, type="n", xlab="Number of sources", ylab="Youden's Index", xaxt="n")
	axis(1, at=c(2, 3, 4, 5))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(30,40,50)) {
		j <- j+1
		run <- subset(runs, SENSINGDISTANCE==i)
		lines(run$NUMSOURCES, run$YOUDEN_INDEX, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("Youden's Index")

	legend(xrange[1], yrange[2], c(30,40,50), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Sensing distance")

	###########################

	xrange <- range(30,50)

	nruns <- 4

	#pdf("youden-sensingDistance.pdf")
	plot(xrange, yrange, type="n", xlab="Sensing distance", ylab="Youden's Index", xaxt="n")
	axis(1, at=c(30,40,50))
	colors <- rainbow(nruns)
	linetype <- c(1:nruns)
	plotchar <- seq(18,18+nruns,1)

	j <- 0
	for (i in c(2,3,4,5)) {
		j <- j+1
		run <- subset(runs, NUMSOURCES==i)
		lines(run$SENSINGDISTANCE, run$YOUDEN_INDEX, type="b", lwd=1.5,
			  lty=linetype[j], col=colors[j], pch=plotchar[j])
	}

	title("Youden's Index")

	legend(xrange[1], yrange[2], c(2,3,4,5), cex=0.8, col=colors, pch=plotchar, lty=linetype, title="# Sources")
}

# Apply myanalyze.cvsdata() to all files
files <- c("170128-081913-Determinants.txt")
sapply(files, myanalyze.cvsdata)
