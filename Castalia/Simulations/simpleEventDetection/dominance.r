library(relaimpo);

pdf("dominance-analysis.pdf")

message("\n");
message("=== Running Dominance Analysis ===\n");

runs <- read.csv("170126-165513-Determinants.txt", sep="\t", header=T);
row.names(runs) <- runs$RUN;
runs <- subset(runs, select=c(-RUN));

message("\n");
message("=== Calculating dominance for ACCURACY ===\n");

crf <- calc.relimp(ACCURACY~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(ACCURACY~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);

message("\n");
message("=== Calculating dominance for PRECISION ===\n");

crf <- calc.relimp(PRECISION~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(PRECISION~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);

message("\n");
message("=== Calculating dominance for SPECIFICITY ===\n");

crf <- calc.relimp(SPECIFICITY~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(SPECIFICITY~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);

message("\n");
message("=== Calculating dominance for F1 SCORE ===\n");

crf <- calc.relimp(F1~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(F1~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);

message("\n");
message("=== Calculating dominance for F2 SCORE ===\n");

crf <- calc.relimp(F2~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(F2~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);

message("\n");
message("=== Calculating dominance for F0.5 SCORE ===\n");

crf <- calc.relimp(F05~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(F05~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);

message("\n");
message("=== Calculating dominance for TRUE POSITIVE RATE ===\n");

crf <- calc.relimp(TRUE_POSITIVE_RATE~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(TRUE_POSITIVE_RATE~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);

message("\n");
message("=== Calculating dominance for FALSE POSITIVE RATE ===\n");

crf <- calc.relimp(FALSE_POSITIVE_RATE~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(FALSE_POSITIVE_RATE~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);

message("\n");
message("=== Calculating dominance for YOUDEN ===\n");

crf <- calc.relimp(YOUDEN_INDEX~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs, type = c("lmg"), rela = TRUE );

crf;
linmod <- lm(YOUDEN_INDEX~NUMSOURCES+DISTRIBUTION+SAMPLEINTERVAL+SENSINGDISTANCE,runs);
crlm <- calc.relimp(linmod, type = c("lmg"), rela = TRUE );

plot(crlm);
