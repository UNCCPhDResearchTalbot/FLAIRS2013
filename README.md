FLAIRS2013
==========

FLAIRS 2013 Conference Paper Code

run file by typing:  source("~/Dropbox/FLAIRS-FINALFILES/runclassify.R");
within R.app which has NLTK installed & dependent libraries too
Different .R files run against different featuresets which are stored in the .txt files
Master file is in the .xlsx file for all statistics / features used

Several files included which contain details on the Hamlet scene for different types of movements and are used in the below script.
Outputs are printed to .png files for the diagrams of the ROC curves, scriptoutput.txt (monitor run status), and testing123.csv (results) for the output of each learning session

Charts for PDF.xlsx has summary information from all the runs for extraction into paper
Movement Counts.xlsx has summary movement count information for the annotations within the scene

FILE:
=======================================================

rm(list=ls());
saveoutput <- data.frame(index=1:1, type=1:45, dataset=1:45, featureset=1:45, ngram=1:45, classifier=1:45, zeros=1:45, ones=1:45, tp=1:45, fn=1:45, fp=1:45, tn=1:45);
saveoutput$index[1] = 1;
  # ===============================================================================
  # START POS BOW with Words BOW
  # ###############################################################################
  for (ngram in 1:5) {
rm(list=setdiff(ls(), "saveoutput"));
data2 <- read.table("~/Dropbox/FLAIRS-FINALFILES/ALLSpeech-nocharpostns-posbow.txt", sep = "\t", header=TRUE);
attach(data2);

data <- data2[order(sortany),];
trainSet <- data$speaker_action_only;
trainSize <- 1806;
  # ngram <- 1;
typename <- 'any';
sortname <-'any';
featureset <-'poswithwordsonly';
for (i in 0:3) {
saveoutput$type[saveoutput$index[1]+i] <- typename;
saveoutput$dataset[saveoutput$index[1]+i] <- 'Small';
saveoutput$featureset[saveoutput$index[1]+i] <- featureset;
saveoutput$ngram[saveoutput$index[1]+i] <- ngram;
}

filename <- paste('~/Dropbox/FLAIRS-FINALFILES/sort',sortname,'Sort-',featureset,'features-',typename,'speakermvmts-split',(trainSize+1),'-ROC-',ngram,'gram.png', sep="");

matrix2 <- create_matrix(data$Speech,  language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE);
matrix3 <- create_matrix(data$POS_sentence, language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=FALSE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE);
matrix <- cbind(matrix2, matrix3);  # , data$num_lines_before, data$num_lines_after, data$anno_before, data$anno_after, data$num_lines_last_mvmt, data$max_repeated_word, data$count_uppercase_words, data$period, data$comma, data$semicolon, data$hyphen, data$total_words, data$question, data$apostrophe, data$exclamation, data$WRB, data$WPSS, data$WP, data$WDT, data$VBZ, data$VBP, data$VBN, data$VBG, data$VBD, data$VB, data$UH, data$TO, data$SYM, data$RP, data$RBS, data$RBR, data$RB, data$PRPSS, data$PRP, data$POS, data$PDT, data$NNS, data$NNPS, data$NNP, data$NN, data$MD, data$LS, data$JJS, data$JJR, data$JJ, data$IN, data$FW, data$EX, data$DT, data$CD, data$CC );

container <-create_container(matrix, trainSet, trainSize=1:trainSize, testSize=(trainSize+1):3477, virgin=FALSE) ;
models <- train_models(container, algorithms=c("SVM", "MAXENT", "BOOSTING", "RF"));
results <- classify_models(container, models);
analytics <- create_analytics(container, results);
analytics@ensemble_summary;
analytics@label_summary;
for (i in 0:3) {
saveoutput$zeros[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[1];
saveoutput$ones[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[2];
}
x <- as.numeric(analytics@document_summary$MANUAL_CODE);
y <- as.numeric(analytics@document_summary$MAXENTROPY_LABEL);
z <- x-y;
fp1 <- length(z[z==-2]);
fn1 <- length(z[z==0]);
tp1 <- length(y[y==2]) - fp1;
tn1 <- length(y[y==1]) - fn1;
saveoutput$tp[saveoutput$index[1]] <- tp1;
saveoutput$tn[saveoutput$index[1]] <- tn1;
saveoutput$fp[saveoutput$index[1]] <- fp1;
saveoutput$fn[saveoutput$index[1]] <- fn1;
saveoutput$classifier[saveoutput$index[1]] <- 'MaxEnt';
saveoutput$index[1] <- saveoutput$index[1]+1;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$SVM_LABEL);
 z <- x-y;
 fp2 <- length(z[z==-2]);
 fn2 <- length(z[z==0]);
 tp2 <- length(y[y==2]) - fp2;
 tn2 <- length(y[y==1]) - fn2;
 saveoutput$tp[saveoutput$index[1]] <- tp2;
saveoutput$tn[saveoutput$index[1]] <- tn2;
saveoutput$fp[saveoutput$index[1]] <- fp2;
saveoutput$fn[saveoutput$index[1]] <- fn2;
saveoutput$classifier[saveoutput$index[1]] <- 'SVM';
saveoutput$index[1] <- saveoutput$index[1]+1;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$LOGITBOOST_LABEL);
 z <- x-y;
 fp3 <- length(z[z==-2]);
 fn3 <- length(z[z==0]);
 tp3 <- length(y[y==2]) - fp3;
 tn3 <- length(y[y==1]) - fn3;
 saveoutput$tp[saveoutput$index[1]] <- tp3;
saveoutput$tn[saveoutput$index[1]] <- tn3;
saveoutput$fp[saveoutput$index[1]] <- fp3;
saveoutput$fn[saveoutput$index[1]] <- fn3;
saveoutput$classifier[saveoutput$index[1]] <- 'Boosting';
saveoutput$index[1] <- saveoutput$index[1]+1;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$FORESTS_LABEL);
 z <- x-y;
 fp4 <- length(z[z==-2]);
 fn4 <- length(z[z==0]);
 tp4 <- length(y[y==2]) - fp4;
 tn4 <- length(y[y==1]) - fn4;
 saveoutput$tp[saveoutput$index[1]] <- tp4;
saveoutput$tn[saveoutput$index[1]] <- tn4;
saveoutput$fp[saveoutput$index[1]] <- fp4;
saveoutput$fn[saveoutput$index[0]] <- fn4;
saveoutput$classifier[saveoutput$index[1]] <- 'Random Forests';
saveoutput$index[1] <- saveoutput$index[1]+1;
 tp1;fn1;fp1;tn1;
tp2;fn2;fp2;tn2;
tp3;fn3;fp3;tn3;
tp4;fn4;fp4;tn4;

 pred <- prediction(analytics@document_summary$MAXENTROPY_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, col='green', lwd=2);
 pred <- prediction(analytics@document_summary$SVM_PROB, trainSet[(trainSize+1):3477]);
 perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='red' , lwd=2);
 pred <- prediction(analytics@document_summary$LOGITBOOST_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='blue' , lwd=2);
 pred <- prediction(analytics@document_summary$FORESTS_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='magenta' , lwd=2);
 dev.copy(png, filename)
 dev.off();
detach(data2);
}

  # ===============================================================================
  # START POS BOW with no Words, only other vars NOT POS or punc
  # ###############################################################################
  for (ngram in 1:5) {
rm(list=setdiff(ls(), "saveoutput"));
data2 <- read.table("~/Dropbox/FLAIRS-FINALFILES/ALLSpeech-nocharpostns-posbow.txt", sep = "\t", header=TRUE);
attach(data2);

data <- data2[order(sortany),];
trainSet <- data$speaker_action_only;
trainSize <- 1806;
  # ngram <- 1;
typename <- 'any';
sortname <-'any';
featureset <-'posbow-nowords-nopunc-nopos';
for (i in 0:3) {
saveoutput$type[saveoutput$index[1]+i] <- typename;
saveoutput$dataset[saveoutput$index[1]+i] <- 'Small';
saveoutput$featureset[saveoutput$index[1]+i] <- featureset;
saveoutput$ngram[saveoutput$index[1]+i] <- ngram;
}
filename <- paste('~/Dropbox/FLAIRS-FINALFILES/sort',sortname,'Sort-',featureset,'features-',typename,'speakermvmts-split',(trainSize+1),'-ROC-',ngram,'gram.png', sep="");

  # matrix2 <- create_matrix(data$Speech,  language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE);
matrix3 <- create_matrix(data$POS_sentence, language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=FALSE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE);
matrix <- cbind(matrix3, data$num_lines_before, data$num_lines_after, data$anno_before, data$anno_after, data$num_lines_last_mvmt, data$max_repeated_word, data$count_uppercase_words, data$total_words);

container <-create_container(matrix, trainSet, trainSize=1:trainSize, testSize=(trainSize+1):3477, virgin=FALSE) ;
models <- train_models(container, algorithms=c("SVM", "MAXENT", "BOOSTING", "RF"));
results <- classify_models(container, models);
analytics <- create_analytics(container, results);
analytics@ensemble_summary;
analytics@label_summary;
for (i in 0:3) {
saveoutput$zeros[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[1];
saveoutput$ones[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[2];
}
x <- as.numeric(analytics@document_summary$MANUAL_CODE);
y <- as.numeric(analytics@document_summary$MAXENTROPY_LABEL);
z <- x-y;
fp1 <- length(z[z==-2]);
fn1 <- length(z[z==0]);
tp1 <- length(y[y==2]) - fp1;
tn1 <- length(y[y==1]) - fn1;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$SVM_LABEL);
 z <- x-y;
 fp2 <- length(z[z==-2]);
 fn2 <- length(z[z==0]);
 tp2 <- length(y[y==2]) - fp2;
 tn2 <- length(y[y==1]) - fn2;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$LOGITBOOST_LABEL);
 z <- x-y;
 fp3 <- length(z[z==-2]);
 fn3 <- length(z[z==0]);
 tp3 <- length(y[y==2]) - fp3;
 tn3 <- length(y[y==1]) - fn3;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$FORESTS_LABEL);
 z <- x-y;
 fp4 <- length(z[z==-2]);
 fn4 <- length(z[z==0]);
 tp4 <- length(y[y==2]) - fp4;
 tn4 <- length(y[y==1]) - fn4;
 saveoutput$tp[saveoutput$index[1]] <- tp1;
saveoutput$tn[saveoutput$index[1]] <- tn1;
saveoutput$fp[saveoutput$index[1]] <- fp1;
saveoutput$fn[saveoutput$index[1]] <- fn1;
saveoutput$classifier[saveoutput$index[1]] <- 'MaxEnt';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp2;
saveoutput$tn[saveoutput$index[1]] <- tn2;
saveoutput$fp[saveoutput$index[1]] <- fp2;
saveoutput$fn[saveoutput$index[1]] <- fn2;
saveoutput$classifier[saveoutput$index[1]] <- 'SVM';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp3;
saveoutput$tn[saveoutput$index[1]] <- tn3;
saveoutput$fp[saveoutput$index[1]] <- fp3;
saveoutput$fn[saveoutput$index[1]] <- fn3;
saveoutput$classifier[saveoutput$index[1]] <- 'Boosting';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp4;
saveoutput$tn[saveoutput$index[1]] <- tn4;
saveoutput$fp[saveoutput$index[1]] <- fp4;
saveoutput$fn[saveoutput$index[0]] <- fn4;
saveoutput$classifier[saveoutput$index[1]] <- 'Random Forests';
saveoutput$index[1] <- saveoutput$index[1]+1;
 tp1;fn1;fp1;tn1;
tp2;fn2;fp2;tn2;
tp3;fn3;fp3;tn3;
tp4;fn4;fp4;tn4;

 pred <- prediction(analytics@document_summary$MAXENTROPY_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, col='green', lwd=2);
 pred <- prediction(analytics@document_summary$SVM_PROB, trainSet[(trainSize+1):3477]);
 perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='red' , lwd=2);
 pred <- prediction(analytics@document_summary$LOGITBOOST_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='blue' , lwd=2);
 pred <- prediction(analytics@document_summary$FORESTS_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='magenta' , lwd=2);
 dev.copy(png, filename)
 dev.off();
detach(data2);
}

  # ===============================================================================
  # START POS BOW with Words, only other vars NOT POS or punc
  # ###############################################################################
  for (ngram in 1:5) {
rm(list=setdiff(ls(), "saveoutput"));
data2 <- read.table("~/Dropbox/FLAIRS-FINALFILES/ALLSpeech-nocharpostns-posbow.txt", sep = "\t", header=TRUE);
attach(data2);

data <- data2[order(sortany),];
trainSet <- data$speaker_action_only;
trainSize <- 1806;
  # ngram <- 1;
typename <- 'any';
sortname <-'any';
featureset <-'posbow-withwords-nopunc-nopos';
for (i in 0:3) {
saveoutput$type[saveoutput$index[1]+i] <- typename;
saveoutput$dataset[saveoutput$index[1]+i] <- 'Small';
saveoutput$featureset[saveoutput$index[1]+i] <- featureset;
saveoutput$ngram[saveoutput$index[1]+i] <- ngram;
}
filename <- paste('~/Dropbox/FLAIRS-FINALFILES/sort',sortname,'Sort-',featureset,'features-',typename,'speakermvmts-split',(trainSize+1),'-ROC-',ngram,'gram.png', sep="");

 matrix2 <- create_matrix(data$Speech,  language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE);
matrix3 <- create_matrix(data$POS_sentence, language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=FALSE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE);
matrix <- cbind(matrix2, matrix3, data$num_lines_before, data$num_lines_after, data$anno_before, data$anno_after, data$num_lines_last_mvmt, data$max_repeated_word, data$count_uppercase_words, data$total_words);

container <-create_container(matrix, trainSet, trainSize=1:trainSize, testSize=(trainSize+1):3477, virgin=FALSE) ;
models <- train_models(container, algorithms=c("SVM", "MAXENT", "BOOSTING", "RF"));
results <- classify_models(container, models);
analytics <- create_analytics(container, results);
analytics@ensemble_summary;
analytics@label_summary;
for (i in 0:3) {
saveoutput$zeros[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[1];
saveoutput$ones[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[2];
}
x <- as.numeric(analytics@document_summary$MANUAL_CODE);
y <- as.numeric(analytics@document_summary$MAXENTROPY_LABEL);
z <- x-y;
fp1 <- length(z[z==-2]);
fn1 <- length(z[z==0]);
tp1 <- length(y[y==2]) - fp1;
tn1 <- length(y[y==1]) - fn1;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$SVM_LABEL);
 z <- x-y;
 fp2 <- length(z[z==-2]);
 fn2 <- length(z[z==0]);
 tp2 <- length(y[y==2]) - fp2;
 tn2 <- length(y[y==1]) - fn2;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$LOGITBOOST_LABEL);
 z <- x-y;
 fp3 <- length(z[z==-2]);
 fn3 <- length(z[z==0]);
 tp3 <- length(y[y==2]) - fp3;
 tn3 <- length(y[y==1]) - fn3;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$FORESTS_LABEL);
 z <- x-y;
 fp4 <- length(z[z==-2]);
 fn4 <- length(z[z==0]);
 tp4 <- length(y[y==2]) - fp4;
 tn4 <- length(y[y==1]) - fn4;
  saveoutput$tp[saveoutput$index[1]] <- tp1;
saveoutput$tn[saveoutput$index[1]] <- tn1;
saveoutput$fp[saveoutput$index[1]] <- fp1;
saveoutput$fn[saveoutput$index[1]] <- fn1;
saveoutput$classifier[saveoutput$index[1]] <- 'MaxEnt';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp2;
saveoutput$tn[saveoutput$index[1]] <- tn2;
saveoutput$fp[saveoutput$index[1]] <- fp2;
saveoutput$fn[saveoutput$index[1]] <- fn2;
saveoutput$classifier[saveoutput$index[1]] <- 'SVM';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp3;
saveoutput$tn[saveoutput$index[1]] <- tn3;
saveoutput$fp[saveoutput$index[1]] <- fp3;
saveoutput$fn[saveoutput$index[1]] <- fn3;
saveoutput$classifier[saveoutput$index[1]] <- 'Boosting';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp4;
saveoutput$tn[saveoutput$index[1]] <- tn4;
saveoutput$fp[saveoutput$index[1]] <- fp4;
saveoutput$fn[saveoutput$index[0]] <- fn4;
saveoutput$classifier[saveoutput$index[1]] <- 'Random Forests';
saveoutput$index[1] <- saveoutput$index[1]+1;
 tp1;fn1;fp1;tn1;
tp2;fn2;fp2;tn2;
tp3;fn3;fp3;tn3;
tp4;fn4;fp4;tn4;

 pred <- prediction(analytics@document_summary$MAXENTROPY_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, col='green', lwd=2);
 pred <- prediction(analytics@document_summary$SVM_PROB, trainSet[(trainSize+1):3477]);
 perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='red' , lwd=2);
 pred <- prediction(analytics@document_summary$LOGITBOOST_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='blue' , lwd=2);
 pred <- prediction(analytics@document_summary$FORESTS_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='magenta' , lwd=2);
 dev.copy(png, filename)
 dev.off();
detach(data2);
}

  # ===============================================================================
  # No Words - only other attribs
  # ###############################################################################
 
rm(list=setdiff(ls(), "saveoutput"));
data2 <- read.table("~/Dropbox/FLAIRS-FINALFILES/ALLSpeech-nocharpostns-posbow.txt", sep = "\t", header=TRUE);
attach(data2);

data <- data2[order(sortany),];
trainSet <- data$speaker_action_only;
trainSize <- 1806;
ngram <- 1;
typename <- 'any';
sortname <-'any';
featureset <-'nowords-onlyattribs';
for (i in 0:3) {
saveoutput$type[saveoutput$index[1]+i] <- typename;
saveoutput$dataset[saveoutput$index[1]+i] <- 'Small';
saveoutput$featureset[saveoutput$index[1]+i] <- featureset;
saveoutput$ngram[saveoutput$index[1]+i] <- ngram;
}
filename <- paste('~/Dropbox/FLAIRS-FINALFILES/sort',sortname,'Sort-',featureset,'features-',typename,'speakermvmts-split',(trainSize+1),'-ROC-',ngram,'gram.png', sep="");

  # matrix2 <- create_matrix(data$Speech,  language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE);
  # matrix3 <- create_matrix(data$POS_sentence, language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=FALSE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE);
tempmatrix <- matrix(nrow=3477, ncol=1);
tempmatrix[,1] <- "A";
matrix1 <- create_matrix(tempmatrix, language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE);
matrix <- cbind(matrix1, data$num_lines_before, data$num_lines_after, data$anno_before, data$anno_after, data$num_lines_last_mvmt, data$max_repeated_word, data$count_uppercase_words, data$total_words);

container <-create_container(matrix, trainSet, trainSize=1:trainSize, testSize=(trainSize+1):3477, virgin=FALSE) ;
models <- train_models(container, algorithms=c("SVM", "MAXENT", "BOOSTING", "RF"));
results <- classify_models(container, models);
analytics <- create_analytics(container, results);
analytics@ensemble_summary;
analytics@label_summary;
for (i in 0:3) {
saveoutput$zeros[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[1];
saveoutput$ones[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[2];
}
x <- as.numeric(analytics@document_summary$MANUAL_CODE);
y <- as.numeric(analytics@document_summary$MAXENTROPY_LABEL);
z <- x-y;
fp1 <- length(z[z==-2]);
fn1 <- length(z[z==0]);
tp1 <- length(y[y==2]) - fp1;
tn1 <- length(y[y==1]) - fn1;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$SVM_LABEL);
 z <- x-y;
 fp2 <- length(z[z==-2]);
 fn2 <- length(z[z==0]);
 tp2 <- length(y[y==2]) - fp2;
 tn2 <- length(y[y==1]) - fn2;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$LOGITBOOST_LABEL);
 z <- x-y;
 fp3 <- length(z[z==-2]);
 fn3 <- length(z[z==0]);
 tp3 <- length(y[y==2]) - fp3;
 tn3 <- length(y[y==1]) - fn3;
 x <- as.numeric(analytics@document_summary$MANUAL_CODE);
 y <- as.numeric(analytics@document_summary$FORESTS_LABEL);
 z <- x-y;
 fp4 <- length(z[z==-2]);
 fn4 <- length(z[z==0]);
 tp4 <- length(y[y==2]) - fp4;
 tn4 <- length(y[y==1]) - fn4;
   saveoutput$tp[saveoutput$index[1]] <- tp1;
saveoutput$tn[saveoutput$index[1]] <- tn1;
saveoutput$fp[saveoutput$index[1]] <- fp1;
saveoutput$fn[saveoutput$index[1]] <- fn1;
saveoutput$classifier[saveoutput$index[1]] <- 'MaxEnt';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp2;
saveoutput$tn[saveoutput$index[1]] <- tn2;
saveoutput$fp[saveoutput$index[1]] <- fp2;
saveoutput$fn[saveoutput$index[1]] <- fn2;
saveoutput$classifier[saveoutput$index[1]] <- 'SVM';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp3;
saveoutput$tn[saveoutput$index[1]] <- tn3;
saveoutput$fp[saveoutput$index[1]] <- fp3;
saveoutput$fn[saveoutput$index[1]] <- fn3;
saveoutput$classifier[saveoutput$index[1]] <- 'Boosting';
saveoutput$index[1] <- saveoutput$index[1]+1;
 saveoutput$tp[saveoutput$index[1]] <- tp4;
saveoutput$tn[saveoutput$index[1]] <- tn4;
saveoutput$fp[saveoutput$index[1]] <- fp4;
saveoutput$fn[saveoutput$index[0]] <- fn4;
saveoutput$classifier[saveoutput$index[1]] <- 'Random Forests';
saveoutput$index[1] <- saveoutput$index[1]+1;
 tp1;fn1;fp1;tn1;
tp2;fn2;fp2;tn2;
tp3;fn3;fp3;tn3;
tp4;fn4;fp4;tn4;

 pred <- prediction(analytics@document_summary$MAXENTROPY_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, col='green', lwd=2);
 pred <- prediction(analytics@document_summary$SVM_PROB, trainSet[(trainSize+1):3477]);
 perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='red' , lwd=2);
 pred <- prediction(analytics@document_summary$LOGITBOOST_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='blue' , lwd=2);
 pred <- prediction(analytics@document_summary$FORESTS_PROB, trainSet[(trainSize+1):3477]);
perf <- performance(pred, measure="tpr", x.measure="fpr");
 plot(perf, add=T, col='magenta' , lwd=2);
 dev.copy(png, filename)
 dev.off();
detach(data2);
}

saveoutput;