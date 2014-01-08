  # detach(data2); 
  # rm(list=setdiff(ls(), "saveoutput"));
saveoutput <- data.frame(index=-1:-1, type=-1:-45, dataset=-1:-45, featureset=-1:-45, ngram=-1:-45, classifier=-1:-45, zeros=-1:-45, ones=-1:-45, tp=-1:-45, fn=-1:-45, fp=-1:-45, tn=-1:-45);
saveoutput$index[1] = 1;
write.matrix(saveoutput, file="~/Dropbox/FLAIRS-FINALFILES/scriptoutput.txt", sep="\t");

  # ===============================================================================
  # START POS BOW with no Words, only other vars NOT POS or punc
  # ###############################################################################
  for (ngram in 2:2) {

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
write.matrix(saveoutput, file="~/Dropbox/FLAIRS-FINALFILES/scriptoutput.txt", sep="\t");
filename <- paste('~/Dropbox/FLAIRS-FINALFILES/sort',sortname,'Sort-',featureset,'features-',typename,'speakermvmts-split',(trainSize+1),'-ROC-',ngram,'gram.png', sep="");
write(paste('prepping matrices - ',ngram), file="~/Dropbox/FLAIRS-FINALFILES/testing123.csv", append=TRUE, sep="\t")
  # matrix2 <- create_matrix(data$Speech,  language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE); 
matrix3 <- create_matrix(data$POS_sentence, language="english", minDocFreq=1, maxDocFreq=Inf, minWordLength=1, maxWordLength=Inf, ngramLength=ngram, originalMatrix=NULL, removeNumbers=TRUE, removePunctuation=FALSE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE); 
matrix <- cbind(matrix3, data$num_lines_before, data$num_lines_after, data$anno_before, data$anno_after, data$num_lines_last_mvmt, data$max_repeated_word, data$count_uppercase_words, data$total_words);
write(paste('Done matrices, creating container - ',ngram), file="~/Dropbox/FLAIRS-FINALFILES/testing123.csv", append=TRUE, sep="\t")
container <-create_container(matrix, trainSet, trainSize=1:trainSize, testSize=(trainSize+1):3477, virgin=FALSE) ;
write(paste('Done container, training - ',ngram), file="~/Dropbox/FLAIRS-FINALFILES/testing123.csv", append=TRUE, sep="\t")
models <- train_models(container, algorithms=c("SVM", "MAXENT"));
write(paste('Done training, classifying - ',ngram), file="~/Dropbox/FLAIRS-FINALFILES/testing123.csv", append=TRUE, sep="\t")
results <- classify_models(container, models);
write(paste('Done classifying - ',ngram), file="~/Dropbox/FLAIRS-FINALFILES/testing123.csv", append=TRUE, sep="\t")
analytics <- create_analytics(container, results);
analytics@ensemble_summary;
analytics@label_summary;
for (i in 0:3) {
saveoutput$zeros[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[1];
saveoutput$ones[saveoutput$index[1]+i] <- analytics@label_summary$NUM_MANUALLY_CODED[2];
}
write.matrix(saveoutput, file="~/Dropbox/FLAIRS-FINALFILES/scriptoutput.txt", sep="\t");
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

write.matrix(saveoutput, file="~/Dropbox/FLAIRS-FINALFILES/scriptoutput.txt", sep="\t");
    tp1;fn1;fp1;tn1;
tp2;fn2;fp2;tn2;


    pred <- prediction(analytics@document_summary$MAXENTROPY_PROB, trainSet[(trainSize+1):3477]);
   perf <- performance(pred, measure="tpr", x.measure="fpr");
    plot(perf, col='green', lwd=2);
 
 dev.copy(png, filename)
 dev.off();
detach(data2);
write(paste('Done with POS BOW with NO Words BOWs, plus other features minus punc and POS - ',ngram), file="~/Dropbox/FLAIRS-FINALFILES/testing123.csv", append=TRUE, sep="\t")
rm(list=setdiff(ls(), "saveoutput"));
gc();
}


saveoutput;