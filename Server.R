library(shiny)
library(tm)
library(stringr)

load("txt_uni_word.RData")
load("txt_bi_words.RData")
load("txt_tri_words.RData")
load("txt_tetra_words.RData")

CleanInputString <- function(InputString)
{
  InputString <- iconv(InputString, "latin1", "ASCII", sub=" ");
  InputStringCrps <- VCorpus(VectorSource(InputString))
  InputStringCrps <- tm_map(InputStringCrps, content_transformer(tolower))
  InputStringCrps <- tm_map(InputStringCrps, removePunctuation)
  InputStringCrps <- tm_map(InputStringCrps, removeNumbers)
  InputStringCrps <- tm_map(InputStringCrps, stripWhitespace)
  CleanInputString <- as.character(InputStringCrps[[1]])
  CleanInputString <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", CleanInputString)
  
  if (nchar(CleanInputString) > 0) {
    return(CleanInputString); 
  } else {
    return("");
  }
}

PredictNextWord <- function(InputString) {
  InputStr <- unlist(strsplit(InputString, split=" "));
  InputStrLen <- length(InputStr);
  IsMatch <- FALSE
  
  #4Gram
  if (InputStrLen >=3 & !IsMatch)  {
    StrToSearch <- paste(InputStr[(InputStrLen-2):InputStrLen], collapse=" "); 
    StrToSearch <- paste("^",StrToSearch, sep = "");
    MatchString <- tetra_corpus_freq[grep(StrToSearch, tetra_corpus_freq$word), ];
    
    if (length(MatchString[,1]) > 0) {
      NextWordMatch <- MatchString[1,1];
      MatchGram <- 4;
      IsMatch <- TRUE;
    }
  }
  
  #3Gram
  if (InputStrLen >= 2 & !IsMatch)  {
    StrToSearch <- paste(InputStr[(InputStrLen-1):InputStrLen], collapse=" "); 
    StrToSearch <- paste("^",StrToSearch, sep = "");
    MatchString <- tri_corpus_freq[grep(StrToSearch, tri_corpus_freq$word), ];
    
    if (length(MatchString[,1]) > 0) {
      NextWordMatch <- MatchString[1,1];
      MatchGram <- 3;
      IsMatch <- TRUE;
    }
  }
  
  #2Gram
  if (InputStrLen >= 1 & !IsMatch)  {
    StrToSearch <- InputStr[InputStrLen]; 
    StrToSearch <- paste("^",StrToSearch, sep = "");
    MatchString <- bi_corpus_freq[grep(StrToSearch, bi_corpus_freq$word), ];
    
    if (length(MatchString[,1]) > 0) {
      NextWordMatch <- MatchString[1,1];
      MatchGram <- 2;
      IsMatch <- TRUE;
    }
  }
  
  #1Gram
  if (InputStrLen > 0 & !IsMatch)  {
    NextWordMatch <- uni_corpus_freq$word[1];
    MatchGram <- 1;
    IsMatch <- TRUE;
  }
  
  if (InputStrLen > 0 & IsMatch) {
    FinalResult <- data.frame(InputStr = InputString, PredictedWord = word(NextWordMatch, -1), MatchStr = NextWordMatch, GramType = MatchGram);
    return(FinalResult);
  } else {
    FinalResult <- data.frame(InputStr = "", PredictedWord = "", MatchStr = "", GramType = "");
    return(FinalResult);
  } 
}

# Predict the next word 
shinyServer(function(input, output) {
  observeEvent(input$do, {
    CleanInputStr <- CleanInputString(input$InputString);
    PredictDF <- PredictNextWord(CleanInputStr);
    output$PredictedWord <- renderText({as.character(PredictDF[1,2])});
  })
}
)
