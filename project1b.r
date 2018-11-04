#CHANGE WORKING DIRECTORY
setwd("~/NJIT/Fall18/CS636/Project/HTMLs")

#REQUIRED LIBRARIES
library(bitops)
library(RCurl)
library(XML)
library(stringr)
library(httr)

#SUPPRESS WARNINGS IF ANY
options(warn=-1)

#SET GLOBAL VARIABLES
lowerLimit = 1
upperLimit = 282
lockBinding("upperLimit", globalenv())
lockBinding("lowerLimit", globalenv())


#FUNCTIONS TO FETCH REQUIRED DATA
#Fetch DOI
fetchDOI = function(a){
  s = "meta.*name.*citation\\_doi"
  d = grep(s, a, value=TRUE)
  s = "\\d+.*\\d"
  d = str_extract(d, s)
  return(d)
  #DOI = d
}

#Fetch Title
fetchTitle = function(a){
  s = "div.*class.*title"
  d = grep(s, a, value=TRUE)
  s = "\\>\\w+.*\\<"
  d = str_extract(d, s)
  d = str_replace_all(d, "\\<\\/p\\>", "")
  d = str_replace_all(d, "\\<", "")
  d = str_replace_all(d, "\\>", "")
  return(d)
}


#Fetch Author
fetchAuthor = function(a){
  s = "p.*class.*author.*author\\-name"
  d = grep(s, a, value=TRUE)
  s = "\\>\\w+.*\\<\\/span\\>"
  d = str_extract_all(d, s, "")
  d = str_replace_all(d, "\\<\\/span\\>", "")
  d = str_replace_all(d, "\\>", "")
  return(d)
}


#Fetch Author Affiliations
fetchAuthorAffiliations = function(a){
  s = "p.*class.*aff.*name.*aff.*"
  d = grep(s, a, value=TRUE)
  s = "\\>[A-z]+.*\\<\\/p\\>"
  d = str_extract_all(d, s)
  d = str_replace_all(d, "\\<\\/p\\>", "")
  d = str_replace_all(d, "\\>", "")
  return(d)
}


#Fetch Corresponding Author
fetchCorrespondingAuthor = function(a){
  s = "div.*Send\\scorrespondence\\sto"
  d = grep(s, a, value=TRUE)
  s = "Send.*E-mail"
  d = str_extract_all(d, s)
  d = str_replace_all(d, "E-mail", "")
  return(d)
}


#Fetch Corresponding Author E-mail
fetchCorrespondingAuthorEmail = function(a){
  s = "E\\-mail.*mailto"
  d = grep(s, a, value=TRUE)
  s = "\\w+\\@\\w+.\\w.*"
  d = str_extract_all(d, s)
  d = str_replace(d, "\\w.*\"\\>", "")
  d = str_replace(d, "\\<\\/a\\>", "")
  return(d)
}


#Fetch Publish Date
fetchPublishDate = function(a){
  s = "meta.*name.*citation\\_date"
  d = grep(s, a, value=TRUE)
  s = "\\d+.*\\d+"
  d = str_extract(d, s)
  return(d)
}


#Fetch Abstract
fetchAbstract = function(a){
  s = "p.*name.*Abstract"
  d = grep(s, a)
  d = d + 1
  d = str_replace_all(a[d], "<\\w>", "")
  d = str_replace_all(d, "<\\/\\w>", "")
  return(d)
}


#Fetch Keywords
fetchKeywords = function(a){
  s = "\\<p.*Keywords"
  d = grep(s, a, value=TRUE)
  s = "\\>\\w.*\\<\\/p\\>"
  d = str_extract(d, s)
  d = str_replace_all(d, "\\<\\/\\w\\>", "")
  d = str_replace_all(d, "\\>", "")
  return(d)
}


#Fetch Fulltext
#d = readLines(filename)

#Main function

#CREATING REQUIRED DATA FRAMES
writeDataFrame = data.frame()
iterateDataFrame = data.frame()

#CREATING A LOOP COUNTER
counter = lowerLimit

#LOOP OVER ALL THE FILES
while(counter <= upperLimit){

  #READ THE FILE
  readFilename = paste(counter, "paperVisited.html", sep = "-")
  fileBufferRead = readLines(readFilename)

  #LIMITED TO ONLY THE FIRST RESEARCH PAPAER DUE TO DIFFERENCE IN THE STRUCTURE OF THE PAPER
  if(counter == 1){

    #Getting DOI
    DOI = fetchDOI(fileBufferRead)

    #Getting TITLE
    TITLE = fetchTitle(fileBufferRead)

    #Getting AUTHOR
    AUTHOR = fetchAuthor(fileBufferRead)

    #Getting AUTHOR AFFILIATIONS
    AUTHORAFFILIATION = fetchAuthorAffiliations(fileBufferRead)

    #Getting CORRESPONDING AUTHOR
    CORRESPONDINGAUTHOR = fetchCorrespondingAuthor(fileBufferRead)

    #Getting CORRESPONDING AUTHOR EMAIL
    CORRESPONDINGAUTHOREMAIL = fetchCorrespondingAuthorEmail(fileBufferRead)

    #Getting PUBLISH DATE
    PUBLISHDATE = fetchPublishDate(fileBufferRead)

    #Getting ABSTRACT
    s = "div.*class\\=\"body"
    d = grep(s, fileBufferRead)
    d = d + 1
    d = fileBufferRead[d]
    d = str_replace_all(d, "<\\w+>", "")
    d = str_replace_all(d, "<\\/\\w+>", "")
    ABSTRACT = d

    #Getting KEYWORDS
    KEYWORDS = NA

    #Getting FULLTEXT
    FULLTEXT = fileBufferRead

    #GENERATING EQUAL ROWS
    maxLength = length(FULLTEXT)
    lengthDOI = length(DOI)
    lengthTITLE = length(TITLE)
    lengthAUTHOR = length(AUTHOR)
    lengthAUTHORAFFILIATION = length(AUTHORAFFILIATION)
    lengthCORRESPONDINGAUTHOR = length(CORRESPONDINGAUTHOR)
    lengthCORRESPONDINGAUTHOREMAIL = length(CORRESPONDINGAUTHOREMAIL)
    lengthPUBLISHDATE = length(PUBLISHDATE)
    lengthABSTRACT = length(ABSTRACT)
    lengthKEYWORDS = length(KEYWORDS)

    toFill = maxLength - lengthDOI
    fillVec = rep("NA", toFill)
    DOI = c(DOI, fillVec)

    toFill = maxLength - lengthTITLE
    fillVec = rep("NA", toFill)
    TITLE = c(TITLE, fillVec)

    toFill = maxLength - lengthAUTHOR
    fillVec = rep("NA", toFill)
    AUTHOR = c(AUTHOR, fillVec)

    toFill = maxLength - lengthAUTHORAFFILIATION
    fillVec = rep("NA", toFill)
    AUTHORAFFILIATION = c(AUTHORAFFILIATION, fillVec)

    toFill = maxLength - lengthCORRESPONDINGAUTHOR
    fillVec = rep("NA", toFill)
    CORRESPONDINGAUTHOR = c(CORRESPONDINGAUTHOR, fillVec)

    toFill = maxLength - lengthCORRESPONDINGAUTHOREMAIL
    fillVec = rep("NA", toFill)
    CORRESPONDINGAUTHOREMAIL = c(CORRESPONDINGAUTHOREMAIL, fillVec)

    toFill = maxLength - lengthPUBLISHDATE
    fillVec = rep("NA", toFill)
    PUBLISHDATE = c(PUBLISHDATE, fillVec)

    toFill = maxLength - lengthABSTRACT
    fillVec = rep("NA", toFill)
    ABSTRACT = c(ABSTRACT, fillVec)

    toFill = maxLength - lengthKEYWORDS
    fillVec = rep("NA", toFill)
    KEYWORDS = c(KEYWORDS, fillVec)

  }

  #Getting DOI
  DOI = fetchDOI(fileBufferRead)


  #Getting TITLE
  TITLE = fetchTitle(fileBufferRead)


  #Getting AUTHOR
  AUTHOR = fetchAuthor(fileBufferRead)


  #Getting AUTHOR AFFILIATIONS
  AUTHORAFFILIATION = fetchAuthorAffiliations(fileBufferRead)


  #Getting CORRESPONDING AUTHOR
  CORRESPONDINGAUTHOR = fetchCorrespondingAuthor(fileBufferRead)


  #Getting CORRESPONDING AUTHOR EMAIL
  CORRESPONDINGAUTHOREMAIL = fetchCorrespondingAuthorEmail(fileBufferRead)


  #Getting PUBLISH DATE
  PUBLISHDATE = fetchPublishDate(fileBufferRead)


  #Getting ABSTRACT
  ABSTRACT = fetchAbstract(fileBufferRead)


  #Getting KEYWORDS
  KEYWORDS = fetchKeywords(fileBufferRead)


  #Getting FULLTEXT
  FULLTEXT = fileBufferRead

  #GENERATING EQUAL ROWS
  maxLength = length(FULLTEXT)
  lengthDOI = length(DOI)
  lengthTITLE = length(TITLE)
  lengthAUTHOR = length(AUTHOR)
  lengthAUTHORAFFILIATION = length(AUTHORAFFILIATION)
  lengthCORRESPONDINGAUTHOR = length(CORRESPONDINGAUTHOR)
  lengthCORRESPONDINGAUTHOREMAIL = length(CORRESPONDINGAUTHOREMAIL)
  lengthPUBLISHDATE = length(PUBLISHDATE)
  lengthABSTRACT = length(ABSTRACT)
  lengthKEYWORDS = length(KEYWORDS)

  toFill = maxLength - lengthDOI
  fillVec = rep("NA", toFill)
  DOI = c(DOI, fillVec)

  toFill = maxLength - lengthTITLE
  fillVec = rep("NA", toFill)
  TITLE = c(TITLE, fillVec)

  toFill = maxLength - lengthAUTHOR
  fillVec = rep("NA", toFill)
  AUTHOR = c(AUTHOR, fillVec)

  toFill = maxLength - lengthAUTHORAFFILIATION
  fillVec = rep("NA", toFill)
  AUTHORAFFILIATION = c(AUTHORAFFILIATION, fillVec)

  toFill = maxLength - lengthCORRESPONDINGAUTHOR
  fillVec = rep("NA", toFill)
  CORRESPONDINGAUTHOR = c(CORRESPONDINGAUTHOR, fillVec)

  toFill = maxLength - lengthCORRESPONDINGAUTHOREMAIL
  fillVec = rep("NA", toFill)
  CORRESPONDINGAUTHOREMAIL = c(CORRESPONDINGAUTHOREMAIL, fillVec)

  toFill = maxLength - lengthPUBLISHDATE
  fillVec = rep("NA", toFill)
  PUBLISHDATE = c(PUBLISHDATE, fillVec)

  toFill = maxLength - lengthABSTRACT
  fillVec = rep("NA", toFill)
  ABSTRACT = c(ABSTRACT, fillVec)

  toFill = maxLength - lengthKEYWORDS
  fillVec = rep("NA", toFill)
  KEYWORDS = c(KEYWORDS, fillVec)

  #DATAFRAME WITH REAL VALUES
  iterateDataFrame = data.frame("DOI" = DOI, "TITLE" = TITLE, "AUTHOR" = AUTHOR, "AUTHORAFFILIATION" = AUTHORAFFILIATION, "CORRESPONDINGAUTHOR" = CORRESPONDINGAUTHOREMAIL, "PUBLISHDATE" = PUBLISHDATE, "ABSTRACT" = ABSTRACT, "KEYWORDS" = KEYWORDS, "FULLTEXT" = FULLTEXT)

  #DATAFRAME USED AS A SEPERATOR
  seperatorDataFrame = data.frame("DOI" = "XXX", "TITLE" = "XXX", "AUTHOR" = "XXX", "AUTHORAFFILIATION" = "XXX", "CORRESPONDINGAUTHOR" = "XXX", "PUBLISHDATE" = "XXX", "ABSTRACT" = "XXX", "KEYWORDS" = "XXX", "FULLTEXT" = "XXX")

  #APPENDING DATAFRAMES INTO A WRITEBUFFER
  writeDataFrame = rbind(writeDataFrame, iterateDataFrame)
  writeDataFrame = rbind(writeDataFrame, seperatorDataFrame)
  counter = counter + 1
}

#WRITING DATAFRAMES INTO TEXT FILE
write.table(writeDataFrame, file="output.txt", append = FALSE, quote = F, sep = "\t\t\t", eol = "\n", na = "NA", dec = ".", row.names = FALSE, qmethod = c("escape", "double"))
