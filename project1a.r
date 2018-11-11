#Required Libraries
library(bitops)
library(RCurl)
library(XML)
library(stringr)
library(httr)

#Supress warnings if any
options(warn=-1)

#Create a new directory
dir.create("~/NJIT/Fall18/CS636/Project/HTMLs")

#Change working directory
setwd("~/NJIT/Fall18/CS636/Project/HTMLs")

#Url of the main page
site = "https://www.gmb.org.br/copia-online-version"

#Parse the main page
main.page = readLines(site)

#Write the main page
write(main.page, file = "~/NJIT/Fall18/CS636/Project/HTMLs/main_page.html")

#Few regex patterns needed
patternYear = ">\\d\\d\\d\\d</"
patternLinks = ">\\d+.[A-z]*</"
patternHref = "<a href=.*>.*\\d+.*</.*/a>"
patternAtags = "(www.)scielo.*nrm=iso"
patternEngLink = "(www.)scielo.*tlng=en.*texto"

#Few Strings which are used later to concatenation operations
secureLink = "https://"
unsecureLink = "http://"

#Length of the links to the Papers
linkLength = 92

#Creating required variables
linksVec = c()
realLinks = c()
i = 3
counter = 1

#Regex operation to get the links
linksVec = grep(patternHref, main.page, value = TRUE)
while (i <= 11){
  b = regexpr("(www.)scielo.*nrm=iso", linksVec[i])
  temp = b[1]
  if(temp == -1){
    i = i + 1
    next
  }
  printStatement = sprintf("index: %d is  %d", i, temp)
  print(printStatement)
  endIndex  = temp + linkLength
  tempLink = substr(linksVec[i], temp, endIndex)
  tempLink = str_replace_all(tempLink, "&amp;", "&")
  realLinks = c(realLinks, tempLink)
  i = i + 1
}


#Reset a Few Variable(s)
i = 0
#Visiting all links in page 1
#Reading the webpage and writing them down into the drive
for (i in realLinks){
  currentLink = i
  currentLink = paste(unsecureLink, currentLink)
  currentPage = readLines(currentLink)
  filename = paste( counter, "linksVisited.html", sep = "-")
  write(currentPage, file = filename)
  counter = counter + 1
}

#Visting the next page
nextUrl = "https://www.gmb.org.br/online-version"

#Reading next webpage
nextPage = readLines(nextUrl)

#Reset Variables(s)
linksVec = c()
realLinks = c()

#Writing down the next page
write(nextPage, file = second_page.html)

#Regex operation to get the links
linksVec = grep(patternHref, nextPage, value = TRUE)

#visiting all links in page 2
i = 5

#Creating a new Variable
start = c()

#Loop over the lines that matched in the previous regex op and extract the link
while( i <= 84 ){
  b = regexpr("(www.)scielo.*nrm=iso", linksVec[i])
  temp = b[1]
  if(temp == -1){
    i = i + 1
    next
  }
  #printStatement = sprintf("index: %d is  %d", i, temp)
  #print(printStatement)
  endIndex  = temp + linkLength
  tempLink = substr(linksVec[i], temp, endIndex)
  tempLink = str_replace_all(tempLink, "&amp;", "&")
  realLinks = c(realLinks, tempLink)
  i = i + 1
}

#Reset Variable(s)
i = 0

#Visiting all links obtained from the loop
for (i in realLinks){
  currentLink = i
  currentLink = paste(unsecureLink, currentLink)
  currentPage = readLines(currentLink)
  filename = paste( counter, "linksVisited.html", sep = "-")
  write(currentPage, file = filename)
  counter = counter + 1
}

#Reset Variable(s)
counter = 1
realLinks = c()

#Creating New Variables
paperLinkLength = 109
limitLength = 0
paperLinks = c()

#Loop over the written html files to extract the links to the actual papers
while(counter < 89){
  filename = paste( counter, "linksVisited.html", sep = "-")
  currentFileRead =  readLines(filename)
  paperLinks = grep(patternEngLink, currentFileRead, value = TRUE)
  limitLength = length(paperLinks)
  j = 1
  while(j <= limitLength){
    tempString = paperLinks[j]
    b = regexpr(patternEngLink, paperLinks[j])
    startIndex = b[1]
    endIndex = startIndex + paperLinkLength
    tempLink = substr(tempString, startIndex, endIndex)
    tempLink = str_replace_all(tempLink, "&amp;", "&")
    realLinks = c(realLinks, tempLink)
    #print(length(realLinks))
    j = j + 1
  }
  #print(counter)
  counter = counter + 1
}

#Reset Variable(s)
counter = 1

#Visiting each paper link and writing them down
for(i in realLinks){
  readingPaper = i
  readingPaper = paste(unsecureLink, readingPaper)
  parsePaper = readLines(readingPaper)
  writeFilename = paste(counter, "paperVisited.html", sep = "-")
  write(parsePaper, file = writeFilename)
  counter = counter + 1
}
