install.packages("stringr")
library(stringr)
install.packages("utils")
library(utils)

flipkart<-function (url , n)  # n = no of pages to extract
  
{ 
  text_page = character(0)   # define blank page
  pb<- txtProgressBar(min=1 , max =n , style = 3)  #define progress bar
  url = unlist(str_split(url , "&"))[1]
  
  for(i in 0:n)
  {
  
    p = i
    e = "&page="
    url0 = paste(url,e,p,sep="")             # create flipkart url in correct format
    text = readLines(url0)                   #read url
        
    text_start = grep("<div class=""> ", text)                  #review start marker
    text_end = grep("</div><span class="_2jRR3v"> ", text)     #review end marker
    
    
    setTxtProgressBar(pb,i)                 #print progress bar
    
    if(length(text_start==0)) break         # 
    
    for( j in 1 :length(text_start))        # consolidate all reviews
    {  
    text_temp = paste(paste(text[(text_start[j]+1)]),collapse = " ")
    text_page = c(text_page,text_temp)
    } 
  }
  
  text_page = gsub("<.*?>","",text_page)         #regex  for removing HTML character
  text_page = gsub("^\\s+|\\s+$","",text_page)   #regex for removing leading and trailing white space
  return(text_page)                              # return reviews
  
}

url = "https://www.flipkart.com/asus-zenfone-max-pro-m2-blue-32-gb/product-reviews/itmfb22gjfzty4am?"
  
 zen = flipkart(url,10)
 length(zen)
 
 zen1<-as.data.frame(zen)
 zen2<-write.table(zen1,'zen1.txt')
 
  
    