library(shiny)
library(stringr)
library(wordcloud)

df<-read.table("Data/df.csv", header = TRUE, sep = ",", row.names = 1, stringsAsFactors = FALSE)
df2<-read.table("Data/df2.csv", header = TRUE, sep = ",", row.names = 1, stringsAsFactors = FALSE)
df3<-read.table("Data/df3.csv", header = TRUE, sep = ",", row.names = 1, stringsAsFactors = FALSE)

shinyServer(
        function(input,output) {
                nextwordKN2<-reactive({
                        ##Remove trailing white spaces and punctuation
                        x<-input$text
                        x<-gsub("[[:space:]]*$|[[:punct:]]","",x)
                        x<-tolower(x)
                        ##Select last word
                        lastword<-word(x,-1)
                        ##Select last bigram
                        lasttwo<-word(x,-2,-1)
                        if (sum(df2$KNP[df2$unigram==lastword])==0) {
                                ##Use unigram model
                                matches<-df[,c("lastword","KNP")]
                        } else {
                                ##Use bigram model
                                bigrams<-df2[df2$unigram==lastword,]
                                unseenunigrams<-df[!df$lastword %in% bigrams$lastword,]
                                lastwordlambda<-df$lambda[df$lastword==lastword]
                                unseenunigrams$KNP<-unseenunigrams$KNP * lastwordlambda
                                names(unseenunigrams)[1]<-"lastword"
                                matches<-bigrams[,c("lastword","KNP")]
                                unseenunigrams<-unseenunigrams [,c("lastword","KNP")]
                                matches<-rbind(matches,unseenunigrams)
                                matches<-matches[order(-matches$KNP),]
                                if (!is.na(lasttwo)) {
                                if (sum(df3$KNP[df3$bigram==lasttwo])!=0) {
                                        ##Use trigram model
                                        matches2<-matches
                                        trigrams<-df3[df3$bigram==lasttwo,]
                                        unseenbigrams<-df2[df2$unigram==lastword,]
                                        unseenbigrams<-unseenbigrams[!unseenbigrams$words %in% trigrams$lasttwo,]
                                        lasttwolambda<-df2$lambda[df2$words==lasttwo]
                                        unseenbigrams$KNP<-unseenbigrams$KNP * lasttwolambda
                                        matches<-trigrams[,c("lastword","KNP")]
                                        unseenbigrams<-unseenbigrams [,c("lastword","KNP")]
                                        matches<-rbind(matches,unseenbigrams)
                                        matches2<-matches2[!matches2$lastword %in% matches$lastword,]
                                        matches<-rbind(matches,matches2)
                                        matches<-matches[order(-matches$KNP),]
                                }}}
                        matches<-matches[1:input$freq,]
                        matches
                })

                wordcloud_rep<-repeatable(wordcloud)
                
                output$plot <- renderPlot({
                        cloud<-nextwordKN2()
                        wordcloud_rep(cloud$lastword,cloud$KNP, scale=c(6,1), colors=brewer.pal(8, "Dark2"))
                })        
               output$nextword = renderText({
                       word<-nextwordKN2()
                       word[1,1]})

               ##Read in documentation file
               output$documentation <- renderText({  
                       readLines("Documentation/Word_Prediction_Model_Documentation.html")  
               })
                               
        }
)