sns<-austen_books()

sns<-sns%>%
  filter(book == 'Sense & Sensibility')

sns$book<-as.character(sns$book)

str_detect(sns$text,'^CHAPTER')

sns<-sns%>%
  filter(!str_detect(sns$text,'^CHAPTER'))

sns<-sns[12:12574,]
sns<-sns[1:12562,]
