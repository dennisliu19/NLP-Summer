Blogs1 <- readLines("C:/Users/Haoyang Liu/Desktop/Summer/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
Twitter1 <- readLines("C:/Users/Haoyang Liu/Desktop/Summer/en_US.twitter.txt",encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
News1 <- readLines("C:/Users/Haoyang Liu/Desktop/Summer/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)

Blogs2 <- sample(Blogs1, size=length(Blogs1)*.002, replace=FALSE) 
Twitter2 <- sample(Twitter1, size=length(Twitter1)*.002, replace=FALSE) 
News2 <- sample(News1, size=length(News1)*.002, replace=FALSE) 

data1 = c(Blogs2, Twitter2, News2)