#content filtering (for movies)

#movies<-read.table("https://storage.googleapis.com/dimensionless/Analytics/u.item.txt", header= FALSE, sep = "|", quote="")
#we can use read.table or read.csv
#delimiter here is |
#no header in data, so header = FALSE
movies=read.csv("https://storage.googleapis.com/dimensionless/Analytics/u.item.txt", header= FALSE, sep = "|")
dim(movies)
movies$V4=NULL
str(movies)
 head(movies)
 colnames(movies)=c("ID", "Title", "ReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary","Drama","Fantasy","FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War","Western")
str(movies)
movies$ID=NULL
movies$ReleaseDate=NULL
movies$IMDB=NULL
# or
movies[,c("ID", "ReleaseDate", "IMDB")]=NULL

class(movies)
str(movies)
dim(movies)
#finding the unique rows
#movies<-unique(movies)
dim(movies)
movies=unique(movies)
dim(movies)
str(movies)
head(movies)
# Quick Question 
table(movies$Comedy)
table(movies$Western)
table(Romance=movies$Romance,Drama=movies$Drama)

#first we'll calculate the distance between each of the points using euclidean method and then perform clustering
dim(movies)
names(movies)


#clustering based on genre and not based on title, so we'll find distance b/w points for cols 2:20
distance<-dist(movies[2:20],method = "euclidean")
#16447 C2 distances will be computed - distance between each row is being computed i.e y distance matrix
summary(distance)
str(distance)
#Performing clustering
clusterMovies<-hclust(d = distance,method="ward.D")
#The ward method cares about the distance between clusters using centroid distance, 
# and also the variance in these clusters. 
#Whichever cluster after grouping will reduce our overall sse (or var) after clubbing
plot(clusterMovies)
#More number of clusters => recommendations are more customized according to groups
#Choosing number of clusters is a business problem
clusterGroups<-cutree(clusterMovies,k=10)
plot(clusterGroups)
clusterGroups#gives cluster number for each row
length(clusterGroups)
#how to label the clusters
movies$CNum=clusterGroups

#To find out which cluster has good amount of action movies
table(Clusters = clusterGroups,ActionMovies=movies$Action)
#so this shows cluster 2 has max ActionMovies=1
#or
tapply(movies$Thriller,clusterGroups,mean)#shows cluster 2 has highest mean
tapply(movies$Thriller,movies$CNum, sum)#shows cluster 2 has highest sum
#so we can conclude cluster 2 is the cluster with Action Movies
tapply(movies$Comedy,clusterGroups,sum)
tapply(movies$Comedy, clusterGroups, length)
tapply(movies$Comedy, clusterGroups, mean)
#note clusters 5, 7,9 are completely comedy only

#aggregate function can be applied on the dataframe completely while tapply needs to be applied on each col
aggregate(movies[2:20], list(clusterGroups), mean) 
#cluster 7 is 1 only in comedy and romance - pure cluster

colMeans(subset(movies[2:20], clusterGroups == 1)) 

spl = split(movies[2:20], clusterGroups)
spl$`1`
lapply(spl, colMeans)


# Recommendation Systems
#so we want to recommend movies to sb who has just liked "men in black"
subset(movies,Title=="Men in Black (1997)")
rn=grep("Men in Black", movies$Title) #supports partial string matching
movies$CNum[rn]
#we can use full string matching using following fn
#so we got the row number of movie men in black in the data. 
#Now v want to know which cluster it belongs to
cl=clusterGroups[rn]
movies2<- subset(movies,movies$CNum==2) 
length(movies2)
#here we have to take care that movies in clusterGroups have same same number of observations as movies
movies2$Title #all these movies can be recommended



#Quick Question
clusterGroups2=cutree(clusterMovies, k=2)
aggregate(movies[2:20], list(clusterGroups2), mean) 
#All the movies in cluster 2 are in drama genre
