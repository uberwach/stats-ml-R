songs = read.csv("songs.csv")
str(subset(songs, year == 2010))

michel = subset(songs, artistname == "Michael Jackson")
nrow(michel)
michel_top = subset(michel, Top10 == 1)
michel_top$songtitle

table(songs$timesignature)

subset(songs, tempo == max(songs$tempo))

SongsTrain = subset(songs, year < 2010)
SongsTest = subset(songs, year == 2010)
nrow(SongsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# To remove these variables from your training and testing sets, type the following commands in your R console:

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(SongsLog1)
cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

song.pred = predict(SongsLog3, type = "response", newdata = SongsTest)
table(SongsTest$Top10 == 1, song.pred >= 0.45)
accuracy = (309+19) / (40+19+5+309)
accuracy
table(SongsTest$Top10)
base_accuracy = 314 / (314+59)
base_accuracy

sensitivity = 19 / (19+40)
specificity = 309 / 314
