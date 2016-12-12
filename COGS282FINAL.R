# COGS 282 MODELING FINAL PROJECT: MODELING THE EFFECTS OF CREATIVITY ON DECEPTION

# creates a list of twenty people with a uniform distribution of creative personalities from 0 to 1.
people <- 100

#prob of priming working zero to one x axis
#prob of creativity affecting it 0 to one y axis
#can do a grid search for each combination
#and where in the space is there a big difference for each combination?
#if you ALWAYS need someone to respond to priming or you ALWAYS need a big relationship 
#between creativity and cheating its assuming theres  YUGE influence of one thing on another which is
#suspect, since most influences are just small
#on the other hand if you only need tiny influences to get people to cheat then ok there seems to be a
#reasonable way to look at this relationship

# since lying/cheating is a prohibited social norm, we can say that a participant needs to be feeling pretty
# cheaty to go ahead and cheat
#the grid search uses its own combination of these two parameters, but they are up here for when I
#am just using the wrapper function
can.excuse.cheating <- 0.85

#this is how often we can say the priming worked: we can alter this parameter to alter the efficacy of the priming
did.priming.work <- 0.85

# first i have to prime them for creativity - the prime should be less effective the more creative they are 
# originally as per the study

#half get primed and half don't

creative.priming <- function(participants){
  
  for(i in 1:50){
    
    #uses exp so that low dispositional creativity results in high primed creativity
    #how effective is the priming manipulation?
    #say theres a probability priming will be effective
    #because that's the other difficult thing to believe - that you can prime creativity
    potential.creativity <- exp(-(participants$dispositional.creativity[i])) 
    
    if(participants$dispositional.creativity[i] < potential.creativity){
      #determines if a random number between zero and one is less than priming parameter to determine
      #if priming works
      if(runif(1) < did.priming.work){
        
      participants$primed.creativity[i] <- potential.creativity
      }
      else {
        #if their creativity was higher than their new creativity would be, they keep the old one
        participants$primed.creativity[i] <- participants$dispositional.creativity[i]
      }
    }
    
    else{
      participants$primed.creativity[i] <- participants$dispositional.creativity[i]
    }
      
  }
  #unprimed group isnt primed, obviously
  for(i in 51:100){
    participants$primed.creativity[i] <- participants$dispositional.creativity[i]
  }

  return(participants)
}

#creative.priming(participants)

cheat.maybe <- function(participants){
  
  #if their creativity is higher than the cheating parameter, they cheat
  for(i in 1:length(participants$subject.number)){
    if(participants$primed.creativity[i] > can.excuse.cheating){
      #put a one in their cheated column and then we can take a subset of everyone who's got a one
      participants$did.they.cheat[i] <- 1
      #the amount of creativity that was higher than the parameter goes into the extent of cheatin
      #idea is that the more creative people would cheat more
      participants$extent.of.cheating[i] <- participants$primed.creativity[i] - can.excuse.cheating
    }
  }
  return(participants)
}

#this is the function that generates participant lists, runs them a bunch of times, and returns whatever
#pertinent information i want (how many people cheated per epoch in each group, 
#the average extent of cheating per epoch in each group,
#or the average creativity for the primed and unprimed groups)
wrapper.function <- function(epochs){
  
  epoch.primed <- numeric(epochs)
  epoch.unprimed <- numeric(epochs)
  
  epoch.extent.of.cheating.primed <- numeric(epochs)
  epoch.extent.of.cheating.unprimed <- numeric(epochs)
  
  epoch.primed.average.creativity <- numeric(epochs)
  epoch.unprimed.average.creativity <- numeric(epochs)
  
  for(i in 1:epochs){
    
    #empty lists to be filled in with the relevant data at the end
    primed.cheaters <- 0
    unprimed.cheaters <- 0
    
    primed.cheat.counts <- c()
    unprimed.cheat.counts <- c()
    
    primed.average.extent <- c()
    unprimed.average.extent <- c()
    
    primed.average.creativity <- c()
    unprimed.average.creativity <- c()
    
    # i make a uniform distribution of dispositional creativity, then a slot for their creativity
    # when taking the priming into effect
    #also makes a slot their primed creativity, their extent of cheating, and whether they 
    #cheated or not
    people <- 100
    subject.number <- (1:100)
    dispositional.creativity <- runif(people,min=0,max=1)
    primed.creativity <- numeric(people)
    extent.of.cheating <- numeric(people)
    did.they.cheat <- numeric(people)
    participants <- data.frame(subject.number,dispositional.creativity,primed.creativity,extent.of.cheating,
                               did.they.cheat)
    
    #first runs them through the priming
    participants <- creative.priming(participants)
  
    #then the cheating function
    participants <- cheat.maybe(participants)
    #print(participants$extent.of.cheating)
    for(j in 1:50){
      if(participants$did.they.cheat[j]==1){
        #for the primed group any cheaters get added to a count list, have their extent of cheating
        #added to a list, and have their creativity added to a list
        primed.cheaters <- primed.cheaters +1
        primed.average.extent <- c(primed.average.extent,participants$extent.of.cheating[j])
        primed.average.creativity <- c(primed.average.creativity,participants$primed.creativity[j])
      }
        else {
          primed.average.creativity <- c(primed.average.creativity,participants$primed.creativity[j])
        }
    }
    for(j in 51:100){
      if(participants$did.they.cheat[j]==1){
        #same for everyone in the unprimed group
        unprimed.cheaters <- unprimed.cheaters +1
        unprimed.average.extent <- c(unprimed.average.extent,participants$extent.of.cheating[j])
        unprimed.average.creativity <- c(unprimed.average.creativity,participants$primed.creativity[j])
      }
        else {
          unprimed.average.creativity <- c(unprimed.average.creativity,participants$primed.creativity[j])
        }
    }
    #these get the relevant information per epoch added to the higher level list
    epoch.primed[i] <- primed.cheaters
    epoch.unprimed[i] <- unprimed.cheaters

    epoch.extent.of.cheating.primed[i] <- mean(primed.average.extent)
    epoch.extent.of.cheating.unprimed[i] <- mean(unprimed.average.extent)
    
    epoch.primed.average.creativity[i] <- mean(primed.average.creativity)
    epoch.unprimed.average.creativity[i] <- mean(unprimed.average.creativity)
    
  }
  #and this puts it all into data frames
  cheats.per.epoch <- data.frame(epoch.primed, epoch.unprimed)
  extent.per.epoch <- data.frame(epoch.extent.of.cheating.primed,epoch.extent.of.cheating.unprimed)
  average.creativity.epoch <- data.frame(epoch.primed.average.creativity,epoch.unprimed.average.creativity)
  return(average.creativity.epoch)
}

#and this gives us the data frame of all however many epochs you want - i generally do 100
cheater.mainframe <- wrapper.function(100)

#this is a t-test for the average creativity - p is waaaaaaay less than zero, indicating that
#people in the primed group were more creative on the whole than the unprimed group
average.creativity.t.test <- t.test(cheater.mainframe$epoch.primed.average.creativity,cheater.mainframe$epoch.unprimed.average.creativity,
       alternative = c("greater"))

#a t test testing the hypothesis that people in the primed group cheated to a greater extent than people in the
#unprimed group. the p value for this one was 0.573: a big negative.
extent.t.test <- t.test(cheater.mainframe$epoch.extent.of.cheating.primed,cheater.mainframe$epoch.extent.of.cheating.unprimed,
                        alternative = c("greater"))

#a t.test to check whether there are a greater number of cheaters in the primed then unprimed group 
#(this may actually be where I shoul("greater"))d use a chi-square test not a t test, but. maybe not? its categorical but??)
#well, p value is way below zero
cheaters.t.test <- t.test(cheater.mainframe$epoch.primed,cheater.mainframe$epoch.unprimed,
                          alternative = c("greater"))

#a shitty way of putting together this list i know
#NOT OPTIMAL, this was debugging for some reason the creativities are exactly the same even though
#the numbers are telling me this shouldnt be the case
one.list <- mean(cheater.mainframe$epoch.primed.average.creativity)
two.list <- mean(cheater.mainframe$epoch.unprimed.average.creativity)
THREE.LIST <- c(one.list,two.list)
barplot(THREE.LIST)

#the grid search finds the optimal parameters for cheating by testing every combination of parameters
#and checking the difference between number of primed cheaters and number of unprimed cheaters
#in every combo. It could be used for the other things I look at (extent of cheating & creativity). 
#As is the result shows the biggest difference when the priming always
#works and the cheating parameter is 0.5

grid.search <- function(){
  excuse <- seq(0.0, 1.0, 0.05)
  did.work <- seq(0.0, 1.0, 0.05)
  search.space <- expand.grid(list(excuse = excuse, did.work = did.work))
  
  #difference.list <- numeric(length(search.space$did.work))
  #extent.t.test.list <- numeric(length(search.space$did.work))
  average.creativity.t.test.list <- numeric(length(search.space$did.work))
  
  pb <- txtProgressBar(min=0, max=length(search.space$did.work),style=3)

  for(i in 1:length(search.space$did.work)){
    did.priming.work <<- search.space$did.work[i]
    can.excuse.cheating <<- search.space$excuse[i]
    
    cheater.mainframe <- wrapper.function(100)
    
    primed.cheater.average <- mean(cheater.mainframe$epoch.primed)
    unprimed.cheater.average <- mean(cheater.mainframe$epoch.unprimed)
    
    #print(cheater.mainframe)
    
    #extent.t.test.list[i] <- t.test(cheater.mainframe$epoch.extent.of.cheating.primed,
    #                                cheater.mainframe$epoch.extent.of.cheating.unprimed,
    #                        alternative = c("greater"))
    difference.list[i] <- primed.cheater.average - unprimed.cheater.average

    #average.creativity.t.test.list[i] <- t.test(cheater.mainframe$epoch.primed.average.creativity,cheater.mainframe$epoch.unprimed.average.creativity,
    #                                alternative = c("greater"))
    
    setTxtProgressBar(pb,i)
  }
  
  search.space[,"difference"] <- difference.list
  search.space[,"t.test.creativity"] <- average.creativity.t.test.list
  return(search.space)
}

#gets this whole thing plottable
cheater.mainframe.v2 <- grid.search()

#gotta have ggplot2 its my best friend
library(ggplot2)

#plots that bad boy
ggplot(cheater.mainframe.v2, aes(x=excuse,y=did.work,size=t.test))+
  geom_point()
