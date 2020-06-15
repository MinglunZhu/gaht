library(dplyr)

#consts

#end consts

itn_chkFtrType <- function(ftrs, ftrLst) {
  #chk type
  ftrs <- ifelse(ftrLst$type == 'integer', round(ftrs), ftrs)

  #chk dependency
  dependencyVal <- sapply(ftrLst$dependency, function(dependency) {
    if (is.na(dependency)) {
      0
    } else {
      ftrs[match(dependency, ftrLst$name)]
    }
  })

  dependencyMin <- ifelse(is.na(ftrLst$dependency_min), 0, ftrLst$dependency_min)

  ifelse(dependencyVal < dependencyMin, 0, ftrs)
}

#ftrs is dataframe
#with each row represent a feature
#a min col and a max col represent the min and max values
#and a type col represents value type
# integer
# numeric
itn_initPop <- function(popSize, ftrs) {
  pop <- list()
  numFtrs <- nrow(ftrs)

  for (i in 1:popSize) {
    pop[[i]] <- vector(length = numFtrs)

    for (fi in 1:numFtrs) {
      pop[[i]][fi] <- runif(1, min = ftrs$min[fi], max = ftrs$max[fi])
    }

    #check each type if type is integer, round to integer
    #pop[[i]] <- ifelse(ftrs$type == 'integer', round(pop[[i]]), pop[[i]])
    pop[[i]] <- itn_chkFtrType(pop[[i]], ftrs)
  }

  pop
}

itn_evolve <- function(pop, lastBest, highestScore, dupGens, dupTrack, ftrs, genCnt, maxGens) {
  ftrCnt <- nrow(ftrs)
  winner <- c()
  popSize <- length(pop)

  while (dupTrack < dupGens) {
    if (genCnt > 0) {
      #pick winners and some random loser, breed next gen, random mutate
      nextGen <- list()

      #our number 1 winner automatically survives as immortal until his score gets beaten
      nextGen[[1]] <- winner

      #then we pick top 25%, and random 5% from the remaining and breed for the next gen pop size - 1
      #each child is then randomly mutated
      #you should make sure popSize is at least 4 so that top 25% would at least be 1 individual
      topCnt <- round(.25 * popSize)
      randCnt <- round(.05 * popSize)

      scoreIdx <- data.frame(
        score = scores
      ) %>%
        mutate(idx = row_number())

      topIdx <- scoreIdx %>%
        top_n(topCnt, score)

      btmIdx <- scoreIdx %>%
        top_n(-(popSize - topCnt), score)

      randIdx <- sample(btmIdx$idx, randCnt)

      breedPool <- list()

      for (i in 1:topCnt) {
        breedPool[[i]] <- pop[[topIdx$idx[i]]]
      }

      #if population ia too small, randCnt  could be 0
      if (randCnt > 0) {
        for (i in 1:randCnt) {
          breedPool[[topCnt + i]] <- pop[[randIdx[i]]]
        }
      }

      #randomly pick 2 and make 1 to 3 child limited by popSize, until pop size is full
      breedPoolSize <- length(breedPool)

      while (length(nextGen) < popSize) {
        parentsIdxs <- sample(1:breedPoolSize, 2)

        father <- breedPool[[parentsIdxs[1]]]
        mother <- breedPool[[parentsIdxs[2]]]

        nextGenSizeRemain <- popSize - length(nextGen)

        childCnt <- pmin(sample(1:3, 1), nextGenSizeRemain)

        for (ci in 1:childCnt) {
          #creat child and add to next gen
          #for each feature, randomly select from father or mother, then mutate for no more than 50% from the original figure
          #the mutate can either increase or decrease
          ni <- length(nextGen) + 1

          nextGen[[ni]] <- vector(length = ftrCnt)

          for (fi in 1:ftrCnt) {
            #choose father or mother
            if(sample(1:2, 1) == 1) {
              nextGen[[ni]][fi] <- father[fi]
            } else {
              nextGen[[ni]][fi] <- mother[fi]
            }

            #limit to min max
            #must limit before mutation and chking ftrs
            nextGen[[ni]][fi] <- pmin(pmax(nextGen[[ni]][fi], ftrs$min[fi]), ftrs$max[fi])

            #mutate
            #mutation chance is dependant on if the father trait is similar to mother trait
            #this happens if inbreeding and father and mother share similar traits
            #mutation chance = similarity rate
            ttlFtrSze <- ftrs$max[fi] - ftrs$min[fi]

            if (ttlFtrSze == 0) {
              simRate <- 1
            } else {
              simRate <- pmax(1 - abs(father[fi] - mother[fi]) / ttlFtrSze, 0)
            }

            mutateProb <- simRate / 2

            if (sample(1:0, 1, prob = c(mutateProb, 1 - mutateProb)) == 1) {
              #radomly choose between min and max
              mutated <- runif(
                1,
                min = ftrs$min[fi],
                max = ftrs$max[fi]
              )

              #limit to 50% change from the original
              maxChg <- .5 * ttlFtrSze

              nextGen[[ni]][fi] <- pmax(
                pmin(mutated, nextGen[[ni]][fi] + maxChg),
                nextGen[[ni]][fi] - maxChg
              )
            }
          }

          #nextGen[[ni]] <- pmin(pmax(ifelse(ftrs$type == 'integer', round(nextGen[[ni]]), nextGen[[ni]]), ftrs$min), ftrs$max)
          nextGen[[ni]] <- itn_chkFtrType(nextGen[[ni]], ftrs)
        }
      }

      pop <- nextGen
    }

    genCnt <- genCnt + 1

    #test each individual within the pop
    scores <- vector(length = popSize)

    for (i in 1:popSize) {
      print(paste('generation', genCnt, 'test subject', i, sep = ' '))

      print(
        data.frame(
          name = ftrs$name,
          val = pop[[i]]
        )
      )

      gc()

      scores[i] <- test(pop[[i]])
    }

    #compare best score with last best, if same, increase dup track, if not reset
    bestScore <- max(scores)

    winner <- pop[[which.max(scores)]]

    #not only the ftrs identical, but if the score doesn't get better
    #then it's also considered duplicate generation
    if (
      identical(winner, lastBest)
      | (bestScore <= highestScore)
      | ((maxGens > 0) & ((genCnt - dupTrack + dupGens) > maxGens))
    ) {
      dupTrack <- dupTrack + 1

      highestScore <- pmax(highestScore, bestScore)
    } else {
      dupTrack <- 0

      lastBest <- winner

      highestScore <- bestScore

      dupGens <- pmax(genCnt, dupGens)
    }
  }

  pop
}

#params
# features:
#   a list of features to evolve
#   each individual in the population will have a set of evolved features
#   the features are then fed to the test function to test
#   each feature must have a min value and max value, the evolution will only happen within this limit
# test:
#   a function that will take the features as argument and test them
#   must return a score, the higher the score the better
#   you must define how the test function works, and how it utilizes the features
# duplicate generations:
#   number of generations which the best score hasn't improved
#   when this is reached, the evolution stops and assumes that no improvements can be made
# pop:
#   you can pre-supply a population, if pop not supplied
#   if will be randomly initialized
# popSize:
#   if pop is not pre-supplied, popSize is needed to randomly inistialize a population
#   obivously, a minimum of 2 is required per Christianity Adams and eve
#   but science seems to suggest the min is much larger
#   one school of thought is that it depends on how many hyper-parameters you are trying to evolve
#   this makes sense as the more parameters you have the bigger the search space
#   we recommend at least a 1:1 ratio between population size and number of params
#   here we set the default to 50
# maxGens:
#   max generations the algorithm will try to evolve,
#   after that all generations are considered duplicated generations
#   and will evolve for specified duplicate generations allowed
#   set to 0 to disable
#
#returns
# the features list with the highest score
evolve <- function(ftrs, test, dupGens = 4, pop = NULL, popSize = 50, maxGens) {
  dupTrack <- 0
  #init to false because used in boolean comparison later
  lastBest <- c()
  highestScore <- 0
  genCnt <- 0

  #randomly pick from max and min for each feature
  #pop is a list of individuals, each individual is a list of features
  if (is.null(pop)) {
    pop <- itn_initPop(popSize, ftrs)
  }

  itn_evolve(pop, lastBest, highestScore, dupGens, dupTrack, ftrs, genCnt, maxGens)
}
