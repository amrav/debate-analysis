library(sm)
library(profvis)
library(assertthat)

NUM_TEAMS = 32
#ROUNDS = 5

generate_perf <- function(mean, rounds) {
  return(rnorm(rounds, mean=mean, sd=0.3))
}

rank_distance <- function(skills, ranking) {
  return(sum(abs(skills - skills[ranking])) * NUM_TEAMS / (max(skills) - min(skills)))
}

rank_perf_distance <- function(scores, standing) {
  rounds <- sum(!is.na(scores[,1]))
  avg_scores <- colSums(scores, na.rm = T) / rounds
  score_order <- order(avg_scores)
  d <- norm(as.matrix(order(score_order) - order(standing)), 'f')
  return(d)
}


generate_brackets <- function(wins, perfs, scores, pulldown) {
  sum_scores <- colSums(scores, na.rm=TRUE)
  brackets = split(1:NUM_TEAMS, wins)
  rounds <- sum(!is.na(scores[,1]))
  if (rounds == 1) {
    sum_perfs <- perfs[1,]
  } else {
    sum_perfs <- colSums(perfs[1:rounds,])  
  }
  
  #str(brackets)
  for (i in 1:length(brackets)) {
    # bracket by MOV
    bracket_scores <- sum_scores[brackets[[i]]]
    
    # bracket by cumulative speaks
    #bracket_scores <- sum_perfs[brackets[[i]]]
    #str(bracket_scores)
    bracket_order <- order(bracket_scores)
    #str(bracket_order)
    brackets[[i]] <- brackets[[i]][bracket_order]
    #str(brackets[[i]])
  }
  #cat('before adjusting brackets\n')
  #str(brackets)
  if (pulldown) {
    brackets <- pulldown(brackets)
  } else {
    brackets <- pullup(brackets) 
  }
  assert_that(sum(sapply(brackets, length)) == length(wins))
  #print('brackets:')
  #print(brackets)
  return(brackets)
}

pullup <- function(brackets) {
  num_teams <- sum(sapply(brackets, length))
  for (i in 1:length(brackets)) {
    if (length(brackets[[i]]) %% 2 != 0) {
      brackets[[i+1]] <- c(tail(brackets[[i]], 1), brackets[[i+1]])
      length(brackets[[i]]) <- length(brackets[[i]]) - 1
    }
    assert_that(length(brackets[[i]]) %% 2 == 0)
  }
  brackets <- brackets[lapply(brackets, length) != 0]
  assert_that(num_teams == sum(sapply(brackets, length)))
  return(brackets)
}

pulldown <- function(brackets) {
  num_teams <- sum(sapply(brackets, length))
  for (i in length(brackets):1) {
    if (length(brackets[[i]]) %% 2 != 0) {
      brackets[[i-1]] <- c(brackets[[i-1]], head(brackets[[i]], 1))
      brackets[[i]] <- tail(brackets[[i]], length(brackets[[i]]) - 1)
    }
    assert_that(length(brackets[[i]]) %% 2 == 0)
  }
  brackets <- brackets[lapply(brackets, length) != 0]
  assert_that(num_teams == sum(sapply(brackets, length)))
  return(brackets)
}

midslide <- function(brackets) {
  pairings = matrix(nrow=0, ncol=2)
  for (i in 1:length(brackets)) {
    b_len <- length(brackets[[i]])
    b_pairings <- cbind(brackets[[i]][1:(b_len/2)],
                        brackets[[i]][((b_len/2)+1):b_len])
    pairings <- rbind(pairings, b_pairings)
  }
  return(pairings)
}

fold <- function(brackets) {
  pairings = matrix(nrow=0, ncol=2)
  for (i in 1:length(brackets)) {
    b_len <- length(brackets[[i]])
    b_pairings <- cbind(brackets[[i]][1:(b_len/2)],
                        rev(brackets[[i]][((b_len/2)+1):b_len]))
    pairings <- rbind(pairings, b_pairings)
  }
  return(pairings)
}

power_bracket <- function(brackets) {
  pairings = matrix(nrow=0, ncol=2)
  #cat('brackets: \n')
  #str(brackets)
  for (i in 1:length(brackets)) {
    b_len <- length(brackets[[i]])
    b_pairings <- cbind(brackets[[i]][c(T, F)],
                        brackets[[i]][c(F, T)])
    pairings <- rbind(pairings, b_pairings)
  }
  return(pairings)
}

random_bracket <- function(brackets) {
  pairings = matrix(nrow=0, ncol=2)
  for (i in 1:length(brackets)) {
    
    b_len <- length(brackets[[i]])
    brackets[[i]] <- brackets[[i]][sample(b_len, b_len)]
    b_pairings <- cbind(brackets[[i]][1:(b_len/2)],
                        brackets[[i]][((b_len/2)+1):b_len])
    pairings <- rbind(pairings, b_pairings)
  }
  return(pairings)
}

generate_pairings <- function(scores, perfs, pulldown, bracket_algo) {
  num_teams <- dim(scores)[2]
  wins <- colSums(scores > 0, na.rm=TRUE)
  sum_scores <- colSums(scores, na.rm=TRUE)
  sorted_order <- order(wins, sum_scores)
  
  assert_that(!all(sum_scores == 0) || bracket_algo == 'random')
  if (bracket_algo == 'random') {
    sorted_order <- sample(num_teams, num_teams)
    brackets <- list(sorted_order)
  } else {
    brackets <- generate_brackets(wins, perfs, scores, pulldown)
  }
  assert_that(sum(sapply(brackets, length)) == num_teams)
  if (bracket_algo == 'fold') {
    pairings <- fold(brackets)
  } else if (bracket_algo == 'mid_slide' || bracket_algo == 'random') {
    pairings <- midslide(brackets)
  } else if (bracket_algo == 'random_bracket') {
    pairings <- random_bracket(brackets)
  } else if (bracket_algo == 'power_bracket') {
    pairings <- power_bracket(brackets)
  } else {
    return(0)
  }
  
  assert_that(dim(pairings)[1] == num_teams/2)

  return(pairings)
}

generate_standings <- function(scores, perfs, round) {
  sum_scores <- colSums(scores, na.rm=TRUE)
  wins <- colSums(scores > 0, na.rm=TRUE)
  if (round == 1) {
    cum_perfs <- perfs[1,]
  } else {
    cum_perfs <- colSums(perfs[1:round,])
  }
  #sorted_order <- order(wins, cum_perfs)
  sorted_order <- order(wins, sum_scores)
  return(sorted_order)
}


simulate_tournament <- function(x, pulldown=TRUE) {

  team_skills = rnorm(NUM_TEAMS)
  team_skills <- sort(team_skills)
  
  perfs = sapply(team_skills, generate_perf)
  
  scores <- matrix(nrow=ROUNDS, ncol=NUM_TEAMS)
  #wins <- rep(0,NUM_TEAMS)
  
  for (i in 1:ROUNDS) {
    pairings <- generate_pairings(scores, wins, pulldown)
    #cat("pairings:\n")
    #print(pairings)
    props <- pairings[1:(dim(pairings)[1]),1]
    opps <- pairings[1:(dim(pairings)[1]),2]
    assert_that(length(props) == length(opps))
    assert_that(length(props) + length(opps) == NUM_TEAMS)
    prop_scores <- perfs[i, props] - perfs[i, opps]
    opp_scores <- -prop_scores
    scores[i, props] <- prop_scores
    scores[i, opps] <- opp_scores
    wins <- wins + (scores[i,] > 0)
    #if (i == ROUNDS) {
    #  cat("scores: \n")
    #  print(scores)
    #}
  }
  standings <- generate_standings(scores, perfs, i)
  order_standings <- order(standings)
  displacement <- order_standings - 1:32
  return(c(norm(as.matrix(displacement[1:4]), 'f'),
           norm(as.matrix(displacement[5:8]), 'f'),
           norm(as.matrix(displacement[9:12]), 'f'),
           norm(as.matrix(displacement[13:16]), 'f'),
           norm(as.matrix(displacement[17:20]), 'f'),
           norm(as.matrix(displacement[21:24]), 'f'),
           norm(as.matrix(displacement[25:28]), 'f'),
           norm(as.matrix(displacement[29:32]), 'f')))
}

simulate_tournaments <- function(num_tournaments) {
  team_skills <- sapply(1:num_tournaments,
                       function(x){rnorm(NUM_TEAMS)})
  team_skills <- apply(team_skills, 2, sort.int, method='quick')
  #print(team_skills)
  perfs <- apply(team_skills, 2,
                 function(x){sapply(x, generate_perf)})
  #print(perfs)
}

NUM_TEAMS = 32

simulate_inf_tournament <- function(ROUNDS, pulldown=TRUE, bracket_algo='fold', roundx=-1, roundx_algo='') {
  
  team_skills = rnorm(NUM_TEAMS)
  team_skills <- sort(team_skills)
  
  #print(team_skills)
  
  perfs <- sapply(team_skills,
                 function(x) {generate_perf(x, ROUNDS)})
  
  perf_ranks <- rank(colSums(perfs))
  
  #print(perfs)
  
  scores <- matrix(nrow=ROUNDS, ncol=NUM_TEAMS)
  #wins <- rep(0,NUM_TEAMS)
  
  displacements <- vector(mode='numeric', length=ROUNDS)
  
  for (i in 1:ROUNDS) {
    if (roundx > 0 && i %% roundx == 0) {
      #cat('random round! at ', i, "\n")
      pairings <-
        generate_pairings(scores, perfs, pulldown, roundx_algo)
    } else if (i == 1) {
      pairings <-
        generate_pairings(scores, perfs, pulldown, 'random')
    }
    else {
      pairings <- generate_pairings(scores, perfs, pulldown, bracket_algo)
    }
    
    #cat("pairings:\n")
    #print(pairings)
    props <- pairings[1:(dim(pairings)[1]),1]
    opps <- pairings[1:(dim(pairings)[1]),2]
    
    #str(props)
    #str(opps)
    #print(NUM_TEAMS)
    
    assert_that(length(props) == length(opps))
    assert_that(length(props) + length(opps) == NUM_TEAMS)
    
    prop_scores <- perfs[i, props] - perfs[i, opps]
    opp_scores <- -prop_scores
    
    # Rank by MOV
    scores[i, props] <- prop_scores
    scores[i, opps] <- opp_scores
    
    # Rank by speaker score
    #scores[i, props] <- prop_scores
    #scores[i, opps] <- opp_scores
    
    #wins <- wins + (scores[i,] > 0)
    #if (i == ROUNDS) {
    #  cat("scores: \n")
    #  print(scores)
    #}
    
    standings <- generate_standings(scores, perfs, i)
    #cat('scores: \n')
    #print(scores)
    #cat("Round", i, ": ", standings, "\n")
    order_standings <- order(standings)
    displacement <- order_standings - 1:NUM_TEAMS
    displacement2 <- rank_distance(team_skills, standings)
    displacement3 <- rank_perf_distance(scores, standings)
    #displacements[i] <- norm(as.matrix(displacement), 'f')
    #displacements[i] <- displacement2
    #displacements[i] <- displacement3
    if (i >= 2) {
      displacements[i] <- cor(colSums(perfs[1:i,], na.rm=T), order_standings, method = 'spearman')
    }
  }
  
  #str(displacements)
  standings <- generate_standings(scores, perfs, i)
  #print(colSums(scores))
  #print(standings)
  #print(order(colSums(scores)))
  #plot(1:ROUNDS, displacements, type='l',
  #     ylim=c(20,100), xlim=c(0,ROUNDS))
  
  return(displacements)
  
}

plot_algo <- function(bracket_algo, pulldown=FALSE, color='black', first=F, roundx=-1, roundx_algo='') {
  ROUNDS <- 7
  tournament_disps <- sapply(1:500, function(x) {
    simulate_inf_tournament(ROUNDS, pulldown=pulldown,
                            bracket_algo=bracket_algo,
                            roundx=roundx,
                            roundx_algo=roundx_algo)
  })
  #print(tournament_disps)
  #avg_disps <- apply(tournament_disps, 1, function(x) {sqrt(sum(x^2)/length(x))})
  avg_disps <- apply(tournament_disps, 1, function(x) {sum(x)/length(x)})
  #print(avg_disps)
  if (first) {
    plot(2:ROUNDS, avg_disps[2:length(avg_disps)], type='b',
         lwd=2,
         ylim=c(0.75,0.95),
         xlim=c(2,ROUNDS), col=color,
         xlab='Number of rounds',
         ylab='Spearman correlation',
         main='Spearman correlation to cumulative performance \nfor pairing algorithms (pull-up, MOV)')
  } else {
    lines(2:ROUNDS, avg_disps[2:length(avg_disps)], type='b',
          col=color, lwd=2)
  }
}

plot_algo('mid_slide', pulldown=F, first=T, color='red', roundx=-5, roundx_algo='power_bracket')
#plot_algo('mid_slide', pulldown=T, color='orange', roundx=-2, roundx_algo='power_bracket')
plot_algo('random_bracket', pulldown=F, color='purple', roundx=-5, roundx_algo='power_bracket')
#plot_algo('random_bracket', pulldown=T, color='pink')
plot_algo('fold', pulldown=F, color='blue')
#plot_algo('fold', pulldown=T, color='skyblue', roundx=5, roundx_algo='random_bracket')
plot_algo('random', pulldown=F, color='rosybrown4')
#plot_algo('random', pulldown=T, color='rosybrown')
plot_algo('power_bracket', pulldown=F, color='darkgreen')
#plot_algo('power_bracket', pulldown=T, color='forestgreen', roundx=-2, roundx_algo = 'random_bracket')

# Dark is pullup, light is pulldown
legend("bottomright",legend=c("MID-SLIDE","RANDOM-BRACKET", "FOLD", "RANDOM", "POWER-BRACKET"),
       lty=c(1,1),
       col=c('red', 'purple', 'blue', 'rosybrown4', 'darkgreen'),
       cex=0.65,
       lwd=2)

#simulate_tournaments(2)

st_pd <- function(x) {
  simulate_tournament(x, pulldown=TRUE)
}

st_pu <- function(x) {
  simulate_tournament(x, pulldown=FALSE)
}

# profvis({d_pd = sapply(1:10000, st_pd)})

pfunc <- function() {
  wins = rep(0,32)
  a <- factor(sample(2,32,TRUE))
  for (i in 1:10000) {
    #wins <- wins + sample(1, 32, TRUE)
    a <- a + rep(0,32)
    split(1:32, a)
  }
}

#profvis({pfunc()})
#d_pu = sapply(1:5000, st_pu)
# sm.density(d_pd[1,], col=rgb(1,0,0,0.8), main="Density plot", xlab="RMS Midslide sd=0.5 10k", xlim=c(-5,30))
# sm.density(d_pd[2,], col=rgb(0,0,1,0.8), add=T)
# sm.density(d_pd[3,], col=rgb(0,0.5,1,0.8), add=T)
# sm.density(d_pd[4,], col=rgb(0.5,0.25,0,0.8), add=T)
# sm.density(d_pd[8,], col=rgb(0.5,0,0.75,0.8), add=T)
# legend("topright",legend=c("1-4","5-8", "9-12", "13-16", "29-32"), lty=c(1,1),
#        col=c(rgb(1,0,0,0.8),
#              rgb(0,0,1,0.8),
#              rgb(0,0.5,1,0.8),
#              rgb(0.5,0.25,0,0.8),
#              rgb(0.5,0,0.75,0.8)
#             ))