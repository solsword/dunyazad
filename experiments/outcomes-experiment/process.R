#!/usr/bin/env Rscript

# TODO: compare vs. normal & vs. all-neutral

# Note: instructions on MWW effect size calculation & reporting:
# http://yatani.jp/teaching/doku.php?id=hcistats:mannwhitney

# Note: confidence intervals for median from:
# http://exploringdatablog.blogspot.sk/2012/04/david-olives-median-confidence-interval.html

library(iterators) # iter() function
library(foreach) # foreach function and %do% operator
library(likert) # likert graphing *Doesn't do grouping any more :(
#library(reshape) # WOULD CAUSE likert error
library(plyr) # rename function
library(coin) # wilcox_test allows computing effect sizes
library(boot) # for bootstrap confidence intervals
library(lattice) # for histogram

# Switch for hypothesis testing:
test.hypotheses = TRUE
#test.hypotheses = FALSE

qnames = c(
  "opt.obvious" = "Considering just the options, there seems to be a clear best option at this choice.",
  "opt.balanced" = "Ignoring outcomes, the options at this choice all seem about equally good (or bad).",
  "opt.nobad" = "Ignoring outcomes, there are no options that seem bad at this choice.",
  "opt.nogood" = "Ignoring outcomes, none of the options at this choice seem good.",
  "opt.stakes" = "Considering just the options, the stakes for this choice seem low.",
  "out.fair" = "Given the options available, the outcome I got is fair.",
  "out.sense" = "The outcome that I got makes sense given the option that I selected.",
  "out.bad" = "I got a bad outcome.",
  "out.happy" = "I'm happy with the option that I chose.",
  "out.unfair" = "The outcome that I got is unfair, given the options available.",
  "out.unexpected" = "The outcome that I got is completely unexpected.",
  "out.trick" = "There is an outcome. (This is a trick question to test whether you're paying attention. Please simply indicate that you are in complete disagreement.)",
  "out.broken" = "There might be a problem with this choice--the outcome I got does not make sense.",
  "out.good" = "The outcome that I got is a good outcome.",
  "out.expected" = "I pretty much expected the outcome that I got.",
  "out.regret" = "I wish I had chosen a different option.",
  "motives" = "Which of the following motive(s) contributed to your decision? (pick one or more)",
  "judge.good" = "Which of the following judgement(s) contributes to how you generally define a \"good\" outcome in interactive experiences like the one you just played? (pick one or more)",
  "judge.bad" = "Which of the following judgement(s) contributes to how you generally define a \"bad\" outcome in interactive experiences like the one you just played? (pick one or more)",
  "consistency" = "Do you feel you approach all interactive experiences (e.g., Choose-Your-Own-Adventure novels, video games, tabletop role-playing games, etc.) with a consistent set of motivations and judgements, or do your motivations and judgements change from story to story?",
  "extra.feedback" = "If you have any other feedback you'd like to give, feel free to enter it here."
)

snames = c(
  "opt.obvious" = "[...] there [is] a clear best option [...].",
  "opt.balanced" = "[...] [the options] [seem about equally promising].",
  "opt.nobad" = "[...] there are no options that seem bad [...].",
  "opt.nogood" = "[...] none of [the options] seem good.",
  "opt.stakes" = "[...] [the stakes] seem low.",

  "out.good" = "The outcome that I got is a good outcome.",
  "out.bad" = "I got a bad outcome.",

  "out.expected" = "I pretty much expected the outcome that I got.",
  "out.unexpected" = "The outcome that I got is completely unexpected.",

  "out.fair" = "[...] the outcome I got is fair.",
  "out.unfair" = "The outcome that I got is unfair [...].",

  "out.sense" = "The outcome that I got makes sense [...].",
  "out.broken" = "[...] the outcome I got does not make sense.",

  "out.happy" = "I'm happy with the option that I chose.",
  "out.regret" = "I wish I had chosen a different option.",

  "out.trick" = "There is an outcome. [...]",

  "motives" = "[Which] motive(s) contributed to your decision? [...]",
  "judge.good" = "[How do you define a \"good\" outcome]?",
  "judge.bad" = "[How you define a \"bad\" outcome]?",
  "consistency" = "Do [you] approach [all experiences] [the same way]?",
  "extra.feedback" = "If you have any other feedback you'd like to give, [enter it here]."
)

likert_questions = c(
  "opt.obvious",
  "opt.balanced",
  "opt.nobad",
  "opt.nogood",
  "opt.stakes",

  "out.good",
  "out.bad",

  "out.expected",
  "out.unexpected",

  "out.fair",
  "out.unfair",

  "out.sense",
  "out.broken",

  "out.happy",
  "out.regret"

#  "out.trick"
)

colnames = c(
  "WorkerId" = "worker",
  "WorkTimeInSeconds" = "duration",
  "AssignmentStatus" = "status",
  "Input.seed" = "seed",
  "Input.condition" = "condition",
  "Input.chosen_setup" = "setup",
  "Answer.decision.backup" = "decision",
  "Answer.opt.balanced" = "opt.balanced",
  "Answer.opt.nobad" = "opt.nobad",
  "Answer.opt.nogood" = "opt.nogood",
  "Answer.opt.obvious" = "opt.obvious",
  "Answer.opt.stakes" = "opt.stakes",
  "Answer.out.bad" = "out.bad",
  "Answer.out.broken" = "out.broken",
  "Answer.out.expected" = "out.expected",
  "Answer.out.fair" = "out.fair",
  "Answer.out.good" = "out.good",
  "Answer.out.happy" = "out.happy",
  "Answer.out.regret" = "out.regret",
  "Answer.out.sense" = "out.sense",
  "Answer.out.trick" = "out.trick",
  "Answer.out.unexpected" = "out.unexpected",
  "Answer.out.unfair" = "out.unfair",
  "Answer.motives" = "motives",
  "Answer.motives.other" = "motives.other",
  "Answer.judge.good" = "judge.good",
  "Answer.judge.good.other" = "judge.good.other",
  "Answer.judge.bad" = "judge.bad",
  "Answer.judge.bad.other" = "judge.bad.other",
  "Answer.consistency" = "consistency",
  "Answer.consistency.other" = "consistency.other",
  "Answer.extra.feedback" = "extra.feedback"
)

conditions = c(
  "expected_success",
  "unexpected_failure",
  "obvious_success",
  "obvious_failure",
  "expected_failure",
  "unexpected_success"
)

correct_decisions = c(
  "20739" = 1,
  "28306" = 3,
  "33152" = NA,
  "40550" = NA,
  "46585" = NA,
  "47371" = 2,
  "47794" = NA,
  "57614" = NA,
  "58403" = NA,
  "67832" = NA,
  "76886" = NA,
  "8015"  = NA,
  "82994" = NA,
  "8638"  = 2,
  "87991" = NA,
  "9114"  = 2,
  "95923" = 2,
  "99500" = NA,
  "--"    = NA
)


filterfactor <- function (data) {
  filtered <- data
  # Filter out too-quick, rejected, and tricked responses:
  filtered <- filtered[
    filtered[["AssignmentStatus"]] == "Approved"
    ,
  ]
  # Get rid of irrelevant columns:
  filtered <- filtered[
    ,
    names(filtered) %in% names(colnames)
  ]
  # Rename things:
  filtered <- rename(filtered, colnames)
  # Filter out incomplete entries:
  # TODO: we'll ignore this for now...
  #filtered <- filtered[
  #  complete.cases(filtered)
  #  ,
  #]

  # Filter out any entries by workers who completed more than one:
  #did_multiple <- duplicated(filtered[["worker"]])
  #print("A")
  #print(filtered[which(duplicated(filtered[["worker"]])),][["worker"]])
  #print("B")
  #print(nrow(filtered))
  #print("C")
  filtered <- filtered[
    !duplicated(filtered[["worker"]]) | filtered[["condition"]] == "uniform"
    ,
  ]

  # Add columns for the "best" option and whether the answer given matched it:
  filtered$best.option <- correct_decisions[as.character(filtered[["seed"]])]
  filtered$decision.case <- ifelse(
      is.na(filtered$best.option)
    | as.character(filtered$decision) == as.character(filtered$best.option)
    ,
    "main",
    "alt"
  )

  #foreach(row=iter(filtered, by="row"), .combine=rbind) %do% (
  #  row
  #)

  for(i in 1:nrow(filtered)) {
    seedhere <- filtered[i,"seed"]
    sameseed <- filtered[
      filtered$seed == seedhere
      , "decision"
    ]
    st <- sort(table(sameseed),decreasing=TRUE)
    filtered[i,"popular.option"] <- names(st)[1]
  }
  filtered$was.obvious <- (
    as.character(filtered$best.option) == as.character(filtered$popular.option)
  )

  # Turn things into factors:
  for (name in names(filtered)) {
    #cat("processing", name, "\n")
    if (
      name
    %in%
      c("worker", "status", "seed", "condition", "setup", "consistency")
    ) {
      filtered[[name]] = factor(filtered[[name]], ordered=FALSE)
    } else if (
      name
    %in%
      c(
        "duration",
        "decision",
        "motives.other",
        "judge.good.other",
        "judge.bad.other",
        "consistency.other",
        "extra.feedback",
        "best.option",
        "decision.case",
        "popular.option",
        "was.obvious"
      )
    ) {
      # nothing here...
    } else if (name %in% c("motives", "judge.good", "judge.bad")) {
      # Split on '|' and add new columns for the results:
      for (i in 1:nrow(filtered)) {
        for (val in strsplit(as.character(filtered[i,name]), "\\|")[[1]]) {
          if (val == "--") { break }
          filtered[i, mrsp(name, val)] <- TRUE
        }
      }
      for (
        col in
        names(filtered)[
          substr(names(filtered), 1, nchar(name)+2) == paste(name, "c", sep=".")
        ]
      ) {
        filtered[is.na(filtered[,col]),col] = FALSE
      }
    } else {
      # cat("FACTORIZE", name, "\n")
      filtered[[name]] = factor(
        filtered[[name]],
        levels=c(1, 2, 3, 4, 5),
        ordered=TRUE
      )
    }
  }
  return(filtered)
}

test_error <- c(
  "passed"="ERROR",
  "pvalue"=NA,
  "mr.compare"=NA,
  "mr.against"=NA,
  "effectsize"=NA,
  "commoneffect"=NA,
  "conf.low"=NA,
  "conf.high"=NA
)

testhypothesis <- function(hyp, data) {
  htype <- as.character(hyp[["type"]]) # TODO: USE THIS
  if (htype == "pctchosen") {
    return(testpcthypothesis(hyp, data))
  } else {
    return(testnormalhypothesis(hyp, data))
  }
}

# For creating individual column names for the multiple-response data:
mrsp <- function(question, response) {
  return(paste(question, "c", gsub("-", ".", response), sep="."))
}

testpcthypothesis <- function(hyp, data) {
  testvar <- as.character(hyp[["question"]])
  filter <- as.character(hyp[["compare"]])
  invert <- FALSE
  if (substr(filter, 1, 1) == "!") {
    filter <- substr(filter, 2, nchar(filter))
    invert <- TRUE
  }
  condition <- as.character(hyp[["against"]])
  cmptrg <- as.character(hyp[["predict"]])
  compare <- substr(cmptrg, 1, 1)
  target <- as.numeric(substr(cmptrg, 2, nchar(cmptrg)))

  filtered <- data[data$is.real,]

  if (testvar == "consistency") { # TODO~ Generalize this check
    if (filter != "all") {
      if (invert) {
        filtered <- filtered[filtered[[testvar]] != filter,]
      } else {
        filtered <- filtered[filtered[[testvar]] == filter,]
      }
    }
    matches <- filtered[filtered[[testvar]] == condition,]
  } else {
    if (filter != "all") {
      if (invert) {
        filtered <- filtered[!filtered[[mrsp(testvar, filter)]],]
      } else {
        filtered <- filtered[filtered[[mrsp(testvar, filter)]],]
      }
    }
    matches <- filtered[filtered[[mrsp(testvar, condition)]],]
  }

  count.base <- nrow(filtered)
  count.matches <- nrow(matches)

  result <- count.matches / count.base

  if (compare == "<") {
    passed <- result < target
  } else if (compare == ">") {
    passed <- result > target
  }

  return (list(
    "type"="count",
    "compare"=compare,
    "target"=target,
    "count.base"=count.base,
    "count.matches"=count.matches,
    "value"=result,
    "passed"=passed
  ))
}

testnormalhypothesis <- function(hyp, data) {
  testvar <- as.character(hyp[["question"]])
  compcol <- "ERROR"
  agcol <- "ERROR"
  onlymain <- "FALSE"
  compare <- hyp[["compare"]]
  against <- hyp[["against"]]
  subcondition <- NA
  subagainst <- NA
  require.was.obv.cond <- FALSE
  require.was.obv.ag <- FALSE
  if (grepl("\\(", compare)) {
    subcondition <- sub(".*\\(", "", sub("\\)$", "", compare))
    compare <- sub("\\(.*\\)$", "", compare)
  }
  if (grepl("\\(", against)) {
    subagainst <- sub(".*\\(", "", sub("\\)$", "", against))
    against <- sub("\\(.*\\)$", "", against)
  }
  if (grepl("\\[", compare)) {
    subcondition <- sub(".*\\[", "", sub("\\]$", "", compare))
    compare <- sub("\\[.*\\]$", "", compare)
    require.was.obv.cond <- TRUE
  }
  if (grepl("\\[", against)) {
    subagainst <- sub(".*\\[", "", sub("\\]$", "", against))
    against <- sub("\\[.*\\]$", "", against)
    require.was.obv.ag <- TRUE
  }
  if (compare %in% data[["condition"]]) {
    compcol <- "condition"
  } else if (compare %in% data[["seed"]]) {
    compcol <- "seed"
  } else if (compare %in% data[["stakes"]]) {
    compcol <- "stakes"
  } else if (compare %in% data[["setup"]]) {
    compcol <- "setup"
  } else if (compare %in% data[["condition"]]) {
  } else {
    cat("Error: invalid hypothesis condition:", paste("'", as.character(compare), "'", sep=""), "\n")
    return(test_error)
  }
  if (against %in% data[["condition"]]) {
    agcol <- "condition"
  } else if (against %in% data[["seed"]]) {
    agcol <- "seed"
  } else if (against %in% data[["stakes"]]) {
    agcol <- "stakes"
  } else if (against %in% data[["setup"]]) {
    agcol <- "setup"
  } else {
    cat("Error: invalid hypothesis against:", paste("'", as.character(against), "'", sep=""), "\n")
    return(test_error)
  }
  testcol <- data[
    as.character(data[[compcol]]) == as.character(compare)
  | as.character(data[[agcol]]) == as.character(against)
    ,
  ]
  if (!is.na(subcondition)) {
    testcol <- testcol[
        testcol[["decision.case"]] == subcondition
      | as.character(testcol[[compcol]]) != as.character(compare)
      ,
    ]
  }
  if (!is.na(subagainst)) {
    testcol <- testcol[
        testcol[["decision.case"]] == subagainst
      | as.character(testcol[[agcol]]) != as.character(against)
      ,
    ]
  }
  if (require.was.obv.cond) {
    testcol <- testcol[
        testcol[["was.obvious"]]
      | as.character(testcol[[compcol]]) != as.character(compare)
      ,
    ]
  }
  if (require.was.obv.ag) {
    testcol <- testcol[
        testcol[["was.obvious"]]
      | as.character(testcol[[agcol]]) != as.character(against)
      ,
    ]
  }
  is.compare = factor(
    apply(
      testcol,
      1,
      function(row) {
        return(
          as.character(row[[compcol]]) == as.character(compare)
        )
      }
    )
  )
  testcol <- testcol[[testvar]]
  testcol <- sapply(
    testcol,
    function (datum) { return(as.numeric(as.character(datum))) }
  )
  rev <- as.character(hyp[["predict"]])
  if (rev == "greater") {
    rev <- "less"
  } else if (rev == "less") {
    rev <- "greater"
  } else {
    rev <- "error"
  }
  result <- wilcox_test(
    testcol ~ is.compare,
    distribution="exact",
    alternative=rev,
    conf.int=TRUE,
    conf.level=0.95
  )
  #compare <- data[
  #  as.character(data[["condition"]]) == as.character(compare)
  #  ,
  #]
  #against <- data[
  #  as.character(data[["condition"]]) == as.character(against)
  #  ,
  #]
  #compare <- as.list(as.numeric(compare[[testvar]]))
  #against <- as.list(as.numeric(against[[testvar]]))
  #grouping = factor(c(rep("compare", length(compare)), rep("
  #result <- wilcox.test(
  #  as.numeric(compare[[testvar]]),
  #  as.numeric(against[[testvar]]),
  #  paired=FALSE,
  #  exact=FALSE,
  #  alternative=as.character(hyp[["predict"]])
  #)
  pass <- as.logical(pvalue(result) < 0.05)
  ranks <- rank(testcol, ties.method="average")
  framed <- data.frame(ranks, is.compare)

  mr.compare <- mean(framed[framed$is.compare == TRUE,][["ranks"]])
  mr.against <- mean(framed[framed$is.compare == FALSE,][["ranks"]])

  count.comp <- nrow(framed[framed$is.compare == TRUE,])
  count.ag <- nrow(framed[framed$is.compare == FALSE,])

  #raweffect <- result@statistic@linearstatistic
  effect <- abs(statistic(result, "test")[["FALSE"]] / sqrt(length(testcol)))
  commoneffect <- 0.5 + effect / 2.0
  conf.low <- confint(result)[["conf.int"]][[1]]
  conf.high <- confint(result)[["conf.int"]][[2]]
  return(
    list(
      "type"="mww",
      "passed"=pass,
      "pvalue"=pvalue(result),
      "mr.compare"=mr.compare,
      "mr.against"=mr.against,
      "effectsize"=effect,
      "commoneffect"=commoneffect,
      "conf.low"=conf.low,
      "conf.high"=conf.high,
      "count.comp"=count.comp,
      "count.ag"=count.ag
    )
  )
}

DOliveCIproc <- function(y, alpha = 0.05){
  #
  #  This procedure implements David Olive's simple
  #  median confidence interval, along with the standard
  #  confidence interval for the mean, for comparison
  #
  #  First, compute the median
  #
  n <- length(y)
  ysort <- sort(y)
  nhalf <- floor(n/2)
  if (2*nhalf < n){
    #  n odd
    med <- ysort[nhalf + 1]
  }
  else{
    # n even
    med <- (ysort[nhalf] + ysort[nhalf+1])/2
  }
  #
  #  Next, compute Olive’s standard error for the median
  #
  Ln <- nhalf - ceiling(sqrt(n/4))
  Un <- n - Ln
  SE <- 0.5*(ysort[Un] - ysort[Ln+1])
  #
  #  Compute the confidence interval based on Student’s t-distribution
  #  The degrees of freedom parameter p is discussed in Olive’s paper
  #
  p <- Un - Ln - 1
  t <- qt(p = 1 - alpha/2, df = p)
  medLCI <- med - t * SE
  medUCI <- med + t * SE
  #
  #  Next, compute the mean and its classical confidence interval
  #
  mu <- mean(y)
  SEmu <- sd(y)/sqrt(n)
  tmu <- qt(p = 1 - alpha/2, df = n-1)
  muLCI <- mu - tmu * SEmu
  muUCI <- mu + tmu * SEmu
  #
  #  Finally, return a data frame with all of the results computed here
  #
  result <- data.frame(
    Median = med,
    LCI = medLCI,
    UCI = medUCI,
    Mean = mu,
    MeanLCI = muLCI,
    MeanUCI = muUCI,
    N = n,
    dof = p,
    tmedian = t,
    tmean = tmu,
    SEmedian = SE,
    SEmean = SEmu
  )
  return(result)
}

#responses <- read.csv(file="study-results.csv",head=TRUE,sep=",")
responses <- read.csv(file="full-results.csv",head=TRUE,sep=",")
hypotheses <- read.csv(file="hypotheses.csv",head=TRUE,sep=",")

filtered <- filterfactor(responses)
filtered$is.real <- filtered[["duration"]] != -1
real <- filtered[filtered[["is.real"]],]

cat("\n---\n")

cat("Total responses:", nrow(responses), "\n")
cat("Acceptable responses:", nrow(filtered), "\n")
cat("Real responses:", nrow(real), "\n")
uniform <- filtered[filtered$condition == "uniform",]
cat(" uniform:", nrow(uniform), "\n")

exp_succ <- filtered[filtered$condition == "expected_success",]

unx_succ <- filtered[filtered$condition == "unexpected_success",]

exp_fail <- filtered[filtered$condition == "expected_failure",]

unx_fail <- filtered[filtered$condition == "unexpected_failure",]

obv_succ <- filtered[filtered$condition == "obvious_success",]

obv_fail <- filtered[filtered$condition == "obvious_failure",]

cat(" exp-succ:", nrow(exp_succ), "\n")
cat("    market:", nrow(exp_succ[exp_succ$setup == "market",]), "\n")
cat("    threat:", nrow(exp_succ[exp_succ$setup=="threatened_innocents",]),"\n")
cat("    monstr:", nrow(exp_succ[exp_succ$setup == "monster_attack",]), "\n")
cat(" unx-fail:", nrow(unx_fail), "\n")
cat("    market:", nrow(unx_fail[unx_fail$setup == "market",]), "\n")
cat("    threat:", nrow(unx_fail[unx_fail$setup=="threatened_innocents",]),"\n")
cat("    monstr:", nrow(unx_fail[unx_fail$setup == "monster_attack",]), "\n")
cat(" exp-fail:", nrow(exp_fail), "\n")
cat("    market:", nrow(exp_fail[exp_fail$setup == "market",]), "\n")
cat("    threat:", nrow(exp_fail[exp_fail$setup=="threatened_innocents",]),"\n")
cat("    monstr:", nrow(exp_fail[exp_fail$setup == "monster_attack",]), "\n")
cat(" unx-succ:", nrow(unx_succ), "\n")
cat("    market:", nrow(unx_succ[unx_succ$setup == "market",]), "\n")
cat("    threat:", nrow(unx_succ[unx_succ$setup=="threatened_innocents",]),"\n")
cat("    monstr:", nrow(unx_succ[unx_succ$setup == "monster_attack",]), "\n")
cat(
  " obv-succ:", nrow(obv_succ),
  "(", nrow(obv_succ[obv_succ$decision.case == "main",]),
  "/", nrow(obv_succ[obv_succ$decision.case == "alt",]), ")",
  "\n"
)
cat("    market:", nrow(obv_succ[obv_succ$setup == "market",]), "\n")
cat("    threat:", nrow(obv_succ[obv_succ$setup=="threatened_innocents",]),"\n")
cat("    monstr:", nrow(obv_succ[obv_succ$setup == "monster_attack",]), "\n")
cat(
  " obv-fail:", nrow(obv_fail),
  "(", nrow(obv_fail[obv_fail$decision.case == "main",]),
  "/", nrow(obv_fail[obv_fail$decision.case == "alt",]), ")",
  "\n"
)
cat("    market:", nrow(obv_fail[obv_fail$setup == "market",]), "\n")
cat("    threat:", nrow(obv_fail[obv_fail$setup=="threatened_innocents",]),"\n")
cat("    monstr:", nrow(obv_fail[obv_fail$setup == "monster_attack",]), "\n")
cat("\n")
cat("\n---\n")
#
cat("Median response time:", median(responses[responses$WorkTimeInSeconds != -1,]$WorkTimeInSeconds),"\n")
cat("Median accepted response time:", median(real$duration), "\n")

cat("\n---\n")

#cat("General statistics:\n")
#for (cond in conditions) {
#  cat(" ", cond, ":\n")
#  for (
#    question
#  in
#    c(
#      "opt.balanced",
#      "opt.nobad",
#      "opt.nogood",
#      "opt.stakes"
#    )
#  ) {
#    col <- filtered[filtered$condition == cond,][[question]]
#    num <- as.numeric(as.character(col[!is.na(col)]))
#   # cis <- DOliveCIproc(num)
#   # wcis <- wilcox.test(
#   #   num,
#   #   conf.int=TRUE,
#   #   conf.level=0.95,
#   #   alternative="two.sided",
#   #   correct=TRUE
#   # )
#    boottype <- "basic"
#    bcis <- boot.ci(
#      boot(num, function(x, i) median(x[i]), R=1000),
#      type=boottype
#    )
#    cat(
#      "   ",
#      format(question, width=12, justify="left"),
#      #format(sprintf("%0.2f", cis[["LCI"]]), width=5, justify="right"),
#      #format(sprintf("%0.2f", wcis[["conf.int"]][[1]]),width=5,justify="right"),
#      format(sprintf("%0.2f", bcis[[boottype]][[4]]), width=5, justify="right"),
#      "--",
#      median(num),
#      "--",
#      #format(sprintf("%0.2f", cis[["UCI"]]), width=5, justify="right"),
#      #format(sprintf("%0.2f", wcis[["conf.int"]][[2]]),width=5,justify="right"),
#      format(sprintf("%0.2f", bcis[[boottype]][[5]]), width=5, justify="right"),
#      "\n"
#    )
#  }
#}


if (test.hypotheses) {
cat("\n---\n")

cat("Hypotheses:\n")
cat(
"Question                                              Compare:               Pred:                   Pass:   P:            effect:  #comp:  #ag:\n"
)
  for (hidx in 1:nrow(hypotheses)) {
    hyp <- as.list(hypotheses[hidx,])
    pred <- "unknown"
    if (hyp[["against"]] == "uniform" | hyp[["against"]] == "normal") {
      if (hyp[["predict"]] == "greater") {
        pred <- "agree"
      } else if (hyp[["predict"]] == "less") {
        pred <- "disagree"
      }
    } else {
      if (hyp[["predict"]] == "greater") {
        pred <- paste(">", hyp[["against"]], sep="")
      } else if (hyp[["predict"]] == "less") {
        pred <- paste("<", hyp[["against"]], sep="")
      }
    }
    result <- testhypothesis(hyp, filtered)
    if (result[["type"]] == "mww") {
      if (result[["passed"]] != "ERROR") {
        cat(
          "",
          format(snames[[as.character(hyp[["question"]])]], width=53, justify="left"),
          format(hyp[["compare"]], width=22, justify="left"),
          format(pred, width=23, justify="left"),
          format(
            as.character(as.logical(result[["passed"]])),
            width=6,
            justify="left"
          ),
          format(result[["pvalue"]], width=12, justify="left"),
          " ",
          format(sprintf("%0.3f", result[["commoneffect"]]), width=8, justify="left"),
          format(sprintf("%d", result[["count.comp"]]), width=7, justify="left"),
          format(sprintf("%d", result[["count.ag"]]), width=7, justify="left"),
  #        format(sprintf("%0.3f", result[["effectsize"]]), width=7, justify="left"),
  #        format(result[["conf.low"]], width=4, justify="left"),
  #        format(result[["conf.high"]], width=4, justify="left"),
          "\n"
        )
      } else {
        cat(
          "",
          format(snames[[as.character(hyp[["question"]])]], width=53, justify="left"),
          format(result[["compare"]], width=22, justify="left"),
          format(pred, width=23, justify="left"),
          "ERROR",
          "\n"
        )
      }
    } else if (result[["type"]] == "count") {
      if (result[["passed"]] != "ERROR") {
        cat(
          "",
          format(snames[[as.character(hyp[["question"]])]], width=53, justify="left"),
          format(
            paste(
              "#", hyp[["against"]], " / #", hyp[["compare"]],
              sep=""
            ),
            width=22,
            justify="left"
          ),
          format(
            paste(
              result[["compare"]], " ",
              result[["target"]],
              sep=""
            ),
            width=23,
            justify="left"
          ),
          format(
            as.character(as.logical(result[["passed"]])),
            width=6,
            justify="left"
          ),
          format(sprintf("%0.2f", result[["value"]]), width=8, justify="right"),
          format(
            paste(
              format(sprintf("%d", result[["count.matches"]]), width=3, justify="right"),
              "/",
              format(sprintf("%d", result[["count.base"]]), width=3, justify="left"),
              sep=""
            ),
            width=11,
            justify="right"
          ),
          "  ",
          format("NA", width=7, justify="left"),
          format("NA", width=7, justify="left"),
          "\n"
        )
      } else {
        cat(
          "",
          format(snames[[as.character(hyp[["question"]])]], width=53, justify="left"),
          format(
            paste("#", hyp[["against"]], " / #", hyp[["compare"]], "...", sep=""),
            width=23,
            justify="left"
          ),
          "ERROR",
          "\n"
        )
      }
    } else {
        cat(
          "ERROR: Invalid test result type",
          paste("'", result[["type"]], "'", sep=""),
          "!\n"
        )
    }
  }
}

cat("\n---\n")

# Get rid of fake entries:

filtered <- filtered[filtered$is.real,]

# Prevent anyone from gouging their eyes out:
sb <- trellis.par.get("strip.background")
sb[["col"]][1] <- "#ddeeff"
trellis.par.set("strip.background", sb)

# Choice histograms
# -----------------

pdf(file="reports/choices.pdf",title="dunyazad-outcomes-choices.report")
filtered$seed = factor(filtered$seed)
histogram(
  ~ decision | seed,
  data=filtered[filtered$condition=="expected_success",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="expected_success"
)
histogram(
  ~ decision | seed,
  data=filtered[filtered$condition=="unexpected_failure",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="unexpected_failure"
)
histogram(
  ~ decision | seed,
  data=filtered[filtered$condition=="obvious_success",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="obvious_success"
)
histogram(
  ~ decision | seed,
  data=filtered[filtered$condition=="obvious_failure",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="obvious_failure"
)
histogram(
  ~ decision | seed,
  data=filtered[filtered$condition=="expected_failure",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="expected_failure"
)
histogram(
  ~ decision | seed,
  data=filtered[filtered$condition=="unexpected_success",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="unexpected_success"
)
dev.off()

## Get rid of the "decision" and "out.trick" columns:
#
#filtered <- filtered[,!( names(filtered) %in% c("decision", "out.trick") )]

# Likert reports:
# ---------------

likert_names = snames[names(snames) %in% likert_questions]

# Combined
# --------

page.1 = 1:5
page.2 = 6:9
page.3 = 10:13
page.4 = c(14:length(likert_questions), 1:2)

#print(likert_questions[page.1])
#print(likert_questions[page.2])
#print(likert_questions[page.3])
#print(likert_questions[page.4])
#exit

ordered <- filtered[,names(filtered) %in% likert_questions][,likert_questions]

report <- rename(ordered, likert_names)

grouping <- filtered[["condition"]]

for (i in 1:ncol(report)) {
  lk <- likert(report[i], grouping=grouping, nlevels=5)
  pdf(
    file=paste("reports/outcomes-report-", sprintf("%02d", i), "-", likert_questions[i], ".pdf", sep=""),
    title=paste("dunyazad-outcomes:", likert_questions[i]),
    width=7,
    height=2.8
  )
  p <- plot(lk, group.order=conditions, ordered=FALSE)
  show(p)
  dev.off()
}


# Individual Conditions
# ---------------------

for (cond in conditions) {
  pdf(
      file=paste("reports/report-", cond, ".pdf", sep=""),
      title=paste("dunyazad-report-", cond, sep="")
  )
  report <- filtered[
    filtered$condition == cond,
    names(filtered) %in% names(likert_names)
  ]
  report <- rename(report, likert_names)
  lk = likert(report)
  lk$results = lk$results[match(likert_names, lk$results$Item),]
  p <- plot(lk, ordered=FALSE, group.order=likert_names)
  show(p)
  dev.off()
}
