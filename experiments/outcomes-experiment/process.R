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
test.hypotheses = FALSE
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


filterfactor <- function (input) {
  filtered <- input
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

# Class for test results:
setClass(
  "HResult",
  representation = representation(
    # For both:
    type = "character",
    subtype = "character",

    in.question = "character",
    in.compare = "character",
    in.against = "character",

    hyp.compare = "character",
    hyp.compcol = "character",
    hyp.subcomp = "character",

    hyp.against = "character",
    hyp.agcol = "character",
    hyp.subag = "character",

    hyp.predict = "character",

    count.comp = "numeric",
    count.ag= "numeric",

    result.confirmed = "logical",

    # For MWW results:
    hyp.req.was.obv.comp = "logical",
    hyp.req.was.obv.ag = "logical",

    result.mr.compare = "numeric",
    result.mr.against = "numeric",
    result.pvalue = "numeric",
    result.effect = "numeric",
    result.effect.common = "numeric",
    result.conf.low = "numeric",
    result.conf.high = "numeric",

    # For counting results:
    hyp.invertcomp = "logical",
    hyp.target = "numeric",

    result.count.base = "numeric",
    result.count.matches = "numeric",
    result.value = "numeric"
  )
)

# TODO: These?
#as.HResult <- function(x) { UseMethod("as.HResult") }
#
#as.HResult.list <- function(l) {
#  # TODO: HERE!
#  class(l) <- "HResult"; return(l)
#}

test_error <- new("HResult") 

parsehypothesis <- function(hyp, input) {
  htype <- as.character(hyp[["type"]])
  if (htype == "pctchosen") {
    return(parsecounthypothesis(hyp))
  } else {
    return(parsemwwhypothesis(hyp, input))
  }
}

testhypothesis <- function(hyp, input) {
  if (hyp@type == "count") {
    return(testpcthypothesis(hyp, input))
  } else {
    return(testmwwhypothesis(hyp, input))
  }
}

# For creating individual column names for the multiple-response data:
mrsp <- function(question, response) {
  return(paste(question, "c", gsub("-", ".", response), sep="."))
}

parsecounthypothesis <- function(hyp) {
  rval <- new("HResult")
  rval@type <- "count"
  rval@subtype <- as.character(hyp[["type"]])
  rval@hyp.compcol <- "ERROR"
  rval@hyp.agcol <- "ERROR"
  rval@in.question <- as.character(hyp[["question"]])
  rval@in.compare <- as.character(hyp[["compare"]])
  rval@in.against <- as.character(hyp[["against"]])
  rval@hyp.invertcomp <- FALSE

  rval@hyp.compare <- as.character(hyp[["compare"]])
  if (substr(rval@hyp.compare, 1, 1) == "!") {
    rval@hyp.compare <- substr(rval@hyp.compare, 2, nchar(rval@hyp.compare))
    rval@hyp.invertcomp <- TRUE
  }
  rval@hyp.against <- as.character(hyp[["against"]])
  cmptrg <- as.character(hyp[["predict"]])
  rval@hyp.predict <- substr(cmptrg, 1, 1)
  rval@hyp.target <- as.numeric(substr(cmptrg, 2, nchar(cmptrg)))

  return(rval)
}

parsemwwhypothesis <- function(hyp, input) {
  rval <- new("HResult")
  rval@type = "mww"
  rval@subtype <- as.character(hyp[["type"]])
  rval@hyp.compcol <- "ERROR"
  rval@hyp.agcol <- "ERROR"
  rval@in.question <- as.character(hyp[["question"]])
  rval@in.compare <- as.character(hyp[["compare"]])
  rval@in.against <- as.character(hyp[["against"]])
  rval@hyp.predict <- as.character(hyp[["predict"]])
  rval@hyp.req.was.obv.comp <- FALSE
  rval@hyp.req.was.obv.ag <- FALSE
  rval@hyp.compare <- rval@in.compare
  rval@hyp.against <- rval@in.against
  if (grepl("\\(", rval@in.compare)) {
    rval@hyp.subcomp <- sub(".*\\(", "", sub("\\)$", "", rval@in.compare))
    rval@hyp.compare <- sub("\\(.*\\)$", "", rval@in.compare)
  }
  if (grepl("\\(", rval@in.against)) {
    rval@hyp.subag <- sub(".*\\(", "", sub("\\)$", "", rval@in.against))
    rval@hyp.against <- sub("\\(.*\\)$", "", rval@in.against)
  }
  if (grepl("\\[", rval@in.compare)) {
    rval@hyp.subcomp <- sub(".*\\[", "", sub("\\]$", "", rval@in.compare))
    rval@hyp.compare <- sub("\\[.*\\]$", "", rval@in.compare)
    rval@hyp.req.was.obv.comp <- TRUE
  }
  if (grepl("\\[", rval@in.against)) {
    rval@hyp.subag <- sub(".*\\[", "", sub("\\]$", "", rval@in.against))
    rval@hyp.against <- sub("\\[.*\\]$", "", rval@in.against)
    rval@hyp.req.was.obv.ag <- TRUE
  }
  if (rval@hyp.compare %in% input[["condition"]]) {
    rval@hyp.compcol <- "condition"
  } else if (rval@hyp.compare %in% input[["seed"]]) {
    rval@hyp.compcol <- "seed"
  } else if (rval@hyp.compare %in% input[["stakes"]]) {
    rval@hyp.compcol <- "stakes"
  } else if (rval@hyp.compare %in% input[["setup"]]) {
    rval@hyp.compcol <- "setup"
  } else if (rval@hyp.compare %in% input[["condition"]]) {
  } else {
    cat(
      "Error: invalid hypothesis condition:",
      paste("'", as.character(rval@hyp.compare), "'", sep=""),
      "\n"
    )
    return(test_error)
  }
  if (rval@hyp.against %in% input[["condition"]]) {
    rval@hyp.agcol <- "condition"
  } else if (rval@hyp.against %in% input[["seed"]]) {
    rval@hyp.agcol <- "seed"
  } else if (rval@hyp.against %in% input[["stakes"]]) {
    rval@hyp.agcol <- "stakes"
  } else if (rval@hyp.against %in% input[["setup"]]) {
    rval@hyp.agcol <- "setup"
  } else {
    cat(
      "Error: invalid hypothesis condition:",
      paste("'", as.character(rval@hyp.against), "'", sep=""),
      "\n"
    )
    return(test_error)
  }
  return(rval)
}

testpcthypothesis <- function(hyp, data) {

  filtered <- data[data$is.real,]

  if (hyp@in.question == "consistency") { # TODO~ Generalize this check
    if (hyp@hyp.compare != "all") {
      if (hyp@hyp.invertcomp) {
        filtered <- filtered[filtered[[hyp@in.question]] != hyp@hyp.compare,]
      } else {
        filtered <- filtered[filtered[[hyp@in.question]] == hyp@hyp.compare,]
      }
    }
    matches <- filtered[filtered[[hyp@in.question]] == hyp@hyp.against,]
  } else {
    if (hyp@hyp.compare != "all") {
      if (hyp@hyp.invertcomp) {
        filtered <- filtered[
          !filtered[[mrsp(hyp@in.question, hyp@hyp.compare)]]
          ,
        ]
      } else {
        filtered <- filtered[
          filtered[[mrsp(hyp@in.question, hyp@hyp.compare)]]
          ,
        ]
      }
    }
    matches <- filtered[filtered[[mrsp(hyp@in.question, hyp@hyp.against)]],]
  }

  hyp@result.count.base <- nrow(filtered)
  hyp@result.count.matches <- nrow(matches)

  hyp@result.value <- hyp@result.count.matches / hyp@result.count.base

  if (hyp@hyp.predict == "<") {
    hyp@result.confirmed <- hyp@result.value < hyp@hyp.target
  } else if (hyp@hyp.predict == ">") {
    hyp@result.confirmed <- hyp@result.value > hyp@hyp.target
  }

  return(hyp)
}

testmwwhypothesis <- function(hyp, data) {
  testcol <- data[
      as.character(data[[hyp@hyp.compcol]]) == hyp@hyp.compare
    | as.character(data[[hyp@hyp.agcol]]) == hyp@hyp.against
    ,
  ]

  is.compare = as.character(testcol[[hyp@hyp.compcol]]) == hyp@hyp.compare
  is.against = as.character(testcol[[hyp@hyp.agcol]]) == hyp@hyp.against

  if (!identical(hyp@hyp.subcomp, character(0))) {
    testcol <- testcol[
        testcol[["decision.case"]] == hyp@hyp.subcomp
      | !is.compare
      ,
    ]
  }

  # Re-align conditions since rows may have changed:
  is.compare = as.character(testcol[[hyp@hyp.compcol]]) == hyp@hyp.compare
  is.against = as.character(testcol[[hyp@hyp.agcol]]) == hyp@hyp.against

  if (!identical(hyp@hyp.subag, character(0))) {
    testcol <- testcol[
        testcol[["decision.case"]] == hyp@hyp.subag
      | !is.against
      ,
    ]
  }

  # Re-align conditions since rows may have changed:
  is.compare = as.character(testcol[[hyp@hyp.compcol]]) == hyp@hyp.compare
  is.against = as.character(testcol[[hyp@hyp.agcol]]) == hyp@hyp.against

  if (hyp@hyp.req.was.obv.comp) {
    testcol <- testcol[
        testcol[["was.obvious"]]
      | !is.compare
      ,
    ]
  }

  # Re-align conditions since rows may have changed:
  is.compare = as.character(testcol[[hyp@hyp.compcol]]) == hyp@hyp.compare
  is.against = as.character(testcol[[hyp@hyp.agcol]]) == hyp@hyp.against

  if (hyp@hyp.req.was.obv.ag) {
    testcol <- testcol[
        testcol[["was.obvious"]]
      | !is.against
      ,
    ]
  }

  # Re-align conditions since rows may have changed:
  is.compare = as.character(testcol[[hyp@hyp.compcol]]) == hyp@hyp.compare
  is.against = as.character(testcol[[hyp@hyp.agcol]]) == hyp@hyp.against

  hyp@count.comp <- sum(is.compare, na.rm=TRUE)
  hyp@count.ag <- sum(is.against, na.rm=TRUE)

  testcol <- testcol[[hyp@in.question]]
  testcol <- sapply(
    testcol,
    function (datum) { return(as.numeric(as.character(datum))) }
  )
  rev <- hyp@hyp.predict
  if (rev == "greater") {
    rev <- "less"
  } else if (rev == "less") {
    rev <- "greater"
  } else {
    rev <- "error"
  }
  result <- wilcox_test(
    testcol ~ factor(is.compare),
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
  #compare <- as.list(as.numeric(compare[[hyp@in.question]]))
  #against <- as.list(as.numeric(against[[hyp@in.question]]))
  #grouping = factor(c(rep("compare", length(compare)), rep("
  #result <- wilcox.test(
  #  as.numeric(compare[[hyp@in.question]]),
  #  as.numeric(against[[hyp@in.question]]),
  #  paired=FALSE,
  #  exact=FALSE,
  #  alternative=as.character(hyp[["predict"]])
  #)
  hyp@result.pvalue <- pvalue(result)
  hyp@result.confirmed <- hyp@result.pvalue < 0.05

  ranks <- rank(testcol, ties.method="average")
  hyp@result.mr.compare <- mean(ranks[is.compare])
  hyp@result.mr.against <- mean(ranks[is.against])

  #raweffect <- result@statistic@linearstatistic
  hyp@result.effect <- abs(statistic(result, "test")[["FALSE"]] / sqrt(length(testcol)))
  hyp@result.effect.common <- 0.5 + hyp@result.effect / 2.0
  hyp@result.conf.low <- confint(result)[["conf.int"]][[1]]
  hyp@result.conf.high <- confint(result)[["conf.int"]][[2]]

  return(hyp)
}

display_result <- function (r) {
  pred <- "unknown"
  if (r@in.against == "uniform" | r@in.against == "normal") {
    if (r@hyp.predict == "greater") {
      pred <- "agree"
    } else if (r@hyp.predict == "less") {
      pred <- "disagree"
    }
  } else {
    if (r@hyp.predict == "greater") {
      pred <- paste(">", r@in.against, sep="")
    } else if (r@hyp.predict == "less") {
      pred <- paste("<", r@in.against, sep="")
    }
  }
  if (r@type == "mww") {
    if (r@result.confirmed %in% c(FALSE, TRUE)) {
      cat(
        "",
        format(snames[[r@in.question]], width=53, justify="left"),
        format(r@in.compare, width=22, justify="left"),
        format(pred, width=23, justify="left"),
        format(
          as.character(r@result.confirmed),
          width=6,
          justify="left"
        ),
        format(r@result.pvalue, width=12, justify="left"),
        " ",
        format(sprintf("%0.3f", r@result.effect.common), width=8, justify="left"),
        format(sprintf("%d", r@count.comp), width=7, justify="left"),
        format(sprintf("%d", r@count.ag), width=7, justify="left"),
#        format(sprintf("%0.3f", r@result.effectsize), width=7, justify="left"),
#        format(r@result.conf.low, width=4, justify="left"),
#        format(r@result.conf.high, width=4, justify="left"),
        "\n"
      )
    } else {
      cat(
        "",
        format(snames[[as.character(hyp[["question"]])]], width=53, justify="left"),
        format(r@result.compare, width=22, justify="left"),
        format(pred, width=23, justify="left"),
        "ERROR",
        "\n"
      )
    }
  } else if (r@type == "count") {
    if (r@result.confirmed != "ERROR") {
      cat(
        "",
        format(snames[[r@in.question]], width=53, justify="left"),
        format(
          paste(
            "#", r@in.against, " / #", r@in.compare,
            sep=""
          ),
          width=22,
          justify="left"
        ),
        format(
          paste(
            r@hyp.predict, " ",
            r@hyp.target,
            sep=""
          ),
          width=23,
          justify="left"
        ),
        format(
          as.character(r@result.confirmed),
          width=6,
          justify="left"
        ),
        format(sprintf("%0.1f%%", 100*r@result.value), width=8, justify="right"),
        format(
          paste(
            format(sprintf("%d", r@result.count.matches), width=3, justify="right"),
            "/",
            format(sprintf("%d", r@result.count.base), width=3, justify="left"),
            sep=""
          ),
          width=11,
          justify="right"
        ),
        "\n"
      )
    } else {
      cat(
        "",
        format(snames[[r@in.question]], width=53, justify="left"),
        format(
          paste("#", r@in.against, " / #", r@in.compare, "...", sep=""),
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
        paste("'", r@type, "'", sep=""),
        "!\n"
      )
  }
}

latex_result <- function (r, last=FALSE) {
  if (r@type == "empty") {
    if (last) {
      return("\\tenplast ")
    } else {
      return("\\tenp ")
    }
  } else if (r@type == "mww") {
    hyp <- "unknown"
    if (r@in.against == "uniform" | r@in.against == "normal") {
      if (r@hyp.predict == "greater") {
        hyp <- "A"
      } else if (r@hyp.predict == "less") {
        hyp <- "D"
      }
    } else {
      if (r@hyp.predict == "greater") {
        hyp <- paste(r@in.compare, ">", r@in.against, sep="")
      } else if (r@hyp.predict == "less") {
        hyp <- paste(r@in.compare, "<", r@in.against, sep="")
      }
    }
    p <- r@result.pvalue
    ef <- r@result.effect.common
    if (p < 0.001) {
      sf <- format(p, scientific=TRUE)
      pattern <- "([0-9.-]+)e\\-([0-9]+)"
      p.mantissa <- as.numeric(sub(pattern, "\\1", sf))
      p.exponent <- as.numeric(sub(pattern, "\\2", sf))
      showp <- paste(
        "$\\bm{",
        sprintf("%1.1f", p.mantissa),
        "\\sqtimes 10^{-",
        sprintf("%d", p.exponent),
        "}}$",
        sep=""
      )
    } else {
      showp <- sprintf("%0.3f", p)
    }
    if (r@result.confirmed) {
      return(
        paste(
          "\\tesig{", hyp, "}{", showp, "}{", sprintf("%2.0f\\%%", 100*ef), "}",
          sep=""
        )
      )
    } else {
      return(
        paste(
          "\\tensig{", hyp, "}{", showp, "}",
          sep=""
        )
      )
    }
  #} else if (r@type == "count") {
    # TODO: HERE?
  } else {
    return(paste("ERROR: Invalid result type '", r@type, "'.", sep=""))
  }
}

lookup_unary_result <- function (results, question, condition) {
  candidates <- list()
  for (r in results) {
    if (
      r@in.question == question
    & r@in.compare == condition
    & r@in.against %in% c("uniform", "neutral")
    ) {
      candidates <- append(candidates, r)
    }
  }
  if (length(candidates) == 1) {
    return(candidates[[1]])
  } else if (length(candidates) > 1) {
    print("ERROR: Multiple candidates for result criteria!")
    print(question)
    print(condition)
    print("Candidates:")
    print(candidates)
    stop("Multiple candidates.")
  } else {
    return(new("HResult", type="empty"))
  }
}

lookup_binary_result <- function (results, question, condition, against) {
  candidates <- list()
  for (r in results) {
    if (
      r@in.question == question
    & r@in.compare == condition
    & r@in.against == against
    ) {
      candidates <- append(candidates, r);
    }
  }
  if (length(candidates) == 1) {
    return(candidates[[1]])
  } else if (length(candidates) > 1) {
    print("ERROR: Multiple candidates for result criteria!")
    print(question)
    print(condition)
    print("Candidates:")
    print(candidates)
    stop("Multiple candidates.")
  } else {
    return(new("HResult", type="empty"))
  }
}

latex_opt_row <- function(results, question, col.1, col.2, col.3) {
  return(c(
    "\\hline",
    paste(
      "\\multirow{2}{7em}{\\hangpara{1.3em}{1}\\sII",
      gsub("\\.", "", question),
      "abbr/} &%",
      sep=""
    ),
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.1[1])),
      " &%",
      sep=""
    ),
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.2[1])),
      " &%",
      sep=""
    ),
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.3[1]), last=TRUE),
      " \\\\",
      sep=""
    ),
    "\\cline{2-10}",
    "&%",
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.1[2])),
      " &%",
      sep=""
    ),
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.2[2])),
      " &%",
      sep=""
    ),
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.3[2]), last=TRUE),
      " \\\\",
      sep=""
    )
  ))
}

latex_out_row <- function(results, question, col.1, col.2) {
  return(c(
    "\\hline",
    paste(
      "\\multirow{2}{8em}{\\hangpara{1.3em}{1}\\sII",
      gsub("\\.", "", question),
      "abbr/} &%",
      sep=""
    ),
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.1[1])),
      " &%",
      sep=""
    ),
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.2[1]), last=TRUE),
      " \\\\",
      sep=""
    ),
    "\\cline{2-7}",
    "&%",
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.1[2])),
      " &%",
      sep=""
    ),
    paste(
      "  ",
      latex_result(lookup_unary_result(results, question, col.2[2]), last=TRUE),
      " \\\\",
      sep=""
    )
  ))
}

# Start of processing...

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
  results <- list()
  # parse and test our hypotheses:
  for (idx in 1:nrow(hypotheses)) {
    rhyp <- as.list(hypotheses[idx,])
    pred <- "unknown"
    if (rhyp[["against"]] == "uniform" | rhyp[["against"]] == "normal") {
      if (rhyp[["predict"]] == "greater") {
        pred <- "agree"
      } else if (rhyp[["predict"]] == "less") {
        pred <- "disagree"
      }
    } else {
      if (rhyp[["predict"]] == "greater") {
        pred <- paste(">", rhyp[["against"]], sep="")
      } else if (rhyp[["predict"]] == "less") {
        pred <- paste("<", rhyp[["against"]], sep="")
      }
    }
    h <- parsehypothesis(rhyp, filtered)
    result <- testhypothesis(h, filtered)
    results <- append(results, result)
  }

  # display our results:
  for (r in results) {
    display_result(r)
  }

  # Open files for writing .tex results:
  fout.opt <- file("retrospective-option-results-table.tex")
  fout.pos.out <- file("retrospective-positive-outcomes-results-table.tex")
  fout.neg.out <- file("retrospective-negative-outcomes-results-table.tex")

  writeLines(unlist(c(
"\\begin{tabular}{l | c|c|c | c|c|c | c|c|c}",
"\\multicolumn{1}{c|}{Question} &%",
"  \\multicolumn{3}{|p{8em}|}{Expected Success} &%",
"  \\multicolumn{3}{|p{7.5em}|}{Obvious Success} &%",
"  \\multicolumn{3}{|p{8em}}{Expected Failure} \\\\",
"\\cline{2-10}",
"&%",
"  \\multicolumn{3}{|p{8em}|}{Unexp. Failure} &%",
"  \\multicolumn{3}{|p{7.5em}|}{Obvious Trap} &%",
"  \\multicolumn{3}{|p{8em}}{Unexp. Success} \\\\",
lapply(
  c("opt.obvious", "opt.balanced", "opt.nobad", "opt.nogood", "opt.stakes"),
  function(question) {
    latex_opt_row(
      results,
      question,
      c("expected_success", "unexpected_failure"),
      c("obvious_success", "obvious_failure"),
      c("expected_failure", "unexpected_success")
    )
  }
),
"\\end{tabular}"
    )),
    fout.opt
  )

  writeLines(unlist(c(
"\\begin{tabular}{l | c|c|c | c|c|c}",
"\\multicolumn{1}{c|}{Question} &%",
"  \\multicolumn{3}{|p{12em}|}{Expected Success} &%",
"  \\multicolumn{3}{|p{12em}}{Unexp. Success} \\\\",
"\\cline{2-7}",
"&%",
"  \\multicolumn{3}{|p{12em}|}{Obvious Success [main]} &%",
"  \\multicolumn{3}{|p{12em}}{Obvious Trap [alt]} \\\\",
lapply(
  c(
    "out.fair", "out.unfair",
    "out.sense", "out.broken",
    "out.good", "out.bad",
    "out.happy", "out.regret",
    "out.expected", "out.unexpected"
  ),
  function(question) {
    latex_out_row(
      results,
      question,
      c("expected_success", "obvious_success(main)"),
      c("unexpected_success", "obvious_failure(alt)")
    )
  }
),
"\\end{tabular}"
    )),
    fout.pos.out
  )

  writeLines(unlist(c(
"\\begin{tabular}{l | c|c|c | c|c|c}",
"\\multicolumn{1}{c|}{Question} &%",
"  \\multicolumn{3}{|p{12em}|}{Expected Failure} &%",
"  \\multicolumn{3}{|p{12em}}{Unexp. Failure} \\\\",
"\\cline{2-7}",
"&%",
"  \\multicolumn{3}{|p{12em}|}{Obvious Success [alt]} &%",
"  \\multicolumn{3}{|p{12em}}{Obvious Trap [main]} \\\\",
lapply(
  c(
    "out.fair", "out.unfair",
    "out.sense", "out.broken",
    "out.good", "out.bad",
    "out.happy", "out.regret",
    "out.expected", "out.unexpected"
  ),
  function(question) {
    latex_out_row(
      results,
      question,
      c("expected_failure", "obvious_success(alt)"),
      c("unexpected_failure", "obvious_failure(main)")
    )
  }
),
"\\end{tabular}"
    )),
    fout.neg.out
  )

  # Close output files:
  close(fout.opt)
  close(fout.pos.out)
  close(fout.neg.out)
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

filtered$seed = factor(filtered$seed)

for (cond in conditions) {
  h <- histogram(
    ~ decision | seed,
    data=filtered[filtered$condition==cond,],
    type="count",
    layout=c(3,1),
    aspect=1,
    col="#ffff99",
    xlab=cond
  )
  pdf(
    file=paste("reports/choices-", cond, ".pdf", sep=""),
    title=paste("dunyazad-outcomes-choices-", cond, "-report", sep="")
  )
  show(h)
  dev.off()
}

# Likert reports:
# ---------------

likert_names = snames[names(snames) %in% likert_questions]

# Grouped by condition:
# ---------------------

ordered <- filtered[,names(filtered) %in% likert_questions][,likert_questions]

oreport <- ordered
report <- rename(ordered, likert_names)

grouping <- filtered[["condition"]]

for (i in 1:ncol(report)) {
  lk <- likert(report[i], grouping=grouping, nlevels=5)
  pdf(
    file=paste(
      "reports/outcomes-report-",
      sprintf("%02d", i),
      "-",
      names(oreport)[i],
      ".pdf",
      sep=""
    ),
    title=paste("dunyazad-outcomes:", names(oreport)[i]),
    width=7,
    height=2.7
  )
  p <- plot(lk, group.order=conditions, ordered=FALSE)
  show(p)
  dev.off()
}


# Filtered by Condition, Grouped by Seed
# --------------------------------------

for (cond in conditions) {
  report <- filtered[
    filtered$condition == cond,
    names(filtered) %in% names(likert_names)
  ]
  oreport <- report
  report <- rename(report, likert_names)
  grouping <- filtered[filtered$condition==cond,"seed"]
  #grouping <- factor(
  #  paste(
  #    as.character(filtered[filtered$condition==cond,"seed"]),
  #    "-",
  #    as.character(filtered[filtered$condition==cond,"decision"]),
  #    sep=""
  #  )
  #)
  for (i in 1:ncol(report)) {
    lk = likert(report[i], grouping=grouping, nlevels=5)
    p <- plot(lk, ordered=FALSE)
    # hack out any layers missing data:
    valid <- list()
    for (l in p$layers) {
      if ("value" %in% names(l[["data"]])) {
        if (!is.na(l$data[["value"]][[1]])) {
          valid <- append(valid, l)
        }
      } else {
        valid <- append(valid, l)
      }
    }
    p$layers <- valid
    pdf(
      file=paste(
        "reports/detailed-report-",
        cond,
        "-",
        sprintf("%02d", i),
        "-",
        names(oreport)[i],
        ".pdf",
        sep=""
      ),
      title=paste(
        "dunyazad-report-",
        cond,
        "-",
        names(oreport)[i],
        sep=""
      ),
      width=7,
      height=2.0
    )
    show(p)
    dev.off()
  }
}
