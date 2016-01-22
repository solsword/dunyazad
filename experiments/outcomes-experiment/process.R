#!/usr/bin/env Rscript

# TODO: compare vs. normal & vs. all-neutral

# Note: instructions on MWW effect size calculation & reporting:
# http://yatani.jp/teaching/doku.php?id=hcistats:mannwhitney

library(iterators) # iter() function
library(foreach) # foreach function and %do% operator
library(ggplot2) # for motive histograms
library(grid) # for gList for likert plots w/ histograms
library(likert) # likert graphing *Doesn't do grouping any more :(
library(gtable) # operating on the likert graphs
#library(reshape) # WOULD CAUSE likert error
library(plyr) # rename function
library(coin) # wilcox_test allows computing effect sizes
library(boot) # for bootstrap confidence intervals
library(lattice) # for histogram
library(scales) # for rescale
library(permute) # for shuffle

# Switch for hypothesis testing:
test.hypotheses = FALSE
#test.hypotheses = TRUE

# Switch for report production:
#produce.reports = FALSE
produce.reports = TRUE

# Palette for motive histograms:
#palette.motives = scale_fill_brewer(palette = "Set3")
#palette.motives = scale_fill_manual(values=c("#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#8dd3c7", "#ffffb3"))
palette.motives = scale_fill_manual(values=c("#bebada", "#fb8072", "#8dd3c7", "#fdb462", "#b3de69", "#80b1d3", "#ffffb3"))
#palette.consistency = scale_fill_brewer(palette = "Set3")
palette.consistency = scale_fill_manual(values=c("#fb8072", "#80b1d3", "#fdb462", "#b3de69"))
palette.multi = scale_fill_brewer(palette="Spectral")
#helper <- scale_fill_distiller(palette = "Spectral", values=rescale(1:11))
#palette.multi = lapply(
#  2:20,
#  function (x) {
#    scale_fill_manual(values=sapply(1-rescale(1:x), helper[["palette"]]))
#  }
#)
#palette.multi = lapply(
#  2:20,
#  function (x) {
#    scale_fill_manual(values=sapply(rep(rescale(1:12),length.out=x), helper[["palette"]]))
#  }
#)
#helper <- scale_fill_brewer(palette = "Spectral")
#lim <- 8
#palette.multi = lapply(
#  2:20,
#  function (x) {
#    if (x <= lim) {
#      values <- helper[["palette"]](x)
#    } else {
#      values <- helper[["palette"]](lim)
#    }
#    #values <- values[shuffle(length(values))]
#    s <- scale_fill_manual(values=rep(values,length.out=x))
#    return(s)
#  }
#)

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

conditions.ext = c(
  "obvious_success(main)",
  "expected_success",
  "unexpected_failure",
  "obvious_success",
  "obvious_failure",
  "obvious_success(main)",
  "obvious_failure(main)",
  "obvious_success(alt)",
  "obvious_failure(alt)",
  "expected_failure",
  "unexpected_success"
)

motives.values = c(
  "speed",
  "curious",
  "role",
  "avatar",
  "interesting",
  "power",
  "other"
)

judge.good.values = c(
  "avatar",
  "interesting",
  "role",
  "power",
  "progress",
  "other"
)

judge.bad.values = c(
  "avatar",
  "interesting",
  "no.control",
  "value.clash",
  "power",
  "progress",
  "other"
)

consistency.values = c(
  "consistent",
  "variable",
  "no.exp",
  "other"
)

shortcond = list(
  "expected_success"="exp. success",
  "unexpected_success"="unexp. success",
  "expected_failure"="exp. failure",
  "unexpected_failure"="unexp. failure",
  "obvious_success"="obv. success",
  "obvious_failure"="obv. failure",
  "obvious_success(main)"="obv. success [main]",
  "obvious_failure(main)"="obv. failure [main]",
  "obvious_success(alt)"="obv. success [alt]",
  "obvious_failure(alt)"="obv. failure [alt]",
  "obvious_success[main]"="obv. success [[main]]",
  "obvious_failure[main]"="obv. failure [[main]]",
  "obvious_success[alt]"="obv. success [[alt]]",
  "obvious_failure[alt]"="obv. failure [[alt]]",
  "8638(main)"="8638 [main]"
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

all_comparisons <- function(pop.1, pop.2) {
  result <- list()
  for (e.1 in pop.1) {
    for (e.2 in pop.2) {
      result[[length(result) + 1]] = list(e.1, e.2)
    }
  }
  return(result)
}

cleffect <- function(pop.1, pop.2, cmp=">") {
  pop.1 <- pop.1[!is.na(pop.1)]
  pop.2 <- pop.2[!is.na(pop.2)]
  ac <- all_comparisons(pop.1, pop.2)
  total <- length(ac)
  supporting <- 0
  for (c in ac) {
    if (c[[1]] > c[[2]]) {
      if (cmp == ">") {
        supporting <- supporting + 1
      }
    } else if (c[[1]] < c[[2]]) {
      if (cmp == "<") {
        supporting <- supporting + 1
      }
    } else {
      supporting <- supporting + 0.5
    }
  }
  return(supporting / total)
}

cleffect.greater <- function(pop.1, pop.2) {
  cleffect(pop.1, pop.2, cmp=">")
}

cleffect.less <- function(pop.1, pop.2) {
  cleffect(pop.1, pop.2, cmp="<")
}



filterfactor <- function (input) {
  filtered <- input
  # Filter out too-quick, rejected, and tricked responses:
  filtered <- filtered[
    as.character(filtered[["AssignmentStatus"]]) == "Approved"
    ,
  ]
  cat(
    "Filtered",
    sum(as.character(input[["AssignmentStatus"]]) != "Approved", na.rm=TRUE),
    "non-approved assignments.\n"
  )
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
  dup <- filtered[duplicated(filtered$worker),"worker"]
  bad <- unique(dup)
  olen <- nrow(filtered[filtered$condition != "uniform",])
  filtered <- filtered[
    !(filtered[["worker"]] %in% bad) | filtered[["condition"]] == "uniform"
    ,
  ]
  nlen <- nrow(filtered[filtered$condition != "uniform",])
  cat(
    "Filtered",
    olen - nlen,
    "submissions by",
    length(bad) - 1,
    "duplicating workers.\n"
  )

  # Add columns for the "best" option and whether the answer given matched it:
  filtered$best.option <- correct_decisions[as.character(filtered[["seed"]])]
  filtered$decision.case <- ifelse(
      is.na(filtered$best.option)
    | as.character(filtered$decision) == as.character(filtered$best.option)
    ,
    "main",
    "alt"
  )

  na.count <- 0
  for (name in likert_questions) {
    na.count <- na.count + sum(is.na(filtered[[name]]))
  }
  cat(
    "There were a total of",
    na.count,
    "missing responses among all the Likert questions.\n"
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
    result.effect.common.alt = "numeric",
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
    sym <- ">"
  } else if (rev == "less") {
    rev <- "greater"
    sym <- "<"
  } else {
    rev <- "error"
    sym <- "error"
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
  hyp@result.effect.common.alt <- 0.5 + hyp@result.effect / 2.0
  hyp@result.effect.common <- cleffect(
    testcol[is.compare],
    testcol[is.against],
    sym
  )
  hyp@result.conf.low <- -10
  hyp@result.conf.high <- -10

  # Cleanup if the distributions turned out to be identical:
  if (is.na(pvalue(result)) & hyp@result.effect.common == 0.5) {
    hyp@result.pvalue <- 1.0
    hyp@result.confirmed <- FALSE
  } else {
    hyp@result.conf.low <- confint(result)[["conf.int"]][[1]]
    hyp@result.conf.high <- confint(result)[["conf.int"]][[2]]
  }

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
        format(sprintf("%0.3f", r@result.effect.common.alt), width=8, justify="left"),
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
        format(snames[[as.character(r@in.question)]], width=53, justify="left"),
        format(r@result.confirmed, width=22, justify="left"),
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

latex_result <- function (r, hyp=FALSE) {
  if (r@type == "empty") {
    return("\\tenp ")
  } else if (r@type == "mww") {
    star <- ""
    if (r@subtype == "low") {
      star <- "\\lc/"
    }
    hypoth <- "unknown"
    if (r@in.against == "uniform" | r@in.against == "normal") {
      if (r@hyp.predict == "greater") {
        hypoth <- "A"
      } else if (r@hyp.predict == "less") {
        hypoth <- "D"
      }
    } else {
      if (r@hyp.predict == "greater") {
        cmp <- ">"
      } else if (r@hyp.predict == "less") {
        cmp <- "<"
      }
      hypoth <- paste(
        shortcond[[r@in.compare]],
        "$", cmp, "$",
        shortcond[[r@in.against]],
        sep=""
      )
    }
    if (hyp) {
      if (hypoth == "A") {
        return(paste("agree", star, sep=""))
      } else if (hypoth == "D") {
        return(paste("disagree", star, sep=""))
      }
      return(paste(hypoth, star, sep=""))
    } else {
      hypoth <- paste(hypoth, star, sep="")
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
            "\\tesig{",
            hypoth,
            "}{",
            showp,
            "}{",
            sprintf("%2.0f\\%%", 100*ef),
            "}",
            sep=""
          )
        )
      } else {
        return(
          paste(
            "\\tensig{", hypoth, "}{", showp, "}",
            sep=""
          )
        )
      }
  #} else if (r@type == "count") {
    # TODO: HERE?
    }
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

latex_opt_row <- function(results, question, col.1, col.2, col.3, hyp=FALSE) {
  if (hyp) {
    sep <- " "
  } else {
    sep <- " & "
  }
  return(c(
    "\\midrule",
    paste(
      "\\multirow{2}{5em}{\\raggedleft \\eII",
      gsub("\\.", "", question),
      "abbr/} &%",
      sep=""
    ),
    paste(
      sep,
      latex_result(lookup_unary_result(results, question, col.1[1]), hyp=hyp),
      " &%",
      sep=""
    ),
    paste(
      sep,
      latex_result(lookup_unary_result(results, question, col.2[1]), hyp=hyp),
      " &%",
      sep=""
    ),
    paste(
      sep,
      latex_result(lookup_unary_result(results, question, col.3[1]), hyp=hyp),
      " \\\\",
      sep=""
    ),
    "&%",
    paste(
      sep,
      latex_result(lookup_unary_result(results, question, col.1[2]), hyp=hyp),
      " &%",
      sep=""
    ),
    paste(
      sep,
      latex_result(lookup_unary_result(results, question, col.2[2]), hyp=hyp),
      " &%",
      sep=""
    ),
    paste(
      sep,
      latex_result(lookup_unary_result(results, question, col.3[2]), hyp=hyp),
      " \\\\",
      sep=""
    )
  ))
}

latex_out_row <- function(results, question, col.1, col.2, hyp=FALSE) {
  return(c(
    "\\midrule",
    paste(
      "\\multirow{2}{8em}{\\raggedleft \\hangpara{1.3em}{1}\\eII",
      gsub("\\.", "", question),
      "abbr/} &%",
      sep=""
    ),
    paste(
      " & ",
      latex_result(lookup_unary_result(results, question, col.1[1]), hyp=hyp),
      " &%",
      sep=""
    ),
    paste(
      " & ",
      latex_result(lookup_unary_result(results, question, col.2[1]), hyp=hyp),
      " \\\\",
      sep=""
    ),
    "&%",
    paste(
      " & ",
      latex_result(lookup_unary_result(results, question, col.1[2]), hyp=hyp),
      " &%",
      sep=""
    ),
    paste(
      " & ",
      latex_result(lookup_unary_result(results, question, col.2[2]), hyp=hyp),
      " \\\\",
      sep=""
    )
  ))
}

latex_rel_row <- function(results, question, hyp.comp, hyp.ag, hyp=FALSE) {
  return(c(
    paste(
      "\\eII", gsub("\\.", "", question), "abbr/ & ",
      latex_result(
        lookup_binary_result(results, question, hyp.comp, hyp.ag),
        hyp=hyp
      ),
      " \\\\",
      sep=""
    )
  ))
}

strip_invalid_plot_layers <- function(plot) {
  # hack out any layers missing data:
  valid <- list()
  for (l in p$layers) {
    if ("value" %in% names(l[["data"]])) {
      if (
        identical(l[["data"]][["value"]], numeric(0))
      ) { # do nothing &/| won't shortcut?
      } else if (
        is.na(l[["data"]][["value"]][[1]])
      ) { # do nothing
      } else {
        valid <- append(valid, l)
      }
    } else {
      valid <- append(valid, l)
    }
  }
  p$layers <- valid
  return(p)
}

grob_with_sample_counts <- function(plot, grouping) {
  counts <- table(grouping)
  labels <- list()
  for (v in p$layers[[length(p$layers)-1]]$data$Group) {
    labels <- append(labels, paste("[", counts[[v]], "]", sep=""))
  }

  g <- ggplotGrob(p)
  g1 <- gtable_filter(ggplotGrob(p), "axis_l")
  # Edit the labels:
  ax <- g1$grobs[[1]][["children"]]["axis"][[1]][["grobs"]][[1]]
  l <- ax[["children"]][[1]]
  l[["label"]] <- labels
  l[["just"]] <- "left"
  l[["hjust"]] <- NULL
  l[["vjust"]] <- NULL
  l[["x"]] <- unit(c(0.1, 0.1, 0.1), "npc")
  ax[["children"]][[1]] <- l
  g1$grobs[[1]][["children"]]["axis"][[1]][["grobs"]][[1]] <- ax

  index <- subset(g$layout, name=="panel-1")

  g <- gtable_add_cols(g, unit(0.5, "in"), pos=index$r)
  g <- gtable_add_grob(g, g1, t=index$t, l=index$r+1, b=index$b, r=index$r+1)
  return(g)
}


writeOptTable <- function(file, results, hyp=FALSE) {
  if (hyp) {
    fmt <- "\\begin{tabular}{r c c c}"
    header <- c(
"\\multirow{2}{5em}{\\centering Question} &%",
" \\eIIexpectedsuccessabbr/ &%",
" \\eIIobvioussuccessabbr/ &%",
" \\eIIexpectedfailureabbr/ \\\\",
"&%",
" \\eIIunexpectedfailureabbr/ &%",
" \\eIIobviousfailureabbr/ &%",
" \\eIIunexpectedsuccessabbr/ \\\\"
)
  } else {
    fmt <- "\\begin{tabular}{r  c  c c c  c  c c c  c  c c c}"
    header <- c(
"\\multirow{2}{5em}{\\centering Question} &%",
" & \\multicolumn{3}{c}{\\eIIexpectedsuccessabbr/} &%",
" & \\multicolumn{3}{c}{\\eIIobvioussuccessabbr/} &%",
" & \\multicolumn{3}{c}{\\eIIexpectedfailureabbr/} \\\\",
"&%",
" & \\multicolumn{3}{c}{\\eIIunexpectedfailureabbr/} &%",
" & \\multicolumn{3}{c}{\\eIIobviousfailureabbr/} &%",
" & \\multicolumn{3}{c}{\\eIIunexpectedsuccessabbr/} \\\\"
)
  }
  writeLines(unlist(c(
fmt,
"\\toprule",
header,
"\\toprule",
lapply(
  c("opt.obvious", "opt.balanced", "opt.nobad", "opt.nogood", "opt.stakes"),
  function(question) {
    latex_opt_row(
      results,
      question,
      c("expected_success", "unexpected_failure"),
      c("obvious_success", "obvious_failure"),
      c("expected_failure", "unexpected_success"),
      hyp=hyp
    )
  }
),
"\\bottomrule",
"\\end{tabular}"
    )),
    file
  )
}

writeOutTable <- function(file, results, col.1, col.2, hyp=FALSE) {
  writeLines(unlist(c(
"\\begin{tabular}{r  c  c c c  c  c c c}",
"\\toprule",
"\\multirow{2}{5em}{\\centering Question} &%",
paste(
  " & \\multicolumn{3}{c}{\\eII",
  gsub("[)_(]", "", col.1[[1]]),
  "abbr/} &%",
  sep=""
),
paste(
  " & \\multicolumn{3}{c}{\\eII",
  gsub("[)_(]", "", col.2[[1]]),
  "abbr/} \\\\",
  sep=""
),
"&%",
paste(
  " & \\multicolumn{3}{c}{\\eII",
  gsub("[)_(]", "", col.1[[2]]),
  "abbr/} &%",
  sep=""
),
paste(
  " & \\multicolumn{3}{c}{\\eII",
  gsub("[)_(]", "", col.2[[2]]),
  "abbr/} \\\\",
  sep=""
),
"\\toprule",
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
      col.1,
      col.2,
      hyp=hyp
    )
  }
),
"\\bottomrule",
"\\end{tabular}"
    )),
    file
  )
}


writeRelTable <- function(
  file,
  results,
  questions,
  hyp.comp,
  hyp.ag,
  add.comp=NA,
  add.ag=NA,
  hyp=FALSE
) {
  if (hyp) {
    format <- "\\begin{tabular}{r c}"
    header <- "Question & Hypothesis \\\\"
  } else {
    format <- "\\begin{tabular}{r c c c}"
    header <- "Question & Hypothesis & $p$-value & Effect \\\\"
  }
  if (is.na(add.comp)) {
    writeLines(unlist(c(
      format,
      "\\toprule",
      header,
      "\\midrule",
      lapply(
        questions,
        function(question) {
          latex_rel_row(
            results,
            question,
            hyp.comp,
            hyp.ag,
            hyp=hyp
          )
        }
      ),
      "\\bottomrule",
      "\\end{tabular}"
      )),
      file
    )
  } else {
    writeLines(unlist(c(
      format,
      "\\toprule",
      header,
      "\\midrule",
      lapply(
        questions,
        function(question) {
          latex_rel_row(
            results,
            question,
            hyp.comp,
            hyp.ag,
            hyp=hyp
          )
        }
      ),
      lapply(
        questions,
        function(question) {
          latex_rel_row(
            results,
            question,
            add.comp,
            add.ag,
            hyp=hyp
          )
        }
      ),
      "\\bottomrule",
      "\\end{tabular}"
      )),
      file
    )
  }
}

# Start of processing...

#responses <- read.csv(file="study-results.csv",head=TRUE,sep=",")
responses <- read.csv(file="full-results.csv",head=TRUE,sep=",")
# TODO: NOT THIS!!!
#responses <- responses[responses$Input.seed != "99500",]
hypotheses <- read.csv(file="hypotheses.csv",head=TRUE,sep=",")

hypotheses <- hypotheses[hypotheses$ignore != "TRUE",]

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

has.other.motive <- real[real$motives.other != "{}",]
has.other.judge.bad <- real[real$judge.bad.other != "{}",]
has.other.judge.good <- real[real$judge.good.other != "{}",]
has.other.consistency <- real[real$consistency.other != "{}",]
has.extra.feedback <- real[real$extra.feedback != "{}",]

cat("Responses with `other' motivations:", nrow(has.other.motive), "\n")
cat("Responses with `other' judge-good reasons:", nrow(has.other.judge.good), "\n")
cat("Responses with `other' judge-bad reasons:", nrow(has.other.judge.bad),"\n")
cat("Responses with `other' consistency responses:", nrow(has.other.consistency),"\n")
cat("Responses with extra feedback:", nrow(has.extra.feedback),"\n")

fout.motive.other <- file("reports/other-motives.txt")
fout.judge.bad.other <- file("reports/other-judge-bad.txt")
fout.judge.good.other <- file("reports/other-judge-good.txt")
fout.consistency.other <- file("reports/other-consistency.txt")
fout.extra.feedback <- file("reports/extra-feedback.txt")

pasterow <- function (row, columns) {
  cols <- lapply(
    columns,
    function (col) {
      gsub("\\n", "\\\\n", as.character(row[[col]]))
    }
  )
  args <- as.list(unlist(cols))
  args <- append(list(" || "), args)
  names(args)[[1]] = "sep"
  return(do.call(paste, args))
}

writecolumns <- function (file, frame, columns) {
  writeLines(
    apply(
      frame,
      1,
      function (row) { pasterow(row, columns) }
    ),
    file
  )
}

writecolumns(
  fout.motive.other,
  has.other.motive,
  c("condition", "seed", "motives", "motives.other")
)

writecolumns(
  fout.judge.good.other,
  has.other.judge.good,
  c("condition", "seed", "judge.good", "judge.good.other")
)

writecolumns(
  fout.judge.bad.other,
  has.other.judge.bad,
  c("condition", "seed", "judge.bad", "judge.bad.other")
)

writecolumns(
  fout.consistency.other,
  has.other.consistency,
  c("condition", "seed", "consistency", "consistency.other")
)

writecolumns(
  fout.extra.feedback,
  has.extra.feedback,
  c("condition", "seed", "extra.feedback")
)

close(fout.motive.other)
close(fout.judge.bad.other)
close(fout.judge.good.other)
close(fout.consistency.other)
close(fout.extra.feedback)

cat("\n---\n")

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
  fout.opt.hyp <- file("reports/retrospective-option-hypotheses-table.tex")
  fout.opt <- file("reports/retrospective-option-results-table.tex")

  fout.pos.out.hyp <- file(
    "reports/retrospective-positive-outcomes-hypotheses-table.tex"
  )
  fout.pos.out <- file(
    "reports/retrospective-positive-outcomes-results-table.tex"
  )

  fout.neg.out.hyp <- file(
    "reports/retrospective-negative-outcomes-hypotheses-table.tex"
  )
  fout.neg.out <- file(
    "reports/retrospective-negative-outcomes-results-table.tex"
  )

  fout.free.forced.failure.hyp <- file(
    "reports/retrospective-free-vs-forced-failure-hypotheses-table.tex"
  )
  fout.free.forced.failure <- file(
    "reports/retrospective-free-vs-forced-failure-results-table.tex"
  )
  fout.free.forced.failure.extra <- file(
    "reports/retrospective-free-vs-forced-failure-extra-results-table.tex"
  )
  fout.chosen.inevitable.success.hyp <- file(
    "reports/retrospective-chosen-vs-inevitable-success-hypotheses-table.tex"
  )
  fout.chosen.inevitable.success <- file(
    "reports/retrospective-chosen-vs-inevitable-success-results-table.tex"
  )
  fout.good.bad.unexpected.hyp <- file(
    "reports/retrospective-good-vs-bad-unexpected-hypotheses-table.tex"
  )
  fout.good.bad.unexpected <- file(
    "reports/retrospective-good-vs-bad-unexpected-results-table.tex"
  )
  fout.expected.unexpected.failure.hyp <- file(
    "reports/retrospective-expected-vs-unexpected-failure-hypotheses-table.tex"
  )
  fout.expected.unexpected.failure <- file(
    "reports/retrospective-expected-vs-unexpected-failure-results-table.tex"
  )
  fout.expected.unexpected.success.hyp <- file(
    "reports/retrospective-expected-vs-unexpected-success-hypotheses-table.tex"
  )
  fout.expected.unexpected.success <- file(
    "reports/retrospective-expected-vs-unexpected-success-results-table.tex"
  )

  writeOptTable(fout.opt.hyp, results, hyp=TRUE)
  writeOptTable(fout.opt, results, hyp=FALSE)

  writeOutTable(
    fout.pos.out.hyp,
    results,
    c("expected_success", "obvious_success(main)"),
    c("unexpected_success", "obvious_failure(alt)"),
    hyp=TRUE
  )
  writeOutTable(
    fout.pos.out,
    results,
    c("expected_success", "obvious_success(main)"),
    c("unexpected_success", "obvious_failure(alt)"),
    hyp=FALSE
  )

  writeOutTable(
    fout.neg.out.hyp,
    results,
    c("expected_failure", "obvious_success(alt)"),
    c("unexpected_failure", "obvious_failure(main)"),
    hyp=TRUE
  )
  writeOutTable(
    fout.neg.out,
    results,
    c("expected_failure", "obvious_success(alt)"),
    c("unexpected_failure", "obvious_failure(main)"),
    hyp=FALSE
  )

  writeRelTable(
    fout.free.forced.failure.hyp,
    results,
    c(
      "out.fair", "out.unfair",
      "out.sense", "out.broken",
      "out.good", "out.bad",
      "out.happy", "out.regret",
      "out.expected", "out.unexpected"
    ),
    "unexpected_failure",
    "obvious_failure(main)",
    hyp=TRUE
  )

  writeRelTable(
    fout.free.forced.failure,
    results,
    c(
      "out.fair", "out.unfair",
      "out.sense", "out.broken",
      "out.good", "out.bad",
      "out.happy", "out.regret",
      "out.expected", "out.unexpected"
    ),
    "unexpected_failure",
    "obvious_failure(main)",
    hyp=FALSE
  )

  writeRelTable(
    fout.free.forced.failure.extra,
    results,
    c(
      "out.fair", "out.unfair",
      "out.sense", "out.broken",
      "out.good", "out.bad",
      "out.happy", "out.regret",
      "out.expected", "out.unexpected"
    ),
    "unexpected_failure",
    "8638(main)",
    hyp=FALSE
  )

  writeRelTable(
    fout.chosen.inevitable.success.hyp,
    results,
    c(
      "out.good", "out.bad",
      "out.happy", "out.regret"
    ),
    "expected_success",
    "obvious_success(main)",
    hyp=TRUE
  )

  writeRelTable(
    fout.chosen.inevitable.success,
    results,
    c(
      "out.good", "out.bad",
      "out.happy", "out.regret"
    ),
    "expected_success",
    "obvious_success(main)",
    hyp=FALSE
  )

  writeRelTable(
    fout.good.bad.unexpected.hyp,
    results,
    c(
      "out.fair", "out.unfair",
      "out.sense", "out.broken"
    ),
    "unexpected_failure",
    "unexpected_success",
    hyp=TRUE
  )

  writeRelTable(
    fout.good.bad.unexpected,
    results,
    c(
      "out.fair", "out.unfair",
      "out.sense", "out.broken"
    ),
    "unexpected_failure",
    "unexpected_success",
    hyp=FALSE
  )

  writeRelTable(
    fout.expected.unexpected.failure.hyp,
    results,
    c(
      "out.good", "out.bad",
      "out.happy", "out.regret"
    ),
    "unexpected_failure",
    "expected_failure",
    add.comp="obvious_failure(main)",
    add.ag="expected_failure",
    hyp=TRUE
  )

  writeRelTable(
    fout.expected.unexpected.failure,
    results,
    c(
      "out.good", "out.bad",
      "out.happy", "out.regret"
    ),
    "unexpected_failure",
    "expected_failure",
    add.comp="obvious_failure(main)",
    add.ag="expected_failure",
    hyp=FALSE
  )

  writeRelTable(
    fout.expected.unexpected.success.hyp,
    results,
    c(
      "out.good", "out.bad",
      "out.happy", "out.regret"
    ),
    "expected_success",
    "unexpected_success",
    add.comp="obvious_success(main)",
    add.ag="unexpected_success",
    hyp=TRUE
  )

  writeRelTable(
    fout.expected.unexpected.success,
    results,
    c(
      "out.good", "out.bad",
      "out.happy", "out.regret"
    ),
    "expected_success",
    "unexpected_success",
    add.comp="obvious_success(main)",
    add.ag="unexpected_success",
    hyp=FALSE
  )

  # Close output files:
  close(fout.opt.hyp)
  close(fout.opt)
  close(fout.pos.out.hyp)
  close(fout.pos.out)
  close(fout.neg.out.hyp)
  close(fout.neg.out)
  close(fout.free.forced.failure.hyp)
  close(fout.free.forced.failure)
  close(fout.chosen.inevitable.success.hyp)
  close(fout.chosen.inevitable.success)
  close(fout.good.bad.unexpected.hyp)
  close(fout.good.bad.unexpected)
  close(fout.expected.unexpected.failure.hyp)
  close(fout.expected.unexpected.failure)
  close(fout.expected.unexpected.success.hyp)
  close(fout.expected.unexpected.success)
}

if (produce.reports) {
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

  for (seed in unique(filtered$seed)) {
    if (seed != "--") {
      fout.seeddesc <- file(paste0("reports/framed-seed-", seed, ".tex"))
      row <- responses[responses$Input.seed == seed,][1,]
      opt.1.fore <- sub("\\(.*$", "", row$Input.opt1)
      opt.1.aft <- sub("^[^(]*(\\(.*)$", "\\1", row$Input.opt1)
      out.1 <- row$Input.out1
      opt.2.fore <- sub("\\(.*$", "", row$Input.opt2)
      opt.2.aft <- sub("^[^(]*(\\(.*)$", "\\1", row$Input.opt2)
      out.2 <- row$Input.out2
      opt.3.fore <- sub("\\(.*$", "", row$Input.opt3)
      opt.3.aft <- sub("^[^(]*(\\(.*)$", "\\1", row$Input.opt3)
      out.3 <- row$Input.out3
      count.1 <- sum(filtered$seed==seed & filtered$decision == "1", na.rm=TRUE)
      count.2 <- sum(filtered$seed==seed & filtered$decision == "2", na.rm=TRUE)
      count.3 <- sum(filtered$seed==seed & filtered$decision == "3", na.rm=TRUE)
      writeLines(
        sapply(c(
"\\fbox{",
"\\parbox{0.95\\linewidth}{",
"\\slshape",
#%as.character(row$Input.framing),
#%"",
#%as.character(row$Input.assets),
#%"",
#%as.character(row$Input.set_off),
#%"",
as.character(row$Input.setup),
" ",
as.character(row$Input.potentials),
" ",
as.character(row$Input.prompt),
"",
"\\begin{enumerate}[itemsep=0pt,topsep=4pt,parsep=0pt,partopsep=0pt]",
paste0("\\item ", opt.1.fore, " \\\\"),
paste0(opt.1.aft, " \\\\[0.3em]"),
paste0("\\choicecount{", count.1,"}$\\rightarrow$ ", out.1, " \\\\[-0.5em]"),
paste0("\\item ", opt.2.fore, " \\\\"),
paste0(opt.2.aft, " \\\\[0.3em]"),
paste0("\\choicecount{", count.2, "}$\\rightarrow$ ", out.2, " \\\\[-0.5em]"),
paste0("\\item ", opt.3.fore, " \\\\"),
paste0(opt.3.aft, " \\\\[0.3em]"),
paste0("\\choicecount{", count.3, "}$\\rightarrow$ ", out.3),
"\\end{enumerate}",
"}",
"}"
          ),
          function(line) {
            r <- gsub("’", "'", line)
            r <- gsub("á", "\\\\'{a}", r)
            r <- gsub("ü", "\\\\:{u}", r)
            return(r)
          }
        ),
        fout.seeddesc
      )
      close(fout.seeddesc)
    }
  }
  #for (cond in conditions) {
  #  f <- filtered[filtered$condition==cond,]
  #  seeds <- unique(f$seed)
  #  otext <- responses[
  #    !duplicated(responses$Input.seed) & responses$WorkTimeInSeconds != -1
  #    ,
  #    c("Input.opt1", "Input.opt2", "Input.opt3", "Input.seed")
  #  ]
  #  otext = rename(otext, c("Input.seed"="seed"))
  #  otext[[1]] = as.character(otext[[1]])
  #  otext[[2]] = as.character(otext[[2]])
  #  otext[[3]] = as.character(otext[[3]])
  #  counts <- lapply(
  #    seeds,
  #    function (s) {
  #      r <- as.list(unlist(lapply(
  #        c(1, 2, 3),
  #        function(d) {
  #          sum(filtered$seed==s & filtered$decision==d)
  #        }
  #      )))
  #      r[["seed"]] = as.character(s)
  #      return(r)
  #    }
  #  )
  #  for (count in counts) {
  #    s <- count[["seed"]]
  #    txt <- as.list(as.character(otext[otext$seed == s,1:3]))
  #    t <- data.frame(
  #      "option"=unlist(txt), 30,
  #      #"option"=1:3,
  #      "count"=unlist(as.list(count[1:3]))
  #    )
  #    wr <- strwrap(
  #      sub("\\(", "\n(", as.character(t$option)),
  #      40,
  #      simplify=FALSE,
  #      prefix="\n",
  #      initial=""
  #    )
  #    wr <- lapply(
  #      wr,
  #      function (lines) {
  #        do.call(paste0, as.list(lines))
  #      }
  #    )
  #    #t$option <- unlist(wr)
  #    t$option <- sub("\\(", "\n(", as.character(t$option))
  #    t$option <- sub("([.,])", "\\1\n", as.character(t$option))
  #    g <- ggplot(t, aes(x=option, y=count))
  #    g <- g + ylim(0, 10)
  #    g <- g + geom_bar(stat="identity", fill="#80b1d3")
  #    g <- g + geom_text(
  #      aes(x=option, y=count, label=count),
  #      size=8,
  #      vjust=0.5,
  #      nudge_x=0,
  #      nudge_y=1
  #    )
  #    g <- g + palette.motives
  #    #g <- g + guides(fill = "legend")
  #    g <- g + guides(fill = "none")
  #    g <- g + coord_flip()
  #    g <- g + theme(
  #      panel.grid.major.x = element_blank(),
  #      panel.grid.major.y = element_blank(),
  #      panel.grid.minor.x = element_blank(),
  #      panel.grid.minor.y = element_blank(),
  #      axis.text.x = element_blank(),
  #      axis.text.y = element_text(size=20, vjust=0.5),
  #      axis.title.x = element_blank(),
  #      axis.title.y = element_blank(),
  #      axis.ticks.x = element_blank()
  #      #axis.ticks.y = element_blank()
  #    )
  #    pdf(
  #      file=paste("reports/choices-", cond, "-", s, ".pdf", sep=""),
  #      title=paste(
  #        "dunyazad-outcomes-choices-",
  #        cond,
  #        "-",
  #        s,
  #        "-report",
  #        sep=""
  #      )
  #    )
  #    show(g)
  #    dev.off()
  #    exit
  #  }
  #}

  # Motivation histograms
  # ---------------------

  nsamples <- nrow(real)
  for (m in c("motives", "judge.good", "judge.bad")) {
    values <- "error"
    if (m == "motives") {
      values <- motives.values
    } else if (m == "judge.good") {
      values <- judge.good.values
    } else if (m == "judge.bad") {
      values <- judge.bad.values
    }
    columns <- lapply(
      values,
      function (x) {
        paste(m, "c", x, sep=".")
      }
    )
    counts <- lapply(
      columns,
      function (col) {
        sum(filtered[filtered$is.real,col], na.rm=TRUE)
      }
    )
    l <- lapply(
      values,
      function (v) {
        gsub("\\.", ".\n", v)
      }
    )
    l <- factor(l, ordered=TRUE, levels=l)
    t <- data.frame(labels=l, count=as.vector(unlist(counts)))
    g <- ggplot(t, aes(x=labels, y=count, fill=labels))
    g <- g + scale_y_continuous(
      limits=c(0, nsamples),
      breaks=c(0, nsamples*0.5, nsamples)
    )
    g <- g + geom_bar(stat="identity", color="#000000")
    g <- g + geom_text(
      aes(
        x=labels,
        y=count,
        label=ifelse(count > 20, count, "")
      ),
      size=8,
      vjust=1,
      nudge_y=-5
    )
    g <- g + geom_text(
      aes(
        x=labels,
        y=count,
        label=ifelse(count > 20, paste0(as.integer(100*count/205), "%"), count)
      ),
      size=8,
      vjust=0,
      nudge_y=4
    )
    g <- g + palette.motives
    g <- g + guides(fill = "none")
    g <- g + theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(size=26, angle=90, vjust=0.5, hjust=1),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    )
    g <- g + labs(x="")
    pdf(
      file=paste("reports/motives-", m, ".pdf", sep=""),
      title=paste("dunyazad-motives-", m, "-report", sep="")
    )
    show(g)
    dev.off()
  }

  # Consistency:
  values <- c("consistent", "variable", "no-exp", "other")
  counts <- lapply(
    values,
    function (x) {
      nrow(filtered[filtered$is.real & as.character(filtered$consistency)==x,])
    }
  )
  values <- c("consistent", "variable", "no.exp", "other")
  t <- data.frame(
    labels=factor(values, ordered=TRUE, levels=values),
    count=as.vector(unlist(counts))
  )
  g <- ggplot(t, aes(x=labels, y=count, fill=labels))
  g <- g + scale_y_continuous(
    limits=c(0, nsamples),
    breaks=c(0, nsamples*0.5, nsamples)
  )
  g <- g + geom_bar(stat="identity", color="#000000")
  g <- g + geom_text(
    aes(
      x=labels,
      y=count,
      label=ifelse(count > 20, count, "")
    ),
    size=8,
    vjust=1,
    nudge_y=-5
  )
  g <- g + geom_text(
    aes(
      x=labels,
      y=count,
      label=ifelse(count > 20, paste0(as.integer(100*count/205), "%"), count)
    ),
    size=8,
    vjust=0,
    nudge_y=4
  )
  g <- g + palette.consistency
  g <- g + guides(fill = "none")
  g <- g + theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size=26, angle=90, vjust=0.5, hjust=1),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )
  pdf(
    file="reports/motives-consistency.pdf",
    title="dunyazad-motives-consistency-report"
  )
  show(g)
  dev.off()

  # Motives multivalues

  nsamples <- nrow(real)
  for (m in c("motives", "judge.good", "judge.bad")) {
    values <- "error"
    if (m == "motives") {
      values <- unique(real$motives)
    } else if (m == "judge.good") {
      values <- unique(real$judge.good)
    } else if (m == "judge.bad") {
      values <- unique(real$judge.bad)
    }
    counts <- lapply(
      values,
      function (val) {
        sum(filtered[[m]] == val, na.rm=TRUE)
      }
    )

    l <- values

    l <- lapply(l, function (v) { gsub("-", ".", v) })

    l <- lapply(l, function (v) { gsub("\\|", " +\n", v) })

    #l <- lapply(values, function (v) { sub("\\|", " + ", v) })
    #l <- lapply(l, function (v) { sub("\\|", " +\n", v) })
    #l <- lapply(l, function (v) { gsub("\\|", " + ", v) })

    #l <- lapply(values, function (v) { gsub("\\|", ", ", v) })
    #l <- lapply(l, function (v) { gsub("avatar", "Ava", v) })
    #l <- lapply(l, function (v) { gsub("power", "Pwr", v) })
    #l <- lapply(l, function (v) { gsub("progress", "Prg", v) })
    #l <- lapply(l, function (v) { gsub("interesting", "Int", v) })
    #l <- lapply(l, function (v) { gsub("role", "Rol", v) })
    #l <- lapply(l, function (v) { gsub("curious", "Cur", v) })
    #l <- lapply(l, function (v) { gsub("no-control", "NCt", v) })
    #l <- lapply(l, function (v) { gsub("value-clash", "VCl", v) })
    counts <- as.vector(unlist(counts))

    # Filter out ones, twos, and threes:
    has.enough <- counts > 10
    #has.enough <- counts > 0
    counts <- counts[has.enough]
    l <- l[has.enough]
    l <- unlist(l)

    t <- data.frame(lbl=l, count=counts)
    t <- t[with(t, order(-count)),]
    t$lbl <- factor(t$lbl, ordered=TRUE, levels=t$lbl)
    t$running <- sapply(as.numeric(t$lbl), function(l) { sum(t$count[1:l]) })

    empty <- ggplot(
    ) + theme_void() + annotate(
      "rect",
      xmin=0, xmax=100,
      ymin=0, ymax=100,
      fill="white",
      color="white"
    )
    eg <- ggplotGrob(empty)

    g <- ggplot(t)
    g <- g + geom_bar(
      aes(x=lbl, y=count, width=0.4 + 0.5*count/(0.25*205), fill=count/205),
      stat="identity",
      color="#000000"
    )
    g <- g + scale_fill_distiller(
      palette="Spectral",
      #values=1-rescale(1:max(t$count)),
      #limits=c(10, max(t$count))
      limits=c(0.035, 0.22)
      #limits=c(0.0, 0.3)
    )
    # create our false bottom:
    g <- g + annotation_custom(
      eg,
      xmin=0, xmax=nrow(t)+1,
      ymin=-54, ymax=0
    )
    g <- g + scale_y_continuous(
      limits=c(-22, 42),
      breaks=205*0.05*0:8
    )
    # And put our false labels in:
    g <- g + geom_text(
      aes(x=lbl, y=count, label=count),
      size=6,
      vjust=0,
      nudge_y=1
    )
    g <- g + geom_text(
      aes(x=lbl, y=count, label=paste0(as.integer(100*count/205), "%")),
      size=6,
      vjust=1,
      nudge_y=-1
    )
    g <- g + geom_text(
      aes(
        x=lbl,
        y=-4,
        label=lbl,
        #size=(count/max(count))
        #label=(
        #  -sapply(
        #    as.character(lbl),
        #    function (l) {
        #      #length(gregexpr("\n", l)[[1]])
        #      sum(strsplit(l, "") == "+")
        #    }
        #  )
        #),
        size=(
          -sapply(
            as.character(lbl),
            function (l) {
              sum(strsplit(l, "")[[1]] == "\n")
            }
          )
        )
      ),
      angle=0,
      vjust=1,
      hjust=0.5
    )
    g <- g + scale_size_continuous(range=c(7, 8))
    #g <- g + geom_line(
    #  aes(x=as.numeric(lbl), y=45*as.numeric(running)/205),
    #  inherit.aes=FALSE
    #)
    #g <- g + geom_line(
    #  aes(x=as.numeric(lbl), y=0),
    #  inherit.aes=FALSE
    #)
    #g <- g + geom_line(
    #  aes(x=as.numeric(lbl), y=45),
    #  inherit.aes=FALSE
    #)
    #g <- g + palette.multi[nrow(t)]
    #g <- g + palette.multi
    g <- g + guides(fill = "none", size="none")
    g <- g + theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      #axis.text.x = element_text(size=18, angle=90, vjust=0.5, hjust=1),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    )
    g <- g + labs(x="")
    pdf(
      file=paste("reports/motives-multi-", m, ".pdf", sep=""),
      title=paste("dunyazad-motives-multi-", m, "-report", sep=""),
      width=9.5,
      height=7
    )
    show(g)
    dev.off()
  }
  exit

  # Likert reports:
  # ---------------

  likert_names = snames[names(snames) %in% likert_questions]

  ordered <- filtered[,names(filtered) %in% likert_questions][,likert_questions]

  oreport <- ordered
  report <- rename(ordered, likert_names)

  # Grouped by condition:
  # ---------------------

  grouping <- filtered[["condition"]]

  for (i in 1:ncol(report)) {
    cat("Creating report for", paste("'", names(oreport)[i], "'...", sep=""))
    lk <- likert(report[i], grouping=grouping, nlevels=5)
    pdf(
      file=paste(
        "reports/outcomes-report-basic-",
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
    #p <- plot(lk, ordered=FALSE, include.histogram=TRUE)
    show(p)
    dev.off()
    cat(" done.\n")
  }

  #grouping <- filtered[["seed"]]
  for (i in 1:ncol(report)) {
    cat("Creating report w/histogram for", paste("'", names(oreport)[i], "'...", sep=""))
    lk <- likert(report[i], grouping=grouping, nlevels=5)
    pdf(
      file=paste(
        "reports/outcomes-report-with-histogram-",
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
    p <- plot(lk, group.order=conditions, ordered=FALSE, include.histogram=TRUE)
    show(p)
    dev.off()
    cat(" done.\n")
  }


  # Filtered by Condition, Grouped by Seed
  # --------------------------------------

  for (cond in conditions.ext) {
    cat("Processing detailed data for condition", paste("'", cond, "'...\n", sep=""))
    if (grepl("\\(", cond)) {
      cond.base = sub("\\(.*$", "", cond)
      cond.case = sub("\\)", "", sub("^.*\\(", "", cond))
      rows <- filtered[
        filtered$condition == cond.base & filtered$decision.case == cond.case
        ,
      ]
    } else {
      rows <- filtered[filtered$condition == cond,]
    }
    report <- rows[, names(filtered) %in% names(likert_names) ]
    oreport <- report
    report <- rename(report, likert_names)
    grouping <- rows[["seed"]]
    #grouping <- factor(
    #  paste(
    #    as.character(rows[,"seed"])
    #    "-",
    #    as.character(rows[,"decision"]),
    #    sep=""
    #  )
    #)
    for (i in 1:ncol(report)) {
      cat("  ...creating detailed report for", paste("'", names(oreport)[i], "'...", sep=""))
      lk = likert(report[i], grouping=grouping, nlevels=5)
      p <- plot(lk, ordered=FALSE)
      p <- strip_invalid_plot_layers(p)

      # hack in counts:
      # Manually remove NAs:
      fgroups <- rows[!is.na(rows[names(oreport)[i]]),"seed"]
      g <- grob_with_sample_counts(p, fgroups)

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
      grid.draw(g)
      #show(p)
      dev.off()
      cat(" done.\n")
    }
    cat("  ...done.\n")
  }

  # Custom Extra Reports
  # --------------------

  cat("Creating custom extra reports...\n")

  ordered <- filtered[,names(filtered) %in% likert_questions][,likert_questions]

  oreport <- ordered
  report <- rename(ordered, likert_names)

  grouping <- filtered[["setup"]]

  r <- report[likert_names[["opt.stakes"]]]
  dev.off()
  lk <- likert(r, grouping=grouping, nlevels=5)
  pdf(
    file="reports/extra-outcomes-report-by-setup-05-opt.stakes.pdf",
    title="dunyazad-extra-outcomes-report-stakes-by-setup",
    width=7,
    height=2.7
  )
  p <- plot(lk, group.order=c("market", "threatened_innocents", "monster_attack"), ordered=FALSE)
  #p <- plot(lk, ordered=FALSE, include.histogram=TRUE)
  show(p)
  dev.off()
  cat("  ...done.\n")
}
