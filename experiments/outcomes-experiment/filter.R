#!/usr/bin/env Rscript

library(plyr) # rename function

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

# For creating individual column names for the multiple-response data:
mrsp <- function(question, response) {
  return(paste(question, "c", gsub("-", ".", response), sep="."))
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

  # Anonymize workers:
  filtered$worker <- as.integer(factor(filtered$worker))

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

#responses <- read.csv(file="study-results.csv",head=TRUE,sep=",")
# TODO: NOT THIS!!!
#responses <- responses[responses$Input.seed != "99500",]
responses <- read.csv(file="raw-results.csv",head=TRUE,sep=",")

filtered <- filterfactor(responses)

write.csv(filtered, file="filtered-results.csv")
