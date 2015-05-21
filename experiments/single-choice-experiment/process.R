#!/usr/bin/env Rscript

# Note: instructions on MWW effect size calculation & reporting:
# http://yatani.jp/teaching/doku.php?id=hcistats:mannwhitney

library(likert) # likert graphing
library(reshape) # fix likert error
library(plyr) # rename function
library(coin) # wilcox_test allows computing effect sizes

qnames = c(
  "nobad" = "There are no bad options at this choice.",
  "clearbest" = "There is a clear best option at this choice.",
  "lowstakes" = "The stakes for this choice are low.",
  "nogood" = "There are no good options at this choice.",
  "balanced" = "All of the options at this choice are about equally promising.",
  "trick" = "There are options at this choice.",
  "difficult" = "This is a difficult choice to make.",
  "consequences" = "This choice feels like it will have important consequences."
)

snames = c(
  "nobad" = "There are no bad options at this choice.",
  "clearbest" = "There is a clear best option at this choice.",
  "lowstakes" = "The stakes for this choice are low.",
  "nogood" = "There are no good options at this choice.",
  "balanced" = "All [options] are about equally promising.",
  "trick" = "There are options at this choice.",
  "difficult" = "This is a difficult choice to make.",
  "consequences" = "This choice [has] important consequences."
)

colnames = c(
  "Input.seed" = "seed",
  "Input.constraints" = "constraints",
  "WorkTimeInSeconds" = "duration",
  "Answer.nobad" = "nobad",
  "Answer.clearbest" = "clearbest",
  "Answer.lowstakes" = "lowstakes",
  "Answer.nogood" = "nogood",
  "Answer.balanced" = "balanced",
  "Answer.difficult" = "difficult",
  "Answer.consequences" = "consequences"
)

seedstakes = c(
  "11828" = "high",
  "19914" = "low",
  "21105" = "high",
  "46466" = "high",
  " 4897" = "low",
  "64487" = "low",
  "72724" = "low",
  "79631" = "low",
  "97623" = "high",
  "    0" = "none"
)

filterfactor <- function (data) {
  filtered <- data
  # Filter out too-quick, rejected, and tricked responses:
  filtered <- filtered[
    filtered[["AssignmentStatus"]] != "Rejected"
  & (
      filtered[["WorkTimeInSeconds"]] == -1
    | filtered[["WorkTimeInSeconds"]] > 90
    )
  & filtered[["Answer.trick"]] == 1
    ,
  ]
  # Get rid of irrelevant columns:
  filtered <- filtered[
    ,
    names(filtered) == "Input.seed"
  | names(filtered) == "Input.constraints"
  | names(filtered) == "WorkTimeInSeconds"
  | (
      substr(names(filtered), 1, 3) == "Ans"
    & names(filtered) != "Answer.agefluency"
    & names(filtered) != "Answer.decision"
    & names(filtered) != "Answer.trick"
    )
  ]
  # Filter out incomplete entries:
  filtered <- filtered[
    complete.cases(filtered)
    ,
  ]
  # Turn things into factors:
  for (name in names(filtered)) {
    #cat("processing", name, "\n")
    if (name == "Input.constraints" | name == "Input.stakes") {
      filtered[[name]] = factor(filtered[[name]], ordered=FALSE)
    } else if (substr(name, 1, 3) == "Ans") {
      #cat("FACTORIZE", name, "\n")
      filtered[[name]] = factor(
        filtered[[name]],
        levels=c(1, 2, 3, 4, 5),
        ordered=TRUE
      )
    }
  }
  # Rename things:
  filtered <- rename(filtered, colnames)
  # Assign stakes based on seed:
  filtered[["stakes"]] = apply(
    filtered,
    1,
    function(row) {
      return(seedstakes[[as.character(row[["seed"]])]])
    }
  )
  return(filtered)
}

testhypothesis <- function(hyp, data) {
  testvar <- as.character(hyp[["question"]])
  testcol <- data[
    as.character(data[["constraints"]]) == as.character(hyp[["compare"]])
  | as.character(data[["constraints"]]) == as.character(hyp[["against"]])
    ,
  ]
  is.compare = factor(
    apply(
      testcol,
      1,
      function(row) {
        return(
          as.character(row[["constraints"]]) == as.character(hyp[["compare"]])
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
    alternative=rev
  )
  #compare <- data[
  #  as.character(data[["constraints"]]) == as.character(hyp[["compare"]])
  #  ,
  #]
  #against <- data[
  #  as.character(data[["constraints"]]) == as.character(hyp[["against"]])
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

  #raweffect <- result@statistic@linearstatistic
  effect <- abs(statistic(result) / sqrt(length(testcol)))
  return(
    c(
      "passed"=pass,
      "pvalue"=pvalue(result),
      "mr.compare"=mr.compare,
      "mr.against"=mr.against,
      #"raweffect"=raweffect
      "effectsize"=effect
    )
  )
}

responses <- read.csv(file="study-results.csv",head=TRUE,sep=",")
hypotheses <- read.csv(file="hypotheses.csv",head=TRUE,sep=",")

filtered <- filterfactor(responses)
real <- filtered[
  filtered[["constraints"]] != "uniform"
  ,
]

cat("\n---\n")

cat("Total responses:", nrow(responses), "\n")
cat("Acceptable responses:", nrow(filtered), "\n")
cat("  relaxed:", nrow(filtered[filtered$constraints == "relaxed",]), "\n")
cat("  obvious:", nrow(filtered[filtered$constraints == "obvious",]), "\n")
cat("  dilemma:", nrow(filtered[filtered$constraints == "dilemma",]), "\n")
cat("  uniform:", nrow(filtered[filtered$constraints == "uniform",]), "\n")
cat("Real responses:", nrow(real), "\n")

cat("\n---\n")

cat("Hypotheses:\n")
cat(
"Question                                      Compare:  Pred:     Pass:  P:\n"
)
for (hidx in 1:nrow(hypotheses)) {
  hyp <- as.list(hypotheses[hidx,])
  pred <- "unknown"
  if (hyp[["against"]] == "uniform") {
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
  cat(
    "",
    format(snames[[as.character(hyp[["question"]])]], width=45, justify="left"),
    format(hyp[["compare"]], width=9, justify="left"),
    format(pred, width=9, justify="left"),
    format(
      as.character(as.logical(result[["passed"]])),
      width=6,
      justify="left"
    ),
    format(sprintf("%0.4f\n", result[["pvalue"]]), width=6, justify="left")
  )
}

cat("\n---\n")

# Generate likert report:

pdf(file="combined-report.pdf",title="dunyazad-study-report")
#png(file="combined-report.png",width=600,height=800)
lk = likert(
  filtered[
    ,
    names(filtered) %in% names(snames)
  ],
  grouping = filtered$constraints
)
plot(lk)
dev.off()
