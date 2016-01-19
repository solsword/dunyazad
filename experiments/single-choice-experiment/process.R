#!/usr/bin/env Rscript

# TODO: compare vs. normal & vs. all-neutral

# Note: instructions on MWW effect size calculation & reporting:
# http://yatani.jp/teaching/doku.php?id=hcistats:mannwhitney

# Note: confidence intervals for median from:
# http://exploringdatablog.blogspot.sk/2012/04/david-olives-median-confidence-interval.html

library(plyr) # rename function
library(coin) # wilcox_test allows computing effect sizes
library(boot) # for bootstrap confidence intervals
library(lattice) # for histogram
library(likert) # likert graphing
#library(reshape) # WOULD CAUSE likert error

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
  "nobad" = "1. There are no bad options at this choice.",
  "clearbest" = "2. There is a clear best option at this choice.",
  "lowstakes" = "3. The stakes for this choice are low.",
  "nogood" = "4. There are no good options at this choice.",
  "balanced" = "5. All [options] are about equally promising.",
#  "trick" = "6. There are options at this choice.",
  "difficult" = "7. This is a difficult choice to make.",
  "consequences" = "8. This choice [has] important consequences."
)

colnames = c(
  "Input.seed" = "seed",
  "Input.constraints" = "constraints",
  "WorkTimeInSeconds" = "duration",
  "Answer.decision" = "decision",
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
  compcol <- "ERROR"
  agcol <- "ERROR"
  if (hyp[["compare"]] %in% data[["constraints"]]) {
    compcol <- "constraints"
  } else if (hyp[["compare"]] %in% data[["seed"]]) {
    compcol <- "seed"
  } else if (hyp[["compare"]] %in% data[["stakes"]]) {
    compcol <- "stakes"
  } else {
    print(hyp[["compare"]])
  }
  if (hyp[["against"]] %in% data[["constraints"]]) {
    agcol <- "constraints"
  } else if (hyp[["against"]] %in% data[["seed"]]) {
    agcol <- "seed"
  } else if (hyp[["against"]] %in% data[["stakes"]]) {
    agcol <- "stakes"
  } else {
    print(hyp[["against"]])
  }
  testcol <- data[
    as.character(data[[compcol]]) == as.character(hyp[["compare"]])
  | as.character(data[[agcol]]) == as.character(hyp[["against"]])
    ,
  ]
  is.compare = apply(
    testcol,
    1,
    function(row) {
      return(
        as.character(row[[compcol]]) == as.character(hyp[["compare"]])
      )
    }
  )
  testcol <- testcol[[testvar]]
  testcol <- sapply(
    testcol,
    function (datum) { return(as.numeric(as.character(datum))) }
  )
  rev <- as.character(hyp[["predict"]])
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
    testcol ~ as.factor(is.compare),
    distribution="exact",
    alternative=rev,
    conf.int=TRUE,
    conf.level=0.95
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
  effect <- abs(statistic(result, "test")[["FALSE"]] / sqrt(length(testcol)))
  commoneffect.alt <- 0.5 + effect / 2.0
  commoneffect <- cleffect(
    testcol[is.compare],
    testcol[!is.compare],
    sym
  )
  conf.low <- confint(result)[["conf.int"]][[1]]
  conf.high <- confint(result)[["conf.int"]][[2]]
  return(
    list(
      "question"=as.character(hyp[["question"]]),
      "compare"=as.character(hyp[["compare"]]),
      "against"=as.character(hyp[["against"]]),
      "predict"=as.character(hyp[["predict"]]),
      "passed"=pass,
      "pvalue"=pvalue(result),
      "mr.compare"=mr.compare,
      "mr.against"=mr.against,
      "effectsize"=effect,
      "commoneffect"=commoneffect,
      "conf.low"=conf.low,
      "conf.high"=conf.high
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


latex_result <- function (result) {
  if (identical(result, NA)) {
    return("\\tenp ")
  }
  hypoth <- "unknown"
  if (result[["against"]] == "uniform" | result[["against"]] == "normal") {
    if (result[["predict"]] == "greater") {
      hypoth <- "A"
    } else if (result[["predict"]] == "less") {
      hypoth <- "D"
    }
  } else {
    if (result[["predict"]] == "greater") {
      cmp <- ">"
    } else if (result[["predict"]] == "less") {
      cmp <- "<"
    }
    hypoth <- paste(
      result[["compare"]],
      "$", cmp, "$",
      result[["against"]],
      sep=""
    )
  }
  p <- result[["pvalue"]]
  ef <- result[["commoneffect"]]
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
  if (result[["passed"]]) {
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
}

lookup_unary_result <- function (results, question, condition) {
  candidates <- list()
  for (r in results) {
    if (
      r[["question"]] == question
    & r[["compare"]] == condition
    & r[["against"]] %in% c("uniform", "neutral")
    ) {
      candidates[[length(candidates) + 1]] = r
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
    return(NA)
  }
}

lookup_binary_result <- function (results, question, condition, against) {
  candidates <- list()
  for (r in results) {
    if (
      r[["question"]] == question
    & r[["compare"]] == condition
    & r[["against"]] == against
    ) {
      candidates[[length(candidates) + 1]] = r
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
    return(NA)
  }
}

latex_main_row <- function(results, question, col.1, col.2, col.3) {
  return(c(
    paste(
      "\\eI",
      gsub("\\.", "", question),
      "abbr/ &%",
      sep=""
    ),
    paste(
      " & ",
      latex_result(lookup_unary_result(results, question, col.1)),
      " &%",
      sep=""
    ),
    paste(
      " & ",
      latex_result(lookup_unary_result(results, question, col.2)),
      " &%",
      sep=""
    ),
    paste(
      " & ",
      latex_result(lookup_unary_result(results, question, col.3)),
      " \\\\",
      sep=""
    )
  ))
}


latex_rel_row <- function(
  results,
  question,
  hyp.comp,
  hyp.ag,
  nxt=FALSE,
  prev=FALSE
) {
  #exspace <- "[2ex]"
  q <- paste("\\eI", gsub("\\.", "", question), "abbr/", sep="")
  if (prev) {
    q <- ""
  }
  if (nxt) {
    #exspace <- ""
    q <- paste(
      "\\multirow{2}{10em}{",
      q,
      "}",
      sep=""
    )
  }
  q <- paste(q, "&")
  return(c(
    paste(
      q,
      latex_result(lookup_binary_result(results, question, hyp.comp, hyp.ag)),
      " \\\\",
      #exspace,
      sep=""
    )
  ))
}

writeMainTable <- function(file, results) {
  writeLines(unlist(c(
"\\begin{tabular}{l  c  c c c  c  c c c  c  c c c}",
"\\toprule",
"Question &%",
" & \\multicolumn{3}{c}{\\eIobviousabbr/} &%",
" & \\multicolumn{3}{c}{\\eIrelaxedabbr/} &%",
" & \\multicolumn{3}{c}{\\eIdilemmaabbr/} \\\\",
"\\midrule",
lapply(
  c("nobad", "clearbest", "lowstakes", "nogood", "balanced", "difficult", "consequences"),
  function(question) {
    latex_main_row(
      results,
      question,
      "obvious",
      "relaxed",
      "dilemma"
    )
  }
),
"\\bottomrule",
"\\end{tabular}"
    )),
    file
  )
}

writeRelTable <- function(file, results) {
  writeLines(unlist(c(
    "\\begin{tabular}{l c c c c}",
    "\\toprule",
    "Question & Hypothesis & $p$-value & Effect \\\\",
    "\\toprule",
    latex_rel_row(results, "nobad", "relaxed", "dilemma"),
    "\\midrule",
    latex_rel_row(results, "clearbest", "obvious", "dilemma"),
    "\\midrule",
    latex_rel_row(results, "nogood", "dilemma", "obvious", nxt=TRUE),
    latex_rel_row(results, "nogood", "dilemma", "relaxed", prev=TRUE),
    "\\midrule",
    latex_rel_row(results, "balanced", "dilemma", "obvious"),
    "\\midrule",
    latex_rel_row(results, "difficult", "dilemma", "obvious", nxt=TRUE),
    latex_rel_row(results, "difficult", "dilemma", "relaxed", prev=TRUE),
    "\\midrule",
    latex_rel_row(results, "consequences", "dilemma", "obvious", nxt=TRUE),
    latex_rel_row(results, "consequences", "dilemma", "relaxed", prev=TRUE),
    "\\bottomrule",
    "\\end{tabular}"
    )),
    file
  )
}

responses <- read.csv(file="study-results.csv",head=TRUE,sep=",")
hypotheses <- read.csv(file="hypotheses.csv",head=TRUE,sep=",")

filtered <- filterfactor(responses)
real <- filtered[
  filtered[["constraints"]] != "uniform"
& filtered[["constraints"]] != "normal"
& filtered[["constraints"]] != "neutral"
& filtered[["constraints"]] != "agree"
& filtered[["constraints"]] != "sagree"
  ,
]

cat("\n---\n")

cat("Total responses:", nrow(responses), "\n")
cat("Acceptable responses:", nrow(filtered), "\n")
cat("     obvious:", nrow(filtered[filtered$constraints == "obvious",]), "\n")
cat("     relaxed:", nrow(filtered[filtered$constraints == "relaxed",]), "\n")
cat("     dilemma:", nrow(filtered[filtered$constraints == "dilemma",]), "\n")
cat("\n")
cat("  low-stakes:", nrow(filtered[filtered$stakes == "low",]), "\n")
cat(" high-stakes:", nrow(filtered[filtered$stakes == "high",]), "\n")
cat("\n")
cat(
  " obvious by stakes:",
  nrow(filtered[filtered$stakes == "low" & filtered$constraints == "obvious",]),
  "/",
  nrow(filtered[filtered$stakes == "high" & filtered$constraints == "obvious",]),
  "\n"
)
cat(
  " relaxed by stakes:",
  nrow(filtered[filtered$stakes == "low" & filtered$constraints == "relaxed",]),
  "/",
  nrow(filtered[filtered$stakes == "high" & filtered$constraints == "relaxed",]),
  "\n"
)
cat(
  " dilemma by stakes:",
  nrow(filtered[filtered$stakes == "low" & filtered$constraints == "dilemma",]),
  "/",
  nrow(filtered[filtered$stakes == "high" & filtered$constraints == "dilemma",]),
  "\n"
)
cat("\n")
cat("     uniform:", nrow(filtered[filtered$constraints == "uniform",]), "\n")
cat("     normal:", nrow(filtered[filtered$constraints == "normal",]), "\n")
cat("     neutral:", nrow(filtered[filtered$constraints == "neutral",]), "\n")
cat("     agree:", nrow(filtered[filtered$constraints == "agree",]), "\n")
cat("     sagree:", nrow(filtered[filtered$constraints == "sagree",]), "\n")
cat("Real responses:", nrow(real), "\n")


cat("\n---\n")

cat("Median response time:", median(responses[responses$WorkTimeInSeconds != -1,]$WorkTimeInSeconds),"\n")
cat("Median accepted response time:", median(real$duration), "\n")

cat("\n---\n")

cat("General statistics:\n")
for (treatment in c("obvious", "relaxed", "dilemma")) {
  cat(" ", treatment, ":\n")
  for (
    question
  in
    c(
      "nobad",
      "nogood",
      "clearbest",
      "lowstakes",
      "balanced",
      "difficult",
      "consequences"
    )
  ) {
    col <- filtered[filtered$constraints == treatment,][[question]]
    num <- as.numeric(as.character(col))
   # cis <- DOliveCIproc(num)
   # wcis <- wilcox.test(
   #   num,
   #   conf.int=TRUE,
   #   conf.level=0.95,
   #   alternative="two.sided",
   #   correct=TRUE
   # )
    boottype <- "basic"
    bcis <- boot.ci(
      boot(num, function(x, i) median(x[i]), R=1000),
      type=boottype
    )
    cat(
      "   ",
      format(question, width=12, justify="left"),
      #format(sprintf("%0.2f", cis[["LCI"]]), width=5, justify="right"),
      #format(sprintf("%0.2f", wcis[["conf.int"]][[1]]),width=5,justify="right"),
      format(sprintf("%0.2f", bcis[[boottype]][[4]]), width=5, justify="right"),
      "--",
      median(num),
      "--",
      #format(sprintf("%0.2f", cis[["UCI"]]), width=5, justify="right"),
      #format(sprintf("%0.2f", wcis[["conf.int"]][[2]]),width=5,justify="right"),
      format(sprintf("%0.2f", bcis[[boottype]][[5]]), width=5, justify="right"),
      "\n"
    )
  }
}


cat("\n---\n")

cat("Hypotheses:\n")
cat(
"Question                                          Compare:  Pred:     Pass:  P:             r:     effect:\n"
)
all_results <- list()
for (hidx in 1:nrow(hypotheses)) {
  hyp <- as.list(hypotheses[hidx,])
  pred <- "unknown"
  if (hyp[["against"]] == "uniform" | hyp[["against"]] == "normal" | hyp[["against"]] == "neutral") {
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
  all_results[[length(all_results) + 1]] = result
  cat(
    "",
    format(snames[[as.character(hyp[["question"]])]], width=49, justify="left"),
    format(hyp[["compare"]], width=9, justify="left"),
    format(pred, width=9, justify="left"),
    format(
      as.character(as.logical(result[["passed"]])),
      width=6,
      justify="left"
    ),
    format(result[["pvalue"]], width=12, justify="left"),
    " ",
    format(sprintf("%0.3f", result[["effectsize"]]), width=7, justify="left"),
    format(sprintf("%0.3f", result[["commoneffect"]]), width=7, justify="left"),
#    format(result[["conf.low"]], width=4, justify="left"),
#    format(result[["conf.high"]], width=4, justify="left"),
    "\n"
  )
}

cat("\n---\n")

fout.normal = file("prospective-normal-results-table.tex")
fout.relative = file("prospective-relative-results-table.tex")

writeMainTable(fout.normal, all_results)
writeRelTable(fout.relative, all_results)

close(fout.normal)
close(fout.relative)

cat("\n---\n")

# Get rid of "normal," "uniform," "neutral," "agree," and "sagree" fake entries:

filtered <- filtered[filtered$constraints != "normal",]
filtered <- filtered[filtered$constraints != "uniform",]
filtered <- filtered[filtered$constraints != "neutral",]
filtered <- filtered[filtered$constraints != "agree",]
filtered <- filtered[filtered$constraints != "sagree",]
filtered$constraints <- factor(filtered$constraints)

# Prevent anyone from gouging their eyes out:
sb <- trellis.par.get("strip.background")
sb[["col"]][1] <- "#ddeeff"
trellis.par.set("strip.background", sb)

# Choice histograms
# -----------------

pdf(file="choices.pdf",title="dunyazad-choices.report")
filtered$seed = factor(filtered$seed)
histogram(
  ~ decision | seed,
  data=filtered[filtered$constraints=="obvious",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="obvious"
)
histogram(
  ~ decision | seed,
  data=filtered[filtered$constraints=="relaxed",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="relaxed"
)
histogram(
  ~ decision | seed,
  data=filtered[filtered$constraints=="dilemma",],
  type="count",
  layout=c(3,1),
  aspect=1,
  col="#ffff99",
  xlab="dilemma"
)
dev.off()

# Get rid of the "decision" column:

filtered <- filtered[,names(filtered) != "decision"]

# Likert reports:
# ---------------

# Combined
# --------

pdf(file="combined-report.pdf",title="dunyazad-study-report")
#png(file="combined-report.png",width=600,height=800)
#report <- rename(filtered[,names(filtered) %in% names(snames)], snames)
#report <- report[,snames]
report <- filtered[,names(filtered) %in% names(snames)]
report$cc <- filtered$constraints
#lk = likert(report[,names(report) != "cc"])
lk = likert(
  report[,names(report) != "cc"],
  grouping = report$cc
)
#plot(lk, group.order=c("uniform", "obvious", "relaxed", "dilemma"))
#plot(lk, group.order=c("obvious", "relaxed", "dilemma"))
plot(lk)
dev.off()


## By Stakes
## ---------
#
#pdf(file="report-by-stakes.pdf",title="dunyazad-stakes-report")
#report <- rename(filtered[,names(filtered) %in% names(snames)], snames)
#report <- report[,snames]
#lk = likert(
#  report,
#  grouping = factor(filtered[["stakes"]])
#)
#plot(lk, group.order=c("low", "high"))
#dev.off()
#
#
## Ungrouped
## --------
#
#pdf(file="ungrouped-report.pdf",title="dunyazad-ungrouped-report")
#report <- rename(filtered[,names(filtered) %in% names(snames)], snames)
#report <- report[,snames]
#lk = likert(report)
#plot(lk)
#dev.off()
#
## Obvious
## --------
#
#pdf(file="obvious-report.pdf",title="dunyazad-obvious-report")
#report <- filtered[
#  filtered$constraints == "obvious",
#  names(filtered) %in% names(snames)
#]
#report <- rename(report, snames)
#report <- report[,snames]
#sort <- filtered[filtered$constraints == "obvious",][["seed"]]
#lk = likert(
#  report,
#  grouping = sort
#)
#plot(lk)
#dev.off()
#
## Relaxed
## -------
#
#pdf(file="relaxed-report.pdf",title="dunyazad-relaxed-report")
#report <- filtered[
#  filtered$constraints == "relaxed",
#  names(filtered) %in% names(snames)
#]
#report <- rename(report, snames)
#report <- report[,snames]
#sort <- filtered[filtered$constraints == "relaxed",][["seed"]]
#lk = likert(
#  report,
#  grouping = sort
#)
#plot(lk)
#dev.off()
#
## Dilemma
## -------
#
#pdf(file="dilemma-report.pdf",title="dunyazad-dilemma-report")
#report <- filtered[
#  filtered$constraints == "dilemma",
#  names(filtered) %in% names(snames)
#]
#report <- rename(report, snames)
#report <- report[,snames]
#sort <- filtered[filtered$constraints == "dilemma",][["seed"]]
#lk = likert(
#  report,
#  grouping = sort
#)
#plot(lk)
#dev.off()
