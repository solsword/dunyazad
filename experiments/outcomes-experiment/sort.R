#!/usr/bin/env Rscript

# Help sorting into approve/reject...

library(dplyr)

qnames = c(
  "opt-obvious" = "Considering just the options, there seems to be a clear best option at this choice.",
  "opt-balanced" = "Ignoring outcomes, the options at this choice all seem about equally good (or bad).",
  "opt-nobad" = "Ignoring outcomes, there are no options that seem bad at this choice.",
  "opt-nogood" = "Ignoring outcomes, none of the options at this choice seem good.",
  "opt-stakes" = "Considering just the options, the stakes for this choice seem low.",
  "out-fair" = "Given the options available, the outcome I got is fair.",
  "out-sense" = "The outcome that I got makes sense given the option that I selected.",
  "out-bad" = "I got a bad outcome.",
  "out-happy" = "I'm happy with the option that I chose.",
  "out-unfair" = "The outcome that I got is unfair, given the options available.",
  "out-unexpected" = "The outcome that I got is completely unexpected.",
  "out-trick" = "There is an outcome. (This is a trick question to test whether you're paying attention. Please simply indicate that you are in complete disagreement.)",
  "out-broken" = "There might be a problem with this choice--the outcome I got does not make sense.",
  "out-good" = "The outcome that I got is a good outcome.",
  "out-expected" = "I pretty much expected the outcome that I got.",
  "out-regret" = "I wish I had chosen a different option.",
  "motives" = "Which of the following motive(s) contributed to your decision? (pick one or more)",
  "judge-good" = "Which of the following judgement(s) contributes to how you generally define a \"good\" outcome in interactive experiences like the one you just played? (pick one or more)",
  "judge-bad" = "Which of the following judgement(s) contributes to how you generally define a \"bad\" outcome in interactive experiences like the one you just played? (pick one or more)",
  "consistency" = "Do you feel you approach all interactive experiences (e.g., Choose-Your-Own-Adventure novels, video games, tabletop role-playing games, etc.) with a consistent set of motivations and judgements, or do your motivations and judgements change from story to story?",
  "extra-feedback" = "If you have any other feedback you'd like to give, feel free to enter it here."
)

snames = c(
  "opt-obvious" = "[...] there [is] a clear best option [...].",
  "opt-balanced" = "[...] [the options] [seem about equally promising].",
  "opt-nobad" = "[...] there are no options that seem bad [...].",
  "opt-nogood" = "[...] none of [the options] seem good.",
  "opt-stakes" = "[...] [the stakes] seem low.",
  "out-fair" = "[...] the outcome I got is fair.",
  "out-sense" = "The outcome that I got makes sense [...].",
  "out-bad" = "I got a bad outcome.",
  "out-happy" = "I'm happy with the option that I chose.",
  "out-unfair" = "The outcome that I got is unfair [...].",
  "out-unexpected" = "The outcome that I got is completely unexpected.",
  "out-trick" = "There is an outcome. [...]",
  "out-broken" = "[...] the outcome I got does not make sense.",
  "out-good" = "The outcome that I got is a good outcome.",
  "out-expected" = "I pretty much expected the outcome that I got.",
  "out-regret" = "I wish I had chosen a different option.",
  "motives" = "Which of the following motive(s) contributed to your decision? [...]",
  "judge-good" = "Which of the following judgement(s) contributes to how you [define a \"good\" outcome]?",
  "judge-bad" = "Which of the following judgement(s) contributes to how you [define a \"bad\" outcome]?",
  "consistency" = "Do you feel you approach all interactive experiences [...] with a consistent set of motivations and judgements [...]?",
  "extra-feedback" = "If you have any other feedback you'd like to give, [enter it here]."
)

valid_agefluency = c(
  "I confirm that I am at least eighteen years of age!",
  "I confirm that I am at least eighteen years of  age!",
  "I confirm that I am at least eighteen years of Age!",
  ",I confirm that I am at least eighteen years of Age!",
  "I confirm thatI am at least eighteen years of Age!",
  "I confirm that I am at least eighteen years of age !",
  "I confirm that I am at least eighteen years of Age !",
  "I confirm that i am at least eighteen years of age!",
  "I confirm that i am at least eighteen years of Age!",
  "I confirm that i am at least eighteen years of age !",
  "I confirm that i am at least eighteen years of Age !",
  "I confirm that I am at least eighteen years of age.",
  "I confirm that I am at least eighteen years of Age.",
  "I, confirm that I am at least eighteen years of age!",
  "I, confirm that I am at least eighteen years of Age!",
  "I confirm that I am at least eighteen years of age",
  "I confirm that I am at least eighteen years of Age",
  "i confirm that i am at least eighteen years of age!",
  "i confirm that i am at least eighteen years of age",
  "I confirm that iam at least eighteen years of age",
  "I conform that I am at least eighteen years of age!",
  "I confirm that I am at least eighteen years of !",
  "I, Confirm that i am at least eighteen years of age!",
  ",I confirm that I am at least eighteen years of Age",
  "\"I confirm that I am at least eighteen years of age!\"",
  "\"I confirm that I am at least eighteen years of Age\"",
  "I confirm that i'am eighteen years of age.",
  "I confirm that I am at least eighteen years of age. (Technically it would be: ega fo sraey neethgie tsael ta ma I taht mrifnoc I)",
  "I confirm that I am at leas eighteen years of age!",
  "I confirm that iam at least eighteen years of age.",
  "I confirm that I am at least eighteen years of age! (I presume that's what you're looking for but the actual rules are a bit ambiguous as to the proper way to parse that... e.g. egA fo sraey...)",
  "I am at least eighteen years of age!",
  ",I mrifnoc taht I ma ta tsael neethgie sraey fo egA!",
  "Ega fo sraey neethgie tsael ta ma I taht mrifnoc I!",
  "I mrifnoc taht I ma ta tsael neethgie sraey fo egA!",
  "I mrifnoc taht I ma ta tsael neethgie sraey fo ega!",
  "I mrifnoc taht I ma ta tsael neethgie sraey fo agA!",
  "I mrifnoc taht I ma ta tsael!",
  ",I mrifnoc taht I ma ta tsael neethgie sraey fo egA\"",
  "backwards, followed by an exclamation point",
  "backwards,followed by an exclamation point",
  "backwards!"
)

suspect_agefluency = c(
  "Age of years eighteen least at am I that confirm I,",
  "Age of years eighteen least at am I that confirm I",
  "I'am at least the age of eighteen years."
)

base <- read.csv(file="partial-results.csv",head=TRUE,sep=",")


#print(base)
#print(base[["Answer.agefluency"]])

cat("\n---\n")

cat("Total responses:", nrow(base), "\n")

cat("Blank agefluency:", nrow(base[base[["Answer.agefluency"]] == "{}",]), "\n")

approved <- base[base[["AssignmentStatus"]] == "Approved",]
rejected <- base[base[["AssignmentStatus"]] == "Rejected",]

cat("Already approved:", nrow(approved), "\n")
cat("Already rejected:", nrow(rejected), "\n")

data <- anti_join(base, approved)
data <- anti_join(data, rejected)

# Too quick:
pass <- data[(data[["WorkTimeInSeconds"]] > 90) | (data[["WorkTimeInSeconds"]] == -1),]
#write.csv(pass, file="p1.csv")

failed_time <- data[(data[["WorkTimeInSeconds"]] < 90) & (data[["WorkTimeInSeconds"]] != -1),]

cat("  pass/time:", nrow(pass), "\n")
cat("    fail/time:", nrow(failed_time), "\n")

# Perhaps not fluent:
pass <- pass[(pass[["Answer.agefluency"]] %in% valid_agefluency),]
#write.csv(pass, file="p2.csv")

failed_fluency <- data[!(data[["Answer.agefluency"]] %in% valid_agefluency),]
suspect_fluency <- failed_fluency[(failed_fluency[["Answer.agefluency"]] %in% suspect_agefluency),]

failed_fluency <- anti_join(failed_fluency, suspect_fluency)

cat("  pass/fluent:", nrow(pass), "\n")
cat("    fail/fluent:", nrow(failed_fluency), "\n")
cat("    fail/fluent (suspect):", nrow(suspect_fluency), "\n")

# Wrong answer to the trick question:
failed_trick <- data[(is.na(data[["Answer.out.trick"]]) | data[["Answer.out.trick"]] != "1"),]
pass <- pass[(pass[["Answer.out.trick"]] == "1"),]
#write.csv(pass, file="p3.csv")

cat("  pass/trick:", nrow(pass), "\n")
cat("    fail/trick:", nrow(failed_trick), "\n")

# Get rid of irrelevant columns:
pass <- pass[
  ,
  names(pass) == "HITId"
| names(pass) == "HITTypeId"
| names(pass) == "WorkerId"
| names(pass) == "AssignmentID"
| names(pass) == "Input.seed"
| names(pass) == "Input.constraints"
| names(pass) == "chosen_setup"
| names(pass) == "WorkTimeInSeconds"
| (
    substr(names(pass), 1, 3) == "Ans"
  & names(pass) != "Answer.agefluency"
  & names(pass) != "Answer.trick"
  )
]
# Filter out incomplete entries:
incomplete <- pass[!complete.cases(pass),]
pass <- pass[complete.cases(pass),]
#write.csv(pass, file="p4.csv")

cat("  pass/complete:", nrow(pass), "\n")
cat("    fail/complete:", nrow(incomplete), "\n")

failed_complete <- semi_join(data, incomplete)

full_pass <- semi_join(data, pass)
failed <- anti_join(data, pass)

full_pass[["Approve"]] = "x"
full_pass[["Reject"]] = ""

failed[["Approve"]] = ""
failed[["Reject"]] = "Your assignment has been rejected for one of the following reasons: 1. You didn't answer the age/fluency question correctly. 2. You completed the entire survey in less than 90 seconds. 3. You weren't paying attention and didn't answer the trick question (which was explicitly labeled as such) correctly. 4. You left one or more response fields blank."

write.csv(full_pass, file="successful-results.csv")
write.csv(failed, file="all-failed.csv")

write.csv(failed_time, file="failed-time.csv")
write.csv(failed_fluency, file="failed-fluency.csv")
write.csv(suspect_fluency, file="suspect-fluency.csv")
write.csv(failed_trick, file="failed-trick.csv")
write.csv(failed_complete, file="failed-complete.csv")

write.csv(base[["WorkerId"]], file="workers.csv")
