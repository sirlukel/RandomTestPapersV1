% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_question.R
\name{new_question}
\alias{new_question}
\title{Creates a new object with class question}
\usage{
new_question(
  name,
  option_num,
  correct_score,
  wrong_loss,
  partial_score,
  given_no_correct,
  correct
)
}
\arguments{
\item{name}{a string to name the question to make it easily identifiable in a test}

\item{option_num}{an integer representing the number of options the question has}

\item{correct_score}{a numeric value representing how many marks you are awarded for answering the question correctly}

\item{wrong_loss}{a numeric value representing how many marks you are awarded for answering the question incorrectly. Usually a negative value}

\item{partial_score}{a logical value stating if a student should be awarded a portion of the marks for choosing some correct choices, but no incorrect choices}

\item{given_no_correct}{a logical value stating if the student is told how many answers to the question are correct}

\item{correct}{a list of integers giving the number of each correct answer. There should be no repeated numbers and no number higher than option_num}
}
\value{
A question type object
}
\description{
Creates a new object with class question
}
