#' Radical Cystectomy for Bladder Cancer
#'
#' A fake data set representing the structure of what would be used to build the model for predicting 5-year recurrence-free survival after radical cystectomy (RC) for bladder cancer and subsequent risk calculator at https://riskcalc.org/bladderCancer/
#'
#' @format ## `bladderCancer`
#' A data frame with 1000 rows and 9 columns:
#' \describe{
#'   \item{Gender}{Patient gender}
#'   \item{Age}{Age of patient at time of RC}
#'   \item{RCTumorPathology, RCTumorHistology, RCTumorGrade}{Pathology, histology, and grade of the tumor at time of RC}
#'   \item{LymphNodeStatus}{Status of the lymph node}
#'   \item{DaysBetweenDXRC}{Number of days between cancer diagnosis and RC}
#'   \item{Recurrence}{Did the cancer re-occur? 1=Yes, 0=No (censored)}
#'   \item{Time}{Years from RC in which the recurrent event occurred or the patient was censored}
#' }
#' @source <https://github.com/ClevelandClinicQHS/riskcalc/blob/main/data-raw/bladderCancer.R>
"bladderCancer"
