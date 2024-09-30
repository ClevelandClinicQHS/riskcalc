prediction_engine <-
  function(input_data) {

    # Extract the full survival curve
    surv_curve <- survfit(model, newdata = input_data)

    # Find the survival at the specified time point
    surv_t <- summary(surv_curve, times = time)

    # Return the survival probability
    surv_t$surv

  }
