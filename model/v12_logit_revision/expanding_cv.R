expanding_cv <- function(x, y, validation = c("one_step", "remaining"),
                         lambda = NULL, step_size = 1L) {
  validation <- match.arg(validation)
  
  # x: numeric predictor matrix, already containing your lagged variables
  # y: numeric binary outcome (0 or 1)
  # Rows must be in chronological order, with missing lag rows removed.
  x <- as.matrix(x)
  n <- nrow(x)
  stopifnot(
    n >= 6L,
    ncol(x) >= 2L,
    length(y) == n,
    is.numeric(x),
    is.numeric(y),
    all(is.finite(x)),
    all(is.finite(y)),
    all(y %in% c(0, 1)),
    is.numeric(step_size),
    length(step_size) == 1L,
    is.finite(step_size),
    step_size >= 1,
    step_size == floor(step_size)
  )

  initial <- floor(n / 2)
  # Binomial glmnet needs more than one observation in each class.
  while (initial < n &&
         min(tabulate(y[seq_len(initial)] + 1L, nbins = 2L)) < 2L) {
    initial <- initial + 1L
  }
  if (initial >= n) {
    stop("Cannot include two observations of each class in training and leave validation data.")
  }
  
  
  # Construct a common penalty grid using only the initial training data.
  if (is.null(lambda)) {
    pilot <- glmnet(
      x[seq_len(initial), , drop = FALSE],
      y[seq_len(initial)],
      family = "binomial",
      alpha = 1,
      nlambda = 100,
      standardize = TRUE
    )
    lambda <- pilot$lambda
  }
  
  lambda <- sort(unique(lambda), decreasing = TRUE)
  # stopifnot(length(lambda) >= 2L,
  #           all(is.finite(lambda)), all(lambda > 0))
  
  # step_size counts rows: 24 for daily steps in hourly data;
  # 12 for hourly steps in five-minute data. Assumes regular spacing.
  # one_step validates the next step_size rows (a shorter final block is allowed).
  # The option name is retained for compatibility with existing calls.
  origins <- seq.int(initial, n - 1L, by = step_size)
  loss_sum <- matrix(NA_real_, length(origins), length(lambda))
  n_valid <- integer(length(origins))
  
  for (i in seq_along(origins)) {
    end_train <- origins[i]
    train <- seq_len(end_train)
    
    valid <- if (validation == "one_step") {
      seq.int(end_train + 1L, min(end_train + step_size, n))
    } else {
      seq.int(end_train + 1L, n)
    }
    
    fit <- glmnet(
      x[train, , drop = FALSE],
      y[train],
      family = "binomial",
      alpha = 1,
      lambda = lambda,
      standardize = TRUE
    )
    
    # One prediction column per candidate lambda.
    p <- as.matrix(predict(
      fit,
      newx = x[valid, , drop = FALSE],
      s = lambda,
      type = "response"
    ))
    
    # Avoid log(0).
    p[] <- pmin(pmax(p, 1e-15), 1 - 1e-15)
    
    yy <- matrix(y[valid], nrow = length(valid), ncol = length(lambda))
    loss_sum[i, ] <- colSums(
      -(yy * log(p) + (1 - yy) * log1p(-p))
    )
    n_valid[i] <- length(valid)
  }
  
  mean_loss <- colSums(loss_sum) / sum(n_valid)
  best_lambda <- which.min(mean_loss)
  
  # Refit on all supplied observations after choosing the penalty.
  final_fit <- glmnet(
    x, y,
    family = "binomial",
    alpha = 1,
    lambda = lambda,
    standardize = TRUE
  )
  
  list(
    lambda_min = best_lambda,
    cv_results = data.frame(lambda = lambda, log_loss = mean_loss),
    fit = final_fit
  )
}