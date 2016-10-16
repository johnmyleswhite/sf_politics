data {
    int<lower = 1> n_rows;
    int<lower = 1> n_cols;
    int<lower = 1> n_obs;

    int<lower = 1, upper = n_rows> i[n_obs];
    int<lower = 1, upper = n_cols> j[n_obs];
    int<lower = 0, upper = 9> v[n_obs];
}

parameters {
    real a[n_rows];
    real x[n_rows];

    real b[n_cols];
    real y[n_cols];
}

model {
    a ~ normal(0, 10);
    x ~ normal(0, 10);

    b ~ normal(0, 10);
    y ~ normal(0, 10);

    for (obs in 1:n_obs) {
        v[obs] ~ bernoulli_logit(
            a[i[obs]] + b[j[obs]] + x[i[obs]] * y[j[obs]]
        );
    }
}
