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
    real x1[n_rows];
    real x2[n_rows];

    real b[n_cols];
    real y1[n_cols];
    real y2[n_cols];
}

model {
    a ~ normal(0, 1);
    x1 ~ normal(0, 1);
    x2 ~ normal(0, 1);

    b ~ normal(0, 1);
    y1 ~ normal(0, 1);
    y2 ~ normal(0, 1);

    for (obs in 1:n_obs) {
        v[obs] ~ bernoulli_logit(
            a[i[obs]] + b[j[obs]] + x1[i[obs]] * y1[j[obs]] + x2[i[obs]] * y2[j[obs]]
        );
    }
}
