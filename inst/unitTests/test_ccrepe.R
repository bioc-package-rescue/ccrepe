test.ccrepe <- function() {
    library(RUnit)
    library(ccrepe)

    testdata <- matrix(c(0.29787234, 0.2978723, 0.2553191, 0.1489362,
                         0.17073171, 0.3170732, 0.2682927, 0.2439024,
                         0.09302326, 0.3255814, 0.2558140, 0.3255814,
                         0.32352941, 0.3235294, 0.1470588, 0.2058824,
                         0.17241379, 0.1724138, 0.4137931, 0.2413793,
                         0.29729730, 0.2162162, 0.2702703, 0.2162162,
                         0.22500000, 0.3250000, 0.2000000, 0.2500000,
                         0.12820513, 0.3589744, 0.2307692, 0.2820513,
                         0.20000000, 0.2250000, 0.2250000, 0.3500000,
                         0.10256410, 0.3076923, 0.1794872, 0.4102564
                        ), nrow=10, ncol=4, byrow = TRUE)
    dimnames(testdata) = list(
        c("Subject 1", "Subject 2","Subject 3","Subject 4","Subject 5","Subject 6","Subject 7","Subject 8","Subject 9","Subject 10"),
        c("bug 1", "bug 2", "bug 3","bug 4")) # column names 

    ccrepe.results <- ccrepe(x=testdata, min.subj=10)
    tol = 0.20

    p.values.results <- matrix(c(NA, 0.6337192, 0.4547206, 0.06927885,
                                 0.63371917,        NA, 0.2432229, 0.19897879,
                                 0.45472060, 0.2432229,        NA, 0.77606014,
                                 0.06927885, 0.1989788, 0.7760601 ,        NA),
                               nrow=4, ncol=4, byrow = TRUE)

    q.values.results <- matrix(c(  NA, 1.801518, 1.615833, 0.9847193,
                                 1.8015179  ,     NA ,1.152378, 1.4141274,
                                 1.6158327, 1.152378,       NA ,1.8384672,
                                 0.9847193 ,1.414127, 1.838467,        NA),
                               nrow=4, ncol=4, byrow = TRUE)

    sim.score.results <- matrix(c(   NA, -0.20691521 ,-0.1639831, -0.77208214,
                                  -0.2069152  ,        NA, -0.6739868 , 0.06805391,
                                  -0.1639831 ,-0.67398679  ,       NA ,-0.22701786,
                                  -0.7720821  ,0.06805391 ,-0.2270179  ,        NA),
                                nrow=4, ncol=4, byrow = TRUE)

    z.stat.results <- matrix(c( NA,  0.4352228,  0.7337243, -1.8029587,
                               0.4352228 ,        NA ,-1.1905054,  1.2389936,
                                0.7337243, -1.1905054,         NA , 0.2843813,
                               -1.8029587 , 1.2389936,  0.2843813 ,        NA ),
                             nrow=4, ncol=4, byrow = TRUE)

    checkEqualsNumeric(as.vector(p.values.results), as.vector(ccrepe.results$p.values), tolerance = tol)
    checkEqualsNumeric(as.vector(q.values.results), as.vector(ccrepe.results$q.values), tolerance = tol)
    checkEqualsNumeric(as.vector(sim.score.results), as.vector(ccrepe.results$sim.score), tolerance = tol)
    checkEqualsNumeric(as.vector(z.stat.results), as.vector(ccrepe.results$z.stat), tolerance = tol)
}
