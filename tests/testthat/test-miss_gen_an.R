testthat::context('Test imposing missing values')

tt <- sim_cont(
p_C = 0.6, p_T = 0.5, n_arm = 200,
mu1 = 4, mu2 = 100, sigma1 = 1,
sigma2 = 20, r12 = -0.3,
b1 = 0.1, b2 = -0.01)

do_val <- 0.2

obj  <- miss_gen_an(tt, m2 = 0.1 , b_x1 = 0.5, do = 0.1, seed = 1555, seed_mice = 1555, alpha = 0.025)
obj1 <- miss_gen_an(tt, m2 = 0.1 , b_x1 = 0.5, do = 0.1, seed = 1555, seed_mice = 1555, alpha = 0.025,
                    dt_out = TRUE)
obj2 <- miss_gen_an(tt, m2 = 0.1 , b_x1 = 0.5, do = 0.1, seed = 1555, seed_mice = 1555, alpha = 0.025,
                    mice_anal = TRUE, mu_C = 0.99, sd_C = 0.01,  method = 'wald', n_mi = 3, m_mi = 10
                    )


testthat::describe('basic usage',{

  it('class',{
    testthat::expect_true(inherits(obj,'list'))
  })

  it('dim',{
    testthat::expect_equal(length(obj), 2)
  })

  it('dim data',{
    testthat::expect_equal(dim(obj1), c(400, 6))
  })

  it('dim mice',{
    testthat::expect_equal(length(obj2), 2)
  })
})
