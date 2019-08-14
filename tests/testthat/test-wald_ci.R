testthat::context('Test wald confidence interval')

t <- sim_cont(
p_C = 0.6, p_T = 0.5, n_arm = 200,
mu1 = 4, mu2 = 100, sigma1 = 1,
sigma2 = 20, r12 = -0.3,
b1 = 0.1, b2 = -0.01)
obj <- wald_ci(t, m2 = 0.1, 'y', alpha = 0.025)

testthat::describe('basic usage',{

  it('class',{
    testthat::expect_true(inherits(obj,'data.frame'))
  })

  it('dim',{
    testthat::expect_equal(dim(obj),c(1,9))
  })

})
