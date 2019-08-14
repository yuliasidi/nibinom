testthat::context('Test the full simulation data')

p_C <- 0.6

obj <- sim_cont(
p_C = p_C, p_T = 0.5, n_arm = 200,
mu1 = 4, mu2 = 100, sigma1 = 1,
sigma2 = 20, r12 = -0.3,
b1 = 0.1, b2 = -0.01)

testthat::describe('basic usage',{

  it('class',{
    testthat::expect_true(inherits(obj,'tbl'))
  })

  it('dim',{
    testthat::expect_equal(dim(obj),c(400,6))
  })

  it('check p_C',{
    testthat::expect_equal(mean(obj$y[obj$trt=='C']),p_C,tolerance=0.05)
  })

})
