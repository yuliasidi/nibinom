testthat::context('Test imposing missing values')

t <- sim_cont(
p_C = 0.6, p_T = 0.5, n_arm = 200,
mu1 = 4, mu2 = 100, sigma1 = 1,
sigma2 = 20, r12 = -0.3,
b1 = 0.1, b2 = -0.01)

do_val <- 0.2

obj <- mimpose(t, do = do_val, b_x1 = 0.2, seed = 1555)

obj1 <- mimpose(t, do = do_val, b_x1 = 0.2, b_ty = 0.5, seed = 1655)

testthat::describe('basic usage',{

  it('class',{
    testthat::expect_true(inherits(obj,'data.frame'))
  })

  it('dim',{
    testthat::expect_equal(dim(obj), dim(t))
  })

  it('do rate',{
    testthat::expect_equal(mean(is.na(obj$y.m)), do_val, tolerance = 0.02)
  })

  it('do rate',{
    testthat::expect_equal(mean(is.na(obj1$y.m)), do_val, tolerance = 0.03)
  })

})
