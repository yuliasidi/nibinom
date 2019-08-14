testthat::context('Test missingness parameters assignment')

obj_do20 <- mpars(do = 0.2, atype = "sing")
obj_do15 <- mpars(do = 0.15, atype = "sing")
obj_do10 <- mpars(do = 0.1, atype = "sing")
obj_do5  <- mpars(do = 0.05, atype = "sing")

objm_do20 <- mpars(do = 0.2, atype = "mice")
objm_do15 <- mpars(do = 0.15, atype = "mice")
objm_do10 <- mpars(do = 0.1, atype = "mice")
objm_do5  <- mpars(do = 0.05, atype = "mice")

testthat::describe('basic usage',{

  it('class',{
    testthat::expect_true(inherits(obj_do20,'tbl'))
  })
  it('class',{
    testthat::expect_true(inherits(obj_do15,'tbl'))
  })
  it('class',{
    testthat::expect_true(inherits(obj_do10,'tbl'))
  })
  it('class',{
    testthat::expect_true(inherits(obj_do5,'tbl'))
  })

  it('dim',{
    testthat::expect_equal(dim(obj_do20),c(10,6))
  })

  it('dim',{
    testthat::expect_equal(dim(obj_do15),c(8,6))
  })

  it('dim',{
    testthat::expect_equal(dim(obj_do10),c(6,6))
  })

  it('dim',{
    testthat::expect_equal(dim(obj_do5),c(4,6))
  })


  it('class',{
    testthat::expect_true(inherits(objm_do20,'tbl'))
  })
  it('class',{
    testthat::expect_true(inherits(objm_do15,'tbl'))
  })
  it('class',{
    testthat::expect_true(inherits(objm_do10,'tbl'))
  })
  it('class',{
    testthat::expect_true(inherits(objm_do5,'tbl'))
  })

  it('dim',{
    testthat::expect_equal(dim(objm_do20),c(2,6))
  })

  it('dim',{
    testthat::expect_equal(dim(objm_do15),c(2,6))
  })

  it('dim',{
    testthat::expect_equal(dim(objm_do10),c(2,6))
  })

  it('dim',{
    testthat::expect_equal(dim(objm_do5),c(2,6))
  })

})
