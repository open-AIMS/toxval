# unsupported glm formulae and subclasses give clear errors

    Code
      toxval_predict(fit)
    Condition
      Error in `toxval_predict.glm()`:
      ! `x_var` must be supplied for a `glm` fit.

---

    Code
      toxval_predict(offset_fit, x_var = "x")
    Condition
      Error in `toxval_predict.glm()`:
      ! `object` must not include an offset; the grid cannot supply it.

---

    Code
      toxval_predict(extra_fit, x_var = "x")
    Condition
      Error in `toxval_predict.glm()`:
      ! `object` must not include predictors other than `x_var` and `group_var`: 'z'.

# mgcv gam is refused before glm prediction

    Code
      toxval_predict(gam_fit, x_var = "x")
    Condition
      Error in `toxval_predict.glm()`:
      ! The `glm` subclass 'gam' is not supported; use a plain `glm` or `MASS::glm.nb` fit.

