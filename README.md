# logistic-puzzle

Logistic-regression is a go-to technique for solving prediction problems with a binary outcome.  However the fits it provides can be problematic if outcomes do not converge to 1 and 0 over the range of your predictors.

This project compares simple logistic regression with a generalised four-parameter logistic regression across a range of response functions:
  1.  Logistic
  2.  Linear
  3.  Quadratic
  4.  Logarithmic


####Two-parameter logistic regression:
![two parameter logistic regression](/logistic-2p.png?raw=true "two parameter logistic regression")

The two-parameter model doesn't capture the behaviour of any of the functions except linear.

####Four-parameter logistic regression:
![four parameter logistic regression](/logistic-4p.png?raw=true "four parameter logistic regression")

The four-parameter model provides a decent fit in all four situations.
