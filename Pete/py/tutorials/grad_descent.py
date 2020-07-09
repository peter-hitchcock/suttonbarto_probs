# Most of this not written by me, code for following grad descent tutorial at
# https://machinelearningmastery.com/implement-linear-regression-stochastic-gradient-descent-scratch-python/

import numpy as np
import matplotlib

# Make a prediction with coefficients
def predict(row, coefficients):
	yhat = coefficients[0]
	for i in range(len(row)-1):
		yhat += coefficients[i + 1] * row[i]
	return yhat

dataset = [[1, 1], [2, 3], [4, 3], [3, 2], [5, 5]]
coef = [0.4, 0.8]
for row in dataset:
	yhat = predict(row, coef)
	print("Expected=%.3f, Predicted=%.3f" % (row[-1], yhat))

# from https://towardsdatascience.com/taking-derivatives-in-python-d6229ba72c64
import sympy as sym
x = sym.Symbol("x")
sym.diff((x**2 - 3*x + 5) ** 3)