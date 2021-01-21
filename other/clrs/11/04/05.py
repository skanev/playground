import numpy as np
from scipy.optimize import root


def fn(a):
    one = np.array([1.0])
    two = np.array([2.0])
    return (two / a) * np.log(one / (one - a)) - one/(one - a)


print("alpha =", root(fn, 0.6).x[0])
