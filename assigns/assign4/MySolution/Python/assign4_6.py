import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *


# Assign4-6:
#
# HX-2023-10-06: 30 points (bonus)
#
# (*
# //
# Please implement the following function
# that enumerates all the pairs (i, j) of natural
# numbers satisfying $i <= j$; a pair (i1, j1) must
# be enumerated ahead of another pair (i2, j2) if the
# following condition holds:
#   i1*i1*i1 + j1*j1*j1 < i2*i2*i2 + j2*j2*j2
# //
# let
# theNatPairs_cubesum(): (int * int) stream = fn () =>
# //
# *)
#
# def theNatPairs_cubesum(): # please give your implementation
#


def theNatPairs_cubesum():
    def pairs(i, j):
        if i <= j:
            if i == 0:
                return strcon_cons((i, j), lambda: pairs(i + 1, j))
            else:
                return strcon_cons((i, j), lambda: pairs(i + 1, j + 1))
        else:
            return strcon_cons((i, j), lambda: pairs(j, i))

    return pairs(0, 0)


# def theNatPairs_cubesum():
#     i = 0
#     j = 0
#     while True:
#         if (i**3 + j**3 < i**3 + (j + 1)**3) and i <= j:
#             yield (i, j)
#             i += 1
#         else:
#             i = 0
#             j += 1