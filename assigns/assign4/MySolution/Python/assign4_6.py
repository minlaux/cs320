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


# def theNatPairs_cubesum():
#     def pairs(i, j):
#         if i == j or j = 0:
#             return strcon_cons((i, j), lambda: pairs(0, j + 1))
#         elif i > j:

#         else:
#             return strcon_cons((i, j), lambda: pairs(i + 1, j))
#     return pairs(0, 0)


def theNatPairs_cubesum():
    i = 0
    j = 0

    while True:
        if i == j:
            if i**3 + j**3 < (j + 1)**3:
                yield (i, j)
                i = 0
                j += 1

        elif i < j:
            if i**3 + j**3 < (i + 1)**3 + j**3:
                yield (i, j)
                i += 1

        else:
            yield (j, i)
            i += 1



# (0, 0) - Cube sum: 0 + 0 = 0
# (0, 1) - Cube sum: 0 + 1 = 1
# (1, 0) - Cube sum: 1 + 0 = 1 xxx
# (1, 1) - Cube sum: 1 + 1 = 2
# (0, 2) - Cube sum: 0 + 8 = 8
# (2, 0) - Cube sum: 8 + 0 = 8 xxx
# (1, 2) - Cube sum: 1 + 8 = 9
# (2, 1) - Cube sum: 8 + 1 = 9 xxx
# (2, 2) - Cube sum: 8 + 8 = 16
# (0, 3) - Cube sum: 0 + 27 = 27
# (3, 0) - Cube sum: 27 + 0 = 27 xxx
# (1, 3) - Cube sum: 1 + 27 = 28
# (3, 1) - Cube sum: 27 + 1 = 28 xxx