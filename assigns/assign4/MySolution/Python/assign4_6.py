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

# let theNatPairs: (int*int) stream = fun () ->
#     let rec pairs(i)(j) =
#         StrCons((i, j), fun () ->
#             if j = 0 then 
#                 pairs(0)(i + 1)

#             else
#                 pairs(i + 1)(j - 1)

#         )
#     in 
#     pairs(0)(0)

# ;;


def pairs(i, j):
    """
    """
    strcon_cons((i, j), lambda i:
    if j = 0:
        pairs(0, (i + 1))    
    )
    return

def theNatPairs_cubesum(limit):
    """
    """
    
    pairs = []
    for i in range(limit):
        for j in range(i, limit):
            if i**3 + j**3 <= j**3 + i**3:
                pairs.append((i, j))
    return pairs

# Example usage:
# To generate and print the first 10 pairs, you can do the following:
pairs = theNatPairs_cubesum(10)
for i, j in pairs:
    print(f"({i}, {j})")