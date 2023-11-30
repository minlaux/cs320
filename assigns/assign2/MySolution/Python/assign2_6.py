import sys 
sys.path.append("./../../../../classlib/Python")
from MyPython import *


# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly


def string_get_at(s, i):
    """
    returns character in string s at index i
    """

    index = 0

    for c in s:
        if index == i:
            return c
        else: 
            index += 1


def string_make_fwork(fwork):
    """
    returns string of the characters processed by fwork function
    """

    xs = []

    def work(char):
        xs.append(char)

    fwork(work)

    result = ''.join(xs)

    return result



def string_merge(cs1, cs2):
    """
    sorts strings cs1 and cs2 to be sequential based on ASCII code
    then merges the final result into one string

    """

    n1 = len(cs1)
    n2 = len(cs2)
    result = []

    def foreach(i1, i2, work):
        if i1 < n1:
            if i2 < n2:
                c1 = string_get_at(cs1, i1)
                c2 = string_get_at(cs2, i2)

                if c1 <= c2:
                    work(c1)
                    foreach((i1 + 1), i2, work)
                else:
                    work(c2)
                    foreach(i1, (i2 + 1), work)
            else:
                int1_foreach((n1 - i1), (lambda i: work(string_get_at(cs1, i1 + i))))
        else:
            int1_foreach((n2 - i2), (lambda i: work(string_get_at(cs2, i2 + i))))
    
    
    return string_make_fwork(lambda work: foreach(0, 0, work))

