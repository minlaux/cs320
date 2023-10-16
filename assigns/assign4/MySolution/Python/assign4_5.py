import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *


# Assign4-5:
#
# HX-2023-10-06: 30 points
# Please translate the following code from OCaml
# into Python:
#
# let
# string_fset_at
# (cs: string)(i0: int)(c0: char) =
# string_tabulate
# (string_length(cs))
# (
# fun i ->
# if i <> i0 then string_get_at(cs)(i) else c0)
# ;;
# (* ****** ****** *)
#
# let
# alphabet =
# string_tabulate(26)(fun i -> chr(ord('a') + i));;
#
# (* ****** ****** *)
#
# let
# list_of_buddies
# (word: string): string list =
# let n0 =
# string_length(word) in
# list_make_fwork
# (
# fun work ->
# int1_foreach(n0)
# (
# fun i0 ->
# let c0 =
# string_get_at(word)(i0) in
# string_foreach(alphabet)
# (fun c1 -> if c1 <> c0 then work(string_fset_at(word)(i0)(c1)))))
# ;; 
# (* ****** ****** *)
#


def string_get_at(cs, i0):
    """
        gets character at index i0 in string cs
    """
    return cs[i0]


def string_length(cs):
    """
        returns length of string cs
    """
    return len(cs)


def string_foreach(cs, work):
    """
        for each character of string cs apply the work function
    """
    lambda_i0 = lambda i0: work(string_get_at(cs, i0))
    return int1_foreach(string_length(cs), lambda_i0)


string_tabulate = lambda n, f: ''.join(f(i) for i in range(n))


def string_fset_at(cs, i0, c0):
    """
        cs: string
        i0: int
        c0: char
    """
    lambda_i = lambda i: string_get_at(cs, i) if i != i0 else c0
    return string_tabulate(string_length(cs), lambda_i)


def list_make_fwork(fwork):
    res = []

    def work(x0):
        res.append(x0)

    fwork(work)
    return list(reversed(res))



alphabet = string_tabulate(26, lambda i: chr(ord('a') + i))


def list_of_buddies(word):
    """
    word: string
    """

    n0 = string_length(word)

    def lambda_work(work):
        def lambda_i0(i0):
            c0 = string_get_at(word, i0)

            def lambda_c1(c1):
                if c1 != c0:
                    work(string_fset_at(word, i0, c1))

            string_foreach(alphabet, lambda_c1)

        int1_foreach(n0, lambda_i0)

    return list_make_fwork(lambda_work)
