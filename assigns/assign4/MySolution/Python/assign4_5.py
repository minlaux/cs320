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


def string_length(str):
    return len(str)


def string_tabulate(length, fun):
    return strcon_cons(length, fun)


def string_fset_at(cs, i0, c0):
    """
        
    """
    
    return string_tabulate(string_length(cs), lambda i: string_get_at(cs, i) if i != i0 else c0)


alphabet = string_tabulate(26, lambda i: chr(ord('a') + i))

def list_of_buddies(word: String):
    """
    """
    n0 = string_length(word)
    list_make_fwork(lambda work: int1_foreach(n0)(lambda i0: ))


# def string_fset_at(cs, i0, c0):
#     return "".join(c if i != i0 else c0 for i, c in enumerate(cs))

# def list_make_fwork(fwork):
#     return fwork()


# def string_foreach(s, func):
#     for c in s:
#         func(c)

# def string_get_at(cs, i):
#     return cs[i]

# def string_length(cs):
#     return len(cs)

# def chr(i):
#     return chr(i)

# def ord(c):
#     return ord(c)

# alphabet = "".join(chr(ord('a') + i) for i in range(26))

# def list_of_buddies(word):
#     n0 = string_length(word)
#     results = []
    
#     def work_callback(new_word):
#         results.append(new_word)
    
#     list_make_fwork(lambda: int1_foreach(n0, lambda i0: 
#         c0 = string_get_at(word, i0)
#         string_foreach(alphabet, lambda c1:
#             if c1 != c0:
#                 work_callback(string_fset_at(word, i0, c1))
#         )
#     ))
    
#     return results

