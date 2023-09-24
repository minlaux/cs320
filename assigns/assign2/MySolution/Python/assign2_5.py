import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml


def fnlist_make_fwork(fwork):
    res = fnlist_nil()

    def work(x0):
        nonlocal res
        res = fnlist_cons(x0, res)

    fwork(work)

    return fnlist_reverse(res)





# (** transforms the work done by fwork into a list. **)
#let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
#  let res = ref([]) in
#    let work(x0) = (res := (x0 :: !res))
#    in(*let*)(fwork(work); list_reverse(!res) )
#;;