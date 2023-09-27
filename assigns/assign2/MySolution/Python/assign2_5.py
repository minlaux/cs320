import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

# Assign2-5: 20 points
# Please implement in Python a function
# of the name fnlist_make_fwork that corresponds
# to the function list_make_fwork in the library
# MyOCaml.ml


def fnlist_make_fwork(fwork):
    """
    returns list of the characters processed by fwork function
    """

    res = []

    fwork(lambda x: res.append(x))

    ans = fnlist_nil()

    for i in range(len(res) - 1, -1, -1):
        ans = fnlist_cons(res[i], ans)
        
    return ans



# (** transforms the work done by fwork into a list. **)
#let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
#  let res = ref([]) in
#    let work(x0) = (res := (x0 :: !res))
#    in(*let*)(fwork(work); list_reverse(!res) )
#;;