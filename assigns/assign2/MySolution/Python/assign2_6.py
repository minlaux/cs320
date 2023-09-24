# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly

# let
# string_merge
# (cs1: string)(cs2: string): string =
# let n1 = string_length(cs1)
# and n2 = string_length(cs2) in
# let rec
# foreach(i1: int)(i2:int)(work) =
# if
# i1 < n1
# then (
# if
# i2 < n2
# then
#   let c1 = string_get_at(cs1)(i1)
#   and c2 = string_get_at(cs2)(i2) in
#   if c1 <= c2
#   then (work(c1); foreach(i1+1)(i2+0)(work))
#   else (work(c2); foreach(i1+0)(i2+1)(work))
# else
#   int1_foreach(n1-i1)
#     (fun i -> work(string_get_at(cs1)(i1+i)))
# ) else (
#   int1_foreach(n2-i2)
#     (fun i -> work(string_get_at(cs2)(i2+i)))
# )
# in
#   string_make_fwork(foreach(0)(0))
# ;;


def string_merge(cs1, cs2):
    """
    sorts strings cs1 and cs2 to be sequential based on ASCII code
    then merges the final result into one string

    """

    n1 = len(cs1)
    n2 = len(cs2)
    result = ""
    i1 = 0
    i2 = 0

    while i1 < n1 and i2 < n2:
        c1 = ord(cs1[i1])
        c2 = ord(cs2[i2])

        if c1 <= c2:
            result += chr(c1)
            i1 += 1
        else:
            result += chr(c2)
            i2 += 1

    while i1 < n1:
        result += cs1[i1]
        i1 += 1

    while i2 < n2:
        result += cs2[i2]
        i2 += 1

    return result