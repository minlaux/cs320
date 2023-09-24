# Assign2-6: 20 points
# Please translate the following code
# into Python and then implement all the
# functions called in the code to make the
# translation (written in Python) works correctly



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