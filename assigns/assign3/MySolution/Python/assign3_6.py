import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *


# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python



class MyList:
    """
        OCaml mylist type to Python class MyList
    """

    def mynil(self):
        return MyNil(self)
    
    def mycons(self):
        return MyCons(self)
    
    def mysnoc(self):
        return MySnoc(self)

    def myreverse(self):
        return MyReverse(self)

    def myappend2(self):
        return MyAppend2(self)


class MyNil(MyList):
    def __init__(self):
        pass


class MyCons(MyList):
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail


class MySnoc(MyList):
    def __init__(self, init, last):
        self.init = init
        self.last = last


class MyReverse(MyList):
    def __init__(self, xs):
        self.xs = xs


class MyAppend2(MyList):
    def __init__(self, xs1, xs2):
        self.xs1 = xs1
        self.xs2 = xs2


def mylist_foreach(xs, work):
    match_dict = {
        MyNil: lambda: None,
        MyCons: lambda: mylist_foreach(xs[1], work) or work(xs[0]),
        MySnoc: lambda: mylist_foreach(xs[0], work) or work(xs[1]),
        MyReverse: lambda: mylist_rforeach(xs[0], work),
        MyAppend2: lambda: mylist_foreach(xs[0], work) or mylist_foreach(xs[1], work),
    }
    return match_dict[type(xs)]()


def mylist_rforeach(xs, work):
    match_dict = {
        MyNil: lambda: None,
        MyCons: lambda: work(xs[0]) or mylist_rforeach(xs[1], work),
        MySnoc: lambda: mylist_rforeach(xs[0], work) or work(xs[1]),
        MyReverse: lambda: mylist_foreach(xs[0], work),
        MyAppend2: lambda: mylist_rforeach(xs[1], work) or mylist_rforeach(xs[0], work),
    }
    return match_dict[type(xs)]()