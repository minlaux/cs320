import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python

"""
type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist
"""

class MyNil:
    def __init__(self):
        self.tag = "MyNil"

class MyCons:
    def __init__(self, head, tail):
        self.tag = "MyCons"
        self.head = head
        self.tail = tail

class MySnoc:
    def __init__(self, tail, last):
        self.tag = "MySnoc"
        self.tail = tail
        self.last = last

class MyReverse:
    def __init__(self, xs):
        self.tag = "MyReverse"
        self.xs = xs

class MyAppend2:
    def __init__(self, xs1, xs2):
        self.tag = "MyAppend2"
        self.xs1 = xs1
        self.xs2 = xs2



class MyList:
    def __init__(self, mynil, mycons, mysnoc, myreverse, myappend2):
        self.mynil = MyNil(self)
        self.mycons = MyCons(self)
        self.mysnoc = MySnoc(self)
        self.myreverse = fnlist_reverse(self)
        self.myappend2 = MyAppend2(self)


class MyNil(MyList):
    def __init__(self):
        def is_empty(self):
        return True


class MyCons(MyList):
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail
    def get_head(self):
        return self.head
    def get_tail(self):
        return self.tail


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



def mylist_foreach(self, work):
    if MyNil(self):
        pass
    elif MyCons(self):
    self.mycons = MyCons(self)
    self.mysnoc = MySnoc(self)
    self.myreverse = mylist_rforeach(self, work)
    self.myappend2 = mylist_foreach(self, work)


    return

if isinstance(xs, MyNil):
        pass
    elif isinstance(xs, MyCons):
        work(xs.head)
        mylist_foreach(xs.tail, work)
    elif isinstance(xs, MySnoc):
        mylist_foreach(xs.tail, work)
        work(xs.last)
    elif isinstance(xs, MyReverse):
        mylist_rforeach(xs.original, work)
    elif isinstance(xs, MyAppend2):
        mylist_foreach(xs.list1, work)
        mylist_foreach(xs.list2, work)

def mylist_rforeach(self, work):
    return