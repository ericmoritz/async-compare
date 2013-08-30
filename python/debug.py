from time import time

def trace(f):
    def inner(*args, **kwargs):
        print("{f.__name__}({args!r}, {kwargs!r})".format(**locals()))
        ret = f(*args, **kwargs)
        return ret
    return inner

def tc(f):
    start = time()
    ret = f()
    end = time()
    return (end-start, ret)

def reporttc((secs, ret)):
    print "{secs}s".format(secs=secs)
    return ret

    
