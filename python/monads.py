
class Monad(object):
    @classmethod
    def lift(cls, f):
        def lifted(m, *args, **kwargs):
            return m.bind(lambda x: cls(f(x, *args, **kwargs)))
        return lifted


class Maybe(Monad):
    def __init__(self, x):
        self.x = x

    def bind(self, f):
        if self.isJust():
            return f(self.x)
        else:
            return Maybe(None)

    ifJust = bind # I friendlier name for bind
        

    def isJust(self):
        return self.x is not None

    def isNothing(self):
        return not self.isJust()

    @classmethod
    def catMaybes(cls, maybes):
        ret = []
        for x in maybes:
            x.bind(ret.append)
        return ret
        
    def default(self, default):
        return self.x if self.isJust() else default

    @classmethod
    def listToMaybe(self, iterator):
        for x in iterator:
            return Maybe(x)
        return Nothing

    def __iter__(self):
        if self.isJust():
            yield self.x

    def __repr__(self):
        if self.isJust():
            return "<Just({0!r}) object at {1:x}>".format(self.x, id(self))
        else:
            return "<Nothing object at {0:x}>".format(id(self))

Just = Maybe
Nothing = Maybe(None)


def getNested(container, *keys):
    def lookup(c, k):
        try:
            return c[k]
        except (KeyError, IndexError):
            return getattr(c, k, None)

    lookupM = Maybe.lift(lookup)

    return reduce(lambda m,key: lookupM(m, key), keys, Maybe(container))
