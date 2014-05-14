import collections


class Dispatcher(collections.MutableMapping):

    """ Method dispatcher.

    Dictionary like object which holds map method_name to method.

    """

    def __init__(self, prototype=None):
        """ Build method dispatcher.

        :param prototype: Initial method mapping.
        :type prototype: None or object or dict

        """
        self.method_map = dict()

        if prototype is not None:
            self.build_method_map(prototype)

    def __getitem__(self, key):
        return self.method_map[key]

    def __setitem__(self, key, value):
        self.method_map[key] = value

    def __delitem__(self, key):
        del self.method_map[key]

    def __len__(self):
        return len(self.method_map)

    def __iter__(self):
        return iter(self.method_map)

    def __repr__(self):
        return repr(self.method_map)

    def add_method(self, f, name=None):
        """ Add a method to the dispatcher.

        :param callable f: Callable to be added.
        :param name: Name to register
        :type name: None or str

        When used as a decorator keep callable object unmodified.
        """
        self.method_map[name or f.__name__] = f
        return f

    def build_method_map(self, prototype):
        """ Add prototype methods to the dispatcher.

        :param prototype: Method mapping.
        :type prototype: None or object or dict

        If given prototype is a dictionary then all callable objects
        will be added to dispatcher.  If given prototype is an object
        then all public methods will be used.

        """
        if not isinstance(prototype, dict):
            prototype = dict((method, getattr(prototype, method))
                             for method in dir(prototype)
                             if not method.startswith('_'))

        for attr, method in prototype.items():
            if callable(method):
                self[attr] = method
