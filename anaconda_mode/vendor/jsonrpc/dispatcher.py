import collections


class Dispatcher(collections.MutableMapping):

    """ Method dispatcher.

    Dictionary like object which holds map method_name to method.

    """

    def __init__(self):
        self.method_map = dict()

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

    def add_method(self, f, name=None):
        """ Add a method to the dispatcher.

        :param callable f: Callable to be added.
        :param name: Name to register
        :type name: None or str

        """
        self.method_map[name or f.__name__] = f
