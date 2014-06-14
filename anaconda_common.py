import functools

import jedi
from jsonrpc import dispatcher


def script_method(f):
    @dispatcher.add_method
    @functools.wraps(f)
    def wrapper(source, line, column, path):
        try:
            return f(jedi.Script(source, line, column, path))
        except jedi.NotFoundError:
            return []

    return wrapper
