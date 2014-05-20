from jsonrpc import dispatcher
import jedi
import functools


def script_method(f):
    @dispatcher.add_method
    @functools.wraps(f)
    def wrapper(source, line, column, path):
        return f(jedi.Script(source, line, column, path))

    return wrapper
