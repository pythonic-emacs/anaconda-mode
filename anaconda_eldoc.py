from jsonrpc import dispatcher
import jedi


def jedi_script(source, line, column, path):
    """Make jedi instance."""

    return jedi.Script(source, line, column, path)


@dispatcher.add_method
def eldoc(*args):
    """Return eldoc format documentation string or None."""

    script = jedi_script(*args)

    signatures = script.call_signatures()

    if len(signatures) == 1:

        call_name = signatures[0].call_name
        params = signatures[0].params
        call_params = [param.get_code(new_line=False) for param in params]

        return '{0}({1})'.format(call_name, ', '.join(call_params))
