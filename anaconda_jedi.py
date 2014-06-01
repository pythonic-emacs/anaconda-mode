import functools

from anaconda_common import script_method


@script_method
def complete(script):
    """Select auto-complete candidates for source position."""

    def first_line(text):
        """Return text first line."""
        return text.strip().split('\n', 1)[0]

    return [{'name': comp.name,
             'doc': comp.docstring() or None,
             'short_doc': first_line(comp.docstring()) or None,
             'annotation': comp.type,
             'path': comp.module_path or None,
             'line': comp.line}
            for comp in script.completions()]


@script_method
def doc(script):
    """Documentation for all definitions at point."""
    docs = ['\n'.join([d.module_name + ' - ' + d.description,
                       '=' * 40,
                       d.docstring() or "- No docstring -"]).strip()
            for d in script.goto_definitions()]

    return ('\n' + '-' * 40 + '\n').join(docs)


def process_definitions(f):
    @functools.wraps(f)
    def wrapper(script):
        cache = {script.path: script.source.splitlines()}

        def get_description(d):
            if d.module_path not in cache:
                with open(d.module_path, 'r') as file:
                    cache[d.module_path] = file.read().splitlines()

            return cache[d.module_path][d.line - 1]

        return [{'line': d.line,
                 'column': d.column,
                 'name': d.name,
                 'description': get_description(d),
                 'module': d.module_name,
                 'type': d.type,
                 'path': d.module_path}
                for d in f(script) if not d.in_builtin_module()]

    return wrapper


@script_method
@process_definitions
def goto_definitions(script):
    return script.goto_definitions()


@script_method
@process_definitions
def goto_assignments(script):
    return script.goto_assignments()


@script_method
@process_definitions
def usages(script):
    return script.usages()
