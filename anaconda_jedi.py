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
def location(script):
    """Find names assignment place."""
    return [details(defn) for defn in all_definitions(script)]
def doc(script):
    """Documentation for all definitions at point."""
    docs = ['\n'.join([d.module_name + ' - ' + d.description,
                       '=' * 40,
                       d.docstring() or "- No docstring -"]).strip()
            for d in script.goto_definitions()]

    return ('\n' + '-' * 40 + '\n').join(docs)

@script_method
def reference(script):
    """Find name reference places."""
    definitions = all_definitions(script)
    return [details(defn)
            for defn in script.usages()
            if defn not in definitions]




def all_definitions(script):
    """List definitions with assignments.

    Filter same definitions in favor of more explicit definition.
    """
    defns = script.goto_assignments() + script.goto_definitions()
    return [defn for defn in defns
            if defn.module_path and defn.type != 'import']


def details(definition):
    """Make hash with definition details."""
    return {
        'path': definition.module_path,
        'line': definition.line,
        'column': definition.column,
        'description': definition.description
    }


def first_line(text):
    """Return text first line."""
    return text.split('\n', 1)[0]
