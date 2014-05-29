from anaconda_common import script_method


@script_method
def complete(script):
    """Select auto-complete candidates for source position."""
    return [{'name': comp.name,
             'doc': comp.doc or None,
             'short_doc': first_line(comp.raw_doc) or None}
            for comp in script.completions()]


@script_method
def location(script):
    """Find names assignment place."""
    return [details(defn) for defn in all_definitions(script)]


@script_method
def reference(script):
    """Find name reference places."""
    definitions = all_definitions(script)
    return [details(defn)
            for defn in script.usages()
            if defn not in definitions]


@script_method
def doc(script):
    """Documentations list for all definitions at point."""
    return [{'short_doc': first_line(defn.raw_doc),
             'doc': defn.doc}
            for defn in script.goto_definitions()
            if defn.raw_doc]


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
