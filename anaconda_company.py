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
