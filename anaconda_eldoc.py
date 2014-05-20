from anaconda_common import script_method


@script_method
def eldoc(script):
    """Return eldoc format documentation string or ''."""
    signatures = script.call_signatures()

    if len(signatures) == 1:
        sgn = signatures[0]
        params = ', '.join(p.description for p in sgn.params)
        return '{0}({1})'.format(sgn.name, params)

    return ''
