from anaconda_common import script_method


@script_method
def eldoc(script):
    """Return eldoc format documentation string or ''."""
    signatures = script.call_signatures()

    if len(signatures) == 1:
        sgn = signatures[0]
        return {
            'name': sgn.name,
            'index': sgn.index,
            'params': [p.description for p in sgn.params]
        }

    return {}
