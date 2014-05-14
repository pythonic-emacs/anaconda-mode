from jsonrpc import dispatcher
import jedi


def jedi_script(source, line, column, path):
    """Make jedi instance."""

    return jedi.Script(source, line, column, path)


@dispatcher.add_method
def complete(*args):
    """Select auto-complete candidates for source position."""

    script = jedi_script(*args)

    completions = []

    for comp in script.completions():

        completions.append({
            'name': comp.name,
            'doc': comp.doc or None,
            'short_doc': first_line(comp.raw_doc) or None,
        })

    return completions


def goto(*args):
    """List definitions with assignments.

    Filter same definitions in favor of more explicit definition.
    """

    script = jedi_script(*args)
    assignments = script.goto_assignments()
    definitions = script.goto_definitions()

    for name in assignments + definitions:

        if name.module_path.endswith('.py') and name.type != 'import':

            yield name


@dispatcher.add_method
def location(*args):
    """Find names assignment place."""

    return dict((summary(name), details(name)) for name in goto(*args))


@dispatcher.add_method
def reference(*args):
    """Find name reference places."""

    locations = goto(*args)

    script = jedi_script(*args)
    usages = script.usages()

    references = [name for name in usages if name not in locations]

    return dict((summary(name), details(name)) for name in references)


@dispatcher.add_method
def doc(*args):
    """Documentations list for all definitions at point."""

    script = jedi_script(*args)

    docs = {}

    for definition in script.goto_definitions():

        if definition.raw_doc:

            docs[first_line(definition.raw_doc)] = definition.doc

    return docs


def details(definition):
    """Make hash with definition details."""

    return {
        'module_path': definition.module_path,
        'line': definition.line,
        'column': definition.column,
        'description': definition.description
    }


def summary(definition):
    """Summarize definition into one string."""

    return '{0}:{1} - {2}'.format(
        definition.module_path,
        definition.line,
        definition.description
    )


def first_line(text):
    """Return text first line."""

    return text.split('\n', 1)[0]
