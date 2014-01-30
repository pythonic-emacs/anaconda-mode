import jedi
import logging

logger = logging.getLogger(__name__)


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


class CompanyJedi():
    """Jedi library interaction."""

    def __init__(self, source, line, column, point, path,
                 company_prefix, company_arg):
        """Initialize Jedi with source."""

        suffix = company_arg[len(company_prefix):]
        context = source[:point] + suffix + source[point:]

        logger.debug('Imply jedi context: \n%s', context)

        self.script = jedi.Script(context, line, column, path)

    def candidates(self):
        """Select auto-complete candidates for source position."""

        completions = [comp.name for comp in self.script.completions()]

        return completions

    def _goto(self):
        """List definitions with assignments.

        Filter same definitions in favor of more explicit definition.
        """

        assignments = self.script.goto_assignments()
        definitions = self.script.goto_definitions()

        for name in assignments + definitions:
            if name.module_path.endswith('.py'):
                yield name

    def location(self):
        """Find names assignment place."""

        # Use list comprehension rather dict comprehension due to Python2.6
        return dict((summary(name), details(name)) for name in self._goto())

    def reference(self):
        """Find name reference places."""

        usages = self.script.usages()

        locations = self._goto()

        references = [name for name in usages if name not in locations]

        return dict((summary(name), details(name)) for name in references)

    def doc(self):
        """Documentations list for all definitions at point."""

        raw_docs = [name.raw_doc for name in self.script.goto_definitions()]

        return dict((first_line(doc), doc) for doc in raw_docs if doc)

    def meta(self):
        """Return single line documentation string or None."""

        documents = [name.raw_doc for name in self.script.goto_definitions()]

        if len(documents) == 1:

            return first_line(documents[0])

    def eldoc(self):
        """Return eldoc format documentation string or None."""

        signatures = self.script.call_signatures()

        logger.debug('Call signatures: %s', signatures)

        if len(signatures) == 1:

            call_name = signatures[0].call_name

            params = signatures[0].params
            call_params = [param.get_code(new_line=False) for param in params]

            return '{0}({1})'.format(call_name, ', '.join(call_params))


def process(attributes, command):
    """Process Jedi operation correspond to request.

    Accepted keywords:
    attributes -- arguments dicitionary passed to Jedi script constructor
    command -- method name called from CompanyJedi backend
    """

    try:

        company = CompanyJedi(**attributes)
        logger.debug('Start jedi processing')

        company_method = getattr(company, command)
        logger.debug('Select company method: %s', company_method)

        result = company_method()

    except AttributeError:

        message = 'Call unsupported operation: {0}'.format(command)
        logger.exception(message)
        result = None

    except TypeError:

        message = 'Missing parameters for Jedi object: {0}'.format(attributes)
        logger.exception(message)
        result = None

    # Protection from empty strings, lists, etc.
    # Must return None in all this cases.
    if result:
        return result
