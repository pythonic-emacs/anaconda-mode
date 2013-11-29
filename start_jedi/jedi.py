import jedi
import logging
import re

logger = logging.getLogger(__name__)


def details(name):
    return {
        'module_path': name.module_path,
        'line': name.line,
        'column': name.column,
        'description': name.description
    }


def summary(name):
    """Format string from base definition."""

    return '{}:{} - {}'.format(name.module_path, name.line, name.description)


def is_py(name):
    pattern = re.compile('^.*\\.py$')
    return pattern.match(name.module_path)


def first_line(string):
    """Return first line from string."""

    return string.split('\n', 1)[0]


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

    def location(self):
        """Find names assignment place."""

        assignments = self.script.goto_assignments()

        locations = [name for name in assignments if is_py(name)]

        return {summary(name): details(name) for name in locations}

    def reference(self):
        """Find name reference places."""

        usages = [name for name in self.script.usages() if is_py(name)]

        assignments = self.script.goto_assignments()

        references = [name for name in usages if name not in assignments]

        return {summary(name): details(name) for name in references}

    def doc(self):
        """Documentations list for all assignments at point."""

        documents = [name.raw_doc for name in self.script.goto_assignments()]

        return {first_line(doc): doc for doc in documents}

    def meta(self):
        """Return single line documentation string or None."""

        documents = [name.raw_doc for name in self.script.goto_assignments()]

        if len(documents) == 1:
            answer = first_line(documents[0])
        else:
            answer = None

        return answer


def process(attributes, command):
    """Process Jedi operation correspond to request.

    Accepted keywords:
    attributes -- arguments dicitionary passed to Jedi script constructor
    command -- method name called from CompanyJedi backend
    """

    class Result():
        pass

    try:

        obj = Result()

        company = CompanyJedi(**attributes)
        logger.debug('Start jedi processing')

        company_method = getattr(company, command)
        logger.debug('Select company method: %s', company_method)

        obj.result = company_method()
        obj.error = None

    except AttributeError:

        message = 'Call unsupported operation: {}'.format(command)
        logger.exception(message)
        obj.result = None
        obj.error = message

    except TypeError:

        message = 'Missing parameters for Jedi object: {}'.format(attributes)
        logger.exception(message)
        obj.result = None
        obj.error = message

    return obj
