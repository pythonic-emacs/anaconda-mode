import jedi
import logging
import re

logger = logging.getLogger(__name__)


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

    def location(self, ):
        """Find names assignment place."""

        def details(name):
            return {
                'module_path': name.module_path,
                'line': name.line,
                'column': name.column
            }

        def is_py(name):
            pattern = re.compile('^.*\\.py$')
            return pattern.match(name.module_path)

        assignments = self.script.goto_assignments

        return [details(name) for name in assignments() if is_py(name)]


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
