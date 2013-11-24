import jedi
import logging

logger = logging.getLogger(__name__)


class CompanyJedi():
    """Jedi library interaction."""

    def __init__(self, source, line, column, path):
        """Initialize Jedi with source."""

        self.script = jedi.Script(source, line, column, path)

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

        return [details(name) for name in self.script.goto_assignments()]


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
