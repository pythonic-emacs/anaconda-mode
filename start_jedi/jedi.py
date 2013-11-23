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


class AdjectiveOperation(Exception):
    """Exception raised on requesting adjective operation."""

    pass


class MissingSource(Exception):
    """Exception raised on incomplete source file parameters."""

    pass


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

        logger.exception('Call unsupported operation: %s', command)
        raise AdjectiveOperation

    except TypeError:

        logger.exception('Missing parameters for Jedi object: %s', attributes)
        raise MissingSource

    return result
