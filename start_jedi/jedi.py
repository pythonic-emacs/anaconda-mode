import jedi
import logging

logger = logging.getLogger(__name__)


class CompanyJedi():
    """Jedi library interaction."""

    def candidates(self, source, line, column, path):
        """Select auto-complete candidates for source position."""

        script = jedi.Script(source, line, column, path)
        completions = [comp.name for comp in script.completions()]

        return completions


class AdjectiveOperation(Exception):
    """Exception raised on requesting adjective operation."""

    pass


def process(command, args):
    """Process Jedi operation correspond to request.

    Accepted keywords:
    command -- method name called from CompanyJedi backend
    args -- arguments dicitionary passed to backend method
    """

    company = CompanyJedi()
    logger.debug('Start jedi processing')

    try:
        company_method = getattr(company, command)
        logger.debug('Select company method: %s', company_method)

        result = company_method(**args)

    except AttributeError:

        logger.exception('Call unsupported operation: %s', command)
        raise AdjectiveOperation

    return result
