import os
import logging

BASE_DIR = os.path.dirname(os.path.dirname(__file__))
ROOT = os.path.realpath(BASE_DIR) + os.path.sep

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger('start_jedi')

if not os.path.isdir(ROOT + 'log'):
    os.mkdir(ROOT + 'log')

handler = logging.FileHandler(ROOT + 'log/test.log')
handler.setLevel(logging.DEBUG)

formatter = logging.Formatter(logging.BASIC_FORMAT)
handler.setFormatter(formatter)

logger.addHandler(handler)
logger.propagate = False  # Not echo to console

# Helper methods.


def editor(path, line, column, command,
           company_prefix='', company_arg=''):
    """Emulate requests from user.

    Return callable api for jedi."""

    with open(path) as f:
        lines = f.readlines()

    source = ''.join(lines)

    point = len(''.join(lines[:line-1]) + lines[line-1][:column])

    return {
        'command': command,
        'attributes': {
            'source': source,
            'line': line,
            'column': column,
            'point': point,
            'path': path,
            'company_prefix': company_prefix,
            'company_arg': company_arg,
        }
    }
