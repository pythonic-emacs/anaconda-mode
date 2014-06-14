import os
from test.helpers import send

import anaconda_company  # noqa


def test_completion_response():
    path = os.path.abspath('test.py')
    rv = send('''\
              def test1(a, b):
                  """First test function."""
                  pass


              def test2(c):
                  """Second test function."""
                  pass

              test_|_
              ''', 'complete', path)

    assert rv == [{
        "name": "test1",
        "doc": 'test1(a, b)\n\nFirst test function.',
        'short_doc': 'test1(a, b)',
        'annotation': 'function',
        'path': path,
        'line': 1
    }, {
        "name": "test2",
        "doc": 'test2(c)\n\nSecond test function.',
        'short_doc': 'test2(c)',
        'annotation': 'function',
        'path': path,
        'line': 6
    }]
