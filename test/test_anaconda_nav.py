import os
from test.helpers import send

import anaconda_nav  # noqa


def test_usages():
    rv = send('''\
              import json
              json.dumps_|_
              ''', 'usages', 'test.py')

    assert set(['test', 'json']) <= set(r['module'] for r in rv)


def test_goto_definitions():
    path = os.path.abspath('test.py')
    rv = send('''\
              def fn(a, b):
                  pass
              fn_|_(1, 2)
              ''', 'goto_definitions', path)

    assert rv == [{
        'line': 1,
        'column': 0,
        'name': 'fn',
        'description': 'def fn(a, b):',
        'module': 'test',
        'type': 'function',
        'path': path
    }]


def test_unknown_definition():
    rv = send('''\
              raise_|_
              ''', 'goto_definitions')

    assert rv == []


def test_goto_assignments():
    rv = send('''\
              if a:      x = 1
              else if b: x = 2
              else if c: x = 3
              else:      x = 4
              x_|_
              ''', 'goto_definitions')

    assert sorted(r['line'] for r in rv) == [1, 2, 3, 4]
