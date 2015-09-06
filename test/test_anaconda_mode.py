from __future__ import (
    absolute_import, unicode_literals, division, print_function)
from os.path import abspath

import anaconda_mode


# Completion.


def test_completion_response():
    """Check completions works."""

    path = abspath('test.py')

    completions = anaconda_mode.complete('''
def test1(a, b):
    """First test function."""
    pass

def test2(c):
    """Second test function."""
    pass

test
''', 10, 4, path)

    assert 'test1' == completions[0]['name']
    assert 'test2' == completions[1]['name']


# Definitions.


def test_goto_definitions():
    """Check goto definitions works."""

    path = abspath('test.py')

    definitions = anaconda_mode.goto_definitions('''
def fn(a, b):
    pass
fn(1, 2)
''', 4, 2, path)

    assert 2 == definitions[0]['line']
    assert 4 == definitions[0]['column']
    assert path == definitions[0]['module-path']


def test_unknown_definition():
    """Check we process not found error."""

    definitions = anaconda_mode.goto_definitions('''
raise
''', 2, 5, None)

    assert not definitions


def test_goto_assignments():
    """Check goto assignments works."""

    assignments = anaconda_mode.goto_assignments('''
a = True
if a:
    x = 1
x
''', 5, 1, None)

    assert 4 == assignments[0]['line']


# Usages.


def test_usages():
    """Check usages search works."""

    usages = anaconda_mode.usages('''
import json
json.dumps
''', 3, 10, 'test.py')

    result = (usage['module-name'] for usage in usages)
    assert set(['test', 'json']) == set(result)


# ElDoc.


def test_eldoc():
    """Check eldoc on function with signature."""

    eldoc = anaconda_mode.eldoc('''
def f(obj, fp, skipkeys=False, ensure_ascii=True,
    check_circular=True, allow_nan=True, cls=None,
    indent=None, separators=None, default=None,
    sort_keys=False, **kw):
    pass

f(123
''', 8, 5, None)

    assert eldoc == {
        'name': 'f',
        'index': 0,
        'params': ['obj', 'fp', 'skipkeys=False', 'ensure_ascii=True',
                   'check_circular=True', 'allow_nan=True', 'cls=None',
                   'indent=None', 'separators=None', 'default=None',
                   'sort_keys=False', '**kw']
    }


def test_eldoc_unknown_function():
    """Check eldoc ignore unknown functions."""

    eldoc = anaconda_mode.eldoc('''
unknown_fn(
''', 2, 11, None)

    assert not eldoc
