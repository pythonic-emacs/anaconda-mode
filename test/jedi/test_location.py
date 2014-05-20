from test.helpers import result_from_fixture, parse_fixture
from test import ROOT


def test_definition_search():
    """Jedi must find correct definitions."""

    response = result_from_fixture('location/simple', 8, 1, 'location')

    expected = {
        ROOT + ('test/jedi/fixtures/location/simple.py:1 - def my_func'): {
            'module_path': ROOT + 'test/jedi/fixtures/location/simple.py',
            'line': 1,
            'column': 0,
            'description': 'def my_func',
        },
        ROOT + ('test/jedi/fixtures/location/simple.py:6'
                ' - inception = my_list[2]'): {
            'module_path': ROOT + 'test/jedi/fixtures/location/simple.py',
            'line': 6,
            'column': 0,
            'description': 'inception = my_list[2]',
        }
    }

    assert response == expected


def test_non_python_definition_filter():
    """Jedi must filter non python sources."""

    result = result_from_fixture('location/non_python', 2, 13, 'location')

    # Don't check len(result) here.  Some python versions doesn't has
    # fixture module defined in python code.  So dictionary length may
    # be different (or even be a None if there is only C sources).
    if result:
        for k, v in result.items():
            assert v['module_path'].endswith('.py')


def test_definition_with_builtins():
    """Jedi must keep builtin definitions.

    But also jedi must properly return python module location
    for those definitions. Not a builtin keyword.
    """

    result = result_from_fixture('location/builtins', 6, 1, 'location')
    assert len(result) == 2

    for _, v in result.items():
        assert v['module_path'].endswith('.py')


def test_empty_definition():
    """Strip unknown definitions."""

    response = result_from_fixture('location/not_defined', 1, 13, 'location')

    assert response == {}


def test_jedi_goto_filter_same_definitions():
    import anaconda_jedi
    import jedi
    """Merge definitions with assignments properly.

    When merge definitions and assignments together
    jedi must filter BaseDefinitions without module_name
    and definitions with the same line numbers in favor of
    more consistent location.
    """

    params = parse_fixture('location/builtins', 6, 1)
    script = jedi.Script(**params)
    result = anaconda_jedi.all_definitions(script)

    for base_def in result:
        assert base_def.module_path.endswith('.py')
        assert type(base_def.line) is int

    assert len(result) == 2


def test_jedi_goto_import_statements_filter():
    """Jedi must ignore imports lines.

    Don't show lines like
    import something
    from module import something
    """

    result = result_from_fixture('location/ignore_imports/ignore', 3, 1, 'location')

    expected = {
        ROOT + ('test/jedi/fixtures/location/ignore_imports/one.py:1'
                ' - def other'): {
            'column': 0,
            'line': 1,
            'description': 'def other',
            'module_path': ROOT + ('test/jedi/fixtures/location/ignore_imports/one.py'),
        }
    }

    assert result == expected
