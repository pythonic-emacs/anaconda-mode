from test.helpers import result_from_fixture
from test import ROOT


def test_references_search():
    """Jedi must find all references to object."""

    response = result_from_fixture('reference/simple', 1, 4, 'reference')
    expected = {
        ROOT + 'test/jedi/fixtures/reference/simple.py:4 - a': {
            'module_path': ROOT + 'test/jedi/fixtures/reference/simple.py',
            'line': 4,
            'column': 4,
            'description': 'a'
        },
        ROOT + 'test/jedi/fixtures/reference/simple.py:5 - a': {
            'module_path': ROOT + 'test/jedi/fixtures/reference/simple.py',
            'line': 5,
            'column': 4,
            'description': 'a'
        }
    }

    assert response == expected

def test_empty_references():
    """Strip unknown references."""

    response = result_from_fixture('reference/useless', 1, 4, 'reference')
    assert response == {}
