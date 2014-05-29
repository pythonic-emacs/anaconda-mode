import os
from test.helpers import result_from_fixture
from test import ROOT


def test_references_search():
    """Jedi must find all references to object."""

    response = result_from_fixture('reference/simple', 1, 4, 'reference')
    expected = [
        {
            'path': os.path.join(
                ROOT, 'test/jedi/fixtures/reference/simple.py'),
            'line': 4,
            'column': 4,
            'description': 'a'
        },
        {
            'path': os.path.join(
                ROOT, 'test/jedi/fixtures/reference/simple.py'),
            'line': 5,
            'column': 4,
            'description': 'a'
        }
    ]

    assert response == expected


def test_empty_references():
    """Strip unknown references."""

    response = result_from_fixture('reference/useless', 1, 4, 'reference')
    assert response == []
