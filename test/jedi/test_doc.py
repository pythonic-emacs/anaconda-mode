from test.helpers import result_from_fixture


def test_doc_search():
    import sys
    print(sys.path)
    """Jedi must find all assignments documentation."""

    response = result_from_fixture('doc/simple', 1, 4, 'doc')

    expected = [{
        'short_doc': 'Document for function f.',
        'doc': '''f(a, b = 1)

Document for function f.''',
    }]

    assert response == expected


def test_empty_doc():
    """Ignore docless functions."""

    response = result_from_fixture('doc/docless', 4, 3, 'doc')

    assert response == []
