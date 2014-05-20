from test.helpers import result_from_fixture


def test_autocomplete():
    """Jedi must complete correct sources."""

    response = result_from_fixture('candidates/simple', 14, 4, 'complete')

    expected = [
        {
            "name": "test1",
            "short_doc": "First test function.",
            "doc": """test1(a, b)

First test function.""",
        },
        {
            "name": "test2",
            "short_doc": "Second test function.",
            "doc": """test2(c)

Second test function.

Accept one argument only."""
        },
    ]

    assert response == expected
