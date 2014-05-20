from test.helpers import result_from_fixture


def test_eldoc():
    """Should return signature string."""

    response = result_from_fixture('eldoc/simple', 8, 5, 'eldoc')

    expected = ("dump(obj, fp, skipkeys = False, ensure_ascii = True, "
                "check_circular = True, allow_nan = True, cls = None, "
                "indent = None, separators = None, default = None, "
                "sort_keys = False, **kw)")

    assert response == expected


def test_empty_eldoc():
    """Don't answer eldoc on unknown functions."""
    response = result_from_fixture('eldoc/broken', 4, 6, 'eldoc')
    assert response == ''
