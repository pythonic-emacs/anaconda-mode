from test.helpers import send

from textwrap import dedent


def test_doc_fn_with_docstring():
    rv = send('''
              def f_|_(a, b=1):
                  """Some docstring."""
                  pass
              ''', 'doc', 'some_module.py')

    assert rv == dedent('''\
        some_module - def f
        ========================================
        f(a, b = 1)

        Some docstring.''')


def test_doc_fn_without_docstring():
    rv = send('''
              def f_|_(a, b=1):
                  pass
              ''', 'doc', 'other_module.py')

    assert rv == dedent('''\
        other_module - def f
        ========================================
        f(a, b = 1)''')
