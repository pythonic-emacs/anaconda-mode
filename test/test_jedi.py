from unittest import TestCase
from start_jedi import jedi
import os


class TestJedi(TestCase):

    def test_unsupported_command(self):
        """Should raise exception on adjective command requests."""

        request = {
            'command': 'some_strange_command',
            'attributes': {
                'source': '''\nimport datetime\ndatetime.da''',
                'line': 3,
                'column': 14,
                'point': 28,
                'path': '',
                'company_prefix': '',
                'company_arg': ''
            }
        }

        self.assertIsNone(jedi.process(**request))

    def test_missing_attributes(self):
        """Should raise exception on incomplete source code."""

        request = {
            'command': 'candidates',
            'attributes': {}
        }

        self.assertIsNone(jedi.process(**request))

    def test_short_documentation_search(self):
        """Jedi must find all short documentations."""

        request = {
            'command': 'meta',
            'attributes': {
                'source': '''
def f(a, b=1):
    """Document for function f.

Here is long f function description."""
    pass''',
                'line': 2,
                'column': 4,
                'point': 5,
                'path': 'example.py',
                'company_prefix': '',
                'company_arg': ''
            }
        }

        response = 'Document for function f.'

        self.assertEqual(response, jedi.process(**request))

    def test_empty_meta(self):
        """Ignore docless functions."""

        request = {
            'command': 'meta',
            'attributes': {
                'source': '''def ttt(a, b, c):
    pass

ttt''',
                'line': 4,
                'column': 3,
                'point': 31,
                'path': 'simple.py',
                'company_prefix': '',
                'company_arg': ''
            }
        }

        self.assertIsNone(jedi.process(**request))
