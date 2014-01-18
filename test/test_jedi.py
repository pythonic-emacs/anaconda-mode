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
