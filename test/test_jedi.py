import test.helper

from unittest import TestCase
from start_jedi import jedi


class TestJedi(TestCase):

    def test_grop_unsupported_command(self, ):
        """Should raise exception on adjective command requests."""

        request = {
            'command': 'some_strange_command',
            'args': {}
        }

        self.assertRaises(jedi.AdjectiveOperation, jedi.process, **request)

    def test_autocomplete(self):
        """Jedi must complete correct sources."""

        request = {
            'command': 'candidates',
            'args': {
                'source': '''\nimport datetime\ndatetime.da''',
                'line': 3,
                'column': 14,
                'path': ''
            }
        }

        response = ['date', 'datetime', 'datetime_CAPI']

        self.assertEqual(response, jedi.process(**request))
