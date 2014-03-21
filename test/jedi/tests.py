from test import TestCase
from anaconda_mode import anaconda
from test import editor


class RegressionTest(TestCase):

    def test_unsupported_command(self):
        """Ignore adjective commands."""

        request = editor('test/jedi/fixtures/simple.py', 2, 11, 'Wrong...')

        self.assertIsNone(anaconda.process(**request))

    def test_missing_attributes(self):
        """Ignore incomplete code attributes."""

        request = {
            'command': 'candidates',
            'attributes': {}
        }

        self.assertIsNone(anaconda.process(**request))
