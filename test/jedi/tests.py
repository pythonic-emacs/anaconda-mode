from test import TestCase, anaconda, editor


class RegressionTest(TestCase):

    def test_unsupported_command(self):
        """Ignore adjective commands."""

        response = editor('test/jedi/fixtures/simple.py', 2, 11, 'Wrong...')

        self.assertIsNone(response)

    def test_missing_attributes(self):
        """Ignore incomplete code attributes."""

        request = {
            'command': 'candidates',
            'attributes': {}
        }

        self.assertIsNone(anaconda.process(**request))
