from test import TestCase
from test import editor


class CandidatesTest(TestCase):

    def test_autocomplete(self):
        """Jedi must complete correct sources."""

        response = editor(
            'test/jedi_candidates/fixtures/simple.py', 14, 4, 'complete')

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

        self.assertEqual(response, expected)
