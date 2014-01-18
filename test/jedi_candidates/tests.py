from unittest import TestCase
from start_jedi import jedi
from test import editor


class CandidatesTest(TestCase):

    def test_autocomplete(self):
        """Jedi must complete correct sources."""

        request = editor(
            'test/jedi_candidates/fixtures/simple.py', 2, 11,
            'candidates'
        )

        response = ['date', 'datetime', 'datetime_CAPI']

        self.assertEqual(response, jedi.process(**request))
