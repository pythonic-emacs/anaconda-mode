from test import TestCase, skip
from anaconda_mode import anaconda
from test import editor


class CandidatesTest(TestCase):

    def test_autocomplete(self):
        """Jedi must complete correct sources."""

        request = editor(
            'test/jedi_candidates/fixtures/simple.py', 2, 11,
            'candidates'
        )

        response = ['date', 'datetime', 'datetime_CAPI']

        self.assertEqual(response, anaconda.process(**request))
