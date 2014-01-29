from unittest import TestCase, skip
from start_jedi import company
from test import editor


class CandidatesTest(TestCase):

    def test_autocomplete(self):
        """Jedi must complete correct sources."""

        request = editor(
            'test/jedi_candidates/fixtures/simple.py', 2, 11,
            'candidates'
        )

        response = ['date', 'datetime', 'datetime_CAPI']

        self.assertEqual(response, company.process(**request))

    @skip('Not implemented yet.')
    def test_autocomplete_from_virtualenv(self):
        """Jedi must search for candidates in virtual environment."""

        request = editor(
            'test/jedi_candidates/fixtures/import_from_venv.py', 1, 20,
            'candidates'
        )

        response = ['ppp_function']

        self.assertEqual(response, company.process(**request))
