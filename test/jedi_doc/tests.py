from test import TestCase
from start_jedi import company
from test import editor


class DocTest(TestCase):

    def test_doc_search(self):
        """Jedi must find all assignments documentation."""

        request = editor('test/jedi_doc/fixtures/simple.py', 1, 4, 'doc')

        response = {'Document for function f.': 'Document for function f.'}

        self.assertEqual(response, company.process(**request))

    def test_empty_doc(self):
        """Ignore docless functions."""

        request = editor('test/jedi_doc/fixtures/docless.py', 4, 3, 'doc')

        self.assertIsNone(company.process(**request))
