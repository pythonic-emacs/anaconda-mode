from test import TestCase, editor


class DocTest(TestCase):

    def test_doc_search(self):
        """Jedi must find all assignments documentation."""

        response = editor('test/jedi_doc/fixtures/simple.py', 1, 4, 'doc')

        expected = {
            'Document for function f.':
            '''f(a, b = 1)

Document for function f.''',
        }

        self.assertEqual(response, expected)

    def test_empty_doc(self):
        """Ignore docless functions."""

        response = editor('test/jedi_doc/fixtures/docless.py', 4, 3, 'doc')

        self.assertIsNone(response)
