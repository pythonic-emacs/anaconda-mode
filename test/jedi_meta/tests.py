from test import TestCase
from anaconda_mode import anaconda
from test import editor


class MetaTest(TestCase):

    def test_meta_search(self):
        """Jedi must find all short documentations."""

        request = editor('test/jedi_meta/fixtures/simple.py', 1, 4, 'meta')

        response = 'Document for function f.'

        self.assertEqual(response, anaconda.process(**request))

    def test_empty_meta(self):
        """Ignore docless functions."""

        request = editor(
            'test/jedi_meta/fixtures/docless.py', 4, 2, 'meta',
            company_prefix='tt',
            company_arg='ttt'
        )

        self.assertIsNone(anaconda.process(**request))
