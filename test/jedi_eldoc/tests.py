from unittest import TestCase
from start_jedi import company
from test import editor


class EldocTest(TestCase):

    def test_eldoc(self):
        """Should return signature string."""

        request = editor('test/jedi_eldoc/fixtures/simple.py', 8, 5, 'eldoc')

        response = ("dump(obj, fp, skipkeys = False, ensure_ascii = True, "
                    "check_circular = True, allow_nan = True, cls = None, "
                    "indent = None, separators = None, default = None, "
                    "sort_keys = False, **kw)")

        self.assertEqual(response, company.process(**request))

    def test_empty_eldoc(self):
        """Don't answer eldoc on unknown functions."""

        request = editor('test/jedi_eldoc/fixtures/broken.py', 4, 6, 'eldoc')

        self.assertIsNone(company.process(**request))
