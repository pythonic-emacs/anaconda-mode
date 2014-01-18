from unittest import TestCase
from start_jedi import jedi
from test import editor


class EldocTest(TestCase):

    def test_eldoc(self):
        """Should return signature string."""

        request = editor('test/jedi_eldoc/fixtures/simple.py', 2, 10, 'eldoc')

        response = ("dump(obj, fp, skipkeys = False, ensure_ascii = True, "
                    "check_circular = True, allow_nan = True, cls = None, "
                    "indent = None, separators = None, default = None, "
                    "sort_keys = False, **kw)")

        self.assertEqual(response, jedi.process(**request))

    def test_empty_eldoc(self):
        """Don't answer eldoc on unknown functions."""

        request = editor('test/jedi_eldoc/fixtures/broken.py', 4, 6, 'eldoc')

        self.assertIsNone(jedi.process(**request))
