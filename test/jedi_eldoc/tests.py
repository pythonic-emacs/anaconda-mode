from test import TestCase, editor


class EldocTest(TestCase):

    def test_eldoc(self):
        """Should return signature string."""

        response = editor('test/jedi_eldoc/fixtures/simple.py', 8, 5, 'eldoc')

        expected = ("dump(obj, fp, skipkeys = False, ensure_ascii = True, "
                    "check_circular = True, allow_nan = True, cls = None, "
                    "indent = None, separators = None, default = None, "
                    "sort_keys = False, **kw)")

        self.assertEqual(response, expected)

    def test_empty_eldoc(self):
        """Don't answer eldoc on unknown functions."""

        response = editor('test/jedi_eldoc/fixtures/broken.py', 4, 6, 'eldoc')

        self.assertIsNone(response)
