from unittest import TestCase
from start_jedi import jedi
from test import editor, ROOT


class LocationTest(TestCase):

    def test_definition_search(self):
        """Jedi must find correct definitions."""

        request = editor(
            'test/jedi_location/fixtures/simple.py', 8, 1,
            'location'
        )

        response = {
            ROOT + 'test/jedi_location/fixtures/simple.py:1 - def my_func': {
                'module_path': ROOT + 'test/jedi_location/fixtures/simple.py',
                'line': 1,
                'column': 0,
                'description': 'def my_func',
            }
        }

        self.assertEqual(response, jedi.process(**request))

    def test_definition_filter(self):
        """Jedi must filter non python sources."""

        request = editor(
            'test/jedi_location/fixtures/non_python.py', 2, 13,
            'location'
        )

        self.assertEqual(1, len(jedi.process(**request)))

    def test_empty_definition(self):
        """Strip unknown definitions."""

        request = editor(
            'test/jedi_location/fixtures/not_defined.py', 1, 13,
            'location'
        )

        self.assertIsNone(jedi.process(**request))
