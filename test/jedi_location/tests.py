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
            },
            ROOT + 'test/jedi_location/fixtures/simple.py:6 - inception = my_list[2]': {
                'module_path': ROOT + 'test/jedi_location/fixtures/simple.py',
                'line': 6,
                'column': 0,
                'description': 'inception = my_list[2]',
            }
        }

        self.assertEqual(response, jedi.process(**request))

    def test_non_python_definition_filter(self):
        """Jedi must filter non python sources."""

        request = editor(
            'test/jedi_location/fixtures/non_python.py', 2, 13,
            'location'
        )

        result = jedi.process(**request)

        self.assertEqual(1, len(result))

        for k, v in result.items():
            self.assertTrue(v['module_path'].endswith('.py'))

    def test_definition_with_builtins(self):
        """Jedi must keep builtin definitions.

        But also jedi must properly return python module location
        for those definitions. Not a builtin keyword.
        """

        request = editor(
            'test/jedi_location/fixtures/builtins.py', 6, 1,
            'location'
        )

        result = jedi.process(**request)

        self.assertEqual(2, len(result))

        for k, v in result.items():
            self.assertTrue(v['module_path'].endswith('.py'))

    def test_empty_definition(self):
        """Strip unknown definitions."""

        request = editor(
            'test/jedi_location/fixtures/not_defined.py', 1, 13,
            'location'
        )

        self.assertIsNone(jedi.process(**request))

    def test_jedi_goto_filter_same_definitions(self):
        """Merge definitions with assignments properly.

        When merge definitions and assignments together
        jedi must filter BaseDefinitions without module_name
        and definitions with the same line numbers in favor of
        more consistent location.
        """

        request = editor(
            'test/jedi_location/fixtures/builtins.py', 6, 1,
            '_goto'
        )

        result = jedi.process(**request)

        result_len = 0
        for base_def in result:
            self.assertTrue(base_def.module_path.endswith('.py'))
            self.assertTrue(type(base_def.line) is int)
            result_len += 1

        self.assertEqual(2, result_len)
