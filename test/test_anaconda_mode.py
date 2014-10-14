from __future__ import absolute_import
from .test_helper import process, make_request, send
from textwrap import dedent
import anaconda_mode
import io
import mock
import os
import socket

# HTTP server.


def status_of(method, params):
    return process(make_request(method, params))[0]


def status_of_req(request):
    return process(request)[0]


def test_correct_post_request():
    """Need status 200 on correct post request with its body."""
    params = dict(source='imp', line=1, column=3, path='')
    assert status_of("complete", params) == 200


def test_handle_jedi_exceptions():
    """Need status 500 on jedi failure."""
    assert status_of("unsupported_method", {}) == 500


def test_broken_content():
    """Need send status 500 on invalid Json body."""
    assert status_of_req('{"aaa": 1, "bb') == 500


def test_incomplete_content():
    """Need send 500 on valid request without necessary keys."""
    assert status_of_req('{"aaa": 1, "bbb": 2}') == 500


def test_server_start(capfd):
    with mock.patch('anaconda_mode.HTTPServer') as cls:
        server = mock.Mock()
        cls.side_effect = [OSError('Address already in use'),
                           socket.error('Address already in use'),
                           server]

        anaconda_mode.main()
        resout, reserr = capfd.readouterr()

        assert server.serve_forever.called
        assert resout == 'anaconda_mode port 24972\n'


def test_http_handler():

    class TestHandler(anaconda_mode.HTTPRequestHandler):
        def __init__(self):
            self.request_version = self.protocol_version
            self.headers = {'content-length': '7'}
            self.rfile = io.BytesIO(b'content')
            self.wfile = io.BytesIO()
            self.process_request = mock.Mock(return_value=(200, u'response'))

        def version_string(self):
            return 'Weight gain 4000'

        def date_time_string(self):
            return 'Ice age'

    handler = TestHandler()
    handler.do_POST()
    response = handler.wfile.getvalue().decode('utf-8')

    handler.process_request.assert_called_with(u'content')
    assert response == '\r\n'.join(['HTTP/1.1 200 OK',
                                    'Server: Weight gain 4000',
                                    'Date: Ice age',
                                    'Content-Length: 8', '', 'response'])

# Completion.


def test_completion_response():
    path = os.path.abspath('test.py')
    rv = send('''\
              def test1(a, b):
                  """First test function."""
                  pass


              def test2(c):
                  """Second test function."""
                  pass

              test_|_
              ''', 'complete', path)

    assert rv == [{
        "name": "test1",
        "doc": 'test1(a, b)\n\nFirst test function.',
        'info': 'First test function.',
        'type': 'function',
        'path': path,
        'line': 1
    }, {
        "name": "test2",
        "doc": 'test2(c)\n\nSecond test function.',
        'info': 'Second test function.',
        'type': 'function',
        'path': path,
        'line': 6
    }]

# Definitions.


def test_goto_definitions():
    path = os.path.abspath('test.py')
    rv = send('''\
              def fn(a, b):
                  pass
              fn_|_(1, 2)
              ''', 'goto_definitions', path)

    assert rv == [{
        'line': 1,
        'column': 0,
        'name': 'fn',
        'description': 'def fn(a, b):',
        'module': 'test',
        'type': 'function',
        'path': path
    }]


def test_unknown_definition():
    rv = send('''\
              raise_|_
              ''', 'goto_definitions')

    assert rv == []


def test_goto_assignments():
    rv = send('''\
              if a:      x = 1
              else if b: x = 2
              else if c: x = 3
              else:      x = 4
              x_|_
              ''', 'goto_assignments')

    assert sorted(r['line'] for r in rv) == [1, 2, 3, 4]

# Documentation.


def test_doc_fn_with_docstring():
    rv = send('''
              def f_|_(a, b=1):
                  """Some docstring."""
                  pass
              ''', 'doc', 'some_module.py')

    assert rv == dedent('''\
        some_module - def f
        ========================================
        f(a, b = 1)

        Some docstring.''')


def test_doc_fn_without_docstring():
    rv = send('''
              def f_|_(a, b=1):
                  pass
              ''', 'doc', 'other_module.py')

    assert rv == dedent('''\
        other_module - def f
        ========================================
        f(a, b = 1)''')

# Usages.


def test_usages():
    rv = send('''\
              import json
              json.dumps_|_
              ''', 'usages', 'test.py')

    assert set(['test', 'json']) <= set(r['module'] for r in rv)

# ElDoc.


def test_eldoc_signature():
    rv = send('''
              def f(obj, fp, skipkeys=False, ensure_ascii=True,
                    check_circular=True, allow_nan=True, cls=None,
                    indent=None, separators=None, default=None,
                    sort_keys=False, **kw):
                  pass

              f(123, _|_
              ''', 'eldoc')

    assert rv == {
        'name': 'f',
        'index': 1,
        'params': ['obj', 'fp', 'skipkeys = False', 'ensure_ascii = True',
                   'check_circular = True', 'allow_nan = True', 'cls = None',
                   'indent = None', 'separators = None', 'default = None',
                   'sort_keys = False', '**kw']
    }


def test_eldoc_unknown_fn():
    rv = send('''
              unknown_fn(_|_
              ''', 'eldoc')

    assert rv == {}
