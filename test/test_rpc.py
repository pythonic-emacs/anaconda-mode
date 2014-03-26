from test import TestCase, mock
from anaconda_mode import rpc

import json


class TestPost(TestCase):

    def setUp(self):
        """Create mock objects for HTTP Handler."""

        self._post = rpc.Handler.do_POST

        patcher = mock.patch('anaconda_mode.rpc.Handler', autospec=True)
        self.addCleanup(patcher.stop)
        self._handler = patcher.start()
        self._handler.rfile = mock.Mock()
        self._handler.wfile = mock.Mock()

    def test_correct_post_request(self):
        """Need status 200 on correct post request with its body."""

        request = json.dumps({
            "command": "complete",
            "attributes": {
                "source": "imp",
                "line": 1,
                "column": 3,
                "path": "",
            }
        })

        self._handler.headers = {'content-length': len(request)}
        self._handler.rfile.read.return_value = request.encode()

        self._post(self._handler)
        self._handler.send_response.assert_called_with(200)

    def test_handle_jedi_exceptions(self):
        """Need status 400 on jedi failure."""

        request = json.dumps({
            "command": "unsupported_command",
            "attributes": {}
        })

        self._handler.headers = {'content-length': '47'}
        self._handler.rfile.return_value = request.encode()

        self._post(self._handler)
        self._handler.send_error.assert_called_with(400)

    def test_missing_content_length(self):
        """Need send status 400 on missing body."""

        request = '{"aaa": 1, "bbb": 2}'

        self._handler.headers = None
        self._handler.rfile.read.return_value = request.encode()

        self._post(self._handler)
        self._handler.send_error.assert_called_with(400)

    def test_broken_content(self):
        """Need send status 400 on invalid Json body."""

        request = '{"aaa": 1, "bb'

        self._handler.headers = {'content-length': '14'}
        self._handler.rfile.read.return_value = request.encode()

        self._post(self._handler)
        self._handler.send_error.assert_called_with(400)

    def test_incomplete_content(self):
        """Need send 400 on valid request without necessary keys."""

        request = '{"aaa": 1, "bbb": 2}'

        self._handler.headers = {'content-length': '20'}
        self._handler.rfile.read.return_value = request.encode()

        self._post(self._handler)
        self._handler.send_error.assert_called_with(400)
