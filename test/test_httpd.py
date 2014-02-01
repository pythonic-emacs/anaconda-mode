from test import TestCase, mock
from start_jedi import httpd

post = httpd.JediHandler.do_POST


class TestPost(TestCase):

    @mock.patch('start_jedi.httpd.JediHandler', autospec=True)
    def test_correct_post_request(self, mock_handler):
        """Need status 200 on correct post request with its body."""

        request = ('{'
                   ' "command": "candidates",'
                   ' "attributes": {'
                   '     "source": "imp",'
                   '     "line": 1,'
                   '     "column": 3,'
                   '     "point": 2,'
                   '     "path": "",'
                   '     "company_prefix": "",'
                   '     "company_arg": ""'
                   ' }'
                   '}')

        mock_handler.headers = {'content-length': len(request)}
        mock_handler.rfile = mock_handler.wfile = mock.Mock()
        mock_handler.rfile.read.return_value = request

        post(mock_handler)
        mock_handler.send_response.assert_called_with(200)

    @mock.patch('start_jedi.httpd.JediHandler', autospec=True)
    def test_handle_jedi_exceptions(self, mock_handler):
        """Need status 400 on jedi failure."""

        request = ('{'
                   ' "command": "unsupported_command",'
                   ' "attributes": {}'
                   '}')

        mock_handler.headers = {'content-length': '47'}
        mock_handler.rfile = mock.Mock()
        mock_handler.rfile.return_value = request

        post(mock_handler)
        mock_handler.send_error.assert_called_with(400)

    @mock.patch('start_jedi.httpd.JediHandler', autospec=True)
    def test_missing_content_length(self, mock_handler):
        """Need send status 400 on missing body."""

        request = '{"aaa": 1, "bbb": 2}'

        mock_handler.headers = None
        mock_handler.rfile = mock.Mock()
        mock_handler.rfile.read.return_value = request

        post(mock_handler)
        mock_handler.send_error.assert_called_with(400)

    @mock.patch('start_jedi.httpd.JediHandler', autospec=True)
    def test_broken_content(self, mock_handler):
        """Need send status 400 on invalid Json body."""

        request = '{"aaa": 1, "bb'

        mock_handler.headers = {'content-length': '14'}
        mock_handler.rfile = mock.Mock()
        mock_handler.rfile.read.return_value = request

        post(mock_handler)
        mock_handler.send_error.assert_called_with(400)

    @mock.patch('start_jedi.httpd.JediHandler', autospec=True)
    def test_incomplete_content(self, mock_handler):
        """Need send 400 on valid request without necessary keys."""

        request = '{"aaa": 1, "bbb": 2}'

        mock_handler.headers = {'content-length': '20'}
        mock_handler.rfile = mock.Mock()
        mock_handler.rfile.read.return_value = request

        post(mock_handler)
        mock_handler.send_error.assert_called_with(400)
