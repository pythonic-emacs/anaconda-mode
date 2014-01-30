from test import TestCase
from start_jedi import httpd


post = httpd.JediHandler.do_POST


class HandlerMock():

    def __init__(self, headers={}, body=''):
        """Create fake handler object with request attribute."""

        self.headers = Headers(headers)
        self.rfile = FileMock(body)
        self.wfile = FileMock()

    def send_response(self, code):
        """Save response code."""

        self.response_code = code

    def end_headers(self):
        """Begin body section."""

        pass

    def send_error(self, code):
        """Save error code as response."""

        self.response_code = code


class FileMock():

    def __init__(self, content=''):
        """Create file fake."""

        self.content = content

    def read(self, n):
        """Read n characters from content attribute."""

        return (self.content[:n]).encode()

    def write(self, message):
        """Write new file content."""

        self.content += message.decode()


class Headers(dict):
    """Fail tolerant dictionary."""

    def __init__(self, dictionary):
        """Save internal dictionary."""

        self.dictionary = dictionary

    def __getitem__(self, key):
        """Return item or None."""

        if key in self.dictionary:
            result = self.dictionary[key]
        else:
            result = None

        return result


class TestPost(TestCase):

    def test_correct_post_request(self):
        """Need status 200 on correct post request with its body."""

        json = ('{'
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

        mock_handler = HandlerMock(
            headers={'content-length': len(json)},
            body=json)
        post(mock_handler)
        self.assertEqual(200, mock_handler.response_code)

    def test_handle_jedi_exceptions(self):
        """Need status 400 on jedi exceptions."""

        mock_handler = HandlerMock(
            headers={'content-length': '47'},
            body=('{'
                  ' "command": "unsupported_command",'
                  ' "attributes": {}'
                  '}'))
        post(mock_handler)
        self.assertEqual(400, mock_handler.response_code)

    def test_missing_content_length(self):
        """Need send status 400 on missing body."""

        mock_handler = HandlerMock(
            body='{"aaa": 1, "bbb": 2}')
        post(mock_handler)
        self.assertEqual(400, mock_handler.response_code)

    def test_broken_content(self):
        """Need send status 400 on invalid Json body."""

        mock_handler = HandlerMock(
            headers={'content-length': '14'},
            body='{"aaa": 1, "bb')
        post(mock_handler)
        self.assertEqual(400, mock_handler.response_code)

    def test_incomplete_content(self, ):
        """Need send 400 on valid json without necessary keys."""

        mock_handler = HandlerMock(
            headers={'content-length': '20'},
            body='{"aaa": 1, "bbb": 2}')

        post(mock_handler)
        self.assertEqual(400, mock_handler.response_code)
