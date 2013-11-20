from unittest import TestCase
from start_jedi import httpd


do_POST = httpd.JediHandler.do_POST


class HandlerMock():

    def __init__(self, headers={}, body=''):
        """Create fake handler object with request attribute."""

        self.headers = headers
        self.rfile = FileMock(body)
        self.wfile = FileMock()

    def send_response(self, code):
        """Save response code."""

        self.response_code = code

    def end_headers(self):
        """Begin body section."""

        pass


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


class TestDoPUSH(TestCase):

    def test_correct_post_request(self):
        """Test correct post request with its body."""
        mock_handler = HandlerMock(
            headers={'content-length': '20'},
            body='{"aaa": 1, "bbb": 2}')
        self.assertIsNone(do_POST(mock_handler))
        self.assertEqual(200, mock_handler.response_code)
