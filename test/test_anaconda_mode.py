import io
import mock
import anaconda_mode
from click.testing import CliRunner
from test.helpers import process, make_request


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


@mock.patch('anaconda_mode.HTTPServer')
def test_server_start(server_cls):
    server = server_cls.return_value
    runner = CliRunner()
    result = runner.invoke(anaconda_mode.main, ['--bind', 'localhost',
                                                '--port', '666',
                                                '--debug'])
    assert server.serve_forever.called
    assert result.output == 'anaconda_mode server started\n'


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

        def log_request(*args):
            pass

    handler = TestHandler()
    handler.do_POST()
    response = handler.wfile.getvalue().decode('utf-8')

    handler.process_request.assert_called_with(u'content')
    assert response == '\r\n'.join(['HTTP/1.1 200 OK',
                                    'Server: Weight gain 4000',
                                    'Date: Ice age',
                                    'Content-Length: 8', '', 'response'])
