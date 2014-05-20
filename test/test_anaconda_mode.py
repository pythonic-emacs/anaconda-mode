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
