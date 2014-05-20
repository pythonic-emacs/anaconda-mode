import json
from anaconda_mode import handle


def parse_fixture(name, line, column):
    path = 'test/jedi/fixtures/{0}.py'.format(name)
    with open(path) as file:
        return dict(source=file.read(), line=line, column=column, path=path)


def result_from_fixture(name, line, column, method):
    params = parse_fixture(name, line, column)
    request = make_request(method, params)
    return process(request)[1]['result']


def make_request(method, params):
    return json.dumps({
        "method": method,
        "id": "0",
        "jsonrpc": "2.0",
        "params": params
    })


def process(request):
    status, response = handle(request)
    return status, json.loads(response)
