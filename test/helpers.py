import json
import textwrap
from anaconda_mode import handle


def parse_fixture(fixture):
    cursor = '_|_'
    lines = textwrap.dedent(fixture).splitlines()
    for i, line in enumerate(lines):
        column = line.find(cursor)
        if column >= 0:
            lines[i] = line.replace(cursor, '')
            return dict(source='\n'.join(lines),
                        line=i + 1, column=column)

    raise Exception('No cursor found in fixture')


def send(fixture, method, path=''):
    params = parse_fixture(fixture)
    params['path'] = path
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
