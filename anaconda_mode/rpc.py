from jsonrpc import JSONRPCResponseManager, Dispatcher
from anaconda_mode.jedi import api

dispatcher = Dispatcher(api)


def handle(request):
    """Perform json rpc call."""

    return JSONRPCResponseManager.handle(request, dispatcher)
