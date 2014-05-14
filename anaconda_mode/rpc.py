from jsonrpc import JSONRPCResponseManager, dispatcher
import anaconda_mode.jedi


def handle(request):
    """Perform json rpc call."""

    return JSONRPCResponseManager.handle(request, dispatcher)
