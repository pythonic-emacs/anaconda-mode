from .utils import JSONSerializable


class JSONRPCBaseRequest(JSONSerializable):

    """ Base class for JSON-RPC 1.0 and JSON-RPC 2.0 requests."""

    def __init__(self, method=None, params=None, _id=None,
                 is_notification=None):
        self.data = dict()
        self.method = method
        self.params = params
        self._id = _id
        self.is_notification = is_notification

    @property
    def data(self):
        return self._data

    @data.setter
    def data(self, value):
        if not isinstance(value, dict):
            raise ValueError("data should be dict")

        self._data = value

    @property
    def args(self):
        """ Method position arguments.

        :return tuple args: method position arguments.

        """
        return tuple(self.params) if isinstance(self.params, list) else ()

    @property
    def kwargs(self):
        """ Method named arguments.

        :return dict kwargs: method named arguments.

        """
        return self.params if isinstance(self.params, dict) else {}

    @property
    def json(self):
        return self.serialize(self.data)


class JSONRPCBaseResponse(JSONSerializable):

    """ Base class for JSON-RPC 1.0 and JSON-RPC 2.0 responses."""

    def __init__(self, result=None, error=None, _id=None):
        self.data = dict()

        self.result = result
        self.error = error
        self._id = _id

        if self.result is None and self.error is None:
            raise ValueError("Either result or error should be used")

    @property
    def data(self):
        return self._data

    @data.setter
    def data(self, value):
        if not isinstance(value, dict):
            raise ValueError("data should be dict")

        self._data = value

    @property
    def json(self):
        return self.serialize(self.data)
