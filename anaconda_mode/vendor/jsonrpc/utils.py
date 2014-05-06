""" Utility functions for package."""
from abc import ABCMeta, abstractmethod
import datetime
import decimal
import json

from . import six


class JSONSerializable(six.with_metaclass(ABCMeta, object)):

    """ Common functionality for json serializable objects."""

    serialize = staticmethod(json.dumps)
    deserialize = staticmethod(json.loads)

    @abstractmethod
    def json(self):
        raise NotImplemented

    @classmethod
    def from_json(cls, json_str):
        data = cls.deserialize(json_str)

        if not isinstance(data, dict):
            raise ValueError("data should be dict")

        return cls(**data)


class DatetimeDecimalEncoder(json.JSONEncoder):

    """ Encoder for datetime and decimal serialization.

    Usage: json.dumps(object, cls=DatetimeDecimalEncoder)
    NOTE: _iterencode does not work

    """

    def default(self, o):
        """ Encode JSON.

        :return str: A JSON encoded string

        """
        if isinstance(o, decimal.Decimal):
            return float(o)

        if isinstance(o, (datetime.datetime, datetime.date)):
            return o.isoformat()

        return json.JSONEncoder.default(self, o)
