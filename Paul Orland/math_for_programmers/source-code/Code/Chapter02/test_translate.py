import pytest

from chapter2_2_4 import translate, Vector


@pytest.mark.parametrize(("vector", "translation", "expected"),
                         [
                             (Vector(1, 1), [Vector(0, 0), Vector(0, 1), Vector(-3, -3)],
                              [Vector(1, 1), Vector(1, 2), Vector(-2, -2)]),
                             (Vector(0, 0), [Vector(1, 1), Vector(2, 2), Vector(3, 3)],
                              [Vector(1, 1), Vector(2, 2), Vector(3, 3)]),
                         ])
def test(vector, translation, expected):
    assert translate(vector, translation) == expected
