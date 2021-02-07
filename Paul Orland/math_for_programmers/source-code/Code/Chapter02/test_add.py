from chapter2_2_4 import add, Vector


def test_sum_vectors():
    expected = add(Vector(1, 2))
    assert expected == Vector(1, 2)

    expected = add(Vector(1, 2), Vector(3, 4))
    assert expected == Vector(4, 6)

    expected = add(Vector(1, 2), Vector(3, 4), Vector(5, 6))
    assert expected == Vector(9, 12)

    expected = add(Vector(1, 2), Vector(3, 4), Vector(5, 6), Vector(-1, -1))
    assert expected == Vector(8, 11)
