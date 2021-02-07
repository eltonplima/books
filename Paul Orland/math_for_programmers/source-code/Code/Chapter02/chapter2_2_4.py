# chapter 2.2.4 mini project
from dataclasses import dataclass
from typing import Tuple, List


@dataclass
class Vector:
    x: int
    y: int


def add(*vectors: Vector) -> Vector:
    xs = []
    ys = []

    for vector in vectors:
        xs.append(vector.x)
        ys.append(vector.y)

    return Vector(sum(xs), sum(ys))


def translate(translation: Vector, vectors: List[Vector]) -> List[Vector]:
    return [add(translation, vector) for vector in vectors]
