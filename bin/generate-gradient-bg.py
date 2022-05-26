#!/bin/env python

import os
import random

GRADIENTS = [
        ['Purple bliss', '#360033', '#0b8793'],
        ['Moonlight asteroid', '#0F2027', '#203A43', '#2C5364'],
        ['Dark ocean', '#373B44', '#4286f4']
]


def main():
    gradient = random.choice(GRADIENTS)
    iter_grad = iter(gradient)
    next(iter_grad)
    cmd = 'hsetroot '
    for color in iter_grad:
        cmd += f'-add "{color}" '
    cmd += f'-gradient {random.randint(0, 360)}'
    os.system(cmd)
    print(f'Apply {gradient[0]}')


if __name__ == "__main__":
    main()
