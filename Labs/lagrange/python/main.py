# ___  ________ _____ _   _______
# |  \/  |  _  /  ___| | / /_   _|
# | .  . | | | \ `--.| |/ /  | |
# | |\/| | | | |`--. \    \  | |
# | |  | \ \_/ /\__/ / |\  \_| |_
# \_|  |_/\___/\____/\_| \_/\___/

"""
Using the power of love, I call cargo to run ./rust/src/main.rs
And get the info placed in a CSV if ran successfully
"""

import subprocess
from matplotlib import pyplot as plt


def gen_data() -> None:
    """
    Runs the rust project in ./rust/ with cargo to generate data in ./data/output/
    """
    proc = subprocess.Popen(
        ["cargo", "run", "--manifest-path", "./rust/Cargo.toml"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    stdout, stderr = proc.communicate()

    if proc.returncode != 0:
        raise ChildProcessError(f"Fuck you, here's an error:\n{stderr.decode()}")

    print(stdout.decode())

