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

proc = subprocess.Popen(
    ["cargo", "run"], stdout=subprocess.PIPE, stderr=subprocess.PIPE
)

stdout, stderr = proc.communicate()

if proc.returncode != 0:
    print(f"Fuck you, here's an error: {stderr.decode()}")
else:
    print(stdout.decode())

# TODO
