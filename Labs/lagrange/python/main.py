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

import json
import subprocess
import pandas as pd
from tabulate import tabulate
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

    print("Rust code run successfully!")
    if stdout.decode():
        print(stdout.decode())


if __name__ == "__main__":
    DATA_DIR: str = "./data"

    gen_data()  # Do this first so Rust checks if 'data.json' conforms to what's expected

    with open(f"{DATA_DIR}/input/data.json", "r", encoding="utf-8") as fin:
        data = json.load(fin)

        # For a general plot :3
        gnral_fig = plt.figure(figsize=(7, 8))
        gnral_fig.subplots_adjust(hspace=0.3)

        gnral_ax_1 = gnral_fig.add_subplot(211)
        gnral_ax_1.set_title(r"Абсолютная погрешность в зависимости от $n$")
        gnral_ax_1.set_xlabel(r"$n$")
        gnral_ax_1.set_ylabel(r"$\Delta f_n$")

        gnral_ax_2 = gnral_fig.add_subplot(212)
        gnral_ax_2.set_title(r"Остаточный член в зависимости от $n$")
        gnral_ax_2.set_xlabel(r"$n$")
        gnral_ax_2.set_ylabel(r"$R_n$")

        for x in data["x_values"]:
            df = pd.read_csv(f"{DATA_DIR}/output/x_{x}.csv")

            # Pretty format the csv ;3
            with open(f"{DATA_DIR}/output/x_{x}.txt", "w", encoding="utf-8") as f:
                f.write(
                    tabulate(
                        df,
                        headers="keys",
                        tablefmt="outline",
                        showindex=False,
                    )
                )

            # Create subplot
            fig, axs = plt.subplots(2, 1, figsize=(7, 8))

            # Plot Delta f_n
            gnral_ax_1.plot(df["n"], df["AbsErr"], "o-", label=rf"$x$ = {x}")
            axs[0].plot(df["n"], df["AbsErr"], "bo-", label=rf"$x$ = {x}")
            axs[0].set_title(gnral_ax_1.get_title())
            axs[0].set_xlabel(gnral_ax_1.get_xlabel())
            axs[0].set_ylabel(gnral_ax_1.get_ylabel())
            axs[0].legend()

            # Plot R_n
            gnral_ax_2.plot(df["n"], df["R"], "^-", label=rf"$x$ = {x}")
            axs[1].plot(df["n"], df["R"], "r^-", label=rf"$x$ = {x}")
            axs[1].set_title(gnral_ax_2.get_title())
            axs[1].set_xlabel(gnral_ax_2.get_xlabel())
            axs[1].set_ylabel(gnral_ax_2.get_ylabel())
            axs[1].legend()

            # Make pretty and save
            fig.subplots_adjust(hspace=0.3)
            fig.savefig(f"{DATA_DIR}/output/x_{x}.png")

        gnral_ax_1.legend()
        gnral_ax_2.legend()

        gnral_fig.savefig(f"{DATA_DIR}/output/general.png")
