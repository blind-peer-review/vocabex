# Project Setup with uv (Python 3.10)

This project uses [uv](https://github.com/astral-sh/uv) as the Python package/dependency manager with **Python 3.10**.

## Prerequisites

- Python **3.10** installed and available on your system.
- Install uv by following the instructions on the [uv docs](https://docs.astral.sh/uv/getting-started/installation/).

## Setup

1. Sync the project's dependencies with the environment by executing in your terminal:
```
uv sync
```
2. Analyze the texts using the following command:
```
PYTHONPATH=./src uv run scripts/analyze_exercises.py
```
3. Replace 'analyze_exercises.py' for one of the other scripts you see in the folder to calculate cohen's or fleiss' kappa.