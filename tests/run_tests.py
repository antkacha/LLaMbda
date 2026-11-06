from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parent.parent
BUILD = ROOT / "build"
BUILD.mkdir(exist_ok=True)
PROGRAM = BUILD / "ChatLLaMbda"

# Собираем актуальный код перед проверками.
subprocess.run(
    ["ghc", "-outputdir", str(BUILD),
     "ChatLLaMbda.hs", "-o", str(PROGRAM)],
    cwd=ROOT,
    check=True,
)


def run_chat(commands, lexicon="animals_plus.elx"):
    inputs = [lexicon, "aniworld.py", *commands, ":quit"]
    result = subprocess.run(
        [str(PROGRAM)],
        input="\n".join(inputs) + "\n",
        text=True,
        capture_output=True,
        cwd=ROOT,
        timeout=20,
        check=True,
    )
    marker = "Starting chat...\n"
    if marker not in result.stdout:
        raise AssertionError(result.stdout + result.stderr)
    return result.stdout.split(marker, 1)[1]


examples = [
    ("Conor sleeps.", "False"),
    ("every grey cat sleeps", "True"),
    ("who sleeps?", "Seamus"),
    ("who is hungry", "Conor, Dara, Ronan"),
    ("which cat is grey", "Seamus"),
    ("who swims", "Brian, Fiona"),
    ("who is not hungry", "Brian, Fiona, Seamus, Tara"),
    ("no dog sleeps", "True"),
    ("Conor eats", "Unknown words: Eats"),
    ("Seamus is hungry and", "That is not a well-formed sentence"),
    ("Seamus sleeps and Seamus is grey and Seamus fears Dara", "True"),
]

for sentence, expected in examples:
    actual = run_chat([sentence])
    assert actual == f"> {expected}\n> Bye!\n", (sentence, actual)
    print("PASS:", sentence)

actual = run_chat(["every cat fears a dog"], "animals.elx")
expected = (
    "> That sentence is ambiguous\n"
    "Sense [Every_0 Cat_1 (\\a. A_3 Dog_4 (\\b. Fears_2 a b))]: True\n"
    "Sense [A_3 Dog_4 (\\a. Every_0 Cat_1 (\\b. Fears_2 b a))]: False\n"
    "> Bye!\n"
)
assert actual == expected, actual
print("PASS: two quantifier scopes in the original lexicon")

actual = run_chat(["everyone fears someone"])
assert actual.count("Sense [") == 2, actual
assert actual.count("]: False") == 2, actual
assert "That sentence is ambiguous" in actual, actual
print("PASS: different readings with the same answer")

actual = run_chat([":derivs", "Conor sleeps", ":derivs"])
expected = (
    "> Showing derivations: on\n"
    "> Derivations: 1\n"
    "  Sleeps_1 Conor_0  ==>  False\n"
    "False\n"
    "> Showing derivations: off\n"
    "> Bye!\n"
)
assert actual == expected, actual
print("PASS: derivation display and quit")

print("\nAll 14 checks passed.")