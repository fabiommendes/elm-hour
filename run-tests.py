#!/usr/bin/env python3

from pathlib import Path
from os import system

BASE = Path(__file__).parent


src = (BASE / "README.md").read_text() 
examples = src.partition("## Examples")[-1]

elm_src = f"""
module Examples exposing (..)

{{-| AUTO GENERATED -- DO NOT EDIT -}}

{{-|
{examples}
-}}
test : Maybe Never
test =
    Nothing
"""
(BASE / "src" / "Examples.elm").write_text(elm_src)

system(f"cd {BASE} && elm-doc-test && elm-test")