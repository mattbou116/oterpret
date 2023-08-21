#!/usr/bin/env bash

echo -e "Testing Lexer"
_build/default/test/lexer_tests.exe

echo -e "\nTesting Parser"
_build/default/test/parser_tests.exe
