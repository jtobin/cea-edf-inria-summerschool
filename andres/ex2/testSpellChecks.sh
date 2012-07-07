#!/bin/bash
time ./spellCheck1 testtext "/usr/share/dict/words" > /dev/null
time ./spellCheck2 testtext "/usr/share/dict/words" > /dev/null # something screwy here
time ./spellCheck3 testtext "/usr/share/dict/words" > /dev/null
