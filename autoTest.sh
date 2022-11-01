#!/bin/bash

echo "-------------BUILDING------------"
cabal build

export rootFolder=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}"  )" &> /dev/null && pwd  )

echo "-------------TESTING-------------"
for testFolder in $rootFolder/tests/*; do
    if [[ -f $testFolder ]]; 
        then continue 
    fi
    
    echo "TEST: $testFolder"
    if [[ -f $testFile/output.txt ]]
    then cat $testFolder/input.pas0 | cabal run -v0 | diff - $testFile/output.txt
    else cat $testFolder/input.pas0 | cabal run -v0
    fi
    echo "---------------------------------"
done

