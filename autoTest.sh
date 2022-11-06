#!/bin/bash

draw () {
    local lvl=0
    local ne=1
    esc=${1@Q}

    for c in $(seq 1 ${#esc}); do
        if [[ ${esc:c+$ne:1} == '(' ]] 
        then
            ((lvl+=1))
            printf '\n'
            printf "%0.s|   " $(seq 1 $lvl)
        elif [[ ${esc:c+$ne:1} == ')' ]]
        then 
            ((lvl-=1))
        elif [[ ${esc:c+$ne:1} == ' ' ]]
        then
            printf ' '
        elif [[ ${esc:c+$ne:1} == '\' && ${esc:c+$ne+1:1} == 'n' ]]
        then
            printf '\n'
            ((ne+=1))
        elif [[ $((c+ne +1)) == ${#esc}  ]]
        then
            break
        else 
            printf '%c' ${esc:c+$ne:1}
        fi
    done
    printf "\n---------------------------------\n"
}

echo "-------------BUILDING------------"
cabal build

export rootFolder=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}"  )" &> /dev/null && pwd  )

echo "-------------TESTING-------------"
local par="*"
if [ -z "$1" ]
    then
        par="*"
    else
        par=$1
fi

for testFolder in $rootFolder/tests/$par; do
    if [[ -f $testFolder ]]; 
        then continue 
    fi
    
    echo "TEST: $testFolder"
    if [[ -f $testFile/output.txt ]]
    then cat $testFolder/input.pas0 | cabal run -v0 | diff - $testFile/output.txt
    else 
        res=$(cat $testFolder/input.pas0 | cabal run -v0)
        draw "$res"
    fi
done

