#!/usr/bin/env bash

set -e
set -o pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
alan="$DIR/alanIR"
llc="llc-6.0"

usage () {
    echo "Usage: $0 [-O] filename" >&2
    echo "  or:  $0 [-O] (-f|-i)"
    echo
    echo "  -O  Enable optimizations"
    echo "  -f  Emit target code"
    echo "  -i  Emit intermediate code"
    echo "  -h  Display this message"
    exit "$1"
}

f="false"
i="false"
OPTSPEC="Olfih"
while getopts "$OPTSPEC" FLAG; do
    case $FLAG in
        O)
            # enable both optimizations
            alO="-O"
            llO="-O3"
            ;;
        f)
            i="false"
            f="true"
            ;;
        i)
            f="false"
            i="true"
            ;;
        h)
            usage 0
            ;;
        \?)
            usage 1
            ;;
    esac
done


shift $((OPTIND - 1))

if [[ -z "$1" ]]; then
    # input/output from stdin

    if [[ "$i" == "true" ]]; then
        # output intermediate code
        "$alan" "$alO" <&0

    elif [[ "$f" == "true" ]]; then

        # output intermediate code to temp
        tmpfile=$(mktemp -t alan.XXXXXX)
        exec 3>"$tmpfile"
        exec 4<"$tmpfile"
        rm "$tmpfile"
        "$alan" "$alO" <&0 >&3
        # input from temp, output target code to stdout
        $llc $llO <&4
    else
        usage 1
    fi
else
    # input from file, no f/i flags should be set
    if [[  "$i" == "true" ]] || [[ "$f" == "true" ]]; then
        usage 1
    fi

    # remove suffix
    base="${1%.*}"

    # output intermediate code to .ll
    if ! "$alan" "$alO" < "$1" > "${base}.ll"; then
        # remove .ll and exit if failed
        rm "${base}.ll";
        exit 1;
    fi

    # input from .ll, output assembly to .s
    $llc $llO < "${base}.ll" > "${base}.s"
    # create final executable
    clang -no-pie "${base}.s" lib/lib.a -o "$base"
    echo "Done, executable in $base"
fi
