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
    if [[ "$i" == "true" ]]; then
        "$alan" "$alO" <&0
    elif [[ "$f" == "true" ]]; then
	tmpfile=$(mktemp -t alan.XXXXXX)
	exec 3>"$tmpfile"
	exec 4<"$tmpfile"
	rm "$tmpfile"
        "$alan" "$alO" <&0 >&3
	$llc <&4
    else
        usage 1
    fi
else
    if [[  "$i" == "true" ]] || [[ "$f" == "true" ]]; then
        usage 1
    fi
    base="${1%.*}"
    if ! "$alan" "$alO" < "$1" > "${base}.ll"; then
    cat "${base}.ll";
	rm "${base}.ll";
	exit 1;
    fi
    $llc < "${base}.ll" > "${base}.s"
    clang "${base}.s" lib/lib.a -o "$base"
    #clang -no-pie "${base}.s" lib/lib.a > "$base"
fi
