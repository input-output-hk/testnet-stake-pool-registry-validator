#!/bin/sh

set -eu

NO_GENERATE=
NO_SIG=
NO_PRECLEAN=
NO_POSTCLEAN=
SAME_BRANCH=
ID=
PRV_FILE=
while true; do case "$1" in
                       --private ) PRV_FILE="$2"; shift 2;;
                       --no-generate ) NO_GENERATE=1; shift;;
                       --no-sig ) NO_SIG=1; shift;;
                       --no-preclean ) NO_PRECLEAN=1; shift;;
                       --no-postclean ) NO_POSTCLEAN=1; shift;;
                       --same-branch ) SAME_BRANCH=1; shift;;
                       --id ) ID="$2"; shift 2;;
                       -v | --verbose ) set -x; shift;;
                       * ) break; esac; done

prv=${PRV_FILE:-$(mktemp -t)}
pub=${prv}.pub
tmp=$(mktemp)
atexit() {
        rm -f ${pub} ${tmp}
        if test -z "${PRV_FILE}"
        then rm -f ${prv}; fi
}
trap atexit EXIT

if test -z "${NO_PRECLEAN}"
then git checkout "master"
     git reset --hard
     git clean -f; fi

if test -z "${ID}"
then if test -z "${PRV_FILE}"
     then jcli key generate --type "ed25519" ${prv}; fi
     jcli key to-public            --input   ${prv} ${pub}; fi

id=${ID:-$(cut -d_ -f2 ${pub} | head -n1)}

JSONtransform="$1"
SHtransform="$2"
branch="$3"
message="${4:-NUON:  new}"
ticker=${5:-NUON}

if test -z "${NO_GENERATE}"
then registry prepare-submission \
       --public-key-file ${pub} \
       --ticker ${ticker} \
       --pool-web https://12345 \
       --pledge-address ed25519_pk15vz9yc5c3upgze8tg5kd7kkzxqgqfxk5a3kudp22hdg0l2za00sq2ufkk7 &&
       if test ! -f ${id}.json
       then echo "ERROR: ${id}.json wasn't created!" >&2; exit 1; fi; fi

if test -n "$JSONtransform"
then jq  --indent 4 --join-output \
        "$JSONtransform" > ${tmp} < ${id}.json &&
     mv -f                 ${tmp}   ${id}.json; fi

if test -z "${NO_SIG}"
then rm -f ${id}.sig
     jcli key sign --secret-key ${prv} --output ${id}.sig ${id}.json; fi

if test -n "$SHtransform"
then eval ${SHtransform}; fi

if test -n "${branch}" -a -n "${message}" || test -n "${SAME_BRANCH}"
then if test -z "${SAME_BRANCH}"
     then git checkout -b "${branch}"; fi

     git add --all
     git commit -m "${message}"
     
     if test -z "${NO_POSTCLEAN}"
     then git checkout master; fi; fi