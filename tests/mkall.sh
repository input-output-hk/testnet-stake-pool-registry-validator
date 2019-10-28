#!/bin/sh

set -e

git clean -f
git checkout master
git branch | grep -v master | sed 's/*//g' | xargs git branch -D 2>/dev/null || true
def="$(dirname $0)/mk.sh"
inc='i=$((i+1)); total=$((total+1))'
total=0

###
### Create basis
###
$def --same-branch '.ticker="TICK"' '' \
'' 'TICK:  new'

key01=$(mktemp -t "XXXXXXXXXX.prv")
jcli key generate --type "ed25519" ${key01}

$def --same-branch --private ${key01} '.ticker="FST"' '' \
'' 'FST:  new'

###
### Valid cases
###
$def  '' '' \
"00-new"

$def --private ${key01} --no-generate '.ticker="OTHR"' '' \
"01-update-ticker"       "OTHR:  update"

$def --private ${key01} --no-generate '.pledge_address="ed25519_pk1uadm2958qvrta7e0j275ftrxtth5p9txaqq3gg9z5fxg0zxq0d3sa9yyys"' '' \
"02-update-pledge"       "FST:  update" "FST"

###
### Invalid cases
###
now=$i
start=9 # That's right, 9, not 10.
skipped=$((start-now))
i=${start}

###
### change structure
###
f=$(ls *.json | head -n1 | xargs echo)
id=$(jq .id <$f | xargs echo | cut -d_ -f2)
echo f=${f} id=${id}
eval $inc; $def --no-generate --no-sig --id "${id}" '' "mv ${id}.{json,sig}" \
"$i-modify" "$i:  Modify!"

eval $inc; $def --no-generate --no-sig '' 'rm *.json' \
"$i-drop-files" "$1:  Delete!"

eval $inc; $def  '' '' \
"$i-commit-message" "Sandals of God"

set -x
eval $inc; $def --no-postclean  '' '' \
"$i-history-too-long" # Generate first commit
$def --no-preclean --same-branch '' '' \
"" "SECD:  new" "SECD"

###
### file hierarchy
###
eval $inc; $def --no-generate --no-sig '' 'touch ../{somewhere,something}' \
"$i-not-in-registry"

eval $inc; $def --no-generate --no-sig '' 'mkdir somewhere; touch somewhere/{else,and.again}' \
"$i-not-in-registry-either"

eval $inc; $def  '' 'cp ${id}.json ${id}1.json' \
"$i-too-many-files"

###
### file names
###
eval $inc; $def  '' 'mv ${id}.json terrible.json' \
"$i-file-id"

###
### file content
###
eval $inc; $def  'empty' '' \
"$i-empty-json"

eval $inc; $def  '.unexpected="really!"' '' \
"$i-unexpected-field"

eval $inc; $def  'del(.homepage)' '' \
"$i-too-few-fields"

###
### file content: Id
###
eval $inc; $def  'del(.id)' '' \
"$i-missing-mandatory"

eval $inc; $def  'del(.pledge_address)' '' \
"$i-missing-mandatory-pledge"

eval $inc; $def  '.id=3.141592653589793' '' \
"$i-mandatory-field-type"

eval $inc; $def  '.id="ed25519_nsacrypto"' '' \
"$i-internal-id-length"

eval $inc; $def  '.id="nsacrypto_pk1dqaghc0luz0m9mr807hyaeprw49rx8ykvpcw6vvldckznl0q0y4qr280s2"' '' \
"$i-internal-id-format"

eval $inc; $def  '.id="ed25519_pk1dqaghc0luz0m9mr807hyaeprw49rx8ykvpcw6vvldckznl0q0y4qr280s2"' '' \
"$i-id-entry-mismatch"

###
### file content: types
###
eval $inc; $def  '.pledge_address=2.7182818284590' '' \
"$i-pledge-field-type"

eval $inc; $def  '.ticker="ZALMAY"' '' \
"$i-ticker-too-long"

eval $inc; $def  '.ticker="NO"' '' \
"$i-ticker-too-short"

eval $inc; $def  '.ticker="FST"' '' \
"$i-duplicate-ticker"

###
### signature
###
eval $inc; $def  '' 'echo -n ed25519_sig1se9knlfd3uny0mp5wc8yc0f20vsz4yjm2c2zklg6fsdy7500w8vln6qwtfnseajn6g3ztyfll8swjxef0804sge67m7huh3mup5dspg3juxmu > ${id}.sig' \
"$i-signature-mismatch"

eval $inc; $def  '' 'echo 1 > ${id}.sig' \
"$i-malformed-signature"

cat <<EOF

Generated:  $total total branches

EOF
