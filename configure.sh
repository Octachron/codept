#!/usr/bin/env sh

function simplify {
  local name=$1
  local va=(${name//./' '})
  version="${va[0]}.${va[1]}"
}

function copy {
for dir in lib full; do 
	if [ -d "$1/${dir}" ]; then
		cp -R $1/${dir}/ .; 
	fi done
}

simplify `ocaml -vnum`;
copy "current";
if [ -d "${version}" ]; then
	copy "${version}"
fi
