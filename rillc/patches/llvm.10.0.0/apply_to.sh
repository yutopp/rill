#!/bin/bash

patch_dir="$1"

files=("install.sh" "META.patch" "opam")

for f in "${files[@]}"; do
    patch -Nfu "$f" "$patch_dir/$f.patch"
    if [ $? -ge 2 ]; then
        exit 1
    fi
done
