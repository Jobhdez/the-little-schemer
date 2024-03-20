#!/bin/bash

for file in *maximum*; do
    if [ -f "$file" ]; then
        new_file=$(echo "$file" | sed 's/maximum/minimum/g')
        sed 's/maximum/minimum/g' "$file" > "$new_file"
    fi
done
