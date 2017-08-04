#!/bin/bash

# Redirect stderr to stdout so they both end up in the same output file.
exec 2>&1

temp_dir=/mnt/log
target_dir="${1:?target directory expected as first argument}"
ini_file="${2:?ini file expected as second argument}"
mem_limit="${3:?memory limit expected as third argument}"

dir_name="${ini_file%/*}"
base_name="${ini_file##*/}"
prefix="${base_name%.*}"

# Create a local file to which the output will be written and create a symlink
# on the share that points to it and that will cause the output to be written to
# it.
temp_bitmap="$(mktemp --tmpdir="$temp_dir" "$prefix.XXXXXXXXXX.bmp")"
ln -s "$temp_bitmap" "$target_dir/$dir_name/$prefix.bmp"

# Run the engine in the same directory as the INI file is in with limited
# resources.
(
        cd "$target_dir/$dir_name"
        ulimit -v $mem_limit

        exec /usr/bin/gdb --return-child-result --batch \
                --eval-command='run' \
                --eval-command='printf "\n"' \
                --eval-command='printf "--- BEGINNING OF BACKTRACE ---\n"' \
                --eval-command='set print frame-arguments all' \
                --eval-command='backtrace full' \
                --eval-command='printf "--- END OF BACKTRACE ---\n"' \
                --args "$target_dir/engine" "$base_name"
)

# Remember the exit status of the engine.
exit_status=$?

exit $exit_status
