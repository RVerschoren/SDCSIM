#!/bin/bash

set -eu

shopt -s nullglob
shopt -s globstar

normative_kflops=1131856 # The speed, in KFLOPS, of a pizza-box.
normative_mips=8626 # The speed, in MIPS, of a pizza-box.
initial_mem=400 # The initial amount of memory, in megabytes, that the engine gets.
mem_increment=500 # The additional amount of memory, in megabytes, that is given to the engine when it runs out of memory.
required_mem=1400 # The minimum amount of memory, in megabytes, that the application eventually gets.
max_duration=3600 # The maximum amount of time, in seconds, that the engine is allowed to run.
submit_dir="$(readlink -m "${1:-.}")" # Get the absolute path of the submit dir.

cd "$submit_dir"
echo $submit_dir
#tracefile="$submit_dir/8Kjedec.csv"
tracefile="rsrch0TRIM.csv"
echo $submit_dir
echo "Submitting traces in $tracefile..."

total_number=0

#compilation_output="$(ls -1 *.cout | awk -F . 'BEGIN{max = -1} {if($1 > max) max = $1} END{printf "%03d.cout", max + 1}')"
#make &>"$compilation_output"

nreq=$(wc -l $tracefile | cut -f1 --delimiter=" ")
echo $nreq

#Submit a job for each line
condor_submit <(
    echo "Description =\"COLD$tracefile\""
    echo "Requirements                     = TARGET.Arch             == \"X86_64\" \\"
    echo "                                && TARGET.FileSystemDomain == \"controller.mosaic.uantwerpen.be\""
    echo "Rank                             = TARGET.KFlops"
    echo "Request_Memory                   = 4096 Mb"

    echo "Universe                         = Vanilla"
    echo "Getenv                           = True"
    echo "Executable                       = /mnt/condor/rverschoren/SimCOLDTrace.exe"
    echo "Should_Transfer_Files            = No"

    echo "Job_Machine_Attrs                = KFlops,Memory,Mips"
    echo "Job_Machine_Attrs_History_Length = 1"
    #echo "JobBatchName = \"$tracefile\""

    #echo "Notification = Error"
    echo "Notification = Always"
    echo "Notify_User  = robin.verschoren@mosaic.uantwerpen.be"

    while read -r textline
    do
        tl=$(echo $textline | sed -e 's@$linecount@'$nreq'@' | sed -e 's@$trace@'$tracefile'@')
        dashtl=$(echo $textline | sed 's@ @-@g')
        #base_name="${ini_file%.*}"
        #escaped_path="${submit_dir// /\\ }/${student_dir// /\\ }/${base_name// /\\ }"

        echo "Arguments             = \"$tl\""
        echo "Log                   = $dashtl.cold.log"
        echo "Output                = $dashtl.cold.out"
        echo "Queue"
    done < paramshcwf.csv
) >/dev/null


#condor_submit <(
#    echo "Description =\"DWF$tracefile\""
#    echo "Requirements                     = TARGET.Arch             == \"X86_64\" \\"
#    echo "                                && TARGET.FileSystemDomain == \"controller.mosaic.uantwerpen.be\""
#    echo "Rank                             = TARGET.KFlops"
#    echo "Request_Memory                   = 4096 Mb"
#
#    echo "Universe                         = Vanilla"
#    echo "Getenv                           = True"
#    echo "Executable                       = /mnt/condor/rverschoren/SimDWFTrace.exe"
#    echo "Should_Transfer_Files            = No"
#
#    echo "Job_Machine_Attrs                = KFlops,Memory,Mips"
#    echo "Job_Machine_Attrs_History_Length = 1"
#    #echo "JobBatchName = \"$tracefile\""
#
#    #echo "Notification = Error"
#    echo "Notification = Always"
#    echo "Notify_User  = robin.verschoren@mosaic.uantwerpen.be"
#
#    while read -r textline
#    do
#        tl=$(echo $textline | sed -e 's@$linecount@'$nreq'@' | sed -e 's@$trace@'$tracefile'@')
#        dashtl=$(echo $textline | sed 's@ @-@g')
#        #base_name="${ini_file%.*}"
#        #escaped_path="${submit_dir// /\\ }/${student_dir// /\\ }/${base_name// /\\ }"
#
#        echo "Arguments             = \"$tl\""
#        echo "Log                   = $dashtl.dwf.log"
#        echo "Output                = $dashtl.dwf.out"
#        echo "Queue"
#    done < paramsdwf.csv
#) >/dev/null

exit 0
