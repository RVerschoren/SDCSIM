#! /bin/bash
COMMANDANDARGS="$@"

valgrind --tool=massif --pages-as-heap=yes --massif-out-file=massif.out $COMMANDANDARGS; grep mem_heap_B massif.out | sed -e 's/mem_heap_B=\(.*\)/\1/' | sort -g | tail -n 1
ms_print massif.out
