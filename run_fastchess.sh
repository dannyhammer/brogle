#!/bin/bash

engine1="$1"
engine2="$2"
name1=$(basename $engine1)
name2=$(basename $engine2)

/home/danny/Programs/fastchess/fastchess \
		-engine cmd=$engine1 name=$name1 \
	    -engine cmd=$engine2 name=$name2 \
	    -openings file=epd/8moves_v3.epd format=epd order=random \
	    -each tc=8+0.08 -rounds 500 -repeat -concurrency 6 -recover
