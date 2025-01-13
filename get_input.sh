#!/bin/bash
if [ "$1" == "" ]; then
	echo "Usage: $0 YEAR DAY. Example 2020 19. "
	exit 0
fi

if [ "$2" == "" ]; then
	echo "Usage: $0 YEAR DAY. Example 2020 19. "
	exit 0
fi

# check if directory src/advent/$1/d$2 exists
if [ ! -d "src/advent/$1/d$2" ]; then
	echo "Creating directory src/advent/$1/d$2"
	mkdir -p src/advent/$1/d$2
fi

echo $1
curl  --cookie "session=$AOC_COOKIE" https://adventofcode.com/$1/day/$2/input > src/advent/$1/d$2/input.txt