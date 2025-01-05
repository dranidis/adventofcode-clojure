# advent of code

Advent of code

https://adventofcode.com/

## Tests

Execute all:

```
lein test
```

Execute only a single day:

```
lein test :only advent.2023.d2-test
```

Execute only a single test (`deftest`) in a day:

```
lein test :only advent.2023.d2-test/p1
```

## Fetch input from AOC site

```
curl  --cookie "session=YOUR-SESSION-COOKIE" https://adventofcode.com/2020/day/10/input > src/advent/2020/d10/input.txt
```

You can find your own session cookie, by visiting `https://adventofcode.com/` while logged in, and inspecting the Storage/Cookies for the site.
