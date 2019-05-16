# Logi
[Nonograms](https://en.wikipedia.org/wiki/Nonogram) solver

## How to use
```bash
cat ./data/dumb.txt | cabal v2-run
```

## Input file format
```
n m         # n - number of rows, m - number of cols

1B 2R 3B    # First row definition. 1B = 1 black box, 2R = 2 red boxes, 3B = 3 black boxes
2R 3Y 4B    # Second row definition
...
...
...         # Nth row definition

1B 1R 1B    # First col definition.
2R 3B 2R    # Second col definition
...
...
...         # Mth col definition
```

