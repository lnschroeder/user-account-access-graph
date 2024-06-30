# user-account-access-graph

## Getting started
To run the app locally you have two options using `cabal` or using `docker`:

### Using `cabal`

Prerequiste:
install [GHCup](https://www.haskell.org/ghcup/) 

Then run:
```sh 
cabal run
```

### Using `docker`
```sh
# build container
docker build . -t <container-name>

# run container
docker run -it --rm -v <path/to/output-dir>:/app <container-name>
```