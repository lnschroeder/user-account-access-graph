FROM haskell:9.4.7

WORKDIR /app

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./UserAccountAccessGraph.cabal /app/UserAccountAccessGraph.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /app
RUN cabal install

CMD ["UserAccountAccessGraph"]