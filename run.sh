#!/bin/bash

set -eu
# -e: Exit immediately if a command exits with a non-zero status.
# -u: Treat unset variables as an error when substituting.

# Function to display script usage
_help() {
  cat <<EOF
Usage: $0 [OPTIONS] <COMMAND> ...
Options:
  -h, --help      Display this help message
  -v, --verbose   Enable verbose mode
Commands:
  build           Build the project
  run             Run the project
EOF
}

# Build the project
build() {
  echo "Building the project..."
  docker build ./backend -f ./backend/Dockerfile -t uaag
  echo "Done."
}

# Run the project
run() {
  echo "Running the project..."
  docker compose up -d
  echo "Done."
}

handle_cmd() {
  if [ $# -eq 0 ]; then
    _help
    exit 0
  fi
  while [ $# -gt 0 ]; do
    case $1 in
    -v | --verbose)
      set -x
      ;;
    -h | --help)
      _help
      exit 0
      ;;
    -* | --*)
      echo "Invalid option: $1" >&2
      _help
      exit 1
      ;;
    build)
      build "${@:2}"
      break
      ;;
    run)
      run "${@:2}"
      break
      ;;
    *)
      echo "Invalid command: $1" >&2
      _help
      exit 1
      ;;
    esac
    shift
  done
}
handle_cmd "$@"
