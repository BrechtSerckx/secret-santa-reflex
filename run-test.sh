#! /usr/bin/env nix-shell
#! nix-shell  -i bash -A shells.ghc

DBFILE=secretsanta.db
PORT=8000


# Create database
CREATE_DB_FLAGS=()
CREATE_DB_FLAGS+=(--sqlite "$DBFILE")

# Run server

SERVE_FLAGS+=(--email-backend dummy)
# FLAGS+=(--email-backend gmail)
# FLAGS+=(--email-backend ses)

SERVE_FLAGS+=(--email-sender "info@secret-santa.link")

# FLAGS+=(--in-memory)
SERVE_FLAGS+=(--sqlite "$DBFILE" --trace)

SERVE_FLAGS+=(--port "$PORT")

# FLAGS=(create-db "${CREATE_DB_FLAGS[@]}")
FLAGS=(serve "${SERVE_FLAGS[@]}")

run_cabal_backend() {
    cabal configure
    # run with configuration in a `.env` file, that for obvious reasons is not
    # included in here
    env $(cat .env | xargs) cabal run exe:backend -- "${FLAGS[@]}" "$@"
}
run_cabal_multi() {
    nix-shell -A shells.ghc --run "cabal configure -f devel"
    backend="cabal run -f devel backend -- ${FLAGS[@]} $@"
    frontend="cabal run frontend"

    init="tmux new-session"
    add="split-window -h"
    remain_on_exit="set-option -p remain-on-exit"
    opts="select-layout tiled \; bind-key -n r respawn-pane \; bind-key -n q kill-session"

    nix-shell -A shells.ghc --run  \
              "$init \"$backend\" \; $remain_on_exit \; $add \"$frontend\" \; $remain_on_exit \; $opts"
}
run_binary() {
    FRONTEND=$(nix-build -o result-frontend -A ghcjs.frontend)
    BACKEND=$(nix-build -o result-backend -A ghc.backend)
    FLAGS+=(--web-root "$FRONTEND"/bin/frontend.jsexe)
    "$BACKEND"/bin/backend "${FLAGS[@]}" "$@"
}
run_docker() {
    docker image load -i $(nix-build docker.nix)
    docker run \
           -p 8081:"$PORT" \
           --name secret-santa-test \
           --rm \
           --env-file .env \
           secret-santa-reflex:latest \
           "${FLAGS[@]}" "$@"
    docker logs -f secret-santa-test
}

case "$1" in
    cabal)
        shift
        run_cabal_multi "$@"
        ;;
    cabal-backend-only)
        shift
        run_cabal_backend "$@"
        ;;
    binary)
        shift
        run_binary "$@"
        ;;
    docker)
        shift
        run_docker "$@"
        ;;
    *)
        run_cabal_multi "$@"
        ;;
esac
