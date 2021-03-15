#! /usr/bin/env nix-shell
#! nix-shell  -i bash -A shells.ghc

FLAGS=()

FLAGS+=(--email-sender "info@secret-santa.link")

FLAGS+=(--email-backend None)
# FLAGS+=(--email-backend GMail)
# FLAGS+=(--email-backend SES)

FLAGS+=(--port 8000)

run_cabal_backend() {
    cabal configure
    # run with configuration in a `.env` file, that for obvious reasons is not
    # included in here
    env $(cat .env | xargs) cabal run backend -- "${FLAGS[@]}"
}
run_cabal_multi() {
    nix-shell -A shells.ghc --run "cabal configure -f devel"
    backend="cabal run -f devel backend -- ${FLAGS[@]}"
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
    "$BACKEND"/bin/backend "${FLAGS[@]}"
}
run_docker() {
    docker image load -i $(nix-build docker.nix)
    docker run -p 8081:8080 --name secret-santa-test --rm --env-file .env secret-santa-reflex:latest "${FLAGS[@]}"
    docker logs -f secret-santa-test
}

# run_cabal_backend
run_cabal_multi
# run_binary
# run_docker
