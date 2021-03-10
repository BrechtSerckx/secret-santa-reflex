#! /usr/bin/env nix-shell
#! nix-shell  -i bash -A shells.ghc

FLAGS=()

FLAGS+=(--email-sender "info@secret-santa.link")

FLAGS+=(--email-backend None)
# FLAGS+=(--email-backend GMail)
# FLAGS+=(--email-backend SES)

run_cabal() {
    # run with configuration in a `.env` file, that for obvious reasons is not
    # included in here
    env $(cat .env | xargs) cabal run backend -- "${FLAGS[@]}"
}
run_binary() {
    FRONTEND=$(nix-build -o result-frontend -A ghcjs.frontend)
    BACKEND=$(nix-build -o result-backend -A ghc.backend)
    # firefox --new-tab "$FRONTEND"/bin/frontend.jsexe/index.html
    "$BACKEND"/bin/backend "${FLAGS[@]}"
}
run_docker() {
    docker image load -i $(nix-build docker.nix)
    docker run -p 8081:8080 --name secret-santa-test --rm --env-file .env secret-santa-reflex:latest "${FLAGS[@]}"
    docker logs -f secret-santa-test
}

run_docker
