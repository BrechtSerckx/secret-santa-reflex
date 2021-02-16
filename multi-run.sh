#! /bin/sh

backend="cabal run backend"
frontend="cabal run frontend"

init="tmux new-session"
add="split-window -h"
remain_on_exit="set-option -p remain-on-exit"
opts="select-layout tiled \; bind-key -n r respawn-pane \; bind-key -n q kill-session"

nix-shell -A shells.ghc --run  \
"$init \"$backend\" \; $remain_on_exit \; $add \"$frontend\" \; $remain_on_exit \; $opts"
