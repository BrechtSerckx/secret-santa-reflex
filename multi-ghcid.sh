#! /bin/sh

common="ghcid -l --command 'cabal repl common'"
backend="ghcid -l --command 'cabal repl backend'"
frontend="ghcid -l --command 'cabal repl frontend'"

init="tmux new-session"
add="split-window -h"
remain_on_exit="set-option -p remain-on-exit"
opts="select-layout tiled \; bind-key -n r respawn-pane \; bind-key -n q kill-session"

nix-shell -A shells.ghc --run  \
  "$init \"$common\" \; $remain_on_exit \; $add \"$backend\" \; $remain_on_exit \; $add \"$frontend\" \; $remain_on_exit \; $opts"
