#! /bin/sh

common="ghcid -l --command 'cabal repl common'"
# backend="ghcid -l --command 'cabal repl frontend' -T Main.main"
# frontend="ghcid -l --command 'cabal repl backend' -T Main.main"
backend="ghcid -l --command 'cabal repl frontend'"
frontend="ghcid -l --command 'cabal repl backend'"

init="tmux new-session"
add="split-window -h"
remain_on_exit="set-option -p remain-on-exit"
opts="select-layout tiled \; bind-key -n r respawn-pane \; bind-key -n q kill-window"

nix-shell -A shells.ghc --run  \
  "$init \"$common\" \; $remain_on_exit \; $add \"$backend\" \; $remain_on_exit \; $add \"$frontend\" \; $remain_on_exit \; $opts"
