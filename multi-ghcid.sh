#! /usr/bin/env bash
# shellcheck disable=SC2039


# packages=(common)
# packages=(common backend)
# packages=(common frontend)
# packages=(common backend frontend)

error() {
    echo "ERROR: $1"
    exit 1
}

# check that packages is not empty
if [ ${#packages[@]} -eq 0 ]; then
    error "No packages specified"
fi


# construct tmux subcommands
tmux_cmds=()

# init session with head package
head=${packages[0]}
unset "packages[0]"
tmux_cmds+=("new-session ghcid -l --command \"cabal repl $head\"")

# split window on every tail package
for package in "${packages[@]}"; do
    tmux_cmds+=("split-window -h ghcid -l --command \"cabal repl $package\"")
    tmux_cmds+=("set-option -p remain-on-exit")
done

# additional options
tmux_cmds+=("select-layout tiled" \
            "bind-key -n r respawn-pane" \
            "bind-key -n q kill-session" \
           )

# construct tmux command
tmux_cmd=()
thead=${tmux_cmds[0]}
unset "tmux_cmds[0]"
tmux_cmd+=("$thead")
for cmd in "${tmux_cmds[@]}"; do
    tmux_cmd+=("\;" "$cmd")
done 

# run in nix shell
nix-shell -A shells.ghc --run "tmux ${tmux_cmd[*]}"
