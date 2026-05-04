#!/usr/bin/env bash
set -euo pipefail

# Agent execution wrapper
# Prints git/worktree/branch/workdir info before running a given command
# and prints status after completion. Intended to be used by automated agents
# so they always announce where (branch + worktree) they operate.

print_notice() {
  printf "AGENT NOTICE: %s\n" "$1"
}

repo_root=""
branch=""
gitdir=""
worktrees=""

if git_root=$(git rev-parse --show-toplevel 2>/dev/null); then
  repo_root="$git_root"
fi
if git_branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null); then
  branch="$git_branch"
fi
if git_dir=$(git rev-parse --git-dir 2>/dev/null); then
  gitdir="$git_dir"
fi

if command -v git >/dev/null 2>&1; then
  # Attempt to list worktrees if supported
  if git worktree list >/dev/null 2>&1; then
    worktrees=$(git worktree list --porcelain 2>/dev/null || true)
  fi
fi

cwd=$(pwd)

print_notice "repo-root: ${repo_root:-(not a git repo)}"
print_notice "git-dir: ${gitdir:-(unknown)}"
print_notice "branch: ${branch:-(unknown/detached)}"
print_notice "cwd: ${cwd}"
if [ -n "$repo_root" ]; then
  # realpath may not be present on all systems, fall back to python if necessary
  if command -v realpath >/dev/null 2>&1; then
    relpath=$(realpath --relative-to "$repo_root" "$cwd" 2>/dev/null || echo ".")
  elif command -v python3 >/dev/null 2>&1; then
    relpath=$(python3 -c "import os,sys;print(os.path.relpath(sys.argv[1], sys.argv[2]))" "$cwd" "$repo_root")
  else
    relpath="$cwd"
  fi
  print_notice "rel-path-from-repo-root: ${relpath}"
fi

if [ -n "$worktrees" ]; then
  print_notice "git-worktrees-present: true"
  # Print the porcelain output for debugging
  printf "%s\n" "$worktrees" | sed 's/^/AGENT NOTICE:   /'
else
  print_notice "git-worktrees-present: none"
fi

if [ "$#" -eq 0 ]; then
  print_notice "no command supplied; nothing to run"
  exit 0
fi

print_notice "running command: $*"

start_ts=$(date --iso-8601=seconds 2>/dev/null || date)
print_notice "started: ${start_ts}"

"$@"
rc=$?

end_ts=$(date --iso-8601=seconds 2>/dev/null || date)
print_notice "exit-code: ${rc}"
print_notice "finished: ${end_ts}"

if [ $rc -ne 0 ]; then
  exit $rc
fi

print_notice "iteration-complete"
exit 0
