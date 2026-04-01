# johpkgs (Deprecated)

**Status: Archived — no longer maintained.**

This repository has been replaced by
[Home Manager](https://github.com/nix-community/home-manager)
modules in the `home-ops` repository.

## Migration details

The home configuration (dotfiles, shell environment, editor
setup, package management) is now declared in
`home-ops/nixos/home/` and deployed via `nixos-rebuild switch`.

### What moved where

| johpkgs component | New location |
|---|---|
| gitconfig, gitignore | `home-ops/nixos/home/git/` |
| Spacemacs config + layers | `home-ops/nixos/home/spacemacs/` |
| EDITOR / VISUAL / ALTERNATE_EDITOR | `home-ops/nixos/home/spacemacs/` |
| Node.js / npm prefix | `home-ops/nixos/home/claude/` |
| Kubernetes tools | `home-ops/nixos/home/kubernetes/` |
| System packages (git, tmux, tree, ...) | NixOS system config / Home Manager |

### What was dropped

| johpkgs component | Reason |
|---|---|
| Shell aliases (git, emacs, hg, python) | Unused (ADR-0013) |
| Mercurial config | Unused (ADR-0014) |
| aspell.conf | macOS-specific, NixOS finds dicts automatically |
| .profile / .zprofile | Dispatch script, unnecessary with Home Manager |
| supervisord / emacs daemon | Replaced by Spacemacs server mode |
| nix.conf | Managed by NixOS flake |
| fix-symlinks.py | Replaced by Home Manager `home.file` |
| daemon-fg | Was for emacs daemon, no longer needed |
| Tryton 4.2 modules | Ancient version, production runs in k8s |
| Emacs packages (jedi, auto-complete, ...) | Spacemacs manages its own packages |
| dub, zanata, python-modules | Unused |

Migration completed 2026-04-01. See `home-ops/docs/decisions/`
for ADRs 0008–0015 documenting the decisions.
