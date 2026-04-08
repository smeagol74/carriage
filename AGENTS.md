# AGENTS.md — Carriage Development Configuration

## Environment

This project uses Nix for development environment. Before running tests or development tasks, enter the development shell:

```bash
nix develop
# or
direnv allow
```

## Running Tests

All tests run inside the Nix development environment:

```bash
# Run all ERT tests
make test
# or via nix
nix run .#tests
```

## HDS Compliance

This project follows HDS (Holographic Development System) methodology as defined in `HDS.org`.

### Key Rules

1. **Surface First**: External/public meaning changes start in `SURFACE.md`, then tests, then code
2. **One Change — One Intent**: Each PR has one dominant goal
3. **Frozen Requires Proof**: Touching `[FROZEN]` items requires explicit Pressure (Bug|Feature|Debt|Ops) + Proof + Migration
4. **Change Gate**: Every change must include Intent, Pressure, Surface impact, Proof

### HDS Files

- `HDS.org` — HDS specification and rules
- `HOLO.md` — Project state: Stage, invariants, decisions
- `SURFACE.md` — Public contract registry with stability markers

### Verification

Run HDS verification before completing any change:

```bash
# Run tests
make test

# Byte compile
make byte-compile
```

### Change Gate Template

When making changes, include:

```
Intent: <what and why>
Pressure: Bug | Feature | Debt | Ops
Surface impact: (none) | touches: <Surface item(s)> [FROZEN/FLUID]
Proof: tests: <paths/commands that validate the intent>
```