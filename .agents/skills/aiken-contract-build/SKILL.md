---
name: aiken-contract-build
description: Use when compiling, rebuilding, or debugging Midgard Aiken contracts under onchain/aiken, especially when verbose traces are needed for Plutus/Aiken script failures or emulator diagnostics.
---

# Aiken Contract Build

## Build Commands

Run Aiken commands from the contract project directory:

```bash
cd onchain/aiken
```

For a traced diagnostic blueprint, use:

```bash
aiken build --trace-level verbose --trace-filter all
```

For a normal blueprint without traces, use:

```bash
aiken build
```

## Repo Workflow

- Treat `onchain/aiken/plutus.json` as the generated blueprint output unless a task explicitly chooses another `--out` path.
- When a node or emulator test must use the freshly built real contracts, set `MIDGARD_REAL_BLUEPRINT_PATH` to the absolute `onchain/aiken/plutus.json` path.
- If tracing is needed for a script failure, rebuild with `--trace-level verbose --trace-filter all` before rerunning the failing emulator test.
- Preserve production correctness: do not switch transaction completion to `.complete({ localUPLCEval: false })` to bypass failures.
