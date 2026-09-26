# Splitting an Aiken module

A pure move of Aiken code leaves every validator's compiled code, and so its
hash, byte-identical. The proof is two builds and one comparison.

## Procedure

Build both sides in fresh directories outside the checkout. The copies have
no `build/` cache and no `plutus.json`, which the
[aiken-contract-build](../../aiken-contract-build/SKILL.md) skill requires for
disposable builds.

```bash
REPO=$(git rev-parse --show-toplevel)
SKILL="$REPO/.agents/skills/splitting-oversized-modules/scripts"
WORK=<a scratch directory of your own, outside the repository>

# 1. The compiler must be the pinned fork. Run the check from the checkout:
#    it reads the pin from .github/workflows/, which the copies do not have.
node "$REPO/onchain/aiken/scripts/pinned-compiler.mjs"

# 2. Both sides from git: the parent of the split commit and the split commit.
mkdir -p "$WORK/before" "$WORK/after"
git archive <split-commit>^ onchain/aiken | tar -x -C "$WORK/before"
git archive <split-commit>  onchain/aiken | tar -x -C "$WORK/after"

# 3. Same --env and the same (default, silent) trace level on both sides.
for side in before after; do
  (cd "$WORK/$side/onchain/aiken" && test ! -e build && test ! -e plutus.json &&
   timeout 900 nice -n 19 aiken build --env testnet --out "$side.json")
done

# 4. Compare.
node "$SKILL/compare-blueprint-hashes.mjs" \
  "$WORK/before/onchain/aiken/before.json" "$WORK/after/onchain/aiken/after.json"
```

Commit the split locally before step 2 so both sides come from git. If the
comparison fails, redo that unpushed commit rather than stacking a fix on
top; a pushed split that needed a fix is two commits to read.
[committing-safely](../../committing-safely/SKILL.md) covers committing in a
shared tree. If you must compare an uncommitted tree, copy
`onchain/aiken` without `build/` and `plutus.json`, and first check with
`git status --short -- onchain/aiken` that every change in it is yours: in a
shared tree the copy carries other sessions' edits too.

Measured 2026-09-25 at `94b70237e`: each niced build took about 55 s of wall
time and 1.35 GB of memory, and built 1,162 validators.

## Why not `deployment:build`

`pnpm --dir demo deployment:build <profile>` runs
`node scripts/deployment-profiles.mjs build <profile>` (`demo/package.json:7`).
That command rewrites `onchain/aiken/env/*.ak` and
`demo/midgard-core/src/generated-deployment-profiles.ts`, then runs
`aiken build --env <profile with - replaced by _>` inside the checkout's
`onchain/aiken` and writes `plutus.json` and `plutus.json.deployment.json`
there (`demo/scripts/deployment-profiles.mjs:186-293`). It builds one tree in
place; it cannot give you a before and an after, and in a shared checkout it
overwrites files other sessions use. Use it for deployments only.

## The comparison script

`compare-blueprint-hashes.mjs <before.json> <after.json>` compares, per
validator title, the `hash` and a SHA-256 of `compiledCode`.

| Exit | First line                          | Meaning                                                                                                               |
| ---- | ----------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| 0    | `Blueprint validators identical: …` | Same titles, same hashes, same code.                                                                                  |
| 1    | `Blueprint validators DIFFER: …`    | One line per `missing`, `unexpected` or `changed` title.                                                              |
| 2    | usage                               | Not exactly two file arguments.                                                                                       |
| 3    | `could not compare: …`              | A file is unreadable or not a blueprint, names a title twice, or the two name different compilers or Plutus versions. |

It also prints the blueprint `definitions` keys found on only one side, and
still exits 0 for them: a type moved to another module changes its key. Anything
that pins such a key needs updating in the same commit (`21760c5f1` updated
three tests, a fixture generator and evidence pins, including
`demo/midgard-node/tests/sdk-aiken-schema-parity.test.ts`).

Blind spots: it cannot tell how the blueprints were built. Two builds with
different trace levels differ for reasons that have nothing to do with the
move (on 2026-09-25, a `--trace-level compact` build differed from the silent
build of the same tree in 334 of 1,162 validators). Two builds with different
`--env` differ wherever the environment constants reach. Tests are not in
the blueprint at all.

## What a pure Aiken move may change

- `use` lines and qualified names in every consumer. `21760c5f1` changed 148
  consumer modules.
- `fn` to `pub fn` for items a new module reaches across the boundary
  (`21760c5f1`: 118 items). Hashes did not move.
- Blueprint `definitions` keys of moved types (above).

## What is not a pure move

- **Moving code between files under `validators/`.** A validator's title is
  `<file>.<validator>.<handler>` (`correction_lock.spend.spend` comes from
  `validators/correction-lock.ak`), and the off-chain packages look scripts
  up by title. The comparison reports the old title `missing` and the new one
  `unexpected`.
- **Splitting a validator into several validators**, as `2c9fee3d0` did for
  native-script-decoding step 03. New scripts mean new hashes, deployments
  and off-chain builders; that is a contract change. Read
  [docs/agents/contracts.md](../../../../docs/agents/contracts.md).

## Test modules

The largest Aiken file, `lib/midgard/validation-machine-v1.test.ak`, is a test
module. Moving tests keeps the blueprint identical, so the comparison proves
nothing about them. Instead:

- Compare the collected test total of `aiken check` before and after; nothing
  compares it for you. CI runs the whole suite in
  `[ci: aiken-ci.yml/Compile and run the Aiken test suite with the pinned fork]`.
- Test selectors are module-qualified. The execution ledgers in
  `onchain/aiken/scripts/*-exec-ledger-v1.json` pin module names (for example
  `"module": "midgard/native-tx-carriage-v1.test"`) and CI's
  `Pin the … execution ledger` steps in `aiken-ci.yml` verify them.
- `demo/midgard-validation/scripts/generate-ordered-collection-boundary-aiken-goldens.mjs`
  rebinds named constants inside `validation-machine-v1.test.ak` by path.
  Keep those constants in that file, or re-point the generator in the same
  commit and prove it with
  `pnpm --dir demo/midgard-validation run fixtures:ordered-collection-boundary-aiken:check`.
  The [regenerating-goldens-and-ledgers](../../regenerating-goldens-and-ledgers/SKILL.md)
  skill covers generators and their check modes.
- `lib/midgard/cek-core-step-v1-golden.test.ak` is generated
  (`generate-cek-core-step-v1-goldens.mjs`, "Do not edit"). Split the
  generator's output, if at all, by changing the generator.

## The proof run

On 2026-09-25 the procedure was run on two `git archive` copies of
`94b70237e`. <!-- doc-links:historical --> The after copy moved `datum_transition_is_valid` from
`lib/midgard/correction-lock.ak` into a new
`lib/midgard/correction-lock-transition.ak` and updated its two consumers
(`validators/correction-lock.ak` and `lib/midgard/correction-lock.test.ak`):
`Blueprint validators identical: 1162 validators before, 1162 after`. A third
copy that also added `terminal` to the function's `or { … }` block reported
`changed` for `correction_lock.spend.spend` and `correction_lock.spend.else`
and exited 1.
