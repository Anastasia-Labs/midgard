---
name: splitting-oversized-modules
description: Split an oversized Midgard TypeScript or Aiken module into smaller modules as a proven pure move, in a commit of its own. Use when a file is too big to work in or review, when asked to modularize, break up, decompose or extract from a god module (validation-dispute/submit.ts, user-event-indexer.ts, rollback-engine.ts, validation-machine-v1.test.ak and the other files over 5,000 lines), when moving declarations between files or packages, or when a change would be easier after first splitting the file it touches. Also use to check that an earlier split changed nothing (verify-pure-move.mjs, compare-blueprint-hashes.mjs).
---

# Splitting oversized modules

A split is a **pure move**: every declaration leaves the old file and arrives
in a new one unchanged, and nothing else in the program changes. A pure move is
cheap to review because it can be proved mechanically. The moment it also
changes behaviour, the reviewer has to read every moved line, and the split has
bought nothing.

So a split is proved, not eyeballed, and it lands **alone**: its own commit
before any behaviour change, never mixed with one.

## 1. Gate: is the split worth it?

Split only if every answer is yes. Record the answers; they go in the commit
message.

- **Separate concerns.** The file holds several things that change for
  different reasons, and you can name the seam (a group of declarations that
  import each other more than the rest). Size alone is not a reason: no lint
  rule or CI step limits file length.
- **It is changing.** `git log --since=<a month ago> --oneline -- <file>`
  shows real churn. A big file nobody edits costs nothing; splitting it
  costs every open branch that touches it a conflict.
- **The seam is a pure move.** If reaching the seam means rewriting a
  function body (the trace-builder generator in `d7368bc37` is one 6,400-line
  function), it is a behavioural refactor, not a split. Stop here and plan it
  as a change of its own; for consensus code use
  [reviewing-consensus-changes](../reviewing-consensus-changes/SKILL.md).
- **Nobody else is editing it.** `git status --short -- <file>` is empty in
  your tree, and no open branch or other session is mid-change in it. A split
  under someone else's edit turns their diff into a conflict against moved
  code.
- **It is not generated.** A generated file (for example
  `onchain/aiken/lib/midgard/cek-core-step-v1-golden.test.ak`, "Do not edit")
  is split by changing its generator. Use
  [regenerating-goldens-and-ledgers](../regenerating-goldens-and-ledgers/SKILL.md).

Done when: all five are yes, or you have stopped and said which one failed.

## 2. Find everything that pins the file

Before moving anything, list what names the file by path or reads it as text:

```bash
git grep -n -F -e '<old path without extension>' -e '<old basename>'
```

Done when: every hit is on a list with what it needs (re-point, keep, or
nothing). [references/typescript-split.md](references/typescript-split.md)
lists the kinds found in earlier splits (subpath exports, `tsup` entries,
tests that read source as text, path-pinned guards, self-relative paths, CI
filters, evidence); read it when the hits include anything beyond ordinary
imports.

## 3. Move

- TypeScript: turn the file into a directory whose `index.ts` re-exports the
  old public surface, so importers change only their path. A private helper
  that a new module needs becomes an export of the module that owns it.
- Aiken: move into new `lib/` modules and update every consumer's `use`
  lines; `fn` becomes `pub fn` where a new module reaches across. Leave
  validator files where they are.
- Change nothing else. A needed non-move edit (a path depth, a re-pointed
  test) is listed for the commit message.

Done when: the old file holds only what stays, and every hit from step 2 is
handled.

## 4. Prove it

**TypeScript**, from the repository root:

```bash
node .agents/skills/splitting-oversized-modules/scripts/verify-pure-move.mjs \
  --before HEAD:<old path> --after <new directory> [--after <old path>]
```

Exit 0 prints `Pure move confirmed: …`. Exit 1 prints `NOT a pure move: …` and
one line per differing name; each is either a mistake to undo or a necessary
edit to name in the commit. Exit 3 means it could not look. Then run the
package's typecheck, lint and tests.

**Aiken**: build the parent and the split commit in fresh scratch copies with
the pinned fork, then

```bash
node .agents/skills/splitting-oversized-modules/scripts/compare-blueprint-hashes.mjs \
  before.json after.json
```

Exit 0 prints `Blueprint validators identical: …`. The full procedure, and why
`pnpm --dir demo deployment:build` cannot do this, is in
[references/aiken-split.md](references/aiken-split.md); read it before any
Aiken build. For moved tests, also compare the `aiken check` test total.

Done when: the verifier or the comparison exits 0 (or every reported
difference is a named necessary edit), and typecheck, lint and tests pass for
every package the move touched.

## 5. Commit alone

Commit the split with
[committing-safely](../committing-safely/SKILL.md), before the change that
motivated it. The message says: what moved where (declaration and module
counts), the verifier's first line, what became exported, and each non-move
edit. The behaviour change follows in a separate commit.

Done when: `git show --stat HEAD` contains only the split and its listed
edits.

## Rules

`[script: verify-pure-move]` is
`.agents/skills/splitting-oversized-modules/scripts/verify-pure-move.mjs`;
`[script: compare-blueprint-hashes]` is the sibling
`compare-blueprint-hashes.mjs`. No CI step runs either; they run when you run
them.

1. **The split lands alone**, as its own commit before any behaviour change.
   `[review]`
2. **Pass the gate first.** `[review]`
3. **A TypeScript split is proved by the verifier**, with the before side
   read from git and never from a copy or an earlier output.
   `[script: verify-pure-move]`
4. **An Aiken split leaves every validator title and hash identical.**
   Moving code between files under `validators/` changes titles and is a
   contract change, not a split. `[script: compare-blueprint-hashes]`
5. **Aiken builds use the pinned fork**, checked with
   `node onchain/aiken/scripts/pinned-compiler.mjs` from the checkout.
   `[script: onchain/aiken/scripts/pinned-compiler.mjs]`
6. **Build Aiken in fresh copies outside the checkout, niced, with the same
   `--env` and trace level on both sides.** The checkout's `plutus.json` is
   shared. `[review]`
7. **Cross-package moves respect the package boundaries**: import siblings
   by package name, never through `../<package>/src`; nothing in the watcher
   imports the node, node tools or the DA committee node.
   `[eslint: no-restricted-imports]`
8. **Everything that pins the old path moves in the same commit**: exports
   maps, `tsup` entries, text-reading tests and scripts, path guards, CI
   filters, evidence. `[review]`
9. **Moved Aiken tests keep the same total** in `aiken check`; nothing
   compares it for you. `[review]`
10. **Generated files are split through their generator.** `[review]`
11. **The commit message carries the proof**: counts, the verifier's first
    line, new exports, non-move edits. `[review]`

## Blind spots

`verify-pure-move.mjs` compares the declarations in the files you list. It
does not see which module an import resolves to, the export surface,
evaluation order, or any file you did not list. `compare-blueprint-hashes.mjs`
sees validators only: tests are not in a blueprint, and it cannot tell how
the two blueprints were built. The package's typecheck, lint and tests carry
the rest. Both scripts need their inputs; the TypeScript verifier also needs
`typescript` from an installed demo workspace, and exits 3 without it.

## References

- [references/oversized-files.md](references/oversized-files.md): read this
  when choosing what to split, or before splitting a file next to an earlier
  split. The files over 5,000 lines (measured 2026-09-25) and the lessons of
  seven earlier splits.
- [references/typescript-split.md](references/typescript-split.md): read this
  for any TypeScript split. Package boundaries, what moves with the code,
  the verifier's options, output and blind spots.
- [references/aiken-split.md](references/aiken-split.md): read this for any
  Aiken split. The build-and-compare procedure, what a pure move may change,
  and test modules.
