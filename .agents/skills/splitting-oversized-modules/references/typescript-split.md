# Splitting a TypeScript module

## Package boundaries

A split inside one package is a change of relative imports. A split that moves
code into another package adds a dependency edge, and the lint rules below
decide whether that edge is allowed. All of them are
`[eslint: no-restricted-imports]` in `demo/eslint.config.mjs`, run in CI by
`[ci: midgard-node-ci.yml/Lint and format-check the demo workspace]`:

- Every package: never import a sibling through `../<package>/src` or
  `../<package>/dist`; import it by name (`@al-ft/midgard-core/hex`) and add
  a workspace dependency and an `exports` entry if one is missing
  (`workspacePackageBoundary`, lines 11–22).
- `midgard-watcher/src`: never import `midgard-node`, `midgard-node-tools` or
  `da-committee-node` (lines 243–268). The watcher verifies the operator
  independently; moving shared code into the node makes it unreachable from
  the watcher.
- `midgard-sdk/src`, `midgard-node`, `midgard-node-tools`: no `@/` alias;
  use relative specifiers (lines 170–188, 271–287).

New files must also sort their imports and exports:
`[eslint: simple-import-sort/imports]` and
`[eslint: simple-import-sort/exports]` (lines 124–125).

Blind spot: the lint rules see import specifiers only. Nothing checks that a
module stays in the package whose concern it is.

## What must move with the code

Each of these was missed or re-pointed by hand in an earlier split (see
[oversized-files.md](oversized-files.md)). Find them all before moving
anything:

```bash
git grep -n -F -e '<package>/src/<old-path-without-.ts>' -e '<old-basename>'
```

- **Subpath exports.** If the file is a subpath in the package's `exports`
  map, move every condition (`midgard-source`, `types`, `import`,
  `require`).
- **Build entries.** Packages list `tsup` entries in their `build` script
  (`d7368bc37` changed `src/validation-machine.ts` to
  `src/validation-machine/index.ts`).
- **Tests and scripts that read source as text.**
  `demo/midgard-node/tests/da-payload-libp2p-producer.test.ts:824` reads
  `../src/commands/listen-router.ts` and asserts what it does _not_ contain;
  `demo/da-committee-node/scripts/check-no-http-da-transport.mjs` lists six
  source files by path. When the named file is deleted they fail loudly;
  when it survives holding less code, they pass while checking less. Point
  them at every module that now holds the code (`2547227ea`). Tests that load
  a module by path, such as
  `demo/midgard-core/tests/availability-operation-journal.test.ts:48`, fail
  loudly when it moves.
- **Path-pinned guards**, such as a test that pins the only file allowed to
  call a function (`72e2ef761`).
- **Code that computes paths from its own location**
  (`new URL("..", import.meta.url)`, `resolve(moduleDir, "../..")`). Its
  depth changes when it moves; `verify-pure-move.mjs` reports it as changed,
  correctly. Name each such edit in the commit message.
- **CI path filters and docs** that name the file (`bfebc4f92` updated
  `.github/workflows/docs-site-ci.yml`), and evidence under
  `docs/exec-plans/evidence/`. [docs/agents/naming-and-versioning.md](../../../../docs/agents/naming-and-versioning.md)
  says to preserve pinned evidence paths, so re-point rather than delete.

## Keeping the old surface

The three clean splits (`d7368bc37`, `bfebc4f92`, `72e2ef761`) all turned the
old file into a directory with an `index.ts` that re-exports the previous
public surface, so importers change only their path. Private helpers that a
new module needs become exports of the module that owns them.

## The verifier

```bash
node .agents/skills/splitting-oversized-modules/scripts/verify-pure-move.mjs \
  --before HEAD:demo/midgard-watcher/src/l1/rollback-engine.ts \
  --after demo/midgard-watcher/src/l1/rollback-engine
```

- `--before <ref>:<path>`, repeatable. Read with `git show`; a plain file
  path, or an empty ref (the index), is refused with exit 2. Use the commit
  the split starts from, usually `HEAD` while the split is uncommitted and
  `<split-commit>^` afterwards.
- `--after <path>`, repeatable. A directory expands to every
  `.ts`/`.tsx`/`.mts`/`.cts` file beneath it. Pass exactly the files the
  split produced plus the shrunk original if it still exists; an unrelated
  file in the list shows up as `unexpected`.
- `typescript` comes from the demo workspace of the repository holding the
  script (`demo/package.json`, then `demo/midgard-core/package.json`,
  because pnpm does not hoist `typescript` to the demo root). Override with
  `--typescript-root <repo>`.

What it compares: every top-level statement except `import`, `export { … }`,
`export * from` and `import x = require()`, reduced to its syntax tree with
comments, whitespace, trailing commas, quote style, numeric spelling
(`0x10` = `16`) and the top-level `export`/`default` modifiers removed.
`let`/`const`, unary operators, heritage clauses and `export` inside a
namespace are kept. Statements are grouped by kind and name (`function add`,
`const LIMIT`, `statement Object.freeze(Limits);`), and each group must be
the same multiset before and after.

Output and exit codes:

| Exit | First line               | Meaning                                                                                                                       |
| ---- | ------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| 0    | `Pure move confirmed: …` | Every group matches. The line names the commit read and the `typescript` used.                                                |
| 1    | `NOT a pure move: …`     | One line per differing name: `missing`, `unexpected`, `copies` (same text, different count) or `changed`, with locations.     |
| 2    | usage                    | Bad arguments, including a before side that is not `<ref>:<path>`.                                                            |
| 3    | `could not look: …`      | git cannot resolve the ref or show the path, a file does not parse, or `typescript` cannot be resolved. Nothing was compared. |

### Blind spots

A confirmed pure move does not prove:

- **Import targets.** An after file can import a same-named binding from a
  different module than the before file used. Typecheck catches type
  mismatches, not two functions with the same signature.
- **The export surface.** Re-exports, `export default` versus named export,
  and what a package's `exports` map exposes are ignored.
- **Evaluation order.** Top-level side effects (`Object.freeze(x)`, a
  registry `set`) are compared as statements, but the order modules evaluate
  in after the split is not.
- **Anything outside the listed files**: callers, tests, text-reading tests,
  build entries and CI filters. Those are the grep list above.
- **Semantics of an accepted difference.** When it reports `changed` for a
  necessary edit (a path depth), you judge that edit.

Run the package's typecheck, lint and tests after the verifier; the verifier
proves the moved text, those prove the wiring.
