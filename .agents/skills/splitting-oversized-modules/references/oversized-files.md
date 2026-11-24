# Size limits and earlier splits

ESLint caps every TypeScript and JavaScript file under `demo` at 500 lines
(`max-lines` in `demo/eslint.config.mjs`); a file listed in
`demo/module-size-exceptions.json` is capped at its recorded size instead,
and `demo/scripts/check-module-size-exceptions.mjs` holds the native, SQL and
out-of-demo entries to their counts. [eslint: max-lines] Aiken modules have no
line limit. Find the largest files before choosing work:

```bash
git ls-files 'demo/*.ts' 'onchain/aiken/*.ak' | xargs wc -l | grep -v ' total$' \
  | sort -rn | head -20
```

## Earlier splits and what they taught

All on 2026-08-26 to 2026-09-03. Read the commit message before you split a
neighbour of one of these files.

| Commit      | Split                                                                                                      | Lesson                                                                                                                                                                                                                                                                                                                                                           |
| ----------- | ---------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `d7368bc37` | `midgard-validation/src/validation-machine.ts` (7,956 lines) into 11 modules                               | `index.ts` re-exports the old surface; the `tsup` entry in `package.json` moved to `validation-machine/index.ts`. The 6.4k-line trace-builder generator function was left whole: splitting it means rewriting its body, which is "a behavioral refactor of consensus code … left for a dedicated change". `verify-pure-move.mjs` confirms 70 of 70 declarations. |
| `2547227ea` | Follow-up to `d7368bc37`                                                                                   | Three tests and two verifiers read `src/validation-machine.ts` **as text**; after the split they had to read every module in the directory, or they would silently check less.                                                                                                                                                                                   |
| `bfebc4f92` | `midgard-node/src/workers/utils/mpf.ts` (8,908 lines) into `src/mpf/`, 20 modules                          | "Helpers that crossed a module boundary became exports of the module that owns them; nothing else changed." A CI trigger path in `docs-site-ci.yml` and evidence references followed the moved code. `verify-pure-move.mjs` confirms 144 of 144.                                                                                                                 |
| `72e2ef761` | `midgard-sdk/src/fraud-proof/contracts.ts` (7,439 lines) into 60 modules                                   | A guard test pins the single sanctioned caller of `applyParamsToScript` by file path; it was re-pointed to `contracts/blueprint.ts`. `verify-pure-move.mjs` confirms 300 of 300.                                                                                                                                                                                 |
| `41ffa3b34` | fault-proofs `tests/support/submit-init-emulator-shared.ts` (5,046 lines) into 17 modules                  | Mixed a split with deduplication and new harness wiring in one commit. `verify-pure-move.mjs` reports 22 differing names for it. `repoRoot` changed from `"../../../.."` to `"../../../../.."`: code that computes paths from its own location must change when it moves.                                                                                        |
| `21760c5f1` | `onchain/aiken/lib/midgard/validation-machine-v1.ak` (19,151 lines) into `lib/midgard/validation-machine/` | 118 private items became `pub`; 148 consumer modules changed their `use` lines. Every one of 805 validators kept a byte-identical `compiledCode`; only the 22 blueprint `definitions` keys of re-homed types moved, and the tests, fixture generator and evidence pins that name them were updated. `aiken check` 3,198 of 3,198.                                |
| `389eb49ac` | Follow-up to `21760c5f1`                                                                                   | A golden generator re-run during the split wrote a golden with the wrong type names, and the split commit carried it. Restored the committed bytes.                                                                                                                                                                                                              |

The `verify-pure-move.mjs` results above were produced on 2026-09-25 by
reading each commit's parent as the before side and extracting the commit's
new modules with `git archive` as the after side.
