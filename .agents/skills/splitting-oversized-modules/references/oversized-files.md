# Oversized files and earlier splits

Measured 2026-09-25 at `94b70237e`. Re-measure before you rely on it:

```bash
git ls-files 'demo/*.ts' 'onchain/aiken/*.ak' | xargs wc -l | grep -v ' total$' \
  | awk '$1 > 5000' | sort -rn
```

## Files over 5,000 lines

Nine files, not "more than 20" as the hardening plan said; the largest,
18,590 lines, matches its "18.6k". Churn is `git log --since=2026-08-26
--oneline -- <file> | wc -l`.

| Lines  | File                                                              | Commits since 2026-08-26 | Note                                                                                                                                           |
| ------ | ----------------------------------------------------------------- | ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| 18,590 | `onchain/aiken/lib/midgard/validation-machine-v1.test.ak`         | 18                       | Aiken test module. `generate-ordered-collection-boundary-aiken-goldens.mjs` rebinds named constants inside it by path.                         |
| 9,242  | `demo/midgard-fault-proofs/src/validation-dispute/submit.ts`      | 31                       | The most active file on the list.                                                                                                              |
| 9,230  | `demo/midgard-node/tests/database.test.ts`                        | 7                        | Test file.                                                                                                                                     |
| 7,127  | `onchain/aiken/lib/midgard/cek-core-step-v1-golden.test.ak`       | 2                        | **Generated** by `demo/midgard-validation/scripts/generate-cek-core-step-v1-goldens.mjs` ("Do not edit"). Change the generator, not this file. |
| 6,901  | `demo/midgard-watcher/src/indexers/user-event-indexer.ts`         | 12                       |                                                                                                                                                |
| 6,777  | `demo/midgard-validation/src/validation-machine/trace-builder.ts` | 10                       | Mostly one generator function; see `d7368bc37` below.                                                                                          |
| 5,476  | `demo/midgard-node-tools/src/commands/stress-wallets.ts`          | 2                        | Tooling; little churn.                                                                                                                         |
| 5,328  | `demo/midgard-watcher/src/l1/rollback-engine.ts`                  | 6                        |                                                                                                                                                |
| 5,043  | `demo/midgard-core/src/deployment-manifest-identity.ts`           | 28                       |                                                                                                                                                |

Counts at other thresholds, same command: 14 files over 4,000 lines, 34 over
3,000, 87 over 2,000, 320 over 1,000. No lint rule or CI step limits file
length (`git grep -n max-lines` finds nothing).

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
