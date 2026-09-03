# Handoff: L1 size-fit plans (session of 2026-09-01)

Written because the authoring session hit its limits. A successor should be
able to finish from this file plus the per-agent handoffs
`HANDOFF-cek.md` and `HANDOFF-resolve-inputs.md` (written by the two agents
that were still running when the session ended; if either is missing, that
agent did not get to write it).

## What the user asked for

One plan document per compiled script that exceeds the 16,384-byte L1
transaction limit (51 scripts; `withdrawal_mistag/step_03` excluded because
the user is fixing it), each covering on-chain size-fit (withdraw-zero split,
prune, chain, or redesign) and, where absent, the off-chain code and Lucid
Evolution emulator scenario tests. Location `docs/fault-proofs/size-plans/`,
shared primer first, subagents on the Fable model. The user chose all three
via AskUserQuestion.

## State of the deliverable

| Item                                | Status                                                                                                                                                                                    |
| ----------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `00-primer.md`                      | Done. Limits, three split precedents, ripple effects, probe method, 11-section template.                                                                                                  |
| `README.md` index                   | Done for all 50 rows (2026-09-02).                                                                                                                                                        |
| Availability challenge (1)          | Plan written.                                                                                                                                                                             |
| Value-and-mint (8)                  | Plans written; replay-asset plan owns the shared asset-fold yield.                                                                                                                        |
| Phase-A (10 + 4 borderline)         | Plans written; signature-between plan is the anchor for the shared payload fix.                                                                                                           |
| Transition-trace finals (2)         | Plans written.                                                                                                                                                                            |
| Script-sources A (8 + 1 borderline) | Plans written; non-output and output-proof-step own shared designs.                                                                                                                       |
| Script-sources B (12)               | Plans written; stage-ten-match is the anchor.                                                                                                                                             |
| CEK (3)                             | Plans written (context and core: multi-tx chains; selection: yields). `HANDOFF-cek.md` is the agent's interim state and can be deleted once the plans are reviewed.                       |
| Resolve-inputs (6)                  | Plans written with measured probes; a review pass on 2026-09-02 aligned them with the shared output-proof yields. No `HANDOFF-resolve-inputs.md` was produced (agent hit the rate limit). |

Nothing in this directory is committed. No source, test, or blueprint file
was modified by this work. The working tree also carries the user's own
uncommitted changes (settlement redeemer layout, watcher tests, withdrawal-
mistag step 03); leave them alone.

## Remaining steps

Editorial work is complete (see the next section). What remains is the
user's decision to commit, then implementation in the primer's landing
order. Do not commit unasked.

## Status after the 2026-09-02 review passes

All 50 plans exist, the index is complete, shared-design ownership and role
naming are reconciled (see README "Landing order"), and prettier has been
run. Remaining open items are technical, not editorial: PA-UNDECODED
soundness sign-off, emulator ExUnits for every split (none measured), the
CEK↔stage-one second-carrier interface (interface-level only), and the
`AvailabilityChallengeExpiryYield` rename (a 32-byte-cap naming choice).

Note for whoever builds next: the resolve-inputs review agent found that the
working tree at that moment had roughly 184 modified and 32 untracked Aiken
files and did not build under the pinned fork (silent exit 1); it probed
from `git archive HEAD` (815b703a9) instead, which reproduced every baseline
size. That tree state is the user's concurrent on-chain work, not this
documentation effort.

## Facts a successor would otherwise rediscover

- Sizes and the 51-script list were measured from `onchain/aiken/plutus.json`
  (raw unapplied `compiledCode`, deduplicated by validator). The blueprint is
  reproducible byte-for-byte with the pinned fork.
- Pinned Aiken: `/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken`
  (v1.1.23+5adf783, md5 ea9b3905…). Not on PATH in non-login shells. Never
  build inside the checkout while agents may be building; copy to `/tmp`.
- Publication overheads: +72–73 bytes applied parameters, +~276 bytes signed
  wrapper. Production admission rejects raw ≥ 16,384
  (`assertReferenceScriptRawBodiesFitL1EnvelopeV1`). Target raw ≤ 15,000.
- Emulator harness passes `oversized: true` for these scripts
  (`tests/support/emulator/reference-scripts.ts`); the dispute suite also
  raises `maxTxSize` to 262,144 for those publications. Removing the flag is
  part of every plan's done criterion.
- Off-chain reality: only CEK (3) and value-and-mint (11) resolvers have
  deployment entries and submit routes in
  `demo/midgard-fault-proofs/src/validation-dispute/submit.ts`; script-
  sources, resolve-inputs, and phase-A resolvers are wired in
  `demo/midgard-sdk/src/fraud-proof/contracts.ts` only. Transition-trace
  finals 4 and 5 have no emulator scenario. The availability challenge has
  one builder (bond from attestation) and no lifecycle test.
- Compiler hazard observed by the value-and-mint agent: the pinned fork
  aborts (SIGABRT, no diagnostic) on `Option<record> == None` and on
  block-expression `Data` upcasts; rewrite those forms.
- Environment: run read-only commands through Bash with
  `wsl.exe -d ubuntu -- bash -c`; anything that writes goes through a script
  written to `\\wsl.localhost\ubuntu\tmp\` and run with the PowerShell tool.
  Inline `$var`/`for` loops get mangled across `wsl.exe`.

## Related docs updated earlier in the session

`docs/public_testnet_readiness.md`, `docs/fault-proofs/{catalogue-status,
testing-status,execution-plan,coverage-matrix,architecture}.md` were brought
in line with the code (min-ADA split, native-script-invalid frontier,
missing-native-script-UTxO, blueprint identity, DA route, provider mode,
finality policy, the 51-script size table, fixture-drift failures in
`inspect-contracts.test.ts` and `submit-init-emulator-min-ada-v1.test.ts`).
All uncommitted.
