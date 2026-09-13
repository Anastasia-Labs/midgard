# Batch 08 report — 11 files (demo/midgard-core, demo/midgard-validation)

Branch: colll78/canonical-v1-watcher-l1-source-checkpoint. Nothing committed, staged,
stashed, reset or checked out. Every production-file mutation used for failure evidence
was byte-backed-up to the scratchpad first and restored from that copy, verified with
`cmp` (never `git show HEAD:` / `git checkout`).

## Cross-cutting work: the nested-boundary Aiken golden channel (new)

Three of the batch files pinned Aiken-side vectors by hand and gated their main oracle
on `MIDGARD_PRINT_AIKEN_VECTOR`. There was no machine channel for them. I built one:

- NEW `demo/midgard-validation/scripts/generate-nested-boundary-aiken-goldens.mjs`
  — runs the three suites under `MIDGARD_WRITE_AIKEN_VECTOR=<dir>` and rebinds named
  top-level `const`s in the two hand-written Aiken modules via
  `rebindAikenConstants` / `goldenChannelEmitter` / `parseGoldenChannelArguments`
  from `@al-ft/midgard-core/scripts/golden-channel.mjs`. `--check` mode fails closed.
  It also throws, naming the file to repair, if the Value frontier's peak heights
  (`VALUE_FRONTIER_PEAK_HEIGHTS = [3, 4, 5, 9, 10]`) ever move.
- EDITED `demo/midgard-validation/package.json` — added
  `fixtures:nested-boundary-aiken:sync` and `fixtures:nested-boundary-aiken:check`.
- EDITED `.github/workflows/midgard-node-ci.yml` — new required step
  "Check nested boundary Aiken constants" running the `:check` script, placed after
  the ordered-collection check, with a comment recording the retired print-channel trap.
- EDITED `onchain/aiken/lib/midgard/cek-data-traverse.max-cardano.test.ak` — generated-
  constants header, `use aiken/primitive/bytearray`, 8 hoisted consts, a `tampered/1`
  helper, and the inline literals in
  `maximum_cardano_nested_data_terminal_matches_typescript` /
  `maximum_cardano_nested_redeemer_terminal_matches_typescript` replaced by them, plus a
  new `cek_data_frame_v1.encode_frame_v1(frame) == ..._terminal_frame` binding.
- EDITED `onchain/aiken/lib/midgard/ledger-output-value-v1.test.ak` — hoisted
  `typescript_maximum_value_cbor_length` / `_memory` / `_asset_count` / `_lovelace` and
  `_asset_peak_hash_0..4`; rewired `maximum_nested_value_terminal_agrees_with_typescript`.
  Both `.ak` files formatted with `/home/gumbo/.aiken/bin/aiken-fork fmt` (the default
  `aiken` binary is v1.1.19 and formats incompatibly with the v1.1.23 fork CI uses).

Channel proven to fail closed in both directions (TS-side change without regeneration →
`:check` red; Aiken constant edited by hand → `:check` red).

---

## 1. demo/midgard-validation/tests/cek-data-scan-boundary.test.ts — REWRITTEN

Contract: the V1 data scanner admits a source of at most 9,215 CBOR bytes and refuses
anything larger, and its terminal summary is the whole-tree commitment.

Changes: was 42 lines / 2 tests, now 5 tests. Added `scanSourceOfExactly` which builds a
`DataConstr(0,[DataB(...)])` of an exactly requested CBOR length and throws if the built
length is wrong (fail-closed fixture). Tests: (a) accept at exactly 9,215 with
`trace.terminal.result` deep-equal to the independent oracle `commitMidgardCekDataTree(data)`
plus offsets and the exact step-kind sequence
`["openConstructor","revealLeaf","closeSequence","foldList","finalizeFrame"]`;
(b) reject at 9,216; (c) reject the depth-2,304 unary source (9,217 bytes) _before_
structural decoding; (d) reject empty; (e) the former mock test rewritten — the expected
whole-tree root is computed from the real module _before_ mocking, then
`commitMidgardCekDataTree` is mocked to throw and the scanner must still produce the
correct summary (a behavioural assertion about on-chain boundedness, not a spy check).

Failure evidence (MUT-A1): in `src/cek-data-scan.ts`, the admitted-source bound raised by
one byte. Failing assertion: the 9,216-byte rejection case
(`expect(() => buildMidgardCekDataScanTrace(...)).toThrow(/source exceeds/u)`) —
received no throw. Restored from `scratchpad/cek-data-scan.ts.bak`, `cmp` identical, green.

## 2. demo/midgard-validation/tests/retained-da-boundary.test.ts — REWRITTEN

Contract: normal and forced retention reconstruct the same large canonical transaction.

Changes: added an independent reference model for the reveal-step count derived from
`deriveMidgardTxFieldPreimages` + `decodeMidgardFieldPreimage` + the bounded chunk size
(`MIDGARD_BOUNDED_ITEM_CHUNK_BYTES`), and replaced `revealStepCount > 0` with equality
against it. Replaced length-only reconstruction checks with digest equality
(`reconstructedCanonicalDigestHex === retainedPreimageDigestHex`) per classification,
plus cross-classification digest identity and commitment agreement.

Failure evidence (MUT-A2): in `src/field-carriage.ts`, one extra chunk emitted for large
items. Failing assertion: `expect(revealStepCount).toBe(expectedRevealStepCount(...))`
— 127,800 vs 127,799. (First attempt gated the extra chunk at `> 1_000` and fired in an
unrelated case; re-run with `> 100_000` so the boundary case is the failing check.)
Restored from backup, `cmp` identical, green.

## 3. demo/midgard-validation/tests/nested-data-boundary.test.ts — STRENGTHENED

Deleted the 30-line hand-transcribed `maximumNestedDataTerminalVector` and the
`MAXIMUM_NESTED_DATA_DATUM_TRAVERSE_STEP_COUNT = 129_311` self-pin. Replaced with a
closed-form reference model `24·leafCount + 23`, validated by running
`buildMidgardCekDataTraverseTrace` over `[1, 2, 5, 50]` leaves before applying it at the
boundary. Removed the `MIDGARD_PRINT_AIKEN_VECTOR` conditional and the trailing console
block; the terminal vector is now published unconditionally through
`publishAikenVector("nested-data-boundary-v1", …)`.

Failure evidence (MUT-A3): in `src/cek-data-traverse.ts`, one extra traverse step for
large traces. Failing assertion: the boundary step-count equality (127,799 expected,
127,800 received). Restored from backup, `cmp` identical, green.

## 4. demo/midgard-validation/tests/nested-redeemer-data-boundary.test.ts — STRENGTHENED

Same treatment; publishes `nested-redeemer-data-boundary-v1` including
`terminalFrameSequenceRootHex`. Verified the structured collateral-rejection assertion is
unconditional. Covered by MUT-A3 (shared traverse path; both suites failed).

## 5. demo/midgard-validation/tests/nested-value-boundary.test.ts — STRENGTHENED

Deleted the ~70-line `maximumNestedValueTerminalVector` (5 frontier hashes, 10 sibling
hashes, control hexes) and both env conditionals; publishes `nested-value-boundary-v1`.
The second test's `outputItemBytes` self-pin replaced by a relation to
`CARDANO_BOUNDARY_MAX_VALUE_SIZE` plus `itemBytes: outputItem.length`.
(Transient: deleting the const broke two later uses — fixed by the derived relation.)

## 6. demo/midgard-validation/tests/ordered-collection-boundary.test.ts — REVIEWED, no further change

Arrived with the inline 26-line terminal-fold vector already deleted in the working tree.
Verified the machine channel `fixtures:ordered-collection-boundary-aiken:check` is already
a CI step (`.github/workflows/midgard-node-ci.yml:272`), so the row's `by_construction`
is satisfied. The four remaining count/byte pins are derived at test time from the built
boundary and carry a stated protection reason; left in place. Re-run green.

## 7. demo/midgard-validation/tests/validation-controls-abi.test.ts — STRENGTHENED (derive-oracle)

Contract: canonical CBOR wire shapes and arities of the V11–V14 validation controls and
the 40-arm ValidationAuxiliaryWitnessV1 tag/arity corpus.

Already removed in the working tree when I arrived: the tautological
`expect(Data.to(decoded)).toBe(vector.cbor)`, the self-consistent corpus digest rebuild,
and the `"rejects adjacent tags, wrong arities, and malformed controls"` test that
exercised only test-file-local helpers (mock-verifies-mock). I confirmed that deletion is
right and that the scenario is not lost: the fixture is generated by the _production_
encoder `encodeValidationAuxiliaryWitnessCbor` in
`scripts/generate-validation-auxiliary-witness-v1-fixture.mjs`, which fails closed on a
wrong tag or a non-constructor encoding, and its `--check` mode is a required CI step
(`midgard-node-ci.yml:305`).

My change (the row's outstanding `why_low`): the four hand-transcribed
`CANONICAL_V14_CONTROL_HASHES` blake2b-256 digests are gone. They are now _derived_:
`CANONICAL_V14_CONTROL_DOMAINS` names the four normative ABI domain-separation labels and
`domainSeparatedDigest(domain, controlCborHex)` recomputes blake2b-256 over
`label ‖ canonical control CBOR` — the CBOR the same test already pins independently. The
derived values reproduced the previous transcriptions exactly (visible in the mutation run
below, where index 0's expectation is still `3dfab23f…`). Added an unconditional
anti-collision assertion: no control's digest may be reachable by hashing its encoding
bare or under a sibling's label.

Failure evidence (MUT-B1): in `demo/midgard-validation/src/cek-context.ts`,
`REDEEMER_CONTEXT_DOMAIN` dropped from `hashMidgardCekRedeemerContextControl`'s preimage.
Failing assertion:
`expect(hashes.map((value) => value.toString("hex"))).toEqual(CANONICAL_V14_CONTROL_HASHES)`
— `- "3dfab23fb96dece2…" / + "dcb9ff99b73f6042…"`. Restored from
`scratchpad/cek-context.ts.bak`, `cmp` identical, 5/5 green.

## 8. demo/midgard-core/tests/deployment-manifest-identity.test.ts — STRENGTHENED

Already removed on arrival: the 43-name transcribed catalogue category list, the catalogue
root pin, and the `manifestId` pin with its five re-pin comments (the tamper cases plus
`verifyDeploymentManifestIdentity` are the real oracle and remain).

My change: the three roster count pins (`524` / `517` / `518`) — the churn the row calls
out — replaced with structural invariants derived from the registry itself:
contract names duplicate-free; every published role has a token name; every role maps to a
registered contract; role→contract injective; token names duplicate-free; the contracts
with no reference-script role are exactly the seven core validators
(`escapeHatchSpend`, `escapeHatchMint`, `fraudProofCatalogueSpend`, `fraudProofSpend`,
`txOrderSpend`, `txOrderMint`, `settlementSpend`); and exactly one token-only role
(`V1 validation-trace CEK direct resolver`). A legitimate append now passes; a validator
registered without a publication role fails.

Failure evidence (MUT-B2): `src/deployment-manifest-identity.ts` line 996 deleted (the
`"V1 fraud-proof network-id forced scan"` role binding). Failing assertion: the
"contracts without a reference-script role" equality — received the seven core names plus
`"fraudProofNetworkIdForcedScan"`. Restored from `scratchpad/dmi.ts.bak`, `cmp` identical,
12/12 green.

## 9. demo/midgard-core/tests/consensus-profile.test.ts — STRENGTHENED

Already removed on arrival: the 27-field restatement of the profile object, ~25 transcribed
measurement literals, and the test-local re-implementation of
`selectValidationCompleteItemCarriageV1` (a rule-6 violation: it evaluated a copy of the
production rule, not the rule). The relational invariants and the CML re-measurement the
row asked to keep are intact. The deleted
`"fails closed until validator-hash-bound L1 release evidence is compiled in"` test could
not be rewritten: `assertMidgardConsensusReleaseReady` and `MIDGARD_RELEASE_EVIDENCE_DIGEST`
no longer exist anywhere in the working tree (grep across the repo returns nothing), so the
API it guarded is gone.

My change: the first test asserted only `MIDGARD_CONSENSUS_PROFILE_DIGEST` matches a hex
regex — no oracle at all after the restatement was removed. Added the two properties that
make the digest binding, without restating any field: the encoded bytes round-trip to the
whole profile (`parseJsonUnknown(encoded) == JSON.parse(JSON.stringify(profile))`), and
the digest equals blake2b-256 over exactly those bytes, recomputed with an independent
`@noble/hashes` call rather than the module's own `computeHash32`.

Failure evidence (MUT-B3): in `src/consensus-profile.ts`, the digest recomputed over
`canonicalJson(MIDGARD_CONSENSUS_PROFILE.limits)` instead of the whole encoded profile.
Failing assertion: `expect(MIDGARD_CONSENSUS_PROFILE_DIGEST).toBe(blake2b(encoded))` —
expected `fb1e4d38…`, received `2e5bbe22…`. Restored from
`scratchpad/consensus-profile.ts.bak`, `cmp` identical, 6/6 green.

## 10. demo/midgard-core/tests/cek-data-ledger-blob.test.ts — REWRITTEN

Arrived reduced to 25 lines: the `maximum === 15841` pin and the five-row transcribed
node-root table were gone, but so was the file's whole contract — nothing about
_commitment_ past the retired admission bound remained (rule 15).

Rewrote the scenario with independent oracles instead of transcriptions:

- the derived search loop and the two relational assertions are kept, plus
  `expect(maximum).toBeGreaterThan(RETIRED_SOURCE_CONSTANT_ADMISSION_BOUND)`;
- for each of 9,215 / 9,216 / `maximum`, `midgardCekDataBytesCborLength(length)` is checked
  against the length the _real_ chunked serialiser produces
  (`aikenSerialisedPlutusDataCbor(encodeCborBytes(payload))`) — a cross-check of the
  semantic model against the actual encoder, not a pinned number;
- the bytes-node root must bind content and length: neither a one-bit flip at the tail nor
  a one-byte truncation may reach the same root. No transcribed digests.

Failure evidence (MUT-B4): in `src/cek-semantic.ts`, `midgardCekDataBytesCborLength`'s
per-chunk framing changed from `2n + 64n` to `1n + 64n`. Failing assertion:
`expect(midgardCekDataBytesCborLength(BigInt(length))).toBe(BigInt(serialised.length))`
— `expected 9362n to be 9505n`. Restored from `scratchpad/cek-semantic.ts.bak`, `cmp`
identical, green.

## 11. demo/midgard-core/tests/plutus-data-deep-datum-retained.test.ts — STRENGTHENED

Already removed on arrival: the four `canonical.length` / sha256 pins (4,396 and 16,472).
`exerciseRetainedReconstruction` (`reconstructed.equals(canonical)`) remains as the real
self-checking oracle.

My change: after the pins were deleted, the file's central claim — that 4,043 is the
_exact_ maximum depth — survived only as a comment. It is now derived and asserted:
`buildSignedUnaryCandidate(4_043).length <= 16_384` and
`buildSignedUnaryCandidate(4_044).length > 16_384`. Rewrote the stale header docstring,
which still described the deleted digests.

Failure evidence:

- MUT-B5 (production): in `src/consensus-validation.ts`,
  `reconstructMidgardTransaction` fed `verified[0]` (spend inputs) as
  `outputsPreimageCbor`. Failing assertion:
  `expect(reconstructed.equals(canonical)).toBe(true)` — all 4 tests red. Restored from
  `scratchpad/cv-core.ts.bak`, `cmp` identical, green. (An earlier swap of the observers/
  signers preimages did _not_ fail — both are empty in this fixture — so I used a field
  pair the fixture actually distinguishes.)
- Controlled fault on the new boundary pair: `4_044` → `4_042` gives
  `AssertionError: expected 16380 to be greater than 16384`, confirming the boundary is
  exact at 4,043 (16,380 at 4,042, ≤16,384 at 4,043, >16,384 at 4,044). Test file restored
  from backup, `cmp` identical.

---

## Verification (all from the package directory)

demo/midgard-core:

- `npx tsc --noEmit` → exit 0
- `npx eslint tests/deployment-manifest-identity.test.ts tests/consensus-profile.test.ts tests/cek-data-ledger-blob.test.ts tests/plutus-data-deep-datum-retained.test.ts --max-warnings=0` → exit 0
- `npx prettier --check` (same four) → all match
- `npx vitest run` (same four) → 4 files / 23 tests passed

demo/midgard-validation:

- `npx tsc --noEmit` → exit 0
- `npx eslint` (the seven batch tests + the new generator, `--max-warnings=0`) → exit 0
- `npx prettier --check` (same + `package.json` + `../../.github/workflows/midgard-node-ci.yml`) → all match
- `npx vitest run tests/cek-data-scan-boundary.test.ts tests/retained-da-boundary.test.ts tests/ordered-collection-boundary.test.ts tests/validation-controls-abi.test.ts` → 4 files / 12 tests passed
- `npx vitest run tests/nested-data-boundary.test.ts tests/nested-redeemer-data-boundary.test.ts tests/nested-value-boundary.test.ts` → 3 files / 4 tests passed (38.6s)
- `pnpm run fixtures:nested-boundary-aiken:check` → checked both `.ak` modules, clean

onchain/aiken (fork binary `/home/gumbo/.aiken/bin/aiken-fork`, v1.1.23 — the default
`aiken` on PATH is v1.1.19 and formats incompatibly):

- `aiken-fork fmt` clean on both edited modules
- `aiken-fork check -m "maximum_cardano_nested"` → 4 passed / 0 failed
- `aiken-fork check -m "maximum_nested_value_terminal"` → 1 passed / 0 failed

No red results, pre-existing or otherwise, in this batch.

## Production `src/` edits

None persist. Every `src` file touched (`demo/midgard-validation/src/cek-data-scan.ts`,
`field-carriage.ts`, `cek-context.ts`, `cek-data-traverse.ts`,
`demo/midgard-core/src/consensus-validation.ts`, `consensus-profile.ts`,
`cek-semantic.ts`, `deployment-manifest-identity.ts`) was mutated only for failure evidence
and restored from a pre-mutation scratchpad copy, each verified with `cmp`. The only
non-test files changed on purpose are the two Aiken test modules, the new generator script,
`demo/midgard-validation/package.json`, and `.github/workflows/midgard-node-ci.yml`.

## Skipped / not done

- `consensus-profile.test.ts`: the release-gate refusal test could not be rewritten — its
  production API no longer exists in the working tree.
- `ordered-collection-boundary.test.ts`: no edit needed; its machine channel was already
  wired into CI.
- The row's alternative suggestion of a generated `manifestId` fixture with provenance was
  not built: `verifyDeploymentManifestIdentity` plus the existing tamper cases already
  recompute and refuse, so a second channel would add churn without added assurance.
