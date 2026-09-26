# Mistakes we make

Each entry is a commit in this repository's history. It says what the missing
or wrong test was and the rule it teaches. Read the group that matches what
you are about to write. The short sha opens the full account:
`git show <sha>`.

## Green because nothing ran

- **`8a4b87707`, `f714e1045`: focused Aiken selectors collected zero tests
  and exited 0.** A published evidence selector named 17 tests and ran none
  under a green gate. A sweep found 48 of a manifest's 100 selectors
  collecting nothing: a `midgard/` prefix on `validators/` modules, dotted
  module names split at the first dot, and bare module selectors. _Rule:_ read
  the collected count, and run focused checks through
  `onchain/aiken/scripts/run-focused-check.mjs`, which fails on a count
  mismatch.
- **`ba238d6b4`: a dotted-module selector that could never pass.** The day
  before the guard landed, a declared selector for tests in
  `native_tx.max_redeemers.test` collected zero tests, because `-m` splits a
  pattern at its first dot. _Rule:_ the wrapper, not care in spelling the
  selector, is what catches this.
- **`1afa5c99f`: a focused vitest run could find no files.** The node
  suite's eight-way extension brace in `include` did not reliably match a
  focused `*.test.ts` filter, so a release-gate run could report "No test
  files found". _Rule:_ check that the file you named actually ran; the
  summary's file and test counts are the evidence.
- **`f85990363`: a committed `bail: 3`.** A red run stopped after three
  failures with about 133 of 153 node test files never executed, so "3
  failed" hid everything else. _Rule:_ never commit `bail`; it is opt-in per
  run through `MIDGARD_NODE_TEST_BAIL`.
- **`f9310109b`: a detached `aiken check` is not evidence.** On the pinned
  v1.1.22 of that date, a run without a terminal reproduced a silent-exit
  pathology; only a pseudo-terminal run counted. _Rule:_ a run whose output
  you did not read to the summary line is not a result.

## Green whatever the code does

- **`7d01f2b71`: `test ... fail` over a four-conjunct `and {}`.** It went
  green as soon as any conjunct was false, so it could not tell a legacy
  field substitution from an unauthenticated proof. Measurement showed three
  of the four conjuncts false. _Rule:_ one disposition per rejection test,
  and the rejection must come from the check you mean.
- **`9b3cca8db`: a mint-path guard with no killing test.** Deleting the
  path's `expect` left all 12 selectors green. The fix added an accepted
  control and a one-field-changed twin; with the `expect` removed, exactly 1
  of 14 then fails. _Rule:_ delete the guard as a temporary edit; some test
  must go red.
- **`bbb95f7da`: split suites that never touched the shared guards.** Four
  mutations of guards shared across step kinds survived. Mutation-killing
  negatives now pin three; the fourth stays alive because an encoder upstream
  refuses the same input first, which the commit records. _Rule:_ when a
  mutation survives, either add the negative that kills it or write down why
  it cannot die.
- **`59b652062`: `.every` over a list bounded only from above.** Any strict
  subset of the configured providers satisfied the finality predicate
  vacuously, and finality was granted with unbound providers never checked.
  The adversarial strict-subset cases fail against the pre-fix module with
  their named outcomes. _Rule:_ an "all of these" check needs a test where
  the list is shorter than the configured set.
- **`1b53eafd8`: a decision gate that production never reaches.** The
  watcher's `replayContext === undefined` gate was vacuous because the launch
  scope always attaches a context. After the fix, disabling the new gate
  fails exactly the eight refusal cases. _Rule:_ test the gate with the
  inputs production actually passes, not the ones that make it fire.
- **`f0d6565d7`, `ac54d01a1`: a hash pin that blessed a broken deployment.**
  Ten fraud-proof resolvers were applied one parameter short, which under
  Plutus V3 makes them always-succeeds scripts. The applied-hash pin pinned
  the under-applied hashes, so it "verifies the broken deployment against
  itself". An emulator soundness test was what caught it. _Rule:_ an expected
  value taken from the artifact under test is not a check; pair every pin
  with a behavioral negative. `fa52b3844` later replaced the arity gates with
  exactly that: emulator scenarios in both polarities.

## Expected values or fixtures that share the bug

- **`ba238d6b4`: one-sided cross-language agreement.** Aiken pinned the
  exact maximum terminals for three fields while the TypeScript boundary
  searches asserted only relative properties, so a drift in the derived
  maximum would not have been caught on either side. Separately, an
  exact-rejection control was unsatisfiable for those fields because the
  fixture fell through to the unchanged encoding. _Rule:_ pin absolute
  boundaries in both languages, and prove each control can be satisfied.
- **`d04f17f8e`: a test added without ever running, with a hand-written
  key.** The literal did not match what the producer builds. The fixture now
  derives the key from its own inputs, with two negative controls so the
  repaired positive cannot be vacuous. _Rule:_ derive expected values from
  the fixture's inputs; run a new test before committing it.
- **`3525d57ba`: a hand-written "maximum" that was not the maximum.** The
  wide payload was 5,443 leaves; the derived maximum is 5,445. _Rule:_ derive
  boundaries from the formula the code uses.
- **`d012905b5`: degenerate inputs hid a production defect.** Six test sites
  used an empty claimed delta, the one input on which the defect does not
  show. They were rebased onto realistic non-empty roots, keeping one
  labelled empty case. _Rule:_ realistic inputs by default; degenerate ones
  labelled and few.
- **`bdcb7d16d`: the fixture was built the way the buggy code builds.** The
  challenger's fixture trie was built from full output bytes, the same stale
  shape as the code under test, so the contradiction with the validator never
  surfaced. It is now built from the descriptor the validator derives, with a
  refusal and an acceptance case. _Rule:_ build fixtures the way the
  consumer, usually the validator, derives them.

## Doubles and environments that are not the real thing

- **`b4a427d61`, `9eabcdc0c`: test doubles answered a shape Ogmios never
  sends.** Ogmios v6 `queryNetwork/tip` carries a slot and block id only. The
  code required a height there, and so did every double, so tests passed
  while the history follower lost its source within one heartbeat against a
  real node. _Rule:_ take a double's responses from the real service or its
  schema, including what it omits.
- **`1e1414b8c`: Kupo v2.11 serves a null redeemer.** Its API schema allows
  it; the parser rejected it and the correction observer failed every pass.
  _Rule:_ doubles cover the nullable and missing fields the schema permits.
- **`92e46f8b5`: a red blamed on the environment was the double's own
  defect.** The fixture's stub returned a quoted ETag the source rejects and
  never dispatched a socket close event; the red had been attributed to a
  missing Kupo. The same commit found a suite defaulting to Postgres port
  5432 instead of the workspace's 5433. _Rule:_ before blaming the
  environment, read the assertion that failed.
- **`11c5bf548`: gates ran against a stale `dist/`.** The watcher gates ran
  against a fault-proofs build that predated the change, hiding a fixture
  defect. _Rule:_ rebuild what the suite imports before trusting its result.
- **`238fc45fb`, `aa68fdeeb`: a blueprint that could be silently stale, and
  a CI job that never built it.** A tracked `plutus.json` let a checkout read
  an old blueprint without complaint; it is now untracked so a missing one
  fails loudly. The watcher workflow had never built the blueprint its tests
  read and failed with ENOENT on every run. _Rule:_ build artifacts are
  rebuilt from source, never committed, and every job that reads one builds
  it.
- **`3e2c712ca`: a prerequisite only the dedicated script built.** A fresh
  worktree failed two node files on a missing native binary. Global setup now
  builds it, and the consumers skip with a printed reason when it is still
  absent. _Rule:_ a missing prerequisite is a loud skip or a failure, never a
  silent pass.
- **`50218f738`: a result that depended on the host locale.** Canonical JSON
  sorted keys with a bare `localeCompare`; `["sz", "st"]` sorts differently
  under `et` than under `en`. The test uses exactly that pair. _Rule:_ when
  output may depend on the host, test with an input that shows the
  difference.
- **`967b1698b`: the emulator accepted what the ledger refuses.** The
  emulator did not enforce input and reference-input disjointness, so the
  negative "a registration cannot prove its own duplication" could only be
  written after bumping lucid-evolution to 0.6.5. _Rule:_ when the emulator is
  more permissive than the ledger, update it; a negative the emulator cannot
  refuse proves nothing.

## Emulator time

- **`b27a8d5c8`: a window ahead of the emulator's clock.** A successor's
  commit window opened one header length after the previous commit, ahead of
  the emulator clock. In the same fixture, the wallet helper handed out the
  reference-script UTxO as a fee input, producing the self-contradicting
  "expected one, found 1". The fix advances the emulator to the window's
  first slot before building and selects a plain-Ada input. _Rule:_ drive time through the emulator; never assume it
  has moved.

## Tests and gates that assert the wrong thing

- **`e7b91208e`: a test stricter than the production contract.** It asserted
  that the first claim attempt found the admission, which the only
  production caller treats as an ordinary retry on the next tick, so it
  failed transiently. The fix is a bounded poll that still fails hard with a
  state dump. _Rule:_ assert the contract,
  and do not loosen the assertion to hide a real failure.
- **`b9d4cb266`: a comment tripped a text-scan gate.** The comment was
  reworded rather than adding the file to the allowlist, which would have
  masked a real read added there later. _Rule:_ fix the trigger, keep the gate
  precise.
- **`6dae2cf6e`: loosening a verifier to let a claim pass.** A bookkeeping
  promotion made a reconciliation verifier fail closed. The promotion was
  reverted; loosening either guard was rejected as the gate-that-cannot-fail
  class. _Rule:_ when a verifier refuses a claim, change the claim or make a
  coordinated change, never weaken the verifier.

## When a unit test is the wrong tool

- **`8569e6bff`: a one-block tip race between Kupo and Ogmios.** Kupo indexes
  a block shortly after the node adopts it, so one read of both tips can
  straddle a block. The committee persisted a terminal quarantine on that
  race. It was found on live services; the fix added a bounded retry and
  provider tests for it (`demo/da-committee-node/tests/provider.test.ts`).
  _Rule:_ timing between real services surfaces at level 6; once understood,
  pin it lower with a double that reproduces the straddle.
