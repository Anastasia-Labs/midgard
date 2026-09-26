---
name: reviewing-consensus-changes
description: Two-pass adversarial review for changes to Midgard consensus-critical code. Use before committing or requesting review on a change that touches onchain/aiken validators or lib, a fraud-proof family, the state queue, DA bond or availability challenges, deposits, withdrawals, reserve or payout, the hub oracle, watcher fault adjudication, or a canonical codec with its TypeScript/Aiken twin and golden channel; when asked for a careful, soundness or adversarial review of such code; when deciding whether a review finding against it is real; and after a reviewer finds a gap the author missed.
---

# Reviewing consensus changes

Consensus defects in Midgard survive ordinary review because each part looks
correct on its own. Ten fraud-proof resolvers shipped as always-succeeds
scripts while every Aiken guard in them was right: the SDK applied one
parameter too few (#605, fixed in #609). One evidence sweep found 24 gates
that could not fail (#519). And a finding can look real and be wrong: "`tx_id`
does not commit `witness_set_hash`" is not a defect, because the split
between the two commitments is deliberate (see the anchoring lens). This skill
is the process that catches the first two kinds without fixing the third.

Argue from structure, not odds. A path an adversary can reach is a finding
however unlikely an honest run is to take it; an adversary chooses the
unlikely inputs.

## Scope

The skill applies when a change touches any of these:

- `onchain/aiken/validators/**` and `onchain/aiken/lib/**`;
- the fraud-proof families: `onchain/aiken/lib/midgard/fraud-proofs/**`,
  `demo/midgard-fault-proofs/**`, `demo/midgard-sdk/src/fraud-proof/**`;
- the state queue, settlement, scheduler, operator directory and hub oracle,
  on chain and in their SDK builders (`demo/midgard-sdk/src/state-queue*.ts`,
  `hub-oracle.ts`, `settlement.ts`, `scheduler.ts`);
- DA bond and availability: `availability-challenge*`, `da-attestation*`,
  `da-params-governor`, and their SDK and watcher code;
- deposits and withdrawals: `validators/user-events/**`, `reserve`, `payout`,
  `demo/midgard-sdk/src/user-events/**`, `withdrawal-*.ts`;
- watcher adjudication: `demo/midgard-watcher/src/fault-proofs/**` and
  `demo/midgard-watcher/src/verification/**`;
- canonical codecs and their twins: `demo/midgard-core/src/codec/**`, the
  `*-golden.test.ak` modules, and every `fixtures:*:check` generator;
- parameter application: `demo/midgard-sdk/src/fraud-proof/contracts/**`,
  `protocol-contracts.ts`, `state-queue-contracts.ts`.

Nothing routes a change here automatically yet `[review]`; the author or the
requester decides.

## The process

Each step ends on the condition written after "Done when".

### 1. Frame the review

Fix the base (`git merge-base HEAD origin/main`, or the commit the task names)
and produce the full diff, committed and uncommitted. List every touched
subtree and pick its invariants file from the table below. List the gates that
cover the touched paths (golden `:check` channels, execution ledgers, focused
Aiken modules, emulator suites).

Done when: the diff, the invariants files, and the gate list are written down.

### 2. Author sweep

Read the final files, not the hunks: a defect usually sits in how the changed
code composes with code the diff does not show. Walk every lens in
[references/lenses.md](references/lenses.md) against the change.

Done when: every lens has either a finding or a one-line reason it is clean.

### 3. Pass 1: a reviewer with no prior context

Hand a fresh reviewer (a new agent session, or a human who did not write the
change) exactly these, and nothing else `[review]`:

1. the diff and the base commit;
2. the matching invariants files from the table below;
3. [references/lenses.md](references/lenses.md), including its calibration
   rules;
4. the finding format below.

Never include the author's reasoning, the task's rationale, earlier findings,
or why a fix is believed correct. Any of these anchors the reviewer on the
author's model, and the author's model is what is under review.

Ask for ranked findings, and for one line per lens that the reviewer found
clean. Silence on a lens is not coverage.

Done when: every lens is answered, with findings or with a clean line.

### 4. Verify every finding before acting on it

Check each finding against the code and upgrade it, downgrade it, or reject
it with evidence `[review]`. Never adopt a finding on the reviewer's
authority: a wrong finding that gets fixed is a new defect in adversary-facing
code. Apply the calibration rules in the lens file; a finding with no trace to
a reachable outcome is dropped, and the drop is recorded with its reason.

Done when: every finding is CONFIRMED, PLAUSIBLE, or rejected with a reason.

### 5. Fix, and red-check every fix

For each fix, one regression test at the cheapest level that catches it: an
Aiken unit test for a validator predicate, an emulator scenario when the
defect only shows in a built transaction. Then red-check it `[review]`:
reintroduce the bug in a scratch copy, run the test, and watch it fail for
the predicted reason. A test that stays green with the bug back in proves
nothing.

Do the temporary revert in a scratch copy, never with `git checkout`,
`git stash` or `git restore` on the working tree: other sessions share it,
and those commands destroy uncommitted work.

- **Aiken.** Copy the tracked and new files, without `build/` or
  `plutus.json`, and run the copy's own focused-check script, which compiles
  in its own directory:

  ```bash
  # S: a fresh directory outside the repository
  S=/path/to/scratch/redcheck-<topic> && mkdir -p "$S"
  git ls-files -z --cached --others --exclude-standard \
    -- onchain/aiken .github/workflows ':!onchain/aiken/plutus.json' |
    xargs -0 cp --parents -t "$S"
  # edit "$S/onchain/aiken/..." to put the bug back, then:
  node "$S/onchain/aiken/scripts/run-focused-check.mjs" <module> <test>
  ```

  The copy needs `.github/workflows` because the script checks the compiler
  against the workflow pin `[script: onchain/aiken/scripts/pinned-compiler.mjs]`.
  The copy has no `build/`, so the first run fetches the Aiken dependencies.
  See the disposable-build rules in
  [aiken-contract-build](../aiken-contract-build/SKILL.md).

- **TypeScript.** Red-check only in a worktree no other session uses. Copy the
  fixed file to the scratch directory, put the bug back in place, run the test,
  copy the saved file back, and confirm `git diff` shows only the fix.

A negative test must fail at the check it names, not somewhere earlier. For
emulator negatives, `expectOnchainRefusal` rejects a failure that did not come
from script execution
`[runtime: expectOnchainRefusal in demo/midgard-fault-proofs/tests/support/emulator/expect-onchain-refusal.ts]`;
it cannot tell which validator check refused, so pin the trace message with a
traced build when the check matters.

Done when: every fix has a test, and the red-check output (red with the bug,
green with the fix) is recorded.

### 6. Pass 2: re-review the fix batch

Give a new reviewer the same inputs as pass 1, the full scope again, with the
fix batch named as the primary attack surface. New code is where new bugs are,
and a fix can move a defect instead of closing it. For every pass-1 finding,
ask for one verdict:

- **CLOSED**: the defect is gone and the regression test pins it.
- **DISPLACED**: the original path is closed, but the defect now appears
  elsewhere; name where.
- **STILL OPEN**: the defect is reachable as before.

Repeat steps 4 to 6 until no finding is DISPLACED or STILL OPEN, or until the
remaining ones are recorded as residual risks.

Done when: every pass-1 finding has a verdict.

### 7. Re-run the gates yourself

Run the gates listed in step 1 and read their output. A report, including a
subagent's, is a claim, not evidence `[review]`. Aiken evidence must show a
nonzero collected count; `run-focused-check.mjs` fails unless exactly the
named tests ran `[script: onchain/aiken/scripts/run-focused-check.mjs]`, and
an `aiken check -m` selector can collect zero tests and exit 0. Before
trusting any local Aiken result, confirm the compiler is the pinned fork
`[script: onchain/aiken/scripts/pinned-compiler.mjs]`.

Done when: each gate's command and its observed result are in the report.

### 8. Record residual risks

List every risk the change leaves open or widens: the defect, why it stays,
and what would close it. A residual that is a fault-proof coverage gap
belongs in [remaining-gaps.md](../../../docs/fault-proofs/remaining-gaps.md)
under that document's own closure rule; propose the entry in the report.
Never let a known residual widen without saying so.

Done when: the register is in the report, or the report says there is none.

## Invariants files

| Subtree                              | File                                                                |
| ------------------------------------ | ------------------------------------------------------------------- |
| State queue: commit, merge, removal  | [invariants-state-queue.md](references/invariants-state-queue.md)   |
| Fraud-proof families, thread, SDK    | [invariants-fraud-proofs.md](references/invariants-fraud-proofs.md) |
| Deposits, withdrawals, reserve       | [invariants-user-events.md](references/invariants-user-events.md)   |
| DA committee, attestation, challenge | [invariants-da.md](references/invariants-da.md)                     |
| Hub oracle, watcher                  | None yet; review against the lenses and the relevant spec section.  |

Read the invariants file for every subtree the diff touches. Each invariant
names the finding that earned it and is marked VERIFIED or PARTIAL; a PARTIAL
entry names what is unproven, which is where to look first. Do not relax an
invariant without understanding what it closes.

## Finding format

```text
[F<n>] <CONFIRMED | PLAUSIBLE>  <blocker | major | minor>
Where:    <path:line>, plus any other sites involved
Defect:   one sentence
Trace:    the adversary's inputs or starting state -> each step, path:line
          -> the wrong outcome (honest block removed, fraud unprovable,
          value moved, liveness lost)
Reach:    who can submit this, and why nothing earlier refuses it
Lens:     the lens or invariant it violates
Missing:  (PLAUSIBLE only) the check that would confirm or kill it
```

CONFIRMED means the trace runs end to end through code you read. PLAUSIBLE
means one link is unverified; name it.

Severity:

- **blocker**: an honest block can be removed or an honest party slashed; a
  fraudulent block cannot be proven; funds can be taken or frozen; a validator
  accepts what it must refuse.
- **major**: liveness loss on a reachable path; a transaction that cannot fit
  its budget in a supported shape; a guard that is present but cannot fail.
- **minor**: a real defect with no consensus consequence today.

If unsure between two levels, choose the lower and say why.

## Report

One report per pass: the base commit and diff scope; the findings in the
format above; the clean line for every lens; for pass 2, a verdict for every
pass-1 finding; the red-check result for every fix; the gates run, with
their results; the residual-risk register; and what you did not check.
