# Installed transition trace replay

The watcher installs `transitionTrace` in canonical catalogue order. Its
manifest-bound constructor checks all nine spending scripts, fifteen rewarding
scripts and three shared witnesses before starting a computation thread.
The accepted/deposit cuts use authenticated yields and bounded continuations;
[the resolver decision](decisions/0003-publishable-semantic-resolvers.md) records
the decomposition, and the executable lifecycle scenarios below own its measurements.

## Replay authority

`createTransitionTraceEventAuthority` binds a local raw L1 observation source
to the deployment. Each classification captures the authenticated hub oracle,
its exact event addresses, address coverage and complete histories for the
discovered event NFTs. Copied handles and caller-authored event verdicts have
no authority. Every committed L1 event requires matching retained raw coverage;
missing coverage fails rather than producing a healthy verdict.

`replayTransitionTraceFromRetainedHistory` requires that handle and an admitted
historical corpus tied to the exact current canonical evidence object. The
predecessor trie contains canonical output descriptors under fixed-index spend
keys. Replay derives membership, nonmembership, deletion and insertion proofs
from that trie. It stops at the first false transition because later claimed
pre-roots no longer describe the honest ledger.

Normal and forced operator acceptance claims open the exact retained endpoint
states, validation context and terminal work witness against the operator's
counted validation trace. Honest replay never constructs operator membership.
The retained chronological states use the coordinates documented in
[retained-validation-trace.md](retained-validation-trace.md).

## Durable transitions and publications

`runOrResumeManifestBoundTransitionTraceWorkflow` reopens the history and raw
L1 authority on every invocation. The journal retains current and predecessor
payload envelopes, exact selected proof, selected event out-ref and raw L1
snapshot. These bytes support recovery and audit; fresh admission supplies the
replay authority. A different selected proof, source, event or header refuses
resume.

The route selects one of eight finals. Accepted and deposit finals retain their
thread NFT while scanning; the cursor admits only the registered successor or
the permanent proof token. Proof/output byte publications use fixed 4,096-byte
chunks. Typed datum evidence uses the shared structured Data carrier, preserving
original map order. Each publication has its own signed intent and authenticated
L1 recovery before the consuming step can proceed. Funding excludes datum-bearing
outputs, script references and tokens so later evidence cannot be spent as fees.

After removal, the live header observation remains unavailable. Recovery uses
`observeRetainedHeader`, which opens the unique NFT mint and exact state-queue
output from release-final history and verifies the linked-list key and header
hash. It does not assert that a removed header is live. The existing raw L1
terminal verifier still checks the permanent proof and removal economics.

## Verification

Run the installed lifecycle, direct final, and forced-source subvariant scenarios
against the current normal testnet blueprint. The installed test recreates the
constructor and directory journal after pending boundaries and advances an
emulator block before reconciliation. Positive journeys must reach permanent
proof and removal; honest/substituted evidence must refuse.

- [Installed lifecycles](../../demo/midgard-fault-proofs/tests/transition-trace-installed-lifecycle.test.ts)
- [Direct final lifecycles](../../demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-final.test.ts)
- [Forced-source subvariants](../../demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-subvariants.test.ts)

Capture new publication and execution measurements for the release artifact.
Direct maxima and installed recovery exercise different obligations; neither
replaces the other. [Remaining acceptance](execution-plan.md) covers real-node
and preprod correction.
