# Immutable forced submissions with one operator verdict

Status: Accepted; implementation and verification in progress, not deployed.
Recorded: 2026-09-10.
Scope: replacement of the undeployed forced-submission format under canonical V1.

## Context and decision

The L1 order authenticates a user submission, while the operator later classifies
it. The current forced leaf embeds both a validity-bearing native proof source
and `OperatorVerdictV1`. Its consumers reconcile those claims by equality checks
or by rewriting the compact validity byte. This changes the proof-source
commitment on rejection even though the transaction body and `tx_id` are stable.
The existing settlement helper copies the L1 source unchanged, whereas the
operator and DA paths adjudicate that source. Those boundaries need one identity.

Use a forced-specific submitted source with no validity field, including in its
nested compact and full encodings. Keep `OperatorVerdictV1` as the sole committed
operator decision. Construct the block leaf from the unchanged submission and
that verdict. Keep the existing L1 order key and transaction-ID derivation.

The canonical encoding, domain-separated commitment and source-kind rules are
specified in [the transaction specification](../../spec/midgard-tx.md).
The [Task 1 handoff](../../exec-plans/forced-inclusion-verdict/01-protocol-design.md)
retains the integration inventory and acceptance matrix; this ADR owns the
rationale. The coordinated Task 2 change updates the component and technical
specifications alongside implementation. This decision supersedes the former
forced-source bit-equality requirement without changing normal admission.

## Domain terms

| Term                           | Meaning                                                                                                                                             |
| ------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| Submitted forced transaction   | The immutable user-authored body and witnesses, with their authenticated compact source and body-derived transaction ID; no validity classification |
| Forced transaction verdict     | The operator's claim: `ForcedTxValid` or `ForcedTxInvalid(reason)`; reuse `OperatorVerdictV1` and its current reason constructors                   |
| Adjudicated forced transaction | The existing `ForcedInclusionTxV1` leaf combining that submission and verdict; no additional persisted wrapper                                      |
| Actual execution result        | The validation machine's independently computed result at the authenticated execution position and pre-state                                        |

The verdict is authoritative for what the operator claims, not for what execution
must conclude. A challenge may compute the opposite result while retaining the
same submission commitment and the operator's original disputed ledger delta.

## Consequences

- Source authentication, field openings, and machine controls bind the immutable
  forced-source commitment. Leaf membership separately authenticates the verdict
  and order. Transaction ID alone does not authenticate witnesses.
- The machine and forced replay use validity-free execution content. No new
  `EffectiveTransactionView` or derived scalar is needed in this implementation:
  existing semantic consumers use body/witness fields, and forced proof polarity
  moves directly to the authenticated verdict. Normal-only validity checks remain.
- Replace the writable SQL `operator_validity` projection with reads derived
  from the stored typed verdict. Preserve submission and classification lifecycle
  status separately; pending is not a third verdict constructor.
- Preserve the existing ledger fee/capability size for equal transaction content.
  The one-byte validity slot contributes a constant size charge, with no scalar
  value constructed or committed. Physical forced-submission/DA bytes are measured
  independently. This keeps the schema repair from silently changing fee eligibility.
- Preserve rejection constructor order, reason coordinates, the non-injective
  reason-to-code bridge, and existing execution/reason adjudication policy.
  A coarse machine rejection code never reconstructs a unique typed reason.
- Update forced full preimages as well as compact sources in DA, journals, SDK,
  watcher, proof reconstruction, and order material recovery. A dedicated client
  conversion may construct a new submission before L1 commitment; downstream
  consumers never normalize or rewrite committed bytes.
- Replace the prelaunch format in place, retaining version 1. Add an explicit
  forced-source encoding identifier to the hashed consensus profile, rebuild
  affected validators and manifests, and reject old deployment identities. No
  compatible decoder, inferred discriminator, or in-place durable-state reset is
  part of this change. A persistent development deployment requires its own
  explicit new deployment/state lifecycle under the [reset rules](../../agents/state-reset.md).
- Existing proof-size and execution evidence becomes stale where source,
  compiler, parameters, or applied validator identity changes. Fresh measurements
  and both challenge directions remain required. The documented variable-width
  order-mint capacity gap is an outstanding acceptance dependency, not resolved
  by changing the source encoding.

## Alternatives

Keeping the bit and enforcing equality leaves two independently writable claims
and makes every consumer enforce the same relationship. Normalizing the source
can repair a local mismatch, but makes the L1 identity differ from the block's
source identity on rejection. Both are rejected as the long-term format.

Removing validity from all native transactions would unnecessarily change normal
admission and its mistag proofs. Keeping native transaction bytes as forced DA
preimages would retain a second validity field in authenticated material. The
chosen boundary preserves normal semantics while giving forced submissions one
unambiguous meaning.

A temporary validity-bearing execution view would be justified only by a real
consumer requiring it. Current forced semantics can consume body/witness content
and an authenticated verdict directly, so serializing an adapted native source
would add machinery without serving a protocol requirement.
