# Cross-block duplicate-event fault proof — on-chain and off-chain plan V1

Status: implemented locally and emulator-proven; unregistered
`crossBlockDuplicateEvent` family. The emulator-only reserved category id is
`00000016`. Production
catalogue registration, deployment-manifest publication, and CLI verbs remain
deferred to the catalogue registration wave.

## 1. Rule and scope

`CrossBlockDuplicateEvent` means that one authenticated L1 event identity is
committed as applied by two different L2 blocks. V1 covers the two fund-moving
event domains:

- `DepositId` in `DepositsRootDomain`; and
- `WithdrawalId` in `WithdrawalsRootDomain`.

Both ids are the L1 `OutputReference` spent by event authentication. Equality
of ids is therefore equality of L1 event identity. The leaf values are still
opened because they are part of each counted-root membership hash, but the
duplicate verdict is key-only: changing the value cannot make a second use of
the same authentication nonce legitimate.

This family does not cover two different withdrawal orders spending the same
L2 output. That is the `double-withdraw` rule. Forced transaction orders are a
future, separately designed arm because their value and verdict route through
the forced-transaction adjudication machinery.

## 2. Sound adjudication boundary

V1 convicts only a live state-queue block whose event key is also present in a
confirmed settlement node:

1. Step 01 authenticates the live challenged `HeaderV1` through the hub's
   `state_queue` policy and the computation-thread asset-name suffix. It opens
   the challenged deposit or withdrawal leaf under the header's canonical
   counted root and explicit count.
2. Step 02 authenticates a settlement UTxO through the same hub's `settlement`
   policy. The settlement NFT asset name is the merged header hash, as enforced
   by `settlement.mint.Spawn`, and its datum is the roots copied by that mint
   from `MergeToConfirmedStateV1`. Step 02 requires a different header hash and
   opens the same key in the same root domain under the settlement datum's
   counted root.
3. Step 02 then finalizes the computation thread into the permanent fraud-proof
   token. The token retains the challenged live header hash, not the historical
   header hash, so state-queue removal targets the block that applied an already
   confirmed event again.

The settlement root has no separate count field. That is intentional: the
canonical root already commits `domain`, raw MPF root, and count. The historical
membership witness supplies the count and step 02 re-derives the counted root;
there is no second root convention or unbound cardinality.

This boundary does not convict one of two still-live blocks by key alone. A
malicious ancestor may include an event early while the honest descendant is
forced to include it in the due window; the live event NFT still exists in that
case, so `transition-trace/OutOfWindowSourceEvent` can identify the out-of-window
copy. Once the earlier block is confirmed, the confirmed-vs-live direction is
unambiguous and this family becomes applicable.

## 3. Evidence liveness and burn survival

The proof never references the deposit or withdrawal event NFT. Settlement may
therefore absorb and burn that NFT before or during the challenged block's
challenge window without destroying the evidence. The surviving evidence is:

- the challenged state-queue node;
- the historical settlement NFT, whose asset name binds the historical header
  hash;
- the settlement datum's counted deposit/withdrawal root; and
- membership proofs for the same canonical event key in both roots.

A settlement node can eventually resolve and burn its own NFT. A duplicate
attempted only after that point has no historical on-chain root witness in V1.
That remaining history-retention requirement is the cross-cutting evidence-
liveness remainder of D-S11 and must be solved at protocol level (for example,
retained settled-root history); it is not papered over with an unauthenticated
archive or operator database assertion.

## 4. Removed-ancestor semantics

A removed live ancestor is not confirmed evidence. Removal burns/removes its
state-queue node before any settlement NFT is spawned; step 02 consequently has
no hub-authenticated settlement witness and must refuse. Re-including that
event after removal is governed by the canonical due-window rule, including the
rule that the next block's interval covers removed intervals.

A settlement node is evidence only while its authentic NFT remains live. A
user-constructed copy of the datum, a former out-reference after resolution,
or a token under another policy is not accepted. Confirmed settlement history
is otherwise final, so the family does not attempt an ancestor-removal branch.

## 5. Off-chain evidence and submission

`prepare-cross-block-duplicate-event` consumes only security-grade inputs:

- an authenticated observation of the challenged state-queue header plus its
  retained public DA payload; and
- an authenticated live settlement UTxO observation, including its NFT asset
  name and inline datum.

Preparation rebuilds the challenged source trie and counted root from retained
DA, checks the header root and count, decodes the settlement datum, finds an
equal key in the same domain, and builds both MPF membership proofs. It rejects
different keys, different domains, equal header hashes, non-security provenance,
root/count mismatches, missing settlement authentication, and evidence from a
removed or resolved ancestor.

The submit path is `init -> step-01 -> step-02`, with explicit `cancel` from
either step and deterministic resume from the live thread UTxO. Every step spend
must use a published, hash-checked reference-script UTxO; there is no inline
validator fallback. Step 01 reads the hub and challenged state-queue node.
Step 02 reads the historical settlement UTxO and uses the shared fraud-proof
mint/finalize machinery.

Production registration is intentionally absent: do not add the family to
`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`, supported production category unions,
deployment manifests, or production inspection gates. Tests register reserved
id `00000016` as an extra emulator category, apply the real chain, publish both
step validators as reference scripts, and pass an explicit category record to
the category-independent removal submitter.

## 6. Acceptance gates

On-chain exact selectors must collect nonzero tests for:

- positive deposit membership at step 01 and positive confirmed duplicate at
  step 02;
- positive withdrawal membership at step 01 and positive confirmed duplicate
  at step 02;
- wrong challenged root/count/category refusal;
- honest/different-event refusal;
- cross-domain refusal;
- same-header refusal; and
- removed-ancestor/untrusted-settlement refusal.

SDK and preparer tests pin constructor order and CBOR, the `00000016 || H28`
thread name, counted-root reconstruction, both variants, and all fail-closed
admission rules. Exact Aiken selectors cover the honest/different-event,
cross-domain, same-header, forged-root, and removed-ancestor refusals. Emulator
gates cover complete deposit and withdrawal lifecycles through fraud-proof mint
and fraudulent-block removal, mandatory reference scripts, cancellation at
each step, and resume from each continued thread state. Formatting, lint,
TypeScript tests, and an Aiken build/check with a nonzero collected total are
required before the family can be reported implemented.
