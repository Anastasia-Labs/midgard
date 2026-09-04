# Withdraw-Zero Yielding

Use this reference when a spending validator delegates one redeemer arm's
semantic work to a rewarding validator in the same Cardano transaction. In
Midgard, call that delegation **yielding**.

## Model

Yielding is horizontal composition inside one transaction, not another
computation-thread step:

```text
one transaction
  ├─ spends the thread UTxO       → generic spending validator
  └─ withdraws zero from Script H → arm-specific rewarding validator
```

The spending validator authenticates `H` through a reference input and
requires `Script(H)` in `tx.withdrawals`. Cardano consequently creates the
rewarding purpose, and the transaction succeeds only when both validators
succeed. There is no return value or inter-script call: the ledger composes
their results by validating every script purpose in the transaction.

This lets a stable spending validator retain generic custody, cancellation,
and routing while `OpenSubject`, `BindDescriptor`, or `AdvanceOrClose` lives in
its own compiled rewarding script. Each script stays below the transaction
publication limit. Invoking the rewarding validator is part of the arm's
existing transaction, so yielding itself adds no runtime thread transaction.
Publishing the reference scripts and registering their reward accounts are
one-time deployment work.

The native-script-decoding family ultimately chose separate spending
validators for those three phases, so it is an example of the alternative
vertical split, not a repository instance of this yielding pattern.

## Yield Handshake

For a redeemer arm carrying `yield_to_index`, the spending validator performs
this handshake:

1. Resolve `reference_inputs[yield_to_index]`. An out-of-range index fails the
   transaction; `list.at` and `head(drop(...))` express the same fail-closed
   lookup.
2. Authenticate the selected output as the intended yield target by requiring
   its exact deployment NFT `(policy_id, token_name, quantity = 1)`.
3. Require `Some(rewarding_script_hash)` in the output's `reference_script`
   field.
4. Require the exact zero-valued withdrawal
   `Pair(Script(rewarding_script_hash), 0)` in `tx.withdrawals`.
5. When the delegation carries a claim, read the redeemer for the exact
   `Withdraw(Script(rewarding_script_hash))` purpose and bind it to the
   spending arm's input, output, state, and action. Ledger map semantics
   already guarantee that this purpose has at most one redeemer.

The reference input does two jobs: its NFT authenticates the deployed yield
target, and its `reference_script` field supplies the code hash that the
withdrawal must invoke. Merely supplying some script witness with the same
hash is harmless; the hash identifies the same program.

The rewarding validator then validates the arm-specific predicate against the
same `Transaction`. It may identify the thread transition through a unique
thread-NFT shape or through an explicit delegation claim. The identification
rule is part of the protocol, not an off-chain convention.

## Binding Invariants

Every yielding design must account for all of these invariants.

### Target

- The spending arm authenticates the expected deployment policy and token
  name. Prover-selected identities are valid only when an already-authenticated
  protocol value constrains them.
- The deployment NFT is unique, and the NFT-bearing reference-script output
  cannot be replaced with different code outside the protocol's explicit
  upgrade authority.
- An arm selects its corresponding rewarding validator. A yield target valid
  for another action does not satisfy the arm.

### Invocation

- The withdrawal key is exactly `Script(reference_script_hash)` and its amount
  is exactly zero. Yielding observes a script; it does not move rewards.
- The transaction contains the matching withdrawal redeemer. Ledger map
  semantics already guarantee uniqueness for the exact
  `Withdraw(Script(rewarding_script_hash))` purpose.
- `get_unique_withdraw_redeemer` is the existing filtered lookup/existence
  helper. Its singleton assertion defensively handles the pair-shaped script
  context; it does not add a separate protocol uniqueness requirement.
- Other withdrawal purposes may coexist. Locate the redeemer by exact purpose
  rather than assuming it is the transaction's only withdrawal.

### Transition

- The rewarding validator proves the predicate for the exact thread transition
  admitted by the spending validator.
- When the transaction may contain more than one compatible thread input, the
  rewarding predicate covers every admitted input or the spending side
  enforces a single yielded input. One rewarding invocation cannot silently
  discharge multiple unrelated spends.
- Phase-specific input and output datum shapes remain validated by one side of
  the handshake. Stable payment credentials do not make phase markers
  optional.

## Deployment and Liveness

For every rewarding validator:

1. Publish it as a reference script in the output authenticated by its
   deployment NFT.
2. Register `Script(rewarding_script_hash)` as a reward account before any
   transaction yields to it.
3. Record the NFT identity, script hash, and reward-account registration in
   the deployment manifest.
4. Have the transaction builder include the authenticated reference input, a
   zero withdrawal, and the rewarding redeemer.

A missing reference-script UTxO or unregistered reward account is a liveness
failure: the semantic rule may be correct while no valid yielding transaction
can be constructed.

## Repository Anchors

- `onchain/aiken/lib/midgard/common/utils.ak`:
  `get_unique_withdraw_redeemer` and the `plutarch_phas*` /
  `plutarch_pexcludes*` delegation checks.
- `onchain/aiken/lib/midgard/fraud-proofs/chunked-inclusion-v1.ak`:
  a spending-side claim bound to a merkelized rewarding validator.
- `onchain/aiken/validators/phas.ak` and `pexcludes.ak`: small rewarding
  validators invoked through withdraw-zero purposes.
- `onchain/aiken/lib/midgard/script-context-v1.ak`:
  Cardano observer representation as zero-valued script withdrawals.
- `demo/midgard-node/docs/PREPROD_DEPOSIT_SEND_WITHDRAW_BLOCKER_FIX_PLAN.md`:
  reward-account registration as an explicit deployment prerequisite.

Review is complete only when target authentication, invocation, transition
scope, deployment, and reward-account liveness are each enforced by code or a
named protocol invariant and covered by positive and substitution tests.
