# Double-withdraw fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `doubleWithdraw` (`00000015`). Generic Init, deployment
inspection/identity, both mandatory reference scripts, a production runner
factory, and watcher installation are wired. Live/preprod acceptance remains
open.

## Fault statement

The family proves that one block commits two distinct payable withdrawal events
which drain the same L2 output reference. Both leaves must be tagged
`WithdrawalIsValid`; an honest non-payable duplicate does not convict.

The family is same-block only. Reuse of the same event in two blocks belongs to
`crossBlockDuplicateEvent`, while a later spend after settlement is handled by
the state-transition/input-validity machinery.

## On-chain chain

The two-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/double-withdraw/`
- `onchain/aiken/lib/midgard/fraud-proofs/double-withdraw/`

Step 01 authenticates the challenged header and first withdrawal leaf. Step 02
authenticates a second distinct leaf under the same counted root, compares the
L2 output references and payable tags, then burns the computation thread and
mints the permanent fraud-proof token. Cancellation is explicit.

## Off-chain surfaces

- SDK schema: `demo/midgard-sdk/src/fraud-proof/double-withdraw-v1.ts`
- family package: `demo/midgard-fault-proofs/src/double-withdraw/`
- preparation: `demo/midgard-fault-proofs/src/prepare-double-withdraw.ts`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

## Verification status

The emulator lifecycle suite covers conviction/removal and refusal of
same-identity, different-output, or non-payable evidence.

## Remaining work

- align the technical-spec DOUBLE-WITHDRAW wording with the implemented
  both-payable condition;
- accept the installed watcher detection/proving path end to end;
- publish live/preprod proof-through-removal evidence.
