# Min-fee fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `minFee` (`00000013`). Generic Init, deployment
inspection/identity, and both mandatory reference scripts are wired. The
family has prepare/submit/cancel modules; autonomous watcher actuation and
live/preprod evidence remain open.

## Fault statement

For the exact canonical native-V1 transaction size and the challenged header's
non-negative fee schedule, the family proves:

```text
fee < min_fee_a * canonical_tx_size + min_fee_b
```

`onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak` is the formula
authority shared by the standalone family and validation machine. Equality is
honest and cannot convict.

## On-chain chain

The two-step chain lives under:

- `onchain/aiken/validators/fraud-proofs/min-fee/`
- `onchain/aiken/lib/midgard/fraud-proofs/min-fee/`

Step 01 authenticates the transaction and carries its compact form, fee, id,
and header fee parameters. Step 02 opens all nine authenticated fields,
recomputes the canonical transaction size and minimum fee, and finalizes only
for the strict violation. Cancellation is explicit.

## Off-chain surfaces

- SDK schema and arithmetic twin: `demo/midgard-sdk/src/fraud-proof/min-fee.ts`
- preparation: `demo/midgard-fault-proofs/src/prepare-min-fee.ts`
- contract/submit modules: `demo/midgard-fault-proofs/src/min-fee-contracts-v1.ts`
  and `demo/midgard-fault-proofs/src/submit-min-fee-*.ts`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

## Verification status

Focused tests cover preparation and exact arithmetic. The emulator suite covers
an under-fee conviction through removal, equality/overpayment refusal,
authenticated size inputs, and adversarial field carriage.

## Remaining work

- mount watcher detection and proving;
- publish live/preprod proof-through-removal evidence;
- refresh proof-fit evidence if field limits, compiler output, or protocol
  transaction limits change.
