# Min-fee fault — implementation reference

Current status: implemented, registered, and emulator-proven. The canonical
category is `minFee` (`00000013`). Generic Init, deployment
inspection/identity, both mandatory reference scripts, a production runner
factory, and watcher installation are wired. The family has
prepare/submit/cancel modules; live/preprod acceptance remains open.

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

Step 01 authenticates either accepted inclusion or the exact forced leaf with
`FeeBelowMinimum`, carrying the canonical verdict subject, compact transaction,
fee, id, and header schedule. Step 02 authenticates all nine lengths and
recomputes the canonical minimum. Accepted-invalid requires `fee < minimum`;
wrongful rejection requires `fee >= minimum`. The forced branch reads lengths
through the authenticated raw-preimage door, so variable-field item grammar
never adds an unbounded scan. Cancellation is explicit at both steps.

Wave 9 wrongful-rejection evidence and six complete maximum-shape lifecycles
are recorded in `size-plans/min-fee-wrongful-rejection-v1.md` and its fit ledger.
The durable workflow reopens exact header/root/count/MPF/source/reason evidence
on restart and plans all field/chunk/certificate publications before terminal
submission.

## Off-chain surfaces

- SDK schema and arithmetic twin: `demo/midgard-sdk/src/fraud-proof/min-fee.ts`
- preparation: `demo/midgard-fault-proofs/src/prepare-min-fee.ts`
- contract/submit modules: `demo/midgard-fault-proofs/src/min-fee-contracts.ts`
  and `demo/midgard-fault-proofs/src/submit-min-fee-*.ts`
- catalogue: `demo/midgard-sdk/src/fraud-proof/catalogue.ts`

## Verification status

Focused tests cover preparation and exact arithmetic. The emulator suite covers
an under-fee conviction through removal, equality/overpayment refusal,
authenticated size inputs, and adversarial field carriage.

## Remaining work

- accept the installed watcher detection/proving path end to end;
- publish live/preprod proof-through-removal evidence;
- rerun maximum-shape lifecycles under the shared Van Rossem emulator limits if
  field limits, compiler output, or protocol transaction limits change.
