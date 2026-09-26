// fixture-path: midgard-sdk/src/fraud-proof/example-carriage.ts
declare const tx: any;
declare const validator: unknown;

// ruleid: midgard/fault-proof-reference-scripts-only
tx.attach.WithdrawalValidator(validator);

// ok: midgard/fault-proof-reference-scripts-only
tx.readFrom([validator]);
