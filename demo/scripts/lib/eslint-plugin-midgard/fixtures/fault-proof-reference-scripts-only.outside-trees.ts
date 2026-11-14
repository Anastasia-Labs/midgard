// fixture-path: midgard-node/tests/example-emulator.test.ts
declare const tx: any;
declare const validator: unknown;

// Only the fault-proof source trees are in scope.
// ok: midgard/fault-proof-reference-scripts-only
tx.attach.SpendingValidator(validator);
