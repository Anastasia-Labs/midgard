// fixture-path: midgard-fault-proofs/src/example-step.ts
declare const tx: any;
declare const validator: unknown;
declare const referenceScriptUtxo: unknown;
declare const attach: (script: unknown) => unknown;

// ruleid: midgard/fault-proof-reference-scripts-only
tx.attach.SpendingValidator(validator);

// ruleid: midgard/fault-proof-reference-scripts-only
tx.collectFrom([]).attach.MintingPolicy(validator);

// ruleid: midgard/fault-proof-reference-scripts-only
tx.attach.Script(validator);

// ok: midgard/fault-proof-reference-scripts-only
tx.readFrom([referenceScriptUtxo]);

// ok: midgard/fault-proof-reference-scripts-only
attach(validator);
