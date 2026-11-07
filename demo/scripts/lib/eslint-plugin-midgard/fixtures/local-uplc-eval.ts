declare const tx: { complete(options: object): Promise<unknown> };
declare const enabled: boolean;
declare const options: { localUPLCEval?: boolean };
declare const target: { localUPLCEval?: boolean };

// ruleid: midgard/local-uplc-eval
await tx.complete({ localUPLCEval: false });

// ruleid: midgard/local-uplc-eval
await tx.complete({ localUPLCEval: enabled });

const localUPLCEval = true;
// ruleid: midgard/local-uplc-eval
await tx.complete({ localUPLCEval });

// A default does not constrain what the caller passes.
// ruleid: midgard/local-uplc-eval
const { localUPLCEval: fromCaller = true } = options;

// ruleid: midgard/local-uplc-eval
target.localUPLCEval = fromCaller;

// ruleid: midgard/local-uplc-eval
await tx.complete({ ["localUPLCEval"]: false });

// ok: midgard/local-uplc-eval
await tx.complete({ localUPLCEval: true });

// ok: midgard/local-uplc-eval
await tx.complete({ localUPLCEval: true as const });

// ok: midgard/local-uplc-eval
export type Options = { localUPLCEval: boolean };

// ok: midgard/local-uplc-eval
export const quoted = "complete({ localUPLCEval: false })";

// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN
// ok: midgard/local-uplc-eval
await tx.complete({ localUPLCEval: false });
// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END
