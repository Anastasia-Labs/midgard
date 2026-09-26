declare const tx: { complete(options: object): Promise<unknown> };

// ruleid: midgard/local-uplc-eval
// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_END

// ok: midgard/local-uplc-eval
await tx.complete({ localUPLCEval: true });

// An opening marker with no end would excuse everything after it.
// ruleid: midgard/local-uplc-eval
// MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN
await tx.complete({ localUPLCEval: false });
