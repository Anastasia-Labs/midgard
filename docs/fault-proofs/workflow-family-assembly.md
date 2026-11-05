# Manifest-bound family assembly

Each derived record in `FAMILY_APPLICATION_REGISTRY`
(`demo/midgard-fault-proofs/src/workflow/family-application-registry.ts`)
carries the definition it is built from: 16 linear and 30 cursor definitions as
of 2026-11-04. Each declares its datum schemas, published step and
witness scripts, certificate requirement, transaction port, complete replayer,
and ordered field/raw-datum and proof-chunk prerequisites.

The shared assembly owns deployment and signer binding, exact published
reference validation, authenticated L1 observations, adapter construction,
prerequisite decoration, terminal verification, and release-finality authority.
Auxiliary references cover family-specific yield and removal scripts and receive
the same manifest out-ref and script checks as step references.

`assembleManifestBoundFamilyWorkflow` binds and assembles in one call. Families
that need fresh recovery material use `bindManifestBoundFamilyWorkflow` once and
`assembleBoundManifestBoundFamilyWorkflow` for each invocation. A bound context
belongs to the exact definition that admitted it. Invocation inputs and caches
remain local to the assembled run; field prerequisites expose the same handle to
both the transaction port and adapter decoration.

Historical families retain their public-DA reconstruction, authenticated L1
boundaries, and corpus-digest checks across resume. Their complete replayers are
created through the admitted replay factories and resolve history only after the
workflow has derived it. Saved-intent recovery continues through
`executeManifestBoundFamilyRecovery`, retaining decision-digest and actuation
checks. The generic retained-DA runner does not replace those guards.

The table-driven assembly tests exercise every registered definition, including exact
reference-script rejection and prerequisite wiring. Family recovery and emulator
suites remain necessary: assembly coverage does not establish transaction fit,
protocol lifecycle acceptance, or live deployment readiness.

Authenticated reference publication in emulator fixtures is owned and paid for
by the deployment funding wallet. Its reserved deployment nonce stays unspent
through publication. A separate prover can consume those reference scripts;
publication does not require the prover's signature.
