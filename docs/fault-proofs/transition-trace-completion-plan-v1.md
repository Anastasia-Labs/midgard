# Transition-trace fault — implementation reference

Current status: implemented, registered, and locally lifecycle-tested. The
canonical category is `transitionTrace` (`00000004`). Its deployment graph is
one route validator plus eight terminal validators; it is not a linear
nine-step chain. All nine validators are mandatory authenticated reference
scripts.

## Fault surface

The family proves inconsistencies in the authenticated block transition trace,
including boundary/link failures, event-to-step mismatches, source-membership
errors, invalid one-step transitions, omitted or out-of-window L1 events,
duplicate trace events, and count faults. The on-chain proof authority is:

- `onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak`
- `onchain/aiken/validators/fraud-proofs/transition-trace/`

The route validator authenticates and classifies the proof, then dispatches to
the corresponding terminal validator. A successful terminal burns the
computation thread and mints the permanent fraud-proof token.

## Off-chain surfaces

- reconstruction, detection, witnesses, and submission:
  `demo/midgard-fault-proofs/src/transition-trace/`
- retained-DA preparation CLI adapter:
  `demo/midgard-fault-proofs/src/prepare-transition-trace.ts`
- proof submission CLI adapter:
  `demo/midgard-fault-proofs/src/submit-transition-trace-proof.ts`
- CLI registration: `demo/midgard-fault-proofs/src/bin.ts`

`prepare-transition-trace` authenticates the retained DA envelope against the
challenged header before producing buildable proof artifacts. Evidence-dependent
variants additionally require the corresponding authenticated L1-event or
ledger evidence. `submit-transition-trace-proof` decodes strict proof CBOR,
resolves required reference inputs, and drives route-to-terminal submission.

## Verification status

Focused Aiken selectors cover all omitted-event, out-of-window-event, and count
subvariants. Package tests cover reconstruction/detection, CLI routing, and
submit routing. Emulator suites cover terminal variants through permanent mint
and faulty-block removal.

## Remaining work

- mount autonomous watcher detection/proving for every evidence class;
- publish live/preprod proof-through-removal evidence;
- retain the authenticated L1/ledger evidence needed by evidence-dependent
  variants for the full challenge horizon.
