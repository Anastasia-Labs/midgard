# Retained operator validation material

The producer retains every deterministic state and its exact work witness in
`validation_trace_witnesses`. Existing nonnegative native-execution aliases
remain available. All state records also have a chronological negative
coordinate, independent of their phase or phase-local control stage.

For descriptor `step_count = n`, there are `n + 1` states. State index `i`
uses `i - n - 1`, initial/context uses `-n - 2`, and terminal/work uses
`-n - 3`. The SDK coordinate functions enforce the existing consensus step cap
and state-index domain. The producer rejects duplicate encoded keys. All records
flow through the existing exact DA payload sizing and admission limits.

The initial endpoint record contains the initial state and its operator trace
proof, with exact validation-context bytes in `witness_cbor` and marker
`phase = -1`. The terminal endpoint contains the terminal state, its proof and
its exact deterministic terminal work bytes. Chronological records retain the
ordinary phase, program counter, work bytes and auxiliary witness.

`readRetainedValidationEndpoints` verifies the state hash, endpoint index/hash,
and Merkle proof against the selected operator descriptor. It checks context
bytes against the initial state's context hash and terminal work bytes against
the terminal state's work root. `readRetainedValidationState` performs the same
state/proof/work checks for an arbitrary chronological state. The caller must
first authenticate the descriptor's event-key and counted-root membership in
the challenged header. Metadata labels alone confer no authority. An honest
replay tree never supplies an operator membership proof.

The SDK functions are `retainedValidationStateCoordinate`,
`retainedValidationEndpointCoordinate`, `readRetainedValidationState` and
`readRetainedValidationEndpoints`. The latter two consume canonical encoded
`DaPayloadEntry` records, an exact event key and the authenticated descriptor.

Expanding retention requires consumers to select their semantic phase before
interpreting work bytes. ScriptIntegrity stage three is selected by the decoded
four-field control's stage tag, then authenticated with the state's actual
program counter. UnusedRedeemer authenticates ScriptSources and NativeScripts
work using each state's phase and deduplicates byte-identical native aliases.
Context and unrelated phase records do not enter these family predicates.

Verification includes the real node producer's ten forced-transaction tests,
SDK payload/dispute round trips, the seven UnusedRedeemer retained-material and
registered-chain cases, and both polarities of accepted/forced ScriptIntegrity
bitmap-zero lifecycles. This retention seam does not by itself install the
transitionTrace or validation-dispute durable workflow.
