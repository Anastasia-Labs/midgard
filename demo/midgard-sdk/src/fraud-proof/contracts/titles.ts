/**
 * Blueprint titles shared across families.
 */

export const VALIDATION_TRACE_RESOLVER_COUNT = 14;

/**
 * Deployed interactive-chain step count for validationTraceDispute: the six
 * control validators, the proof item, every semantic resolver, the shared
 * ScriptSources item chain, the CEK context and item stages, the canonical
 * decode item stages, and the fourteen prepare resolvers. The family builder
 * asserts the composed `steps` array has exactly this length, so this pin can
 * never drift from the real composition; downstream deployment tooling must
 * derive its expectations from this constant rather than re-pinning a number.
 */
export const VALIDATION_TRACE_DISPUTE_STEP_COUNT = 177;

export const FAULT_PROOF_SHARED_TITLES = {
  computationThreadMint: "computation_thread.mint.mint",
  fraudProofMint: "fraud_proof.mint.mint",
  fraudProofSpend: "fraud_proof.spend.else",
  fieldPreimageCertificateMint:
    "field_preimage_certificate.field_preimage_certificate.mint",
  scriptDecodeMint: "script_decode_v1.script_decode_v1.mint",
  scriptDecodeSpend: "script_decode_v1.script_decode_v1.spend",
} as const;
