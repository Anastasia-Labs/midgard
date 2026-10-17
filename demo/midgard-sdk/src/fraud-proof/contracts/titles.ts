import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
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
  computationThreadMint: deployedTitle("computationThreadMint"),
  fraudProofMint: deployedTitle("fraudProofMint"),
  fraudProofSpend: deployedTitle("fraudProofSpend"),
  fieldPreimageCertificateMint: deployedTitle("fieldPreimageCertificateMint"),
  scriptDecodeMint: deployedTitle("scriptDecodeMint"),
  scriptDecodeSpend: deployedTitle("scriptDecodeSpend"),
} as const;
