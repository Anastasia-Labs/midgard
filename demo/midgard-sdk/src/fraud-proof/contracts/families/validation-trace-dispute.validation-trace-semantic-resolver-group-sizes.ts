/**
 * The semantic-resolver group each prepare validator routes into, in
 * {@link VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares} order, and the
 * cardinality each validator's `expected_semantic_resolver_count` argument
 * mirrors on chain (`validators/fraud-proofs/validation-trace/<phase>-v1.ak`,
 * `lib/midgard/validation-resolver-v1.ak`). The validators no longer re-check
 * the deployed list against that count on every execution — deployment
 * parameterization is trusted on chain — so the builder asserts it here, once,
 * before the list is applied.
 */
export const VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUP_SIZES = {
  canonicalDecode: 2,
  compactBinding: 1,
  staticLedgerRules: 1,
  inputSets: 2,
  signatures: 4,
  phaseANativeScripts: 14,
  phaseAScriptPreconditions: 2,
  resolveInputs: 6,
  scriptSources: 29,
  nativeScripts: 3,
  scriptIntegrity: 4,
  cek: 4,
  valueAndMint: 11,
  ledgerDelta: 8,
} as const;
