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
} as const;

const range = (from: number, to: number): readonly number[] =>
  Array.from({ length: to - from + 1 }, (_, offset) => from + offset);

/**
 * The deployed semantic resolvers each prepare validator routes into, by
 * their index in the chain's `semanticResolvers`, in the order each prepare
 * validator's list holds them (its `semantic_resolver_index`). A one-step
 * resolution reads the resolver its step selects from here.
 */
export const VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS = {
  canonicalDecode: range(0, 1),
  compactBinding: range(2, 2),
  staticLedgerRules: range(3, 3),
  inputSets: range(4, 5),
  signatures: range(6, 9),
  phaseANativeScripts: range(10, 23),
  phaseAScriptPreconditions: range(24, 25),
  resolveInputs: range(26, 31),
  scriptSources: [...range(32, 59), 82],
  nativeScripts: range(60, 62),
  scriptIntegrity: range(63, 66),
  cek: range(67, 70),
  valueAndMint: range(71, 81),
} as const satisfies Record<
  keyof typeof VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUP_SIZES,
  readonly number[]
>;
