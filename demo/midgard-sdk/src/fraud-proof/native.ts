import { Data } from "@lucid-evolution/lucid";

import { H32Schema, ProofSchema } from "../common.js";

export const NativeTxBodyCompactSchema = Data.Object({
  spend_inputs_hash: H32Schema,
  reference_inputs_hash: H32Schema,
  outputs_hash: H32Schema,
  fee: Data.Integer(),
  validity_interval_start: Data.Integer(),
  validity_interval_end: Data.Integer(),
  required_observers_hash: H32Schema,
  required_signers_hash: H32Schema,
  mint_hash: H32Schema,
  script_integrity_hash: H32Schema,
  auxiliary_data_hash: H32Schema,
  network_id: Data.Integer(),
});
export type NativeTxBodyCompact = Data.Static<typeof NativeTxBodyCompactSchema>;
export const NativeTxBodyCompact =
  NativeTxBodyCompactSchema as unknown as NativeTxBodyCompact;

export const NativeTxCompactSchema = Data.Object({
  body: NativeTxBodyCompactSchema,
  witness_set_hash: H32Schema,
  validity_code: Data.Integer(),
});
export type NativeTxCompact = Data.Static<typeof NativeTxCompactSchema>;
export const NativeTxCompact =
  NativeTxCompactSchema as unknown as NativeTxCompact;

export const NativeTxInclusionArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  native_tx_id: H32Schema,
  // Canonical CBOR `Data(L2TransactionSourceV1)`, exactly the transactions-root
  // MPF leaf value. This keeps compact/witness/field-length evidence atomic.
  l2_transaction_source_cbor: Data.Bytes(),
  // Raw transactions MPF root; authenticated on-chain against the header's
  // counted `transactions_root`. Positional slot must match the aiken
  // `NativeTxInclusionArgs` type.
  transactions_phas_root: H32Schema,
  tx_membership_proof: ProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
});
export type NativeTxInclusionArgs = Data.Static<
  typeof NativeTxInclusionArgsSchema
>;
export const NativeTxInclusionArgs =
  NativeTxInclusionArgsSchema as unknown as NativeTxInclusionArgs;

/**
 * Published-chunk carriage of a step's transactions-root membership opening
 * (issue #545). Positional slots mirror the aiken
 * `PublishedChunkInclusionArgs` record: identical to `NativeTxInclusionArgs`
 * up to the proof, which is replaced by the order of the publication UTxOs as
 * indices into `tx.reference_inputs`.
 */
export const PublishedChunkInclusionArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  state_queue_node_ref_input_index: Data.Integer(),
  native_tx_id: H32Schema,
  l2_transaction_source_cbor: Data.Bytes(),
  transactions_phas_root: H32Schema,
  ordered_chunk_reference_input_indices: Data.Array(Data.Integer()),
});
export type PublishedChunkInclusionArgs = Data.Static<
  typeof PublishedChunkInclusionArgsSchema
>;
export const PublishedChunkInclusionArgs =
  PublishedChunkInclusionArgsSchema as unknown as PublishedChunkInclusionArgs;

/**
 * How a step's membership opening reaches L1. Constructor order mirrors the
 * aiken `NativeTxInclusionCarriage` sum exactly: `RedeemerCarriedInclusion`
 * first (the pre-#545 route, proof in the redeemers), `PublishedChunkInclusion`
 * second (proof published beforehand, this transaction names its chunks).
 */
export const NativeTxInclusionCarriageSchema = Data.Enum([
  Data.Object({
    RedeemerCarriedInclusion: Data.Tuple([NativeTxInclusionArgsSchema]),
  }),
  Data.Object({
    PublishedChunkInclusion: Data.Tuple([PublishedChunkInclusionArgsSchema]),
  }),
]);
export type NativeTxInclusionCarriage = Data.Static<
  typeof NativeTxInclusionCarriageSchema
>;
export const NativeTxInclusionCarriage =
  NativeTxInclusionCarriageSchema as unknown as NativeTxInclusionCarriage;

/** Published-chunk carriage of a non-membership opening (issue #545). */
export const PublishedProofCarriageSchema = Data.Object({
  ordered_chunk_reference_input_indices: Data.Array(Data.Integer()),
});
export type PublishedProofCarriage = Data.Static<
  typeof PublishedProofCarriageSchema
>;
export const PublishedProofCarriage =
  PublishedProofCarriageSchema as unknown as PublishedProofCarriage;

/**
 * How a step's non-membership opening reaches L1. Constructor order mirrors
 * the aiken `NonMembershipCarriage` sum exactly.
 */
export const NonMembershipCarriageSchema = Data.Enum([
  Data.Object({
    RedeemerCarriedNonMembership: Data.Object({
      non_membership_proof: ProofSchema,
      non_membership_proof_script_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    PublishedChunkNonMembership: Data.Tuple([PublishedProofCarriageSchema]),
  }),
]);
export type NonMembershipCarriage = Data.Static<
  typeof NonMembershipCarriageSchema
>;
export const NonMembershipCarriage =
  NonMembershipCarriageSchema as unknown as NonMembershipCarriage;

/** Membership twin of `NonMembershipCarriageSchema`. */
export const MembershipCarriageSchema = Data.Enum([
  Data.Object({
    RedeemerCarriedMembership: Data.Object({
      membership_proof: ProofSchema,
      membership_proof_script_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    PublishedChunkMembership: Data.Tuple([PublishedProofCarriageSchema]),
  }),
]);
export type MembershipCarriage = Data.Static<typeof MembershipCarriageSchema>;
export const MembershipCarriage =
  MembershipCarriageSchema as unknown as MembershipCarriage;

/**
 * One published proof chunk's inline datum: nothing but MPF proof steps.
 * Mirrors the aiken `mpf_chunked_proof_v1.ProofChunkDatum`.
 */
export const ProofChunkDatumSchema = Data.Object({
  proof_steps: ProofSchema,
});
export type ProofChunkDatum = Data.Static<typeof ProofChunkDatumSchema>;
export const ProofChunkDatum =
  ProofChunkDatumSchema as unknown as ProofChunkDatum;

/**
 * Which terminal a published-chunk claim demands. Constructor order mirrors the
 * aiken `chunked_inclusion_v1.ProofTerminal` sum.
 */
export const ProofTerminalSchema = Data.Enum([
  Data.Literal("Membership"),
  Data.Literal("NonMembership"),
]);
export type ProofTerminal = Data.Static<typeof ProofTerminalSchema>;
export const ProofTerminal = ProofTerminalSchema as unknown as ProofTerminal;

/**
 * Redeemer of the merkelized `mpf_chunked_verify` withdraw validator, and the
 * exact value a delegating step requires to find in it.
 *
 * The walk lives in that one shared validator rather than in every step's own
 * script, so adding the chunked route does not enlarge the redeemer-carried
 * route it exists to remediate (issue #545).
 */
export const ChunkedProofClaimSchema = Data.Object({
  mode: ProofTerminalSchema,
  merkle_root: H32Schema,
  key_bytes: Data.Bytes(),
  value_hash: H32Schema,
  ordered_chunk_reference_input_indices: Data.Array(Data.Integer()),
});
export type ChunkedProofClaim = Data.Static<typeof ChunkedProofClaimSchema>;
export const ChunkedProofClaim =
  ChunkedProofClaimSchema as unknown as ChunkedProofClaim;

/** Blueprint title of the merkelized published-chunk verifier. */
export const MPF_CHUNKED_VERIFY_WITHDRAW_TITLE =
  "mpf_chunked_verify.verify.withdraw";

/** Blueprint title of the direct MPF non-membership verifier. */
export const PEXCLUDES_EXCLUSION_WITHDRAW_TITLE =
  "pexcludes.exclusion.withdraw";

/** The `value_hash` slot of a non-membership claim, which has no value. */
export const ABSENT_VALUE_HASH = "00".repeat(32);

/** Maximum proof steps one publication UTxO may carry (aiken-pinned). */
export const MAXIMUM_CHUNK_PROOF_STEP_COUNT = 16;

/** Maximum publication UTxOs one verifying transaction may reference. */
export const MAXIMUM_PROOF_CHUNK_COUNT = 8;

/**
 * Per-category witness commitments of a native V1 transaction, the preimage of
 * the compact transaction's `witness_set_hash`. Positional order mirrors the
 * aiken `NativeTxWitnessSetCompact` record exactly.
 */
export const NativeTxWitnessSetCompactSchema = Data.Object({
  addr_tx_wits_hash: H32Schema,
  script_tx_wits_hash: H32Schema,
  redeemer_tx_wits_hash: H32Schema,
});
export type NativeTxWitnessSetCompact = Data.Static<
  typeof NativeTxWitnessSetCompactSchema
>;
export const NativeTxWitnessSetCompact =
  NativeTxWitnessSetCompactSchema as unknown as NativeTxWitnessSetCompact;

/** One Ed25519 address witness: a verification key and its signature. */
export const MidgardAddressWitnessSchema = Data.Object({
  verification_key: Data.Bytes({ minLength: 32, maxLength: 32 }),
  signature: Data.Bytes({ minLength: 64, maxLength: 64 }),
});
export type MidgardAddressWitness = Data.Static<
  typeof MidgardAddressWitnessSchema
>;
export const MidgardAddressWitness =
  MidgardAddressWitnessSchema as unknown as MidgardAddressWitness;

export const MidgardAddressWitnessListSchema = Data.Array(
  MidgardAddressWitnessSchema,
);
export type MidgardAddressWitnessList = Data.Static<
  typeof MidgardAddressWitnessListSchema
>;
export const MidgardAddressWitnessList =
  MidgardAddressWitnessListSchema as unknown as MidgardAddressWitnessList;

export const MidgardTxInputSchema = Data.Object({
  tx_id: H32Schema,
  output_index: Data.Integer(),
});
export type MidgardTxInput = Data.Static<typeof MidgardTxInputSchema>;
export const MidgardTxInput = MidgardTxInputSchema as unknown as MidgardTxInput;

export const MidgardTxInputListSchema = Data.Array(MidgardTxInputSchema);
export type MidgardTxInputList = Data.Static<typeof MidgardTxInputListSchema>;
export const MidgardTxInputList =
  MidgardTxInputListSchema as unknown as MidgardTxInputList;

export const FaultProofStepCancelSchema = Data.Object({
  input_index: Data.Integer(),
  computation_thread_mint_redeemer_index: Data.Integer(),
});
export type FaultProofStepCancel = Data.Static<
  typeof FaultProofStepCancelSchema
>;
export const FaultProofStepCancel =
  FaultProofStepCancelSchema as unknown as FaultProofStepCancel;

type DataSchema = Parameters<typeof Data.Nullable>[0];

export const faultProofStepRedeemerSchema = <A extends DataSchema>(
  argsSchema: A,
) =>
  Data.Enum([
    Data.Object({ Cancel: FaultProofStepCancelSchema }),
    Data.Object({ Continue: Data.Tuple([argsSchema]) }),
  ]);

export const faultProofStepDatumSchema = <A extends DataSchema>(
  stateSchema: A,
) =>
  Data.Object({
    fraud_prover: Data.Bytes({ minLength: 28, maxLength: 28 }),
    data: Data.Nullable(stateSchema),
  });
