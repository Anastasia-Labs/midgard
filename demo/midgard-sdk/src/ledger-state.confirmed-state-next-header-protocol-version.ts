import { MIDGARD_PROTOCOL_VERSION } from "@al-ft/midgard-core/consensus-profile";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressSchema,
  H32Schema,
  hashHexWithBlake2b,
  HashingError,
  MerkleRootSchema,
  OutputReferenceSchema,
  POSIXTimeSchema,
} from "./common.js";
import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
} from "./ledger-constants.js";
import { Header, HeaderHashSchema } from "./ledger-state.header-schema.js";
import { encodeHeaderCbor } from "./ledger-state.validate-header-transition-commitments-program.js";
import { OperatorVerdictSchema } from "./rejection-reason.js";

export const hashBlockHeader = (
  header: Header,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(encodeHeaderCbor(header).toString("hex"), 28);

export const ConfirmedStateSchema = Data.Object({
  headerHash: HeaderHashSchema,
  prevHeaderHash: HeaderHashSchema,
  utxoRoot: MerkleRootSchema,
  startTime: POSIXTimeSchema,
  endTime: POSIXTimeSchema,
  protocolVersion: Data.Integer(),
});

export type ConfirmedState = Data.Static<typeof ConfirmedStateSchema>;

export const ConfirmedState = asDataType<ConfirmedState>(ConfirmedStateSchema);

export const castConfirmedStateToData = (
  confirmedState: ConfirmedState,
): unknown => Data.castTo(confirmedState, ConfirmedState);

export const makeGenesisConfirmedState = (
  genesisTime: bigint,
): ConfirmedState => {
  if (genesisTime < 0n) {
    throw new Error("Genesis confirmed-state time must be non-negative");
  }
  return {
    headerHash: GENESIS_HEADER_HASH,
    prevHeaderHash: GENESIS_HEADER_HASH,
    utxoRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: genesisTime,
    endTime: genesisTime,
    protocolVersion: GENESIS_PROTOCOL_VERSION,
  };
};

/**
 * Authenticates the only two protocol identities a V1 confirmed-state root may
 * carry. Genesis is a distinct sentinel state; every committed state is V1
 * and must have left the all-zero genesis header identity.
 */
export const confirmedStateNextHeaderProtocolVersion = (
  confirmedState: ConfirmedState,
): bigint | null => {
  const protocol = BigInt(MIDGARD_PROTOCOL_VERSION);
  const isGenesis =
    confirmedState.protocolVersion === GENESIS_PROTOCOL_VERSION &&
    confirmedState.headerHash === GENESIS_HEADER_HASH &&
    confirmedState.prevHeaderHash === GENESIS_HEADER_HASH &&
    confirmedState.utxoRoot === EMPTY_MERKLE_TREE_ROOT &&
    confirmedState.startTime >= 0n &&
    confirmedState.startTime === confirmedState.endTime;
  if (isGenesis) {
    return protocol;
  }

  const isOrdinary =
    confirmedState.protocolVersion === protocol &&
    confirmedState.headerHash !== GENESIS_HEADER_HASH &&
    confirmedState.startTime >= 0n &&
    confirmedState.startTime <= confirmedState.endTime;
  return isOrdinary ? protocol : null;
};

export const CardanoDatumSchema = Data.Enum([
  Data.Literal("NoDatum"),
  Data.Object({
    DatumHash: Data.Object({
      hash: Data.Bytes(),
    }),
  }),
  Data.Object({
    InlineDatum: Data.Object({
      data: Data.Any(),
    }),
  }),
]);

export type CardanoDatum = Data.Static<typeof CardanoDatumSchema>;

export const CardanoDatum = asDataType<CardanoDatum>(CardanoDatumSchema);

export const DepositInfoSchema = Data.Object({
  l2_address: AddressSchema,
  l2_network_id: Data.Integer(),
  l2_datum: Data.Nullable(Data.Any()),
});

export type DepositInfo = Data.Static<typeof DepositInfoSchema>;

export const DepositInfo = asDataType<DepositInfo>(DepositInfoSchema);

export const DepositEventSchema = Data.Object({
  id: OutputReferenceSchema,
  info: DepositInfoSchema,
});

export type DepositEvent = Data.Static<typeof DepositEventSchema>;

export const DepositEvent = asDataType<DepositEvent>(DepositEventSchema);

/**
 * Twin of `midgard/ledger_state.MidgardTxValidity`. #640 collapsed the old
 * six-arm enum to the bare validity bit; the per-reason vocabulary moved to
 * `RejectionReasonV1` behind the forced leaf's `OperatorVerdictV1`.
 */
export const MidgardTxValiditySchema = Data.Enum([
  Data.Literal("TxIsValid"),
  Data.Literal("TxIsInvalid"),
]);

export type MidgardTxValidity = Data.Static<typeof MidgardTxValiditySchema>;

export const MidgardTxValidity = asDataType<MidgardTxValidity>(
  MidgardTxValiditySchema,
);

export const NativeTxProofSourceSchema = Data.Object({
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
  field_preimage_lengths_cbor: Data.Bytes(),
});

export type NativeTxProofSource = Data.Static<typeof NativeTxProofSourceSchema>;

export const NativeTxProofSource = asDataType<NativeTxProofSource>(
  NativeTxProofSourceSchema,
);

export const BoundedBlobFrontierPeakSchema = Data.Object({
  height: Data.Integer(),
  hash: H32Schema,
});

export type BoundedBlobFrontierPeak = Data.Static<
  typeof BoundedBlobFrontierPeakSchema
>;

export const BoundedBlobFrontierPeak = asDataType<BoundedBlobFrontierPeak>(
  BoundedBlobFrontierPeakSchema,
);

export const BoundedBlobChunkProofSchema = Data.Object({
  version: Data.Integer(),
  field_index: Data.Integer(),
  total_length: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk: Data.Bytes(),
  frontier: Data.Array(BoundedBlobFrontierPeakSchema),
  siblings: Data.Array(H32Schema),
});

export type BoundedBlobChunkProof = Data.Static<
  typeof BoundedBlobChunkProofSchema
>;

export const BoundedBlobChunkProof = asDataType<BoundedBlobChunkProof>(
  BoundedBlobChunkProofSchema,
);

export const BoundedCollectionItemProofSchema = Data.Object({
  version: Data.Integer(),
  field_index: Data.Integer(),
  item_count: Data.Integer(),
  item_index: Data.Integer(),
  item_length: Data.Integer(),
  item_commitment: H32Schema,
  frontier: Data.Array(BoundedBlobFrontierPeakSchema),
  siblings: Data.Array(H32Schema),
});

export type BoundedCollectionItemProof = Data.Static<
  typeof BoundedCollectionItemProofSchema
>;

export const BoundedCollectionItemProof =
  asDataType<BoundedCollectionItemProof>(BoundedCollectionItemProofSchema);

export const BoundedItemChunkProofSchema = Data.Object({
  version: Data.Integer(),
  field_index: Data.Integer(),
  item_index: Data.Integer(),
  total_length: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk: Data.Bytes(),
  frontier: Data.Array(BoundedBlobFrontierPeakSchema),
  siblings: Data.Array(H32Schema),
});

export type BoundedItemChunkProof = Data.Static<
  typeof BoundedItemChunkProofSchema
>;

export const BoundedItemChunkProof = asDataType<BoundedItemChunkProof>(
  BoundedItemChunkProofSchema,
);

// `TxFieldPreimageV1Schema` and `TxFieldReceiptV1Schema` used to sit here as the
// twins of `midgard/ledger_state`'s two counted publication datums. Both retired
// in #587 with the chain they described: under `docs/spec/midgard-tx.md` §4 a
// field commitment is one flat `blake2b_256` over the whole preimage, so the
// per-item Merkle opening they carried has nothing to be checked against and the
// receipt mint policy that read them was unsatisfiable for any payload whose
// commitments were the §4 flat hashes of real material (a payload declaring
// counted roots could still satisfy it, which is why the replacement closes the
// gap by construction rather than by arithmetic). The §8 replacement is
// `FieldPreimageCertificateV1` in `native-tx-field-access.ts`, whose manifest
// is over §8.4 chunks of a preimage rather than over items of a counted
// collection — so it is a different artifact, not a renamed one, and nothing here
// forwards to it.

export const CekProgramMaterialDatumSchema = Data.Object({
  kind: Data.Integer(),
  root: H32Schema,
  preimage: Data.Bytes(),
});

export type CekProgramMaterialDatum = Data.Static<
  typeof CekProgramMaterialDatumSchema
>;

export const CekProgramMaterialDatum = asDataType<CekProgramMaterialDatum>(
  CekProgramMaterialDatumSchema,
);

/**
 * Twin of `midgard/ledger_state.TxOrderPayload`.
 *
 * **No `terminal_receipt_reference`.** It named the last link of the counted
 * publication receipt chain, which retired in #587 — see the note above
 * `CekProgramMaterialDatumSchema`. The §8 re-expression of the availability
 * role it served is not in this datum; `verify_order_material` on the Aiken side
 * carries the `field_carriage_availability` note that says what the mint checks
 * today and which issue owns the rest.
 */
export const ForcedTxProofSourceSchema = Data.Object({
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
  field_preimage_lengths_cbor: Data.Bytes(),
});

export type ForcedTxProofSource = Data.Static<typeof ForcedTxProofSourceSchema>;

export const ForcedTxProofSource = asDataType<ForcedTxProofSource>(
  ForcedTxProofSourceSchema,
);

export const TxOrderPayloadSchema = Data.Object({
  tx_id: H32Schema,
  transaction_commitment: H32Schema,
  submitted_source: ForcedTxProofSourceSchema,
});

export type TxOrderPayload = Data.Static<typeof TxOrderPayloadSchema>;

export const TxOrderPayload = asDataType<TxOrderPayload>(TxOrderPayloadSchema);

export const TxOrderEventSchema = Data.Object({
  id: OutputReferenceSchema,
  tx: TxOrderPayloadSchema,
});

export type TxOrderEvent = Data.Static<typeof TxOrderEventSchema>;

export const TxOrderEvent = asDataType<TxOrderEvent>(TxOrderEventSchema);

/**
 * Twin of `midgard/ledger_state.L2TransactionSource`.
 *
 * **No `transaction_commitment`.** It used to sit between `tx_id` and `source`.
 * Under `docs/spec/midgard-tx.md` §4's flat reversion the transition-trace
 * family authenticates the compact bytes against the tx-id anchor through the
 * §8.8 door and never reads it, leaving `validation_claim_v1` as the only
 * consumer — and that consumer compared the carried value against the
 * `native_tx_proof_commitment_v1` it re-derived from `source` in the same
 * expression. The on-chain type dropped the field and re-anchored on the
 * derivation; this schema moves with it, because a committed leaf that encodes
 * three fields where the validator expects two does not decode.
 */
export const L2TransactionSourceSchema = Data.Object({
  tx_id: H32Schema,
  source: NativeTxProofSourceSchema,
});

export type L2TransactionSource = Data.Static<typeof L2TransactionSourceSchema>;

export const L2TransactionSource = asDataType<L2TransactionSource>(
  L2TransactionSourceSchema,
);

/**
 * Twin of `midgard/ledger_state.ForcedInclusionTxV1`. It shed
 * `transaction_commitment` for the same reason and in the same change — see
 * {@link L2TransactionSourceSchema}.
 */
export const ForcedInclusionTxV1Schema = Data.Object({
  tx_id: H32Schema,
  submitted_source: ForcedTxProofSourceSchema,
  verdict: OperatorVerdictSchema,
});

export type ForcedInclusionTxV1 = Data.Static<typeof ForcedInclusionTxV1Schema>;

export const ForcedInclusionTxV1 = asDataType<ForcedInclusionTxV1>(
  ForcedInclusionTxV1Schema,
);

export const TransitionPhaseSchema = Data.Enum([
  Data.Literal("Withdrawal"),
  Data.Literal("ForcedTransaction"),
  Data.Literal("L2Transaction"),
  Data.Literal("Deposit"),
]);

export type TransitionPhase = Data.Static<typeof TransitionPhaseSchema>;

export const TransitionPhase = asDataType<TransitionPhase>(
  TransitionPhaseSchema,
);
