import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_PROTOCOL_V1_VERSION,
  MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { Data } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  AddressSchema,
  DataCoercionError,
  GenericErrorFields,
  H32Schema,
  hashHexWithBlake2b,
  HashingError,
  MerkleRoot,
  MerkleRootSchema,
  OutputReferenceSchema,
  POSIXTimeSchema,
  PubKeyHashSchema,
  ValueSchema,
} from "@/common.js";
import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
} from "@/ledger-constants.js";

export const HeaderHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });
export type HeaderHash = Data.Static<typeof HeaderHashSchema>;
export const HeaderHash = HeaderHashSchema as unknown as HeaderHash;

/** Canonical proof-complete Midgard V1 block header. */
export const HeaderV1Schema = Data.Object({
  prevUtxosRoot: MerkleRootSchema,
  utxosRoot: MerkleRootSchema,
  withdrawalsRoot: MerkleRootSchema,
  forcedTransactionsRoot: MerkleRootSchema,
  transactionsRoot: MerkleRootSchema,
  depositsRoot: MerkleRootSchema,
  transitionTraceRoot: MerkleRootSchema,
  eventToStepRoot: MerkleRootSchema,
  validationTracesRoot: MerkleRootSchema,
  withdrawalCount: Data.Integer(),
  forcedTransactionCount: Data.Integer(),
  l2TransactionCount: Data.Integer(),
  depositCount: Data.Integer(),
  totalEventCount: Data.Integer(),
  transitionStepCount: Data.Integer(),
  validationTraceCount: Data.Integer(),
  startTime: POSIXTimeSchema,
  endTime: POSIXTimeSchema,
  blockSlot: Data.Integer(),
  expectedNetworkId: Data.Integer(),
  minFeeA: Data.Integer(),
  minFeeB: Data.Integer(),
  prevHeaderHash: HeaderHashSchema,
  operatorVkey: PubKeyHashSchema,
  protocolVersion: Data.Integer(),
});
export type HeaderV1 = Data.Static<typeof HeaderV1Schema>;
export const HeaderV1 = HeaderV1Schema as unknown as HeaderV1;

export const HeaderTransitionCommitmentsV1Schema = Data.Object({
  forcedTransactionsRoot: MerkleRootSchema,
  transitionTraceRoot: MerkleRootSchema,
  eventToStepRoot: MerkleRootSchema,
  validationTracesRoot: MerkleRootSchema,
  withdrawalCount: Data.Integer(),
  forcedTransactionCount: Data.Integer(),
  l2TransactionCount: Data.Integer(),
  depositCount: Data.Integer(),
  totalEventCount: Data.Integer(),
  transitionStepCount: Data.Integer(),
  validationTraceCount: Data.Integer(),
});
export type HeaderTransitionCommitmentsV1 = Data.Static<
  typeof HeaderTransitionCommitmentsV1Schema
>;
export const HeaderTransitionCommitmentsV1 =
  HeaderTransitionCommitmentsV1Schema as unknown as HeaderTransitionCommitmentsV1;

export const EMPTY_HEADER_TRANSITION_COMMITMENTS_V1: HeaderTransitionCommitmentsV1 =
  {
    forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
    transitionTraceRoot: EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: EMPTY_MERKLE_TREE_ROOT,
    validationTracesRoot: EMPTY_MERKLE_TREE_ROOT,
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };

export type HeaderTransitionCommitmentSourceRootsV1 = Pick<
  HeaderV1,
  | "withdrawalsRoot"
  | "forcedTransactionsRoot"
  | "transactionsRoot"
  | "depositsRoot"
>;

export type HeaderTransitionCommitmentCountsV1 = Pick<
  HeaderTransitionCommitmentsV1,
  | "withdrawalCount"
  | "forcedTransactionCount"
  | "l2TransactionCount"
  | "depositCount"
>;

export type MakeHeaderTransitionCommitmentsV1Input =
  HeaderTransitionCommitmentSourceRootsV1 &
    HeaderTransitionCommitmentCountsV1 &
    Partial<
      Pick<
        HeaderTransitionCommitmentsV1,
        "transitionTraceRoot" | "eventToStepRoot" | "transitionStepCount"
      >
    > & {
      readonly validationTracesRoot: MerkleRoot;
      readonly validationTraceCount: bigint;
    };

export type ValidateHeaderTransitionCommitmentsV1Input =
  HeaderTransitionCommitmentsV1 &
    Pick<HeaderV1, "withdrawalsRoot" | "transactionsRoot" | "depositsRoot">;

export class HeaderTransitionCommitmentsError extends EffectData.TaggedError(
  "HeaderTransitionCommitmentsError",
)<GenericErrorFields> {}

const headerTransitionCommitmentsError = (
  message: string,
  cause: unknown,
): HeaderTransitionCommitmentsError =>
  new HeaderTransitionCommitmentsError({ message, cause });

export const validateHeaderTransitionCommitmentsV1Program = (
  input: ValidateHeaderTransitionCommitmentsV1Input,
): Effect.Effect<
  HeaderTransitionCommitmentsV1,
  HeaderTransitionCommitmentsError
> =>
  Effect.gen(function* () {
    const commitments: HeaderTransitionCommitmentsV1 = {
      forcedTransactionsRoot: input.forcedTransactionsRoot,
      transitionTraceRoot: input.transitionTraceRoot,
      eventToStepRoot: input.eventToStepRoot,
      validationTracesRoot: input.validationTracesRoot,
      withdrawalCount: input.withdrawalCount,
      forcedTransactionCount: input.forcedTransactionCount,
      l2TransactionCount: input.l2TransactionCount,
      depositCount: input.depositCount,
      totalEventCount: input.totalEventCount,
      transitionStepCount: input.transitionStepCount,
      validationTraceCount: input.validationTraceCount,
    };
    const countEntries = [
      [
        "withdrawalCount",
        commitments.withdrawalCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxWithdrawalCount,
      ],
      [
        "forcedTransactionCount",
        commitments.forcedTransactionCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxForcedTransactionCount,
      ],
      [
        "l2TransactionCount",
        commitments.l2TransactionCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxL2TransactionCount,
      ],
      [
        "depositCount",
        commitments.depositCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxDepositCount,
      ],
      [
        "totalEventCount",
        commitments.totalEventCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxTotalEventCount,
      ],
      [
        "transitionStepCount",
        commitments.transitionStepCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxTransitionStepCount,
      ],
      [
        "validationTraceCount",
        commitments.validationTraceCount,
        MIDGARD_CONSENSUS_LIMITS_V1.maxValidationTraceCount,
      ],
    ] as const;
    for (const [field, count, maximum] of countEntries) {
      if (count < 0n) {
        return yield* Effect.fail(
          headerTransitionCommitmentsError(
            "Header transition commitment counts must be non-negative",
            `${field}=${count.toString()}`,
          ),
        );
      }
      if (count > BigInt(maximum)) {
        return yield* Effect.fail(
          headerTransitionCommitmentsError(
            "Header transition commitment count exceeds the compiled consensus bound",
            `${field}=${count.toString()},maximum=${maximum.toString()}`,
          ),
        );
      }
    }
    yield* validateSourceRootCountV1(
      "withdrawals",
      input.withdrawalsRoot,
      commitments.withdrawalCount,
    );
    yield* validateSourceRootCountV1(
      "forced_transactions",
      commitments.forcedTransactionsRoot,
      commitments.forcedTransactionCount,
    );
    yield* validateSourceRootCountV1(
      "transactions",
      input.transactionsRoot,
      commitments.l2TransactionCount,
    );
    yield* validateSourceRootCountV1(
      "deposits",
      input.depositsRoot,
      commitments.depositCount,
    );

    const expectedTotal =
      commitments.withdrawalCount +
      commitments.forcedTransactionCount +
      commitments.l2TransactionCount +
      commitments.depositCount;
    if (commitments.totalEventCount !== expectedTotal) {
      return yield* Effect.fail(
        headerTransitionCommitmentsError(
          "Header transition total_event_count does not match source event counts",
          `expected=${expectedTotal.toString()},actual=${commitments.totalEventCount.toString()}`,
        ),
      );
    }
    if (commitments.transitionStepCount !== commitments.totalEventCount) {
      return yield* Effect.fail(
        headerTransitionCommitmentsError(
          "Header transition_step_count must equal total_event_count",
          `transition_step_count=${commitments.transitionStepCount.toString()},total_event_count=${commitments.totalEventCount.toString()}`,
        ),
      );
    }

    const hasTransitionEvents = commitments.totalEventCount > 0n;
    if (hasTransitionEvents) {
      if (commitments.transitionTraceRoot === EMPTY_MERKLE_TREE_ROOT) {
        return yield* Effect.fail(
          headerTransitionCommitmentsError(
            "Refusing non-empty transition counts with an empty transition_trace_root",
            `total_event_count=${commitments.totalEventCount.toString()}`,
          ),
        );
      }
      if (commitments.eventToStepRoot === EMPTY_MERKLE_TREE_ROOT) {
        return yield* Effect.fail(
          headerTransitionCommitmentsError(
            "Refusing non-empty transition counts with an empty event_to_step_root",
            `total_event_count=${commitments.totalEventCount.toString()}`,
          ),
        );
      }
    } else if (
      commitments.transitionTraceRoot !== EMPTY_MERKLE_TREE_ROOT ||
      commitments.eventToStepRoot !== EMPTY_MERKLE_TREE_ROOT
    ) {
      return yield* Effect.fail(
        headerTransitionCommitmentsError(
          "Empty transition counts must use empty transition roots",
          `transition_trace_root=${commitments.transitionTraceRoot},event_to_step_root=${commitments.eventToStepRoot}`,
        ),
      );
    }

    const expectedValidationTraceCount =
      commitments.forcedTransactionCount + commitments.l2TransactionCount;
    if (commitments.validationTraceCount !== expectedValidationTraceCount) {
      return yield* Effect.fail(
        headerTransitionCommitmentsError(
          "Proof header validation_trace_count must equal forced_transaction_count + l2_transaction_count",
          `expected=${expectedValidationTraceCount.toString()},actual=${commitments.validationTraceCount.toString()}`,
        ),
      );
    }
    yield* validateSourceRootCountV1(
      "validation_traces",
      commitments.validationTracesRoot,
      commitments.validationTraceCount,
    );
    return commitments;
  });

const validateSourceRootCountV1 = (
  label: string,
  root: MerkleRoot,
  count: bigint,
): Effect.Effect<void, HeaderTransitionCommitmentsError> => {
  if (root === EMPTY_MERKLE_TREE_ROOT && count > 0n) {
    return Effect.fail(
      headerTransitionCommitmentsError(
        "Refusing non-empty source event count with an empty source root",
        `${label}_root=${root},${label}_count=${count.toString()}`,
      ),
    );
  }
  if (root !== EMPTY_MERKLE_TREE_ROOT && count === 0n) {
    return Effect.fail(
      headerTransitionCommitmentsError(
        "Refusing non-empty source root with a zero source event count",
        `${label}_root=${root},${label}_count=0`,
      ),
    );
  }
  return Effect.void;
};

export const makeHeaderTransitionCommitmentsV1Program = (
  input: MakeHeaderTransitionCommitmentsV1Input,
): Effect.Effect<
  HeaderTransitionCommitmentsV1,
  HeaderTransitionCommitmentsError
> =>
  Effect.gen(function* () {
    const totalEventCount =
      input.withdrawalCount +
      input.forcedTransactionCount +
      input.l2TransactionCount +
      input.depositCount;
    return yield* validateHeaderTransitionCommitmentsV1Program({
      withdrawalsRoot: input.withdrawalsRoot,
      forcedTransactionsRoot: input.forcedTransactionsRoot,
      transactionsRoot: input.transactionsRoot,
      depositsRoot: input.depositsRoot,
      transitionTraceRoot: input.transitionTraceRoot ?? EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: input.eventToStepRoot ?? EMPTY_MERKLE_TREE_ROOT,
      validationTracesRoot: input.validationTracesRoot,
      withdrawalCount: input.withdrawalCount,
      forcedTransactionCount: input.forcedTransactionCount,
      l2TransactionCount: input.l2TransactionCount,
      depositCount: input.depositCount,
      totalEventCount,
      transitionStepCount: input.transitionStepCount ?? totalEventCount,
      validationTraceCount: input.validationTraceCount,
    });
  });

export const NO_DA_ATTESTATION = "";

export const StateQueueNodeV1Schema = Data.Object({
  header: HeaderV1Schema,
  da_attestation: Data.Bytes(),
});
export type StateQueueNodeV1 = Data.Static<typeof StateQueueNodeV1Schema>;
export const StateQueueNodeV1 =
  StateQueueNodeV1Schema as unknown as StateQueueNodeV1;
export const castStateQueueNodeV1ToData = (node: StateQueueNodeV1): unknown =>
  Data.castTo(node, StateQueueNodeV1);

const assertCanonicalCbor = (
  bytes: Uint8Array,
  canonicalHex: string,
  format: string,
): void => {
  if (Buffer.from(bytes).toString("hex") !== canonicalHex) {
    throw new Error(`${format} CBOR must use its exact canonical encoding`);
  }
};

export const encodeHeaderV1Cbor = (header: HeaderV1): Buffer => {
  if (header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_V1_VERSION)) {
    throw new Error(
      `HeaderV1 protocol version must equal ${MIDGARD_PROTOCOL_V1_VERSION.toString()}`,
    );
  }
  return Buffer.from(Data.to(header, HeaderV1), "hex");
};

export const decodeHeaderV1Cbor = (bytes: Uint8Array): HeaderV1 => {
  const header = Data.from(Buffer.from(bytes).toString("hex"), HeaderV1);
  const canonicalHex = Data.to(header, HeaderV1);
  assertCanonicalCbor(bytes, canonicalHex, "HeaderV1");
  if (header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_V1_VERSION)) {
    throw new Error(
      `HeaderV1 protocol version must equal ${MIDGARD_PROTOCOL_V1_VERSION.toString()}`,
    );
  }
  return header;
};

export const encodeStateQueueNodeV1Cbor = (node: StateQueueNodeV1): Buffer => {
  if (node.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_V1_VERSION)) {
    throw new Error(
      `StateQueueNodeV1 header protocol version must equal ${MIDGARD_PROTOCOL_V1_VERSION.toString()}`,
    );
  }
  return Buffer.from(Data.to(node, StateQueueNodeV1), "hex");
};

export const decodeStateQueueNodeV1Cbor = (
  bytes: Uint8Array,
): StateQueueNodeV1 => {
  const node = Data.from(Buffer.from(bytes).toString("hex"), StateQueueNodeV1);
  const canonicalHex = Data.to(node, StateQueueNodeV1);
  assertCanonicalCbor(bytes, canonicalHex, "StateQueueNodeV1");
  if (node.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_V1_VERSION)) {
    throw new Error(
      `StateQueueNodeV1 header protocol version must equal ${MIDGARD_PROTOCOL_V1_VERSION.toString()}`,
    );
  }
  return node;
};

export const getHeaderV1FromStateQueueDatum = (nodeDatum: {
  readonly data: Parameters<typeof Data.castFrom>[0];
}): Effect.Effect<HeaderV1, DataCoercionError> =>
  Effect.try({
    try: () => {
      const header = Data.castFrom(nodeDatum.data, StateQueueNodeV1).header;
      if (header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_V1_VERSION)) {
        throw new Error(
          `Expected proof protocol version ${MIDGARD_PROTOCOL_V1_VERSION.toString()}, got ${header.protocolVersion.toString()}`,
        );
      }
      return header;
    },
    catch: (cause) =>
      new DataCoercionError({
        message: "Failed coercing block's datum data to `StateQueueNodeV1`",
        cause,
      }),
  });

export const getStateQueueNodeV1FromStateQueueDatum = (nodeDatum: {
  readonly data: Parameters<typeof Data.castFrom>[0];
}): Effect.Effect<StateQueueNodeV1, DataCoercionError> =>
  Effect.try({
    try: () => {
      const node = Data.castFrom(nodeDatum.data, StateQueueNodeV1);
      if (node.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_V1_VERSION)) {
        throw new Error(
          `Expected protocol version ${MIDGARD_PROTOCOL_V1_VERSION.toString()}, got ${node.header.protocolVersion.toString()}`,
        );
      }
      return node;
    },
    catch: (cause) =>
      new DataCoercionError({
        message: "Failed coercing block's datum data to `StateQueueNodeV1`",
        cause,
      }),
  });

export const hashBlockHeaderV1 = (
  header: HeaderV1,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(encodeHeaderV1Cbor(header).toString("hex"), 28);

export const ConfirmedStateSchema = Data.Object({
  headerHash: HeaderHashSchema,
  prevHeaderHash: HeaderHashSchema,
  utxoRoot: MerkleRootSchema,
  startTime: POSIXTimeSchema,
  endTime: POSIXTimeSchema,
  protocolVersion: Data.Integer(),
});
export type ConfirmedState = Data.Static<typeof ConfirmedStateSchema>;
export const ConfirmedState = ConfirmedStateSchema as unknown as ConfirmedState;
export const castConfirmedStateToData = (
  confirmedState: ConfirmedState,
): unknown => Data.castTo(confirmedState, ConfirmedState);

export const makeGenesisConfirmedStateV1 = (
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
export const confirmedStateNextHeaderProtocolVersionV1 = (
  confirmedState: ConfirmedState,
): bigint | null => {
  const protocolV1 = BigInt(MIDGARD_PROTOCOL_V1_VERSION);
  const isGenesis =
    confirmedState.protocolVersion === GENESIS_PROTOCOL_VERSION &&
    confirmedState.headerHash === GENESIS_HEADER_HASH &&
    confirmedState.prevHeaderHash === GENESIS_HEADER_HASH &&
    confirmedState.utxoRoot === EMPTY_MERKLE_TREE_ROOT &&
    confirmedState.startTime >= 0n &&
    confirmedState.startTime === confirmedState.endTime;
  if (isGenesis) {
    return protocolV1;
  }

  const isOrdinaryV1 =
    confirmedState.protocolVersion === protocolV1 &&
    confirmedState.headerHash !== GENESIS_HEADER_HASH &&
    confirmedState.startTime >= 0n &&
    confirmedState.startTime <= confirmedState.endTime;
  return isOrdinaryV1 ? protocolV1 : null;
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
export const CardanoDatum = CardanoDatumSchema as unknown as CardanoDatum;

export const DepositInfoSchema = Data.Object({
  l2_address: AddressSchema,
  l2_network_id: Data.Integer(),
  l2_datum: Data.Nullable(Data.Any()),
});
export type DepositInfo = Data.Static<typeof DepositInfoSchema>;
export const DepositInfo = DepositInfoSchema as unknown as DepositInfo;

export const DepositEventSchema = Data.Object({
  id: OutputReferenceSchema,
  info: DepositInfoSchema,
});
export type DepositEvent = Data.Static<typeof DepositEventSchema>;
export const DepositEvent = DepositEventSchema as unknown as DepositEvent;

export const MidgardTxValiditySchema = Data.Enum([
  Data.Literal("TxIsValid"),
  Data.Literal("NonExistentInputUtxo"),
  Data.Literal("InvalidSignature"),
  Data.Literal("FailedScript"),
  Data.Literal("FeeTooLow"),
  Data.Literal("UnbalancedTx"),
]);
export type MidgardTxValidity = Data.Static<typeof MidgardTxValiditySchema>;
export const MidgardTxValidity =
  MidgardTxValiditySchema as unknown as MidgardTxValidity;

export const NativeTxProofSourceV1Schema = Data.Object({
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
  field_preimage_lengths_cbor: Data.Bytes(),
});
export type NativeTxProofSourceV1 = Data.Static<
  typeof NativeTxProofSourceV1Schema
>;
export const NativeTxProofSourceV1 =
  NativeTxProofSourceV1Schema as unknown as NativeTxProofSourceV1;

export const BoundedBlobFrontierPeakV1Schema = Data.Object({
  height: Data.Integer(),
  hash: H32Schema,
});
export type BoundedBlobFrontierPeakV1 = Data.Static<
  typeof BoundedBlobFrontierPeakV1Schema
>;
export const BoundedBlobFrontierPeakV1 =
  BoundedBlobFrontierPeakV1Schema as unknown as BoundedBlobFrontierPeakV1;

export const BoundedBlobChunkProofV1Schema = Data.Object({
  version: Data.Integer(),
  field_index: Data.Integer(),
  total_length: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk: Data.Bytes(),
  frontier: Data.Array(BoundedBlobFrontierPeakV1Schema),
  siblings: Data.Array(H32Schema),
});
export type BoundedBlobChunkProofV1 = Data.Static<
  typeof BoundedBlobChunkProofV1Schema
>;
export const BoundedBlobChunkProofV1 =
  BoundedBlobChunkProofV1Schema as unknown as BoundedBlobChunkProofV1;

export const BoundedCollectionItemProofV1Schema = Data.Object({
  version: Data.Integer(),
  field_index: Data.Integer(),
  item_count: Data.Integer(),
  item_index: Data.Integer(),
  item_length: Data.Integer(),
  item_commitment: H32Schema,
  frontier: Data.Array(BoundedBlobFrontierPeakV1Schema),
  siblings: Data.Array(H32Schema),
});
export type BoundedCollectionItemProofV1 = Data.Static<
  typeof BoundedCollectionItemProofV1Schema
>;
export const BoundedCollectionItemProofV1 =
  BoundedCollectionItemProofV1Schema as unknown as BoundedCollectionItemProofV1;

export const BoundedItemChunkProofV1Schema = Data.Object({
  version: Data.Integer(),
  field_index: Data.Integer(),
  item_index: Data.Integer(),
  total_length: Data.Integer(),
  chunk_index: Data.Integer(),
  chunk: Data.Bytes(),
  frontier: Data.Array(BoundedBlobFrontierPeakV1Schema),
  siblings: Data.Array(H32Schema),
});
export type BoundedItemChunkProofV1 = Data.Static<
  typeof BoundedItemChunkProofV1Schema
>;
export const BoundedItemChunkProofV1 =
  BoundedItemChunkProofV1Schema as unknown as BoundedItemChunkProofV1;

export const TxFieldPreimageV1Schema = Data.Object({
  field_receipt_policy_id: Data.Bytes({ minLength: 28, maxLength: 28 }),
  tx_order_policy_id: Data.Bytes({ minLength: 28, maxLength: 28 }),
  tx_order_id: OutputReferenceSchema,
  transaction_commitment: H32Schema,
  collection_proof: BoundedCollectionItemProofV1Schema,
  proof: BoundedItemChunkProofV1Schema,
});
export type TxFieldPreimageV1 = Data.Static<typeof TxFieldPreimageV1Schema>;
export const TxFieldPreimageV1 =
  TxFieldPreimageV1Schema as unknown as TxFieldPreimageV1;

export const TxFieldReceiptV1Schema = Data.Object({
  field_receipt_policy_id: Data.Bytes({ minLength: 28, maxLength: 28 }),
  tx_order_policy_id: Data.Bytes({ minLength: 28, maxLength: 28 }),
  tx_order_id: OutputReferenceSchema,
  transaction_commitment: H32Schema,
  collection_proof: BoundedCollectionItemProofV1Schema,
  chunk_index: Data.Integer(),
  field_reference: OutputReferenceSchema,
  predecessor_receipt_reference: Data.Nullable(OutputReferenceSchema),
  field_encoded_size: Data.Integer(),
});
export type TxFieldReceiptV1 = Data.Static<typeof TxFieldReceiptV1Schema>;
export const TxFieldReceiptV1 =
  TxFieldReceiptV1Schema as unknown as TxFieldReceiptV1;

export const CekProgramMaterialDatumV1Schema = Data.Object({
  kind: Data.Integer(),
  root: H32Schema,
  preimage: Data.Bytes(),
});
export type CekProgramMaterialDatumV1 = Data.Static<
  typeof CekProgramMaterialDatumV1Schema
>;
export const CekProgramMaterialDatumV1 =
  CekProgramMaterialDatumV1Schema as unknown as CekProgramMaterialDatumV1;

export const TxOrderPayloadV1Schema = Data.Object({
  tx_id: H32Schema,
  transaction_commitment: H32Schema,
  source: NativeTxProofSourceV1Schema,
  terminal_receipt_reference: Data.Nullable(OutputReferenceSchema),
});
export type TxOrderPayloadV1 = Data.Static<typeof TxOrderPayloadV1Schema>;
export const TxOrderPayloadV1 =
  TxOrderPayloadV1Schema as unknown as TxOrderPayloadV1;

export const TxOrderEventV1Schema = Data.Object({
  id: OutputReferenceSchema,
  tx: TxOrderPayloadV1Schema,
});
export type TxOrderEventV1 = Data.Static<typeof TxOrderEventV1Schema>;
export const TxOrderEventV1 = TxOrderEventV1Schema as unknown as TxOrderEventV1;

export const L2TransactionSourceV1Schema = Data.Object({
  tx_id: H32Schema,
  transaction_commitment: H32Schema,
  source: NativeTxProofSourceV1Schema,
});
export type L2TransactionSourceV1 = Data.Static<
  typeof L2TransactionSourceV1Schema
>;
export const L2TransactionSourceV1 =
  L2TransactionSourceV1Schema as unknown as L2TransactionSourceV1;

export const ForcedInclusionTxV1Schema = Data.Object({
  tx_id: H32Schema,
  transaction_commitment: H32Schema,
  source: NativeTxProofSourceV1Schema,
  operator_validity: MidgardTxValiditySchema,
});
export type ForcedInclusionTxV1 = Data.Static<typeof ForcedInclusionTxV1Schema>;
export const ForcedInclusionTxV1 =
  ForcedInclusionTxV1Schema as unknown as ForcedInclusionTxV1;

export const TransitionPhaseSchema = Data.Enum([
  Data.Literal("Withdrawal"),
  Data.Literal("ForcedTransaction"),
  Data.Literal("L2Transaction"),
  Data.Literal("Deposit"),
]);
export type TransitionPhase = Data.Static<typeof TransitionPhaseSchema>;
export const TransitionPhase =
  TransitionPhaseSchema as unknown as TransitionPhase;

export const EventKeySchema = Data.Enum([
  Data.Object({
    WithdrawalEventKey: Data.Object({
      withdrawal_id: OutputReferenceSchema,
    }),
  }),
  Data.Object({
    ForcedTransactionEventKey: Data.Object({
      tx_order_id: OutputReferenceSchema,
    }),
  }),
  Data.Object({
    L2TransactionEventKey: Data.Object({
      tx_id: H32Schema,
    }),
  }),
  Data.Object({
    DepositEventKey: Data.Object({
      deposit_id: OutputReferenceSchema,
    }),
  }),
]);
export type EventKey = Data.Static<typeof EventKeySchema>;
export const EventKey = EventKeySchema as unknown as EventKey;

export const EventToStepValueSchema = Data.Object({
  step_index: Data.Integer(),
  phase: TransitionPhaseSchema,
});
export type EventToStepValue = Data.Static<typeof EventToStepValueSchema>;
export const EventToStepValue =
  EventToStepValueSchema as unknown as EventToStepValue;

export const TRANSITION_STEP_V1_SCHEMA_VERSION = BigInt(
  MIDGARD_TRANSITION_STEP_V1_SCHEMA_VERSION,
);

export const TransitionStepV1Schema = Data.Object({
  schema_version: Data.Integer(),
  step_index: Data.Integer(),
  event_key: EventKeySchema,
  phase: TransitionPhaseSchema,
  pre_utxos_root: MerkleRootSchema,
  post_utxos_root: MerkleRootSchema,
});
export type TransitionStepV1 = Data.Static<typeof TransitionStepV1Schema>;
export const TransitionStepV1 =
  TransitionStepV1Schema as unknown as TransitionStepV1;

// The unqualified names remain source aliases for consumers of the canonical
// schema; they do not define a second wire identity.
export type TransitionStep = TransitionStepV1;
export const TransitionStepSchema = TransitionStepV1Schema;
export const TransitionStep = TransitionStepV1;

export const ValidationVerdictV1Schema = Data.Enum([
  // Preserve the exact Aiken constructor indexes. Pending is not a valid
  // terminal descriptor verdict and is rejected by semantic conversion, but
  // omitting it here would encode Accepted/Rejected as constructors 0/1
  // instead of 1/2.
  Data.Literal("Pending"),
  Data.Literal("Accepted"),
  Data.Literal("Rejected"),
]);
export type ValidationVerdictV1 = Data.Static<typeof ValidationVerdictV1Schema>;
export const ValidationVerdictV1 =
  ValidationVerdictV1Schema as unknown as ValidationVerdictV1;

export const ValidationTraceDescriptorV1Schema = Data.Object({
  schema_version: Data.Integer(),
  machine_version: Data.Integer(),
  trace_root: H32Schema,
  step_count: Data.Integer(),
  initial_state_hash: H32Schema,
  terminal_state_hash: H32Schema,
  verdict: ValidationVerdictV1Schema,
  rejection_code_hash: H32Schema,
});
export type ValidationTraceDescriptorV1 = Data.Static<
  typeof ValidationTraceDescriptorV1Schema
>;
export const ValidationTraceDescriptorV1 =
  ValidationTraceDescriptorV1Schema as unknown as ValidationTraceDescriptorV1;

export const WithdrawalBodySchema = Data.Object({
  l2_outref: OutputReferenceSchema,
  l2_owner: Data.Bytes({ minLength: 28, maxLength: 28 }),
  l2_value: ValueSchema,
  l1_address: AddressSchema,
  l1_datum: CardanoDatumSchema,
});
export type WithdrawalBody = Data.Static<typeof WithdrawalBodySchema>;
export const WithdrawalBody = WithdrawalBodySchema as unknown as WithdrawalBody;

export const WithdrawalSignatureSchema = Data.Tuple([
  Data.Bytes(),
  Data.Bytes(),
]);
export type WithdrawalSignature = Data.Static<typeof WithdrawalSignatureSchema>;
export const WithdrawalSignature =
  WithdrawalSignatureSchema as unknown as WithdrawalSignature;

export const WithdrawalValiditySchema = Data.Enum([
  Data.Literal("WithdrawalIsValid"),
  Data.Literal("NonExistentWithdrawalUtxo"),
  Data.Object({
    SpentWithdrawalUtxo: Data.Object({
      l2_tx_id: Data.Bytes(),
    }),
  }),
  Data.Literal("IncorrectWithdrawalOwner"),
  Data.Literal("IncorrectWithdrawalValue"),
  Data.Literal("IncorrectWithdrawalSignature"),
  Data.Literal("TooManyTokensInWithdrawal"),
  Data.Literal("UnpayableWithdrawalValue"),
]);
export type WithdrawalValidity = Data.Static<typeof WithdrawalValiditySchema>;
export const WithdrawalValidity =
  WithdrawalValiditySchema as unknown as WithdrawalValidity;

export const WithdrawalInfoSchema = Data.Object({
  body: WithdrawalBodySchema,
  signature: WithdrawalSignatureSchema,
  validity: WithdrawalValiditySchema,
});
export type WithdrawalInfo = Data.Static<typeof WithdrawalInfoSchema>;
export const WithdrawalInfo = WithdrawalInfoSchema as unknown as WithdrawalInfo;

export const WithdrawalEventSchema = Data.Object({
  id: OutputReferenceSchema,
  info: WithdrawalInfoSchema,
});
export type WithdrawalEvent = Data.Static<typeof WithdrawalEventSchema>;
export const WithdrawalEvent =
  WithdrawalEventSchema as unknown as WithdrawalEvent;
