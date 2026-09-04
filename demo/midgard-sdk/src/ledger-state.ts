import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_PROTOCOL_VERSION,
  MIDGARD_TRANSITION_STEP_SCHEMA_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
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
} from "./common.js";
import { DaAvailabilityStateQueueStatusSchema } from "./da-availability-state.js";
import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
} from "./ledger-constants.js";
import { OperatorVerdictSchema } from "./rejection-reason.js";

export { NO_DA_ATTESTATION } from "./da-availability-state.js";

export const HeaderHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });
export type HeaderHash = Data.Static<typeof HeaderHashSchema>;
export const HeaderHash = asDataType<HeaderHash>(HeaderHashSchema);

/** Canonical proof-complete Midgard V1 block header. */
export const HeaderSchema = Data.Object({
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
export type Header = Data.Static<typeof HeaderSchema>;
export const Header = asDataType<Header>(HeaderSchema);

export const HeaderTransitionCommitmentsSchema = Data.Object({
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
export type HeaderTransitionCommitments = Data.Static<
  typeof HeaderTransitionCommitmentsSchema
>;
export const HeaderTransitionCommitments =
  asDataType<HeaderTransitionCommitments>(HeaderTransitionCommitmentsSchema);

export const EMPTY_HEADER_TRANSITION_COMMITMENTS: HeaderTransitionCommitments =
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

export type HeaderTransitionCommitmentSourceRoots = Pick<
  Header,
  | "withdrawalsRoot"
  | "forcedTransactionsRoot"
  | "transactionsRoot"
  | "depositsRoot"
>;

export type HeaderTransitionCommitmentCounts = Pick<
  HeaderTransitionCommitments,
  | "withdrawalCount"
  | "forcedTransactionCount"
  | "l2TransactionCount"
  | "depositCount"
>;

export type MakeHeaderTransitionCommitmentsInput =
  HeaderTransitionCommitmentSourceRoots &
    HeaderTransitionCommitmentCounts &
    Partial<
      Pick<
        HeaderTransitionCommitments,
        "transitionTraceRoot" | "eventToStepRoot" | "transitionStepCount"
      >
    > & {
      readonly validationTracesRoot: MerkleRoot;
      readonly validationTraceCount: bigint;
    };

export type ValidateHeaderTransitionCommitmentsInput =
  HeaderTransitionCommitments &
    Pick<Header, "withdrawalsRoot" | "transactionsRoot" | "depositsRoot">;

export class HeaderTransitionCommitmentsError extends EffectData.TaggedError(
  "HeaderTransitionCommitmentsError",
)<GenericErrorFields> {}

const headerTransitionCommitmentsError = (
  message: string,
  cause: unknown,
): HeaderTransitionCommitmentsError =>
  new HeaderTransitionCommitmentsError({ message, cause });

export const validateHeaderTransitionCommitmentsProgram = (
  input: ValidateHeaderTransitionCommitmentsInput,
): Effect.Effect<
  HeaderTransitionCommitments,
  HeaderTransitionCommitmentsError
> =>
  Effect.gen(function* () {
    const commitments: HeaderTransitionCommitments = {
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
        MIDGARD_CONSENSUS_LIMITS.maxWithdrawalCount,
      ],
      [
        "forcedTransactionCount",
        commitments.forcedTransactionCount,
        MIDGARD_CONSENSUS_LIMITS.maxForcedTransactionCount,
      ],
      [
        "l2TransactionCount",
        commitments.l2TransactionCount,
        MIDGARD_CONSENSUS_LIMITS.maxL2TransactionCount,
      ],
      [
        "depositCount",
        commitments.depositCount,
        MIDGARD_CONSENSUS_LIMITS.maxDepositCount,
      ],
      [
        "totalEventCount",
        commitments.totalEventCount,
        MIDGARD_CONSENSUS_LIMITS.maxTotalEventCount,
      ],
      [
        "transitionStepCount",
        commitments.transitionStepCount,
        MIDGARD_CONSENSUS_LIMITS.maxTransitionStepCount,
      ],
      [
        "validationTraceCount",
        commitments.validationTraceCount,
        MIDGARD_CONSENSUS_LIMITS.maxValidationTraceCount,
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
    yield* validateSourceRootCount(
      "withdrawals",
      input.withdrawalsRoot,
      commitments.withdrawalCount,
    );
    yield* validateSourceRootCount(
      "forced_transactions",
      commitments.forcedTransactionsRoot,
      commitments.forcedTransactionCount,
    );
    yield* validateSourceRootCount(
      "transactions",
      input.transactionsRoot,
      commitments.l2TransactionCount,
    );
    yield* validateSourceRootCount(
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
    yield* validateSourceRootCount(
      "validation_traces",
      commitments.validationTracesRoot,
      commitments.validationTraceCount,
    );
    return commitments;
  });

const validateSourceRootCount = (
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

export const makeHeaderTransitionCommitmentsProgram = (
  input: MakeHeaderTransitionCommitmentsInput,
): Effect.Effect<
  HeaderTransitionCommitments,
  HeaderTransitionCommitmentsError
> =>
  Effect.gen(function* () {
    const totalEventCount =
      input.withdrawalCount +
      input.forcedTransactionCount +
      input.l2TransactionCount +
      input.depositCount;
    return yield* validateHeaderTransitionCommitmentsProgram({
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

export const StateQueueNodeSchema = Data.Object({
  header: HeaderSchema,
  da_attestation: DaAvailabilityStateQueueStatusSchema,
});
export type StateQueueNode = Data.Static<typeof StateQueueNodeSchema>;
export const StateQueueNode = asDataType<StateQueueNode>(StateQueueNodeSchema);
export const castStateQueueNodeToData = (node: StateQueueNode): unknown =>
  Data.castTo(node, StateQueueNode);

const assertCanonicalCbor = (
  bytes: Uint8Array,
  canonicalHex: string,
  format: string,
): void => {
  if (Buffer.from(bytes).toString("hex") !== canonicalHex) {
    throw new Error(`${format} CBOR must use its exact canonical encoding`);
  }
};

export const encodeHeaderCbor = (header: Header): Buffer => {
  if (header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION)) {
    throw new Error(
      `HeaderV1 protocol version must equal ${MIDGARD_PROTOCOL_VERSION.toString()}`,
    );
  }
  return Buffer.from(Data.to(header, Header), "hex");
};

export const decodeHeaderCbor = (bytes: Uint8Array): Header => {
  const header = Data.from(Buffer.from(bytes).toString("hex"), Header);
  const canonicalHex = Data.to(header, Header);
  assertCanonicalCbor(bytes, canonicalHex, "HeaderV1");
  if (header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION)) {
    throw new Error(
      `HeaderV1 protocol version must equal ${MIDGARD_PROTOCOL_VERSION.toString()}`,
    );
  }
  return header;
};

export const encodeStateQueueNodeCbor = (node: StateQueueNode): Buffer => {
  if (node.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION)) {
    throw new Error(
      `StateQueueNodeV1 header protocol version must equal ${MIDGARD_PROTOCOL_VERSION.toString()}`,
    );
  }
  return Buffer.from(Data.to(node, StateQueueNode), "hex");
};

export const decodeStateQueueNodeCbor = (bytes: Uint8Array): StateQueueNode => {
  const node = Data.from(Buffer.from(bytes).toString("hex"), StateQueueNode);
  const canonicalHex = Data.to(node, StateQueueNode);
  assertCanonicalCbor(bytes, canonicalHex, "StateQueueNodeV1");
  if (node.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION)) {
    throw new Error(
      `StateQueueNodeV1 header protocol version must equal ${MIDGARD_PROTOCOL_VERSION.toString()}`,
    );
  }
  return node;
};

export const getHeaderFromStateQueueDatum = (nodeDatum: {
  readonly data: Parameters<typeof Data.castFrom>[0];
}): Effect.Effect<Header, DataCoercionError> =>
  Effect.try({
    try: () => {
      const header = Data.castFrom(nodeDatum.data, StateQueueNode).header;
      if (header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION)) {
        throw new Error(
          `Expected proof protocol version ${MIDGARD_PROTOCOL_VERSION.toString()}, got ${header.protocolVersion.toString()}`,
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

export const getStateQueueNodeFromStateQueueDatum = (nodeDatum: {
  readonly data: Parameters<typeof Data.castFrom>[0];
}): Effect.Effect<StateQueueNode, DataCoercionError> =>
  Effect.try({
    try: () => {
      const node = Data.castFrom(nodeDatum.data, StateQueueNode);
      if (node.header.protocolVersion !== BigInt(MIDGARD_PROTOCOL_VERSION)) {
        throw new Error(
          `Expected protocol version ${MIDGARD_PROTOCOL_VERSION.toString()}, got ${node.header.protocolVersion.toString()}`,
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
export const TxOrderPayloadSchema = Data.Object({
  tx_id: H32Schema,
  transaction_commitment: H32Schema,
  source: NativeTxProofSourceSchema,
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
  source: NativeTxProofSourceSchema,
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
export const EventKey = asDataType<EventKey>(EventKeySchema);

export const EventToStepValueSchema = Data.Object({
  step_index: Data.Integer(),
  phase: TransitionPhaseSchema,
});
export type EventToStepValue = Data.Static<typeof EventToStepValueSchema>;
export const EventToStepValue = asDataType<EventToStepValue>(
  EventToStepValueSchema,
);

export const TRANSITION_STEP_SCHEMA_VERSION = BigInt(
  MIDGARD_TRANSITION_STEP_SCHEMA_VERSION,
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
export const TransitionStepV1 = asDataType<TransitionStepV1>(
  TransitionStepV1Schema,
);

// The unqualified names remain source aliases for consumers of the canonical
// schema; they do not define a second wire identity.
export type TransitionStep = TransitionStepV1;
export const TransitionStepSchema = TransitionStepV1Schema;
export const TransitionStep = TransitionStepV1;

export const ValidationVerdictSchema = Data.Enum([
  // Preserve the exact Aiken constructor indexes. Pending is not a valid
  // terminal descriptor verdict and is rejected by semantic conversion, but
  // omitting it here would encode Accepted/Rejected as constructors 0/1
  // instead of 1/2.
  Data.Literal("Pending"),
  Data.Literal("Accepted"),
  Data.Literal("Rejected"),
]);
export type ValidationVerdict = Data.Static<typeof ValidationVerdictSchema>;
export const ValidationVerdict = asDataType<ValidationVerdict>(
  ValidationVerdictSchema,
);

export const ValidationTraceDescriptorSchema = Data.Object({
  schema_version: Data.Integer(),
  machine_version: Data.Integer(),
  trace_root: H32Schema,
  step_count: Data.Integer(),
  initial_state_hash: H32Schema,
  terminal_state_hash: H32Schema,
  verdict: ValidationVerdictSchema,
  rejection_code_hash: H32Schema,
});
export type ValidationTraceDescriptor = Data.Static<
  typeof ValidationTraceDescriptorSchema
>;
export const ValidationTraceDescriptor = asDataType<ValidationTraceDescriptor>(
  ValidationTraceDescriptorSchema,
);

export const WithdrawalBodySchema = Data.Object({
  l2_outref: OutputReferenceSchema,
  l2_owner: Data.Bytes({ minLength: 28, maxLength: 28 }),
  l2_value: ValueSchema,
  l1_address: AddressSchema,
  l1_datum: CardanoDatumSchema,
});
export type WithdrawalBody = Data.Static<typeof WithdrawalBodySchema>;
export const WithdrawalBody = asDataType<WithdrawalBody>(WithdrawalBodySchema);

export const WithdrawalSignatureSchema = Data.Tuple([
  Data.Bytes(),
  Data.Bytes(),
]);
export type WithdrawalSignature = Data.Static<typeof WithdrawalSignatureSchema>;
export const WithdrawalSignature = asDataType<WithdrawalSignature>(
  WithdrawalSignatureSchema,
);

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
export const WithdrawalValidity = asDataType<WithdrawalValidity>(
  WithdrawalValiditySchema,
);

export const WithdrawalInfoSchema = Data.Object({
  body: WithdrawalBodySchema,
  signature: WithdrawalSignatureSchema,
  validity: WithdrawalValiditySchema,
});
export type WithdrawalInfo = Data.Static<typeof WithdrawalInfoSchema>;
export const WithdrawalInfo = asDataType<WithdrawalInfo>(WithdrawalInfoSchema);

export const WithdrawalEventSchema = Data.Object({
  id: OutputReferenceSchema,
  info: WithdrawalInfoSchema,
});
export type WithdrawalEvent = Data.Static<typeof WithdrawalEventSchema>;
export const WithdrawalEvent = asDataType<WithdrawalEvent>(
  WithdrawalEventSchema,
);
