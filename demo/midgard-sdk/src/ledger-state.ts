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
import { EMPTY_MERKLE_TREE_ROOT } from "@/ledger-constants.js";

export const HeaderHashSchema = Data.Bytes({ minLength: 28, maxLength: 28 });
export type HeaderHash = Data.Static<typeof HeaderHashSchema>;
export const HeaderHash = HeaderHashSchema as unknown as HeaderHash;

export const HeaderSchema = Data.Object({
  prevUtxosRoot: MerkleRootSchema,
  utxosRoot: MerkleRootSchema,
  withdrawalsRoot: MerkleRootSchema,
  forcedTransactionsRoot: MerkleRootSchema,
  transactionsRoot: MerkleRootSchema,
  depositsRoot: MerkleRootSchema,
  transitionTraceRoot: MerkleRootSchema,
  eventToStepRoot: MerkleRootSchema,
  withdrawalCount: Data.Integer(),
  forcedTransactionCount: Data.Integer(),
  l2TransactionCount: Data.Integer(),
  depositCount: Data.Integer(),
  totalEventCount: Data.Integer(),
  transitionStepCount: Data.Integer(),
  startTime: POSIXTimeSchema,
  endTime: POSIXTimeSchema,
  prevHeaderHash: HeaderHashSchema,
  operatorVkey: PubKeyHashSchema,
  protocolVersion: Data.Integer(),
});
export type Header = Data.Static<typeof HeaderSchema>;
export const Header = HeaderSchema as unknown as Header;

export type HeaderTransitionCommitments = Pick<
  Header,
  | "forcedTransactionsRoot"
  | "transitionTraceRoot"
  | "eventToStepRoot"
  | "withdrawalCount"
  | "forcedTransactionCount"
  | "l2TransactionCount"
  | "depositCount"
  | "totalEventCount"
  | "transitionStepCount"
>;

export const EMPTY_HEADER_TRANSITION_COMMITMENTS: HeaderTransitionCommitments =
  {
    forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
    transitionTraceRoot: EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: EMPTY_MERKLE_TREE_ROOT,
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
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
    >;

export class HeaderTransitionCommitmentsError extends EffectData.TaggedError(
  "HeaderTransitionCommitmentsError",
)<GenericErrorFields> {}

const headerTransitionCommitmentsError = (
  message: string,
  cause: unknown,
): HeaderTransitionCommitmentsError =>
  new HeaderTransitionCommitmentsError({ message, cause });

export const validateHeaderTransitionCommitmentsProgram = (
  commitments: HeaderTransitionCommitments,
): Effect.Effect<
  HeaderTransitionCommitments,
  HeaderTransitionCommitmentsError
> =>
  Effect.gen(function* () {
    const countEntries = [
      ["withdrawalCount", commitments.withdrawalCount],
      ["forcedTransactionCount", commitments.forcedTransactionCount],
      ["l2TransactionCount", commitments.l2TransactionCount],
      ["depositCount", commitments.depositCount],
      ["totalEventCount", commitments.totalEventCount],
      ["transitionStepCount", commitments.transitionStepCount],
    ] as const;
    for (const [field, count] of countEntries) {
      if (count < 0n) {
        return yield* Effect.fail(
          headerTransitionCommitmentsError(
            "Header transition commitment counts must be non-negative",
            `${field}=${count.toString()}`,
          ),
        );
      }
    }

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
    yield* validateSourceRootCount(
      "withdrawals",
      input.withdrawalsRoot,
      input.withdrawalCount,
    );
    yield* validateSourceRootCount(
      "forced_transactions",
      input.forcedTransactionsRoot,
      input.forcedTransactionCount,
    );
    yield* validateSourceRootCount(
      "transactions",
      input.transactionsRoot,
      input.l2TransactionCount,
    );
    yield* validateSourceRootCount(
      "deposits",
      input.depositsRoot,
      input.depositCount,
    );

    const totalEventCount =
      input.withdrawalCount +
      input.forcedTransactionCount +
      input.l2TransactionCount +
      input.depositCount;
    return yield* validateHeaderTransitionCommitmentsProgram({
      forcedTransactionsRoot: input.forcedTransactionsRoot,
      transitionTraceRoot: input.transitionTraceRoot ?? EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: input.eventToStepRoot ?? EMPTY_MERKLE_TREE_ROOT,
      withdrawalCount: input.withdrawalCount,
      forcedTransactionCount: input.forcedTransactionCount,
      l2TransactionCount: input.l2TransactionCount,
      depositCount: input.depositCount,
      totalEventCount,
      transitionStepCount: input.transitionStepCount ?? totalEventCount,
    });
  });

export const NO_DA_ATTESTATION = "";

export const StateQueueNodeSchema = Data.Object({
  header: HeaderSchema,
  da_attestation: Data.Bytes(),
});
export type StateQueueNode = Data.Static<typeof StateQueueNodeSchema>;
export const StateQueueNode = StateQueueNodeSchema as unknown as StateQueueNode;
export const castStateQueueNodeToData = (node: StateQueueNode): unknown =>
  Data.castTo(node, StateQueueNode);

export const getHeaderFromStateQueueDatum = (nodeDatum: {
  readonly data: Parameters<typeof Data.castFrom>[0];
}): Effect.Effect<Header, DataCoercionError> =>
  Effect.try({
    try: () => Data.castFrom(nodeDatum.data, StateQueueNode).header,
    catch: (cause) =>
      new DataCoercionError({
        message: "Failed coercing block's datum data to `StateQueueNode`",
        cause,
      }),
  });

export const getStateQueueNodeFromStateQueueDatum = (nodeDatum: {
  readonly data: Parameters<typeof Data.castFrom>[0];
}): Effect.Effect<StateQueueNode, DataCoercionError> =>
  Effect.try({
    try: () => Data.castFrom(nodeDatum.data, StateQueueNode),
    catch: (cause) =>
      new DataCoercionError({
        message: "Failed coercing block's datum data to `StateQueueNode`",
        cause,
      }),
  });

export const hashBlockHeader = (
  header: Header,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b(Data.to(header, Header), 28);

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

export const MidgardNetworkIdSchema = Data.Enum([
  Data.Literal("Mainnet"),
  Data.Literal("Testnet"),
]);
export type MidgardNetworkId = Data.Static<typeof MidgardNetworkIdSchema>;
export const MidgardNetworkId =
  MidgardNetworkIdSchema as unknown as MidgardNetworkId;

export const MidgardTxWitnessSetCompactSchema = Data.Object({
  addr_tx_wits: H32Schema,
  script_tx_wits: H32Schema,
  redeemer_tx_wits: H32Schema,
});
export type MidgardTxWitnessSetCompact = Data.Static<
  typeof MidgardTxWitnessSetCompactSchema
>;
export const MidgardTxWitnessSetCompact =
  MidgardTxWitnessSetCompactSchema as unknown as MidgardTxWitnessSetCompact;

export const IntervalBoundTypeSchema = Data.Enum([
  Data.Literal("NegativeInfinity"),
  Data.Object({ Finite: Data.Tuple([Data.Integer()]) }),
  Data.Literal("PositiveInfinity"),
]);
export type IntervalBoundType = Data.Static<typeof IntervalBoundTypeSchema>;
export const IntervalBoundType =
  IntervalBoundTypeSchema as unknown as IntervalBoundType;

export const IntervalBoundSchema = Data.Object({
  bound_type: IntervalBoundTypeSchema,
  is_inclusive: Data.Boolean(),
});
export type IntervalBound = Data.Static<typeof IntervalBoundSchema>;
export const IntervalBound = IntervalBoundSchema as unknown as IntervalBound;

export const ValidityRangeSchema = Data.Object({
  lower_bound: IntervalBoundSchema,
  upper_bound: IntervalBoundSchema,
});
export type ValidityRange = Data.Static<typeof ValidityRangeSchema>;
export const ValidityRange = ValidityRangeSchema as unknown as ValidityRange;

export const MidgardTxBodyCompactSchema = Data.Object({
  spend_inputs: H32Schema,
  reference_inputs: H32Schema,
  outputs: H32Schema,
  fee: Data.Integer(),
  validity_interval: ValidityRangeSchema,
  required_observers: H32Schema,
  required_signer_hashes: H32Schema,
  mint: H32Schema,
  script_integrity_hash: H32Schema,
  auxiliary_data_hash: H32Schema,
  network_id: MidgardNetworkIdSchema,
});
export type MidgardTxBodyCompact = Data.Static<
  typeof MidgardTxBodyCompactSchema
>;
export const MidgardTxBodyCompact =
  MidgardTxBodyCompactSchema as unknown as MidgardTxBodyCompact;

export const MidgardTxCompactSchema = Data.Object({
  body: MidgardTxBodyCompactSchema,
  wits: H32Schema,
  validity: MidgardTxValiditySchema,
});
export type MidgardTxCompact = Data.Static<typeof MidgardTxCompactSchema>;
export const MidgardTxCompact =
  MidgardTxCompactSchema as unknown as MidgardTxCompact;

export const MidgardTxCompactWithoutValiditySchema = Data.Object({
  body: MidgardTxBodyCompactSchema,
  wits: H32Schema,
});
export type MidgardTxCompactWithoutValidity = Data.Static<
  typeof MidgardTxCompactWithoutValiditySchema
>;
export const MidgardTxCompactWithoutValidity =
  MidgardTxCompactWithoutValiditySchema as unknown as MidgardTxCompactWithoutValidity;

export const ForcedInclusionTxSchema = Data.Object({
  tx_compact: MidgardTxCompactWithoutValiditySchema,
  operator_validity: MidgardTxValiditySchema,
});
export type ForcedInclusionTx = Data.Static<typeof ForcedInclusionTxSchema>;
export const ForcedInclusionTx =
  ForcedInclusionTxSchema as unknown as ForcedInclusionTx;

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

export const TransitionStepSchema = Data.Object({
  schema_version: Data.Integer(),
  step_index: Data.Integer(),
  event_key: EventKeySchema,
  phase: TransitionPhaseSchema,
  pre_utxos_root: MerkleRootSchema,
  post_utxos_root: MerkleRootSchema,
});
export type TransitionStep = Data.Static<typeof TransitionStepSchema>;
export const TransitionStep = TransitionStepSchema as unknown as TransitionStep;

export const TxOrderEventSchema = Data.Object({
  id: OutputReferenceSchema,
  tx: MidgardTxCompactSchema,
});
export type TxOrderEvent = Data.Static<typeof TxOrderEventSchema>;
export const TxOrderEvent = TxOrderEventSchema as unknown as TxOrderEvent;

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
