import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { DataCoercionError } from "./common.js";
import { DaAvailabilityStateQueueStatusSchema } from "./da-availability-state.js";
import { EMPTY_MERKLE_TREE_ROOT } from "./ledger-constants.js";
import {
  Header,
  HeaderSchema,
  HeaderTransitionCommitments,
  HeaderTransitionCommitmentsError,
  headerTransitionCommitmentsError,
  type MakeHeaderTransitionCommitmentsInput,
  type ValidateHeaderTransitionCommitmentsInput,
  validateSourceRootCount,
} from "./ledger-state.header-schema.js";

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
  proven_fraud: Data.Nullable(Data.Bytes({ minLength: 32, maxLength: 32 })),
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
