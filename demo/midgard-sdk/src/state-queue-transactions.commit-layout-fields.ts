import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  Data,
  type Script,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
} from "./active-operators.js";
import { type CorrectionLockUTxO } from "./correction-lock.js";
import { NO_DA_ATTESTATION, type StateQueueNode } from "./ledger-state.js";
import {
  StateQueueError,
  StateQueueRedeemer,
  StateQueueSpendRedeemer,
} from "./state-queue.js";

export const ACTIVE_OPERATOR_MATURITY_DURATION_MS = BigInt(
  MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
);

export const MIN_SETTLEMENT_OUTPUT_LOVELACE = 5_000_000n;

// Aiken's sole fieldless constructor is represented by Plutus `Constr 0 []`.

export const COMMIT_MAX_VALIDITY_RANGE_MS =
  SELECTED_DEPLOYMENT_PROFILE.timing.max_validity_range_ms;

export const assertCommitHeadDaDeadline = (
  head: StateQueueNode,
  inclusiveValidityUpperBoundMs: number,
  nowMs = Date.now(),
): Effect.Effect<void, StateQueueError> => {
  const deadlineMs =
    head.header.endTime +
    BigInt(SELECTED_DEPLOYMENT_PROFILE.timing.da_attestation_timeout_ms);
  if (
    head.da_attestation === NO_DA_ATTESTATION &&
    BigInt(inclusiveValidityUpperBoundMs) >= deadlineMs
  ) {
    return Effect.fail(
      new StateQueueError({
        message:
          BigInt(nowMs) >= deadlineMs
            ? "Commit paused until expired unattested suffix is corrected"
            : "Commit waiting for DA attestation: validity upper bound reaches the head deadline",
        cause: `inclusive_upper_bound_ms=${inclusiveValidityUpperBoundMs},da_deadline_ms=${deadlineMs}`,
      }),
    );
  }
  return Effect.void;
};

export const isCommitValidityInterval = ({
  validFrom,
  validTo,
}: {
  readonly validFrom: number;
  readonly validTo: number;
}): boolean =>
  Number.isSafeInteger(validFrom) &&
  Number.isSafeInteger(validTo) &&
  validFrom < validTo &&
  validTo - validFrom <= COMMIT_MAX_VALIDITY_RANGE_MS;

export const commitHeaderMatchesValidityUpperBound = ({
  headerEndTime,
  validTo,
}: {
  readonly headerEndTime: bigint;
  readonly validTo: number;
}): boolean =>
  Number.isSafeInteger(validTo) && headerEndTime === BigInt(validTo - 1);

export type OperatorWalletViewLike = {
  readonly knownUtxos: readonly UTxO[];
  readonly consumedOutRefs: readonly string[];
};

export type StateQueueCommitWitnessContext = {
  readonly operatorKeyHash: string;
  readonly schedulerRefInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  /** Authenticated deployment singleton; append is permitted only while Idle. */
  readonly correctionLockRefInput: CorrectionLockUTxO;
  /** Authenticated singleton root; required exactly for a non-empty queue. */
  readonly confirmedStateRefInput?: UTxO;
  /** Authenticated current head; required when the consumed tail is deeper. */
  readonly headStateQueueNodeRefInput?: UTxO;
  readonly activeOperatorInput: UTxO & { readonly datum: string };
  readonly activeOperatorsSpendingScript: Script;
  readonly activeOperatorsSpendingScriptRef?: UTxO;
  readonly stateQueueSpendingScriptRef?: UTxO;
  readonly stateQueueMintingScriptRef?: UTxO;
  readonly stateQueueCommitYieldScriptRef: UTxO;
  readonly operatorWalletView: OperatorWalletViewLike;
};

export type StateQueueCommitLayout = {
  readonly yieldToRefInputIndex: bigint;
  readonly schedulerRefInputIndex: bigint;
  readonly newBlockOutputIndex: bigint;
  readonly continuedLatestBlockOutputIndex: bigint;
  readonly activeOperatorsInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly activeOperatorOutputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueMintRedeemerIndex: bigint;
  readonly confirmedStateRefInputIndex: bigint | null;
  readonly headStateQueueNodeRefInputIndex: bigint | null;
};

type StateQueueCommitRedeemer = {
  readonly CommitBlockHeader: {
    readonly yield_to_ref_input_index: bigint;
    readonly new_block_output_index: bigint;
    readonly continued_latest_block_output_index: bigint;
    readonly operator: string;
    readonly scheduler_ref_input_index: bigint;
    readonly active_operators_input_index: bigint;
    readonly active_operators_redeemer_index: bigint;
    readonly m_confirmed_state_ref_input_index: bigint | null;
    readonly m_head_state_queue_node_ref_input_index: bigint | null;
  };
};

type ActiveOperatorCommitRedeemer = {
  readonly UpdateBondHoldNewState: {
    readonly active_operator: string;
    readonly active_node_input_index: bigint;
    readonly active_node_output_index: bigint;
    readonly hub_oracle_ref_input_index: bigint;
    readonly state_queue_redeemer_index: bigint;
  };
};

export const requireOperatorWalletInputs = (
  walletUtxos: readonly UTxO[],
  transactionLabel: string,
): Effect.Effect<readonly UTxO[], StateQueueError> =>
  Effect.gen(function* () {
    if (walletUtxos.length === 0) {
      return yield* Effect.fail(
        new StateQueueError({
          message: `No operator wallet inputs available to fund ${transactionLabel}`,
          cause: "operator wallet has no available UTxO",
        }),
      );
    }
    return walletUtxos;
  });

export const availableOperatorWalletUtxos = (
  view: OperatorWalletViewLike,
): readonly UTxO[] => {
  const consumedOutRefs = new Set(view.consumedOutRefs);
  return view.knownUtxos.filter(
    (utxo) => !consumedOutRefs.has(outRefLabel(utxo)),
  );
};

export const decodeActiveOperatorDatum = (data: unknown): ActiveOperatorDatum =>
  Data.castFrom(
    data as never,
    ActiveOperatorDatum as never,
  ) as ActiveOperatorDatum;

const makeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout,
): StateQueueCommitRedeemer => ({
  CommitBlockHeader: {
    yield_to_ref_input_index: layout.yieldToRefInputIndex,
    new_block_output_index: layout.newBlockOutputIndex,
    continued_latest_block_output_index: layout.continuedLatestBlockOutputIndex,
    operator: operatorKeyHash,
    scheduler_ref_input_index: layout.schedulerRefInputIndex,
    active_operators_input_index: layout.activeOperatorsInputIndex,
    active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
    m_confirmed_state_ref_input_index: layout.confirmedStateRefInputIndex,
    m_head_state_queue_node_ref_input_index:
      layout.headStateQueueNodeRefInputIndex,
  },
});

const makeActiveOperatorCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout,
): ActiveOperatorCommitRedeemer => ({
  UpdateBondHoldNewState: {
    active_operator: operatorKeyHash,
    active_node_input_index: layout.activeOperatorsInputIndex,
    active_node_output_index: layout.activeOperatorOutputIndex,
    hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
    state_queue_redeemer_index: layout.stateQueueMintRedeemerIndex,
  },
});

export const encodeStateQueueCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout,
): string =>
  Data.to(
    makeStateQueueCommitRedeemer(operatorKeyHash, layout) as never,
    StateQueueRedeemer as never,
  );

export const encodeActiveOperatorCommitRedeemer = (
  operatorKeyHash: string,
  layout: StateQueueCommitLayout,
): string =>
  Data.to(
    makeActiveOperatorCommitRedeemer(operatorKeyHash, layout) as never,
    ActiveOperatorSpendRedeemer as never,
  );

export const encodeStateQueueLinkedListMutationSpendRedeemer = (): string =>
  Data.to("LinkedListMutation" as never, StateQueueSpendRedeemer as never);

type CommitLayoutLike = {
  readonly yieldToRefInputIndex: bigint;
  readonly schedulerRefInputIndex: bigint;
  readonly activeOperatorsInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly stateQueueMintRedeemerIndex: bigint;
  readonly newBlockOutputIndex: bigint;
  readonly continuedLatestBlockOutputIndex: bigint;
  readonly activeOperatorOutputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly confirmedStateRefInputIndex: bigint | null;
  readonly headStateQueueNodeRefInputIndex: bigint | null;
};

const COMMIT_LAYOUT_FIELDS = [
  { key: "yieldToRefInputIndex", label: "yield_to_ref_input_index" },
  { key: "schedulerRefInputIndex", label: "scheduler_ref_input_index" },
  { key: "activeOperatorsInputIndex", label: "active_operators_input_index" },
  {
    key: "activeOperatorsRedeemerIndex",
    label: "active_operators_redeemer_index",
  },
  {
    key: "stateQueueMintRedeemerIndex",
    label: "state_queue_mint_redeemer_index",
  },
  { key: "newBlockOutputIndex", label: "new_block_output_index" },
  {
    key: "continuedLatestBlockOutputIndex",
    label: "continued_latest_block_output_index",
  },
  { key: "activeOperatorOutputIndex", label: "active_operator_output_index" },
  { key: "hubOracleRefInputIndex", label: "hub_oracle_ref_input_index" },
  {
    key: "confirmedStateRefInputIndex",
    label: "m_confirmed_state_ref_input_index",
  },
  {
    key: "headStateQueueNodeRefInputIndex",
    label: "m_head_state_queue_node_ref_input_index",
  },
] as const satisfies readonly {
  readonly key: keyof CommitLayoutLike;
  readonly label: string;
}[];

export const formatCommitLayout = (layout: CommitLayoutLike): string =>
  COMMIT_LAYOUT_FIELDS.map(
    ({ key, label }) => `${label}=${layout[key]?.toString() ?? "null"}`,
  ).join(",");

export const requireUniqueContextOutputIndex = (
  outputs: readonly TxOutput[],
  predicate: (output: TxOutput) => boolean,
  label: string,
): bigint => {
  let foundIndex: bigint | undefined;
  for (let index = 0; index < outputs.length; index += 1) {
    if (!predicate(outputs[index]!)) {
      continue;
    }
    if (foundIndex !== undefined) {
      throw new Error(`${label} output selector matched multiple outputs`);
    }
    foundIndex = BigInt(index);
  }
  if (foundIndex === undefined) {
    throw new Error(`${label} output is missing from final tx outputs`);
  }
  return foundIndex;
};
