import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { castActiveOperatorDatumToData } from "./active-operators.js";
import { scriptRewardAddress } from "./cardano-addresses.js";
import type { HashingError, MidgardValidators } from "./common.js";
import {
  CorrectionLockDatum,
  type CorrectionLockUTxO,
} from "./correction-lock.js";
import {
  castStateQueueNodeToData,
  getStateQueueNodeFromStateQueueDatum,
  hashBlockHeader,
  type Header,
  NO_DA_ATTESTATION,
} from "./ledger-state.js";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  encodeLinkedListNodeView,
  LinkedListDatum,
  linkedListDatumToNodeView,
  type LinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import {
  STATE_QUEUE_NODE_MIN_LOVELACE,
  StateQueueError,
  type StateQueueFetchConfig,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "./state-queue.js";
import {
  buildDeterministicCommitTxBuilder,
  type CommitBlockHeaderParams,
  type CommitBlockHeaderResult,
} from "./state-queue-transactions.build-deterministic-commit-tx-builder.js";
import {
  ACTIVE_OPERATOR_MATURITY_DURATION_MS,
  assertCommitHeadDaDeadline,
  COMMIT_MAX_VALIDITY_RANGE_MS,
  commitHeaderMatchesValidityUpperBound,
  decodeActiveOperatorDatum,
  isCommitValidityInterval,
} from "./state-queue-transactions.commit-layout-fields.js";

export const buildCommitBlockHeaderTxProgram = ({
  lucid,
  contracts,
  latestBlock,
  updatedNodeDatum,
  newHeader,
  validFrom,
  validTo,
  witness,
  headerNodeLovelace = STATE_QUEUE_NODE_MIN_LOVELACE,
  activeOperatorMaturityDurationMs = ACTIVE_OPERATOR_MATURITY_DURATION_MS,
}: CommitBlockHeaderParams): Effect.Effect<
  CommitBlockHeaderResult,
  StateQueueError | HashingError
> =>
  Effect.gen(function* () {
    if (witness.correctionLockRefInput.datum !== "Idle") {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Refusing to append while state correction is locked",
          cause: Data.to(
            witness.correctionLockRefInput.datum,
            CorrectionLockDatum,
          ),
        }),
      );
    }
    const queueIsEmpty = latestBlock.datum.key === "Empty";
    if (
      queueIsEmpty !== (witness.confirmedStateRefInput === undefined) ||
      (queueIsEmpty && witness.headStateQueueNodeRefInput !== undefined)
    ) {
      return yield* Effect.fail(
        new StateQueueError({
          message:
            "Refusing to build a commit transaction without the canonical root/head append-fence witnesses",
          cause: `queue_empty=${String(queueIsEmpty)},confirmed_state_ref=${String(witness.confirmedStateRefInput !== undefined)},head_ref=${String(witness.headStateQueueNodeRefInput !== undefined)}`,
        }),
      );
    }
    const inclusiveValidityUpperBound = validTo - 1;
    if (!isCommitValidityInterval({ validFrom, validTo })) {
      return yield* Effect.fail(
        new StateQueueError({
          message:
            "Refusing to build a commit transaction with an invalid bounded validity interval",
          cause: `valid_from_ms=${String(validFrom)},valid_to_ms=${String(validTo)},max_range_ms=${COMMIT_MAX_VALIDITY_RANGE_MS.toString()}`,
        }),
      );
    }
    if (
      !commitHeaderMatchesValidityUpperBound({
        headerEndTime: newHeader.endTime,
        validTo,
      })
    ) {
      return yield* Effect.fail(
        new StateQueueError({
          message:
            "Refusing to build a commit transaction whose header end-time is not the inclusive validity upper bound",
          cause: `header_end_time_ms=${newHeader.endTime.toString()},valid_to_ms=${validTo.toString()},inclusive_upper_bound_ms=${inclusiveValidityUpperBound.toString()}`,
        }),
      );
    }
    if (!queueIsEmpty) {
      const head =
        witness.headStateQueueNodeRefInput === undefined
          ? latestBlock
          : yield* utxoToStateQueueUTxO(
              witness.headStateQueueNodeRefInput,
              contracts.stateQueue.policyId,
            ).pipe(
              Effect.mapError(
                (cause) =>
                  new StateQueueError({
                    message: "Failed to decode canonical append-fence head",
                    cause,
                  }),
              ),
            );
      const headNode = yield* getStateQueueNodeFromStateQueueDatum(
        head.datum,
      ).pipe(
        Effect.mapError(
          (cause) =>
            new StateQueueError({
              message: "Failed to inspect append-fence head DA deadline",
              cause,
            }),
        ),
      );
      yield* assertCommitHeadDaDeadline(headNode, inclusiveValidityUpperBound);
    }
    const newHeaderHash = yield* hashBlockHeader(newHeader);
    const headerNodeUnit = toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + newHeaderHash,
    );
    const commitMintAssets = { [headerNodeUnit]: 1n };
    const headerNodeOutputAssets = {
      lovelace: headerNodeLovelace,
      ...commitMintAssets,
    };
    const appendedNodeDatum: LinkedListNodeView = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: ("validationTracesRoot" in newHeader
        ? castStateQueueNodeToData({
            proven_fraud: null,
            header: newHeader,
            da_attestation: NO_DA_ATTESTATION,
          })
        : castStateQueueNodeToData({
            proven_fraud: null,
            header: newHeader,
            da_attestation: NO_DA_ATTESTATION,
          })) as LinkedListNodeView["data"],
    };
    const appendedNodeDatumCbor = encodeLinkedListNodeView(appendedNodeDatum);
    const updatedNodeDatumCbor = encodeLinkedListNodeView(updatedNodeDatum);
    const updatedActiveOperatorDatumCbor = yield* Effect.try({
      try: () => {
        const activeOperatorLinkedListDatum = Data.from(
          witness.activeOperatorInput.datum,
          LinkedListDatum,
        );
        const activeOperatorNodeView = linkedListDatumToNodeView(
          activeOperatorLinkedListDatum,
          ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + witness.operatorKeyHash,
        );
        const activeOperatorDatum = decodeActiveOperatorDatum(
          activeOperatorNodeView.data,
        );
        return encodeLinkedListNodeView({
          ...activeOperatorNodeView,
          data: castActiveOperatorDatumToData({
            ...activeOperatorDatum,
            bond_unlock_time:
              BigInt(validTo) - 1n + activeOperatorMaturityDurationMs,
          }) as LinkedListNodeView["data"],
        });
      },
      catch: (cause) =>
        new StateQueueError({
          message:
            "Failed to update active-operator bond-hold datum for commit tx",
          cause,
        }),
    });

    const makeBaseCommitTx = (
      stateQueueCommitRedeemer: BuildTxWithRedeemer | string,
    ) =>
      lucid
        .newTx()
        .validFrom(validFrom)
        .validTo(validTo)
        .collectFrom([latestBlock.utxo], stateQueueCommitRedeemer)
        .pay.ToContract(
          contracts.stateQueue.spendingScriptAddress,
          {
            kind: "inline",
            value: appendedNodeDatumCbor,
          },
          headerNodeOutputAssets,
        )
        .pay.ToContract(
          contracts.stateQueue.spendingScriptAddress,
          {
            kind: "inline",
            value: updatedNodeDatumCbor,
          },
          latestBlock.utxo.assets,
        );

    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new StateQueueError({
          message:
            "Cannot build a state-queue commit yield without a configured Lucid network",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const tx = yield* buildDeterministicCommitTxBuilder({
      contracts,
      witness,
      headerNodeUnit,
      appendedNodeDatumCbor,
      previousHeaderNodeDatumCbor: updatedNodeDatumCbor,
      updatedActiveOperatorDatumCbor,
      commitMintAssets,
      yieldRewardAddress: scriptRewardAddress(
        network,
        contracts.stateQueue.yields.commit.withdrawalScript,
      ),
      makeBaseCommitTx,
    });

    return { tx, newHeaderHash };
  });

export type StateQueueMergeReferenceScripts = {
  readonly stateQueueSpending?: UTxO;
  readonly stateQueueMinting?: UTxO;
  readonly settlementMinting?: UTxO;
};

export type MergeToConfirmedStateParams = {
  readonly lucid: LucidEvolution;
  readonly fetchConfig: StateQueueFetchConfig;
  readonly contracts: MidgardValidators;
  readonly confirmedUTxO: StateQueueUTxO;
  readonly firstBlockUTxO: StateQueueUTxO;
  readonly validFrom: number;
  readonly presetWalletInputs?: readonly UTxO[];
  readonly hubOracleRefInput: UTxO;
  /** Authenticated deployment singleton; merge is permitted only while Idle. */
  readonly correctionLockRefInput: CorrectionLockUTxO;
  readonly stateQueueMergeYieldRefInput: UTxO;
  readonly referenceScripts?: StateQueueMergeReferenceScripts;
  readonly settlementOutputLovelace?: bigint;
};

export type MergeRedeemerLayout = {
  readonly yieldToRefInputIndex: number;
  readonly confirmedStateOutputIndex: number;
  readonly settlementOutputIndex: number;
  readonly stateQueueRedeemerIndex: number;
  readonly settlementRedeemerIndex: number;
  readonly hubOracleRefInputIndex: number;
};

export type MergeLayoutDiagnostics = {
  readonly stateQueueRedeemerTxInfoIndex: number;
  readonly settlementRedeemerTxInfoIndex: number;
  readonly stateQueueRedeemerCbor: string;
  readonly settlementRedeemerCbor: string;
};

export type MergeToConfirmedStateResult = {
  readonly tx: TxSignBuilder;
  readonly headerNodeKey: string;
  readonly blockHeader: Header;
  readonly layout: MergeRedeemerLayout;
  readonly diagnostics: MergeLayoutDiagnostics;
};

const makeJsonSafe = (value: unknown): unknown => {
  try {
    return JSON.parse(
      JSON.stringify(value, (_key, nestedValue) =>
        typeof nestedValue === "bigint" ? nestedValue.toString() : nestedValue,
      ),
    ) as unknown;
  } catch {
    return formatUnknownError(value);
  }
};

export const mergeStateQueueError = (
  errorCode: string,
  message: string,
  cause: unknown,
): StateQueueError =>
  new StateQueueError({
    message: `${errorCode}: ${message}`,
    cause: {
      error_code: errorCode,
      details: makeJsonSafe(cause),
    },
  });
