import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  type Assets,
  type BuildTxWithRedeemer,
  Data,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "./cardano-addresses.js";
import type { DataCoercionError, HashingError } from "./common.js";
import { outputReferenceFromUTxO } from "./common.js";
import { CorrectionLockDatum } from "./correction-lock.js";
import {
  castConfirmedStateToData,
  type ConfirmedState,
  confirmedStateNextHeaderProtocolVersion,
  getStateQueueNodeFromStateQueueDatum,
  hashBlockHeader,
} from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "./linked-list.js";
import { SettlementDatum, SettlementMintRedeemer } from "./settlement.js";
import {
  encodeStateQueueYieldRedeemer,
  getConfirmedStateFromStateQueueDatum,
  StateQueueError,
  StateQueueRedeemer,
} from "./state-queue.js";
import { assertMergeRedeemerInvariants } from "./state-queue-transactions.assert-merge-redeemer-invariants.js";
import {
  type MergeLayoutDiagnostics,
  type MergeRedeemerLayout,
  mergeStateQueueError,
  type MergeToConfirmedStateParams,
  type MergeToConfirmedStateResult,
} from "./state-queue-transactions.build-commit-block-header-tx-program.js";
import {
  encodeStateQueueLinkedListMutationSpendRedeemer,
  MIN_SETTLEMENT_OUTPUT_LOVELACE,
} from "./state-queue-transactions.commit-layout-fields.js";
import {
  deriveMergeLayoutFromRedeemerContext,
  makeSettlementSpawnRedeemer,
  makeStateQueueMergeRedeemer,
} from "./state-queue-transactions.derive-merge-layout-from-redeemer-context.js";
import { completeOptionsWithLocalEval } from "./tx-completion.js";
import { requireOwnMintPurpose } from "./tx-context-redeemer.js";

export const buildMergeToConfirmedStateTxProgram = ({
  lucid,
  fetchConfig,
  contracts,
  confirmedUTxO,
  firstBlockUTxO,
  validFrom,
  presetWalletInputs,
  hubOracleRefInput,
  correctionLockRefInput,
  stateQueueMergeYieldRefInput,
  referenceScripts,
  settlementOutputLovelace = MIN_SETTLEMENT_OUTPUT_LOVELACE,
}: MergeToConfirmedStateParams): Effect.Effect<
  MergeToConfirmedStateResult,
  StateQueueError | DataCoercionError | HashingError
> =>
  Effect.gen(function* () {
    if (correctionLockRefInput.datum !== "Idle") {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Refusing to merge while state correction is locked",
          cause: Data.to(correctionLockRefInput.datum, CorrectionLockDatum),
        }),
      );
    }
    const { data: confirmedStateData } =
      yield* getConfirmedStateFromStateQueueDatum(confirmedUTxO.datum);
    if (confirmedStateNextHeaderProtocolVersion(confirmedStateData) === null) {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Failed to build merge transaction",
          cause: `invalid confirmed state protocol identity version=${confirmedStateData.protocolVersion.toString()},header_hash=${confirmedStateData.headerHash}`,
        }),
      );
    }
    const firstBlockNode = yield* getStateQueueNodeFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    if (firstBlockNode.proven_fraud !== null) {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Refusing to merge a header with completed fraud",
          cause: `proof=${firstBlockNode.proven_fraud}; state correction is required`,
        }),
      );
    }
    const blockHeader = firstBlockNode.header;
    if (firstBlockUTxO.datum.key === "Empty") {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Failed to build merge transaction",
          cause: "first queued block cannot be a root node",
        }),
      );
    }
    const headerNodeKey = firstBlockUTxO.datum.key.Key.key;
    const recomputedHeaderHash = yield* hashBlockHeader(blockHeader);
    if (recomputedHeaderHash !== headerNodeKey) {
      return yield* Effect.fail(
        new StateQueueError({
          message:
            "Failed to build merge transaction: queued block key/hash mismatch",
          cause: `datumKey=${headerNodeKey},computed=${recomputedHeaderHash}`,
        }),
      );
    }

    const updatedConfirmedState: ConfirmedState = {
      headerHash: headerNodeKey,
      prevHeaderHash: confirmedStateData.headerHash,
      utxoRoot: blockHeader.utxosRoot,
      startTime: confirmedStateData.startTime,
      endTime: blockHeader.endTime,
      protocolVersion: blockHeader.protocolVersion,
    };
    const updatedConfirmedNodeDatum: LinkedListNodeView = {
      ...confirmedUTxO.datum,
      data: castConfirmedStateToData(
        updatedConfirmedState,
      ) as LinkedListNodeView["data"],
      next: firstBlockUTxO.datum.next,
    };

    const stateQueueAssetsToBurn: Assets = {
      [toUnit(fetchConfig.stateQueuePolicyId, firstBlockUTxO.assetName)]: -1n,
    };
    const settlementUnit = toUnit(contracts.settlement.policyId, headerNodeKey);
    const settlementAssetsToMint: Assets = { [settlementUnit]: 1n };
    const settlementOutputAssets: Assets = {
      lovelace: settlementOutputLovelace,
      ...settlementAssetsToMint,
    };
    const settlementDatum = {
      deposits_root: blockHeader.depositsRoot,
      withdrawals_root: blockHeader.withdrawalsRoot,
      forced_transactions_root: blockHeader.forcedTransactionsRoot,
      transactions_root: blockHeader.transactionsRoot,
      resolution_claim: null,
    };
    const encodedConfirmedNodeDatum = encodeLinkedListNodeView(
      updatedConfirmedNodeDatum,
    );
    const encodedSettlementDatum = Data.to(settlementDatum, SettlementDatum);
    const mergeReferenceInputs = [
      hubOracleRefInput,
      correctionLockRefInput.utxo,
      stateQueueMergeYieldRefInput,
      ...(referenceScripts?.stateQueueSpending === undefined
        ? []
        : [referenceScripts.stateQueueSpending]),
      ...(referenceScripts?.stateQueueMinting === undefined
        ? []
        : [referenceScripts.stateQueueMinting]),
      ...(referenceScripts?.settlementMinting === undefined
        ? []
        : [referenceScripts.settlementMinting]),
    ];

    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        mergeStateQueueError(
          "E_MERGE_NETWORK_UNDEFINED",
          "Cannot build a state-queue merge yield without a configured Lucid network",
          "lucid.config().network is undefined",
        ),
      );
    }
    const makeMergeTx = (
      encodedStateQueueMergeRedeemer: BuildTxWithRedeemer | string,
      encodedSettlementSpawnRedeemer: BuildTxWithRedeemer | string,
    ) =>
      lucid
        .newTx()
        .validFrom(validFrom)
        .collectFrom(
          [confirmedUTxO.utxo, firstBlockUTxO.utxo],
          encodeStateQueueLinkedListMutationSpendRedeemer(),
        )
        .readFrom(mergeReferenceInputs)
        .pay.ToContract(
          fetchConfig.stateQueueAddress,
          { kind: "inline", value: encodedConfirmedNodeDatum },
          confirmedUTxO.utxo.assets,
        )
        .pay.ToContract(
          contracts.settlement.spendingScriptAddress,
          { kind: "inline", value: encodedSettlementDatum },
          settlementOutputAssets,
        )
        .mintAssets(stateQueueAssetsToBurn, encodedStateQueueMergeRedeemer)
        .mintAssets(settlementAssetsToMint, encodedSettlementSpawnRedeemer)
        .withdraw(
          scriptRewardAddress(
            network,
            contracts.stateQueue.yields.merge.withdrawalScript,
          ),
          0n,
          encodeStateQueueYieldRedeemer(),
        );

    const makeMergeTxWithScripts = (
      encodedStateQueueMergeRedeemer: BuildTxWithRedeemer | string,
      encodedSettlementSpawnRedeemer: BuildTxWithRedeemer | string,
    ) => {
      const tx = makeMergeTx(
        encodedStateQueueMergeRedeemer,
        encodedSettlementSpawnRedeemer,
      );
      const withStateQueueSpendingScript =
        referenceScripts?.stateQueueSpending === undefined
          ? tx.attach.Script(contracts.stateQueue.spendingScript)
          : tx;
      const withStateQueueMintingScript =
        referenceScripts?.stateQueueMinting === undefined
          ? withStateQueueSpendingScript.attach.Script(
              contracts.stateQueue.mintingScript,
            )
          : withStateQueueSpendingScript;
      return referenceScripts?.settlementMinting === undefined
        ? withStateQueueMintingScript.attach.Script(
            contracts.settlement.mintingScript,
          )
        : withStateQueueMintingScript;
    };

    let mergeLayout: MergeRedeemerLayout | undefined;
    let stateQueueRedeemerCbor: string | undefined;
    let settlementRedeemerCbor: string | undefined;
    const layoutFromContext = (
      ctx: Parameters<BuildTxWithRedeemer>[0],
    ): MergeRedeemerLayout => {
      const layout = deriveMergeLayoutFromRedeemerContext({
        ctx,
        confirmedUTxO,
        hubOracleRefInput,
        stateQueueMergeYieldRefInput,
        stateQueuePolicyId: fetchConfig.stateQueuePolicyId,
        stateQueueAddress: fetchConfig.stateQueueAddress,
        encodedConfirmedNodeDatum,
        settlementPolicyId: contracts.settlement.policyId,
        settlementAddress: contracts.settlement.spendingScriptAddress,
        encodedSettlementDatum,
        settlementOutputAssets,
      });
      mergeLayout = layout;
      return layout;
    };
    const stateQueueMergeRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        fetchConfig.stateQueuePolicyId,
        "state-queue merge state_queue mint",
      );
      const redeemer = Data.to(
        makeStateQueueMergeRedeemer({
          layout: layoutFromContext(ctx),
          headerNodeKey,
          blockHeader,
          confirmedStateInputOutRef: outputReferenceFromUTxO(
            confirmedUTxO.utxo,
          ),
        }),
        StateQueueRedeemer,
      );
      stateQueueRedeemerCbor = redeemer;
      return redeemer;
    }) satisfies BuildTxWithRedeemer;
    const settlementSpawnRedeemer = ((ctx) => {
      requireOwnMintPurpose(
        ctx,
        contracts.settlement.policyId,
        "state-queue merge settlement mint",
      );
      const redeemer = Data.to(
        makeSettlementSpawnRedeemer({
          layout: layoutFromContext(ctx),
          headerNodeKey,
        }),
        SettlementMintRedeemer,
      );
      settlementRedeemerCbor = redeemer;
      return redeemer;
    }) satisfies BuildTxWithRedeemer;

    const txBuilder = yield* Effect.tryPromise({
      try: () =>
        makeMergeTxWithScripts(
          stateQueueMergeRedeemer,
          settlementSpawnRedeemer,
        ).complete(completeOptionsWithLocalEval({ presetWalletInputs })),
      catch: (cause) =>
        mergeStateQueueError(
          "E_MERGE_UPLC_EVAL_FAILED",
          "Failed to finalize the transaction for merging oldest block into confirmed state",
          { remote: formatUnknownError(cause) },
        ),
    });
    if (
      mergeLayout === undefined ||
      stateQueueRedeemerCbor === undefined ||
      settlementRedeemerCbor === undefined
    ) {
      return yield* Effect.fail(
        mergeStateQueueError(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "BuildTxWithRedeemer did not resolve final merge redeemer layout",
          {
            mergeLayoutResolved: mergeLayout !== undefined,
            stateQueueRedeemerResolved: stateQueueRedeemerCbor !== undefined,
            settlementRedeemerResolved: settlementRedeemerCbor !== undefined,
          },
        ),
      );
    }
    const resolvedMergeLayout = mergeLayout;
    const resolvedStateQueueRedeemerCbor = stateQueueRedeemerCbor;
    const resolvedSettlementRedeemerCbor = settlementRedeemerCbor;
    const diagnostics: MergeLayoutDiagnostics = {
      stateQueueRedeemerTxInfoIndex:
        resolvedMergeLayout.stateQueueRedeemerIndex,
      settlementRedeemerTxInfoIndex:
        resolvedMergeLayout.settlementRedeemerIndex,
      stateQueueRedeemerCbor: resolvedStateQueueRedeemerCbor,
      settlementRedeemerCbor: resolvedSettlementRedeemerCbor,
    };
    yield* Effect.logInfo(
      `🔸 Merge redeemer layout: confirmed_output=${resolvedMergeLayout.confirmedStateOutputIndex},settlement_output=${resolvedMergeLayout.settlementOutputIndex},hub_ref_input=${resolvedMergeLayout.hubOracleRefInputIndex},state_queue_redeemer_index=${resolvedMergeLayout.stateQueueRedeemerIndex},settlement_redeemer_index=${resolvedMergeLayout.settlementRedeemerIndex}`,
    );
    yield* Effect.try({
      try: () =>
        assertMergeRedeemerInvariants({
          layout: resolvedMergeLayout,
          headerNodeKey,
          blockHeader,
          confirmedStateInputOutRef: outputReferenceFromUTxO(
            confirmedUTxO.utxo,
          ),
          encodedStateQueueMergeRedeemer: resolvedStateQueueRedeemerCbor,
          encodedSettlementSpawnRedeemer: resolvedSettlementRedeemerCbor,
        }),
      catch: (cause) =>
        mergeStateQueueError(
          "E_MERGE_REDEEMER_INDEX_MISMATCH",
          "Failed final merge redeemer invariant checks",
          { cause: formatUnknownError(cause), layout: resolvedMergeLayout },
        ),
    });

    return {
      tx: txBuilder,
      headerNodeKey,
      blockHeader,
      layout: resolvedMergeLayout,
      diagnostics,
    };
  });
