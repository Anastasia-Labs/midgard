import {
  Assets,
  type BuildTxWithRedeemer,
  Data,
  LucidEvolution,
  toUnit,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { HashingError } from "./common.js";
import { CorrectionLockDatum } from "./correction-lock.js";
import {
  castStateQueueNodeToData,
  hashBlockHeader,
  NO_DA_ATTESTATION,
} from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  LinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import { StateQueueError } from "./state-queue.build-state-queue-removal-tx.js";
import {
  type EmulatorStateQueueCommitBlockHeaderParams,
  type StateQueueFetchConfig,
} from "./state-queue.emulator-state-queue-commit-block-header-params.js";
import {
  applyStateQueueZeroYield,
  encodeActiveOperatorSpendRedeemer,
  STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER,
  STATE_QUEUE_NODE_MIN_LOVELACE,
  StateQueueRedeemer,
} from "./state-queue.state-queue-redeemer-schema.js";
import {
  requireInputIndex,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

export const incompleteEmulatorCommitBlockHeaderTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  params: EmulatorStateQueueCommitBlockHeaderParams,
): Effect.Effect<TxBuilder, HashingError | StateQueueError> =>
  Effect.gen(function* () {
    if (params.correctionLockRefInput.datum !== "Idle") {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Refusing to append while state correction is locked",
          cause: Data.to(
            params.correctionLockRefInput.datum,
            CorrectionLockDatum,
          ),
        }),
      );
    }
    const queueIsEmpty = params.anchorUTxO.datum.key === "Empty";
    if (
      queueIsEmpty !== (params.confirmedStateRefInput === undefined) ||
      (queueIsEmpty && params.headStateQueueNodeRefInput !== undefined)
    ) {
      return yield* Effect.fail(
        new StateQueueError({
          message:
            "Refusing to build an emulator commit without the canonical root/head append-fence witnesses",
          cause: `queue_empty=${String(queueIsEmpty)},confirmed_state_ref=${String(params.confirmedStateRefInput !== undefined)},head_ref=${String(params.headStateQueueNodeRefInput !== undefined)}`,
        }),
      );
    }
    const newHeaderHash = yield* hashBlockHeader(params.newHeader);
    const newBlockAssetName =
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + newHeaderHash;
    const newBlockAssets: Assets = {
      [toUnit(config.stateQueuePolicyId, newBlockAssetName)]: 1n,
    };
    const continuedAnchorDatum: LinkedListNodeView = {
      ...params.anchorUTxO.datum,
      next: { Key: { key: newHeaderHash } },
    };
    const newBlockDatum: LinkedListNodeView = {
      key: { Key: { key: newHeaderHash } },
      next: "Empty",
      data: castStateQueueNodeToData({
        proven_fraud: null,
        header: params.newHeader,
        da_attestation: NO_DA_ATTESTATION,
      }) as LinkedListNodeView["data"],
    };
    const newBlockDatumCbor = encodeLinkedListNodeView(newBlockDatum);
    const continuedAnchorDatumCbor =
      encodeLinkedListNodeView(continuedAnchorDatum);
    const continuedAnchorUnit = toUnit(
      config.stateQueuePolicyId,
      params.anchorUTxO.assetName,
    );
    const stateQueueCommitRedeemer = ((ctx) =>
      Data.to(
        {
          CommitBlockHeader: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              params.yieldWitness.referenceInput,
              "emulator state-queue commit yield target",
            ),
            new_block_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === config.stateQueueAddress &&
                outputDatumCborMatches(output, newBlockDatumCbor) &&
                (output.assets[
                  toUnit(config.stateQueuePolicyId, newBlockAssetName)
                ] ?? 0n) === 1n,
              "emulator state-queue commit new block",
            ),
            continued_latest_block_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === config.stateQueueAddress &&
                outputDatumCborMatches(output, continuedAnchorDatumCbor) &&
                (output.assets[continuedAnchorUnit] ?? 0n) === 1n,
              "emulator state-queue commit continued latest block",
            ),
            operator: params.newHeader.operatorVkey,
            scheduler_ref_input_index: requireReferenceInputIndex(
              ctx,
              params.schedulerRefInput,
              "emulator state-queue commit scheduler",
            ),
            active_operators_input_index: requireInputIndex(
              ctx,
              params.activeOperatorInput,
              "emulator state-queue commit active operator",
            ),
            active_operators_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              params.activeOperatorInput,
              "emulator state-queue commit active operator",
            ),
            m_confirmed_state_ref_input_index:
              params.confirmedStateRefInput === undefined
                ? null
                : requireReferenceInputIndex(
                    ctx,
                    params.confirmedStateRefInput,
                    "emulator state-queue commit confirmed-state root",
                  ),
            m_head_state_queue_node_ref_input_index:
              params.headStateQueueNodeRefInput === undefined
                ? null
                : requireReferenceInputIndex(
                    ctx,
                    params.headStateQueueNodeRefInput,
                    "emulator state-queue commit current head",
                  ),
          },
        } satisfies StateQueueRedeemer,
        StateQueueRedeemer,
      )) satisfies BuildTxWithRedeemer;
    const stateQueueCommitSpendRedeemer = (() =>
      STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER) satisfies BuildTxWithRedeemer;

    const additionalInputs = params.additionalInputs ?? [];
    let tx = lucid.newTx();
    if (additionalInputs.length > 0) {
      tx = tx.collectFrom([...additionalInputs]);
    }
    tx = tx
      .collectFrom([params.anchorUTxO.utxo], stateQueueCommitSpendRedeemer)
      .collectFrom(
        [params.activeOperatorInput],
        encodeActiveOperatorSpendRedeemer(params.activeOperatorSpendRedeemer),
      )
      .readFrom([
        ...(params.additionalRefInputs ?? []),
        params.schedulerRefInput,
        params.correctionLockRefInput.utxo,
        ...(params.confirmedStateRefInput === undefined
          ? []
          : [params.confirmedStateRefInput]),
        ...(params.headStateQueueNodeRefInput === undefined
          ? []
          : [params.headStateQueueNodeRefInput]),
        params.yieldWitness.referenceInput,
      ])
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: newBlockDatumCbor },
        {
          ...newBlockAssets,
          lovelace: params.headerNodeLovelace ?? STATE_QUEUE_NODE_MIN_LOVELACE,
        },
      )
      .pay.ToContract(
        config.stateQueueAddress,
        {
          kind: "inline",
          value: continuedAnchorDatumCbor,
        },
        params.anchorUTxO.utxo.assets,
      )
      .mintAssets(newBlockAssets, stateQueueCommitRedeemer)
      .addSignerKey(params.newHeader.operatorVkey)
      .attach.Script(params.stateQueueSpendingScript)
      .attach.Script(params.stateQueueMintingScript)
      .attach.Script(params.activeOperatorSpendingScript);

    tx = applyStateQueueZeroYield(lucid, tx, params.yieldWitness);

    if (params.validFrom !== undefined) {
      tx = tx.validFrom(Number(params.validFrom));
    }
    if (params.validTo !== undefined) {
      tx = tx.validTo(Number(params.validTo));
    }
    if (params.continuedActiveOperatorOutput !== undefined) {
      tx = tx.pay.ToContract(
        params.continuedActiveOperatorOutput.address,
        {
          kind: "inline",
          value: params.continuedActiveOperatorOutput.datum,
        },
        params.continuedActiveOperatorOutput.assets,
      );
    }

    return tx;
  });
