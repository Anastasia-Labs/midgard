import {
  Assets,
  type BuildTxWithRedeemer,
  Data,
  LucidEvolution,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";

import { outputReferenceFromUTxO } from "./common.js";
import {
  type CorrectionIdentity,
  CorrectionLockDatum,
  CorrectionLockRedeemer,
} from "./correction-lock.js";
import {
  encodeLinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import { requireCorrectionLockForTarget } from "./state-queue.build-state-queue-removal-tx.js";
import { type StateQueueFetchConfig } from "./state-queue.emulator-state-queue-commit-block-header-params.js";
import {
  type StateQueuePruneUnattestedDescendantParams,
  type StateQueueRemoveLastUnattestedBlockParams,
  type StateQueueTimeoutRemovalCommonParams,
} from "./state-queue.incomplete-remove-fraudulent-blocks-link-tx-program.js";
import {
  applyStateQueueZeroYield,
  STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER,
  StateQueueRedeemer,
  type StateQueueUTxO,
} from "./state-queue.state-queue-redeemer-schema.js";
import {
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import { dedupeAndSortUtxos } from "./tx-out-ref-order.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

const requireBlockHeaderHash = (
  node: StateQueueUTxO,
  label: string,
): string => {
  if (
    node.datum.key === "Empty" ||
    !node.assetName.startsWith(STATE_QUEUE_NODE_ASSET_NAME_PREFIX)
  ) {
    throw new Error(`${label} must be a state-queue block node`);
  }
  const headerHash = node.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  if (node.datum.key.Key.key !== headerHash) {
    throw new Error(`${label} key does not match its state-queue asset name`);
  }
  return headerHash;
};

const timeoutRemovalBaseTx = ({
  lucid,
  config,
  params,
  collectedStateQueueInputs,
  continuedOutput,
  assetsToBurn,
  mintRedeemer,
  requiredReferenceInputs,
  correctionLockOutputDatum,
}: {
  readonly lucid: LucidEvolution;
  readonly config: StateQueueFetchConfig;
  readonly params: StateQueueTimeoutRemovalCommonParams;
  readonly collectedStateQueueInputs: readonly UTxO[];
  readonly continuedOutput: { readonly datum: string; readonly assets: Assets };
  readonly assetsToBurn: Assets;
  readonly mintRedeemer: BuildTxWithRedeemer;
  readonly requiredReferenceInputs: readonly UTxO[];
  readonly correctionLockOutputDatum: CorrectionLockDatum;
}): TxBuilder => {
  const referenceScriptInputs = Object.values(
    params.referenceScripts ?? {},
  ).filter((utxo): utxo is UTxO => utxo !== undefined);
  const referenceInputs = dedupeAndSortUtxos([
    params.hubOracleRefInput,
    ...requiredReferenceInputs,
    ...(params.additionalRefInputs ?? []),
    ...referenceScriptInputs,
    params.yieldWitness.referenceInput,
  ]);
  let tx = lucid
    .newTx()
    .validFrom(Number(params.validFrom))
    .validTo(Number(params.validTo));
  if ((params.additionalInputs ?? []).length > 0) {
    tx = tx.collectFrom([...(params.additionalInputs ?? [])]);
  }
  tx = tx
    .collectFrom(
      [...collectedStateQueueInputs],
      STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER,
    )
    .collectFrom([params.correctionLockInput.utxo], ((ctx) =>
      Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              params.hubOracleRefInput,
              "timeout correction lock hub oracle",
            ),
          },
        } satisfies CorrectionLockRedeemer,
        CorrectionLockRedeemer,
      )) satisfies BuildTxWithRedeemer)
    .readFrom(referenceInputs)
    .pay.ToContract(
      config.stateQueueAddress,
      { kind: "inline", value: continuedOutput.datum },
      continuedOutput.assets,
    )
    .pay.ToContract(
      params.correctionLockInput.utxo.address,
      {
        kind: "inline",
        value: Data.to(correctionLockOutputDatum, CorrectionLockDatum),
      },
      params.correctionLockInput.utxo.assets,
    )
    .mintAssets(assetsToBurn, mintRedeemer);
  if (params.referenceScripts?.stateQueueSpend === undefined) {
    tx = tx.attach.Script(params.stateQueueSpendingScript);
  }
  if (params.referenceScripts?.correctionLockSpend === undefined) {
    tx = tx.attach.Script(params.correctionLockSpendingScript);
  }
  const withMintScript =
    params.referenceScripts?.stateQueueMint === undefined
      ? tx.attach.Script(params.stateQueueMintingScript)
      : tx;
  return applyStateQueueZeroYield(lucid, withMintScript, params.yieldWitness);
};

/**
 * Permissionlessly prunes the immediate descendant of the authenticated,
 * unattested timed-out block. Its predecessor is retained as a reference input,
 * while the target is spent and continued with the descendant's successor link.
 */
export const incompletePruneUnattestedBlockDescendantTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  params: StateQueuePruneUnattestedDescendantParams,
): TxBuilder => {
  const timedOutHeaderHash = requireBlockHeaderHash(
    params.timedOutBlockUTxO,
    "timed-out block",
  );
  const correctionIdentity: CorrectionIdentity = "AttestationTimeout";
  requireCorrectionLockForTarget(
    params.correctionLockInput,
    timedOutHeaderHash,
    correctionIdentity,
  );
  const removedHeaderHash = requireBlockHeaderHash(
    params.removedDescendantUTxO,
    "timed-out descendant",
  );
  if (
    params.predecessorRefInput.datum.next === "Empty" ||
    params.predecessorRefInput.datum.next.Key.key !== timedOutHeaderHash ||
    params.timedOutBlockUTxO.datum.next === "Empty" ||
    params.timedOutBlockUTxO.datum.next.Key.key !== removedHeaderHash
  ) {
    throw new Error(
      "Timed-out descendant pruning requires predecessor -> timed-out block -> immediate descendant topology",
    );
  }
  const continuedTargetDatum = encodeLinkedListNodeView({
    ...params.timedOutBlockUTxO.datum,
    next: params.removedDescendantUTxO.datum.next,
  });
  const continuedTargetUnit = toUnit(
    config.stateQueuePolicyId,
    params.timedOutBlockUTxO.assetName,
  );
  const assetsToBurn: Assets = {
    [toUnit(config.stateQueuePolicyId, params.removedDescendantUTxO.assetName)]:
      -1n,
  };
  const mintRedeemer = ((ctx) =>
    Data.to(
      {
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: requireReferenceInputIndex(
            ctx,
            params.yieldWitness.referenceInput,
            "timed-out removal yield target",
          ),
          timed_out_header_hash: timedOutHeaderHash,
          removal_approach: {
            PruneUnattestedBlockDescendant: {
              predecessor_ref_input_index: requireReferenceInputIndex(
                ctx,
                params.predecessorRefInput.utxo,
                "timed-out removal predecessor",
              ),
              timed_out_node_input_outref: outputReferenceFromUTxO(
                params.timedOutBlockUTxO.utxo,
              ),
              timed_out_node_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.address === config.stateQueueAddress &&
                  outputDatumCborMatches(output, continuedTargetDatum) &&
                  (output.assets[continuedTargetUnit] ?? 0n) === 1n,
                "timed-out removal continued target",
              ),
            },
          },
        },
      } satisfies StateQueueRedeemer,
      StateQueueRedeemer,
    )) satisfies BuildTxWithRedeemer;
  return timeoutRemovalBaseTx({
    lucid,
    config,
    params,
    collectedStateQueueInputs: [
      params.timedOutBlockUTxO.utxo,
      params.removedDescendantUTxO.utxo,
    ],
    continuedOutput: {
      datum: continuedTargetDatum,
      assets: params.timedOutBlockUTxO.utxo.assets,
    },
    assetsToBurn,
    mintRedeemer,
    requiredReferenceInputs: [params.predecessorRefInput.utxo],
    correctionLockOutputDatum: {
      Locked: {
        target_header_hash: timedOutHeaderHash,
        correction_identity: correctionIdentity,
      },
    },
  });
};

/** Remove a terminal unattested target and preserve its immediate predecessor. */
export const incompleteRemoveLastUnattestedBlockTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  params: StateQueueRemoveLastUnattestedBlockParams,
): TxBuilder => {
  const timedOutHeaderHash = requireBlockHeaderHash(
    params.timedOutBlockUTxO,
    "timed-out block",
  );
  const correctionIdentity: CorrectionIdentity = "AttestationTimeout";
  requireCorrectionLockForTarget(
    params.correctionLockInput,
    timedOutHeaderHash,
    correctionIdentity,
  );
  if (
    params.predecessorUTxO.datum.next === "Empty" ||
    params.predecessorUTxO.datum.next.Key.key !== timedOutHeaderHash ||
    params.timedOutBlockUTxO.datum.next !== "Empty"
  ) {
    throw new Error(
      "Unattested timeout removal requires a terminal target linked directly from its predecessor",
    );
  }
  const continuedPredecessorDatum = encodeLinkedListNodeView({
    ...params.predecessorUTxO.datum,
    next: "Empty",
  });
  const continuedPredecessorUnit = toUnit(
    config.stateQueuePolicyId,
    params.predecessorUTxO.assetName,
  );
  const assetsToBurn: Assets = {
    [toUnit(config.stateQueuePolicyId, params.timedOutBlockUTxO.assetName)]:
      -1n,
  };
  const mintRedeemer = ((ctx) =>
    Data.to(
      {
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: requireReferenceInputIndex(
            ctx,
            params.yieldWitness.referenceInput,
            "timed-out removal yield target",
          ),
          timed_out_header_hash: timedOutHeaderHash,
          removal_approach: {
            RemoveLastUnattestedBlock: {
              predecessor_input_outref: outputReferenceFromUTxO(
                params.predecessorUTxO.utxo,
              ),
              predecessor_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.address === config.stateQueueAddress &&
                  outputDatumCborMatches(output, continuedPredecessorDatum) &&
                  (output.assets[continuedPredecessorUnit] ?? 0n) === 1n,
                "timed-out removal continued predecessor",
              ),
            },
          },
        },
      } satisfies StateQueueRedeemer,
      StateQueueRedeemer,
    )) satisfies BuildTxWithRedeemer;
  return timeoutRemovalBaseTx({
    lucid,
    config,
    params,
    collectedStateQueueInputs: [
      params.predecessorUTxO.utxo,
      params.timedOutBlockUTxO.utxo,
    ],
    continuedOutput: {
      datum: continuedPredecessorDatum,
      assets: params.predecessorUTxO.utxo.assets,
    },
    assetsToBurn,
    mintRedeemer,
    requiredReferenceInputs: [],
    correctionLockOutputDatum: "Idle",
  });
};
