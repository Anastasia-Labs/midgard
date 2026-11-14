import {
  Assets,
  type BuildTxWithRedeemer,
  Data,
  LucidEvolution,
  Script,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";

import { outputReferenceFromUTxO } from "./common.js";
import { type CorrectionLockUTxO } from "./correction-lock.js";
import {
  encodeLinkedListNodeView,
  LinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import {
  buildStateQueueRemovalTx,
  requireCorrectionLockForTarget,
  requireFraudProofCorrectionIdentity,
} from "./state-queue.build-state-queue-removal-tx.js";
import { resolveRemoveSlashingApproach } from "./state-queue.collect-remove-slashing-inputs.js";
import {
  type EmulatorStateQueueRemoveFraudulentBlocksLinkHeaderParams,
  type EmulatorStateQueueRemoveLastFraudulentBlockHeaderParams,
  type StateQueueFetchConfig,
  type StateQueueRemoveReferenceScriptUTxOs,
} from "./state-queue.emulator-state-queue-commit-block-header-params.js";
import {
  StateQueueRedeemer,
  type StateQueueUTxO,
  type StateQueueYieldWitness,
} from "./state-queue.state-queue-redeemer-schema.js";
import {
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

/**
 * Production-shaped RemoveFraudulentBlockHeader + RemoveLastFraudulentBlock
 * builder. Reference-script UTxOs are read as ordinary reference inputs; when
 * one is absent the matching inline script is attached for test/emulator flows.
 */
export const incompleteRemoveLastFraudulentBlockHeaderTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  params: EmulatorStateQueueRemoveLastFraudulentBlockHeaderParams,
): TxBuilder => {
  const fraudulentBlocksHeaderHash =
    params.fraudulentBlocksHeaderHash ??
    params.fraudulentBlockUTxO.assetName.slice(
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
    );
  const correctionIdentity = requireFraudProofCorrectionIdentity(
    params.fraudProofRefInput,
    params.fraudProofPolicyId,
  );
  requireCorrectionLockForTarget(
    params.correctionLockInput,
    fraudulentBlocksHeaderHash,
    correctionIdentity,
  );
  const assetsToBurn: Assets = {
    [toUnit(config.stateQueuePolicyId, params.fraudulentBlockUTxO.assetName)]:
      -1n,
  };
  const continuedAnchorDatum: LinkedListNodeView = {
    ...params.anchorUTxO.datum,
    next: "Empty",
  };
  const continuedAnchorDatumCbor =
    encodeLinkedListNodeView(continuedAnchorDatum);
  const continuedAnchorUnit = toUnit(
    config.stateQueuePolicyId,
    params.anchorUTxO.assetName,
  );
  const defaultStateQueueMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.stateQueuePolicyId,
      "emulator state-queue remove burn",
    );
    return Data.to(
      {
        RemoveFraudulentBlockHeader: {
          yield_to_ref_input_index: requireReferenceInputIndex(
            ctx,
            params.yieldWitness.referenceInput,
            "emulator state-queue fraud removal yield target",
          ),
          fraudulent_operator: params.fraudulentOperator,
          fraudulent_blocks_header_hash: fraudulentBlocksHeaderHash,
          slashing_approach: resolveRemoveSlashingApproach(
            ctx,
            params.slashing,
          ),
          fraud_proof_ref_input_index: requireReferenceInputIndex(
            ctx,
            params.fraudProofRefInput,
            "emulator state-queue remove fraud proof",
          ),
          block_removal_approach: {
            RemoveLastFraudulentBlock: {
              anchor_element_input_outref: outputReferenceFromUTxO(
                params.anchorUTxO.utxo,
              ),
              anchor_element_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.address === config.stateQueueAddress &&
                  outputDatumCborMatches(output, continuedAnchorDatumCbor) &&
                  (output.assets[continuedAnchorUnit] ?? 0n) === 1n,
                "emulator state-queue remove continued anchor",
              ),
            },
          },
        },
      } satisfies StateQueueRedeemer,
      StateQueueRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  return buildStateQueueRemovalTx(lucid, config.stateQueueAddress, {
    collectedStateQueueInputs: [
      params.anchorUTxO.utxo,
      params.fraudulentBlockUTxO.utxo,
    ],
    continuedOutput: {
      datum: continuedAnchorDatumCbor,
      // Keep the removed node's state-queue rent inside the authenticated
      // queue instead of leaving it for wallet change.  A reward-bearing
      // slash must have exactly one output at the prover enterprise address;
      // allowing Lucid to return this residual ADA to the submitting wallet
      // would create a second prover output whenever that wallet is the prover.
      assets: {
        ...params.anchorUTxO.utxo.assets,
        lovelace:
          (params.anchorUTxO.utxo.assets.lovelace ?? 0n) +
          (params.fraudulentBlockUTxO.utxo.assets.lovelace ?? 0n),
      },
    },
    assetsToBurn,
    stateQueueMintRedeemer:
      params.stateQueueMintRedeemer ?? defaultStateQueueMintRedeemer,
    additionalInputs: params.additionalInputs,
    validFrom: params.validFrom,
    validTo: params.validTo,
    fraudProofRefInput: params.fraudProofRefInput,
    hubOracleRefInput: params.hubOracleRefInput,
    correctionLockInput: params.correctionLockInput,
    correctionLockOutputDatum: "Idle",
    correctionLockSpendingScript: params.correctionLockSpendingScript,
    additionalRefInputs: params.additionalRefInputs,
    stateQueueSpendingScript: params.stateQueueSpendingScript,
    stateQueueMintingScript: params.stateQueueMintingScript,
    referenceScripts: params.referenceScripts,
    slashing: params.slashing,
    yieldWitness: params.yieldWitness,
  });
};

/**
 * Production-shaped RemoveFraudulentBlockHeader +
 * RemoveFraudulentBlocksLink builder. This removes the immediate successor of
 * a fraud-proved state-queue block and preserves the fraud-proved block with
 * its successor link spliced forward.
 */
export const incompleteRemoveFraudulentBlocksLinkTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  params: EmulatorStateQueueRemoveFraudulentBlocksLinkHeaderParams,
): TxBuilder => {
  const removedBlockHash = params.removedBlockUTxO.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  const correctionIdentity = requireFraudProofCorrectionIdentity(
    params.fraudProofRefInput,
    params.fraudProofPolicyId,
  );
  requireCorrectionLockForTarget(
    params.correctionLockInput,
    params.fraudulentBlocksHeaderHash,
    correctionIdentity,
  );
  if (
    params.fraudulentBlockUTxO.datum.next === "Empty" ||
    params.fraudulentBlockUTxO.datum.next.Key.key !== removedBlockHash
  ) {
    throw new Error(
      "RemoveFraudulentBlocksLink requires the removed block to be the immediate successor of the fraud-proved block.",
    );
  }

  const assetsToBurn: Assets = {
    [toUnit(config.stateQueuePolicyId, params.removedBlockUTxO.assetName)]: -1n,
  };
  const continuedFraudulentNodeDatum: LinkedListNodeView = {
    ...params.fraudulentBlockUTxO.datum,
    next: params.removedBlockUTxO.datum.next,
  };
  const continuedFraudulentNodeDatumCbor = encodeLinkedListNodeView(
    continuedFraudulentNodeDatum,
  );
  const continuedFraudulentNodeUnit = toUnit(
    config.stateQueuePolicyId,
    params.fraudulentBlockUTxO.assetName,
  );
  const defaultStateQueueMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.stateQueuePolicyId,
      "state-queue remove fraudulent successor burn",
    );
    return Data.to(
      {
        RemoveFraudulentBlockHeader: {
          yield_to_ref_input_index: requireReferenceInputIndex(
            ctx,
            params.yieldWitness.referenceInput,
            "state-queue fraud removal yield target",
          ),
          fraudulent_operator: params.fraudulentOperator,
          fraudulent_blocks_header_hash: params.fraudulentBlocksHeaderHash,
          slashing_approach: resolveRemoveSlashingApproach(
            ctx,
            params.slashing,
          ),
          fraud_proof_ref_input_index: requireReferenceInputIndex(
            ctx,
            params.fraudProofRefInput,
            "state-queue remove fraud proof",
          ),
          block_removal_approach: {
            RemoveFraudulentBlocksLink: {
              fraudulent_node_input_outref: outputReferenceFromUTxO(
                params.fraudulentBlockUTxO.utxo,
              ),
              fraudulent_node_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.address === config.stateQueueAddress &&
                  outputDatumCborMatches(
                    output,
                    continuedFraudulentNodeDatumCbor,
                  ) &&
                  (output.assets[continuedFraudulentNodeUnit] ?? 0n) === 1n,
                "state-queue remove continued fraud-proved block",
              ),
            },
          },
        },
      } satisfies StateQueueRedeemer,
      StateQueueRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  return buildStateQueueRemovalTx(lucid, config.stateQueueAddress, {
    collectedStateQueueInputs: [
      params.fraudulentBlockUTxO.utxo,
      params.removedBlockUTxO.utxo,
    ],
    continuedOutput: {
      datum: continuedFraudulentNodeDatumCbor,
      // Preserve the pruned node's state-queue rent in its authenticated
      // anchor.  This makes the removal value-conserving without an implicit
      // wallet change output that could duplicate the exact prover reward.
      assets: {
        ...params.fraudulentBlockUTxO.utxo.assets,
        lovelace:
          (params.fraudulentBlockUTxO.utxo.assets.lovelace ?? 0n) +
          (params.removedBlockUTxO.utxo.assets.lovelace ?? 0n),
      },
    },
    assetsToBurn,
    stateQueueMintRedeemer:
      params.stateQueueMintRedeemer ?? defaultStateQueueMintRedeemer,
    additionalInputs: params.additionalInputs,
    validFrom: params.validFrom,
    validTo: params.validTo,
    fraudProofRefInput: params.fraudProofRefInput,
    hubOracleRefInput: params.hubOracleRefInput,
    correctionLockInput: params.correctionLockInput,
    correctionLockOutputDatum: {
      Locked: {
        target_header_hash: params.fraudulentBlocksHeaderHash,
        correction_identity: correctionIdentity,
      },
    },
    correctionLockSpendingScript: params.correctionLockSpendingScript,
    additionalRefInputs: params.additionalRefInputs,
    stateQueueSpendingScript: params.stateQueueSpendingScript,
    stateQueueMintingScript: params.stateQueueMintingScript,
    referenceScripts: params.referenceScripts,
    slashing: params.slashing,
    yieldWitness: params.yieldWitness,
  });
};

export type StateQueueTimeoutRemovalReferenceScriptUTxOs = Pick<
  StateQueueRemoveReferenceScriptUTxOs,
  "correctionLockSpend" | "stateQueueSpend" | "stateQueueMint"
>;

export type StateQueueTimeoutRemovalCommonParams = {
  readonly timedOutBlockUTxO: StateQueueUTxO;
  readonly additionalInputs?: readonly UTxO[];
  readonly additionalRefInputs?: readonly UTxO[];
  readonly hubOracleRefInput: UTxO;
  readonly correctionLockInput: CorrectionLockUTxO;
  readonly correctionLockSpendingScript: Script;
  readonly validFrom: bigint;
  readonly validTo: bigint;
  readonly stateQueueSpendingScript: Script;
  readonly stateQueueMintingScript: Script;
  readonly yieldWitness: StateQueueYieldWitness;
  readonly referenceScripts?: StateQueueTimeoutRemovalReferenceScriptUTxOs;
};

export type StateQueuePruneUnattestedDescendantParams =
  StateQueueTimeoutRemovalCommonParams & {
    readonly predecessorRefInput: StateQueueUTxO;
    readonly removedDescendantUTxO: StateQueueUTxO;
  };

export type StateQueueRemoveLastUnattestedBlockParams =
  StateQueueTimeoutRemovalCommonParams & {
    readonly predecessorUTxO: StateQueueUTxO;
  };
