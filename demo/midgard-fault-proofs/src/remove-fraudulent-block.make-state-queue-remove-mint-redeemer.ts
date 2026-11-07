import {
  outputReferenceFromUTxO,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerData,
  type StateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type RemoveFraudulentBlockContracts,
  type RemoveFraudulentBlockLayout,
} from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import {
  type RemoveFraudulentBlockSlashing,
  type RemoveTransactionKind,
  requireOutputIndexByUnit,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import { resolveStateQueueSlashingApproach } from "./remove-fraudulent-block.resolve-state-queue-slashing-approach.js";

export const makeStateQueueRemoveMintRedeemer = ({
  kind,
  anchor,
  removed,
  fraudulentOperator,
  fraudulentBlocksHeaderHash,
  fraudProofRefInput,
  yieldRefInput,
  slashing,
  contracts,
  onLayout,
}: {
  readonly kind: RemoveTransactionKind;
  readonly anchor: StateQueueUTxO;
  readonly removed: StateQueueUTxO;
  readonly fraudulentOperator: string;
  readonly fraudulentBlocksHeaderHash: string;
  readonly fraudProofRefInput: UTxO;
  readonly yieldRefInput: UTxO;
  readonly slashing: RemoveFraudulentBlockSlashing;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly onLayout: (layout: RemoveFraudulentBlockLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.stateQueuePolicyId,
      "remove-fraudulent-block state-queue burn",
    );
    const stateQueueRedeemerTxInfoIndex = requireMintRedeemerIndex(
      ctx,
      contracts.stateQueuePolicyId,
      "remove-fraudulent-block state-queue burn",
    );
    const { slashingApproach, layout: slashingLayout } =
      resolveStateQueueSlashingApproach({ ctx, slashing, contracts });
    const fraudulentNodeInput = kind === "remove-successor" ? anchor : removed;
    const fraudProofRefInputIndex = requireReferenceInputIndex(
      ctx,
      fraudProofRefInput,
      "remove-fraudulent-block fraud-proof token",
    );
    const continuedAnchorUnit = toUnit(
      contracts.stateQueuePolicyId,
      anchor.assetName,
    );
    const commonLayout = {
      fraudProofRefInputIndex,
      stateQueueRedeemerTxInfoIndex,
      ...slashingLayout,
    };
    const commonRedeemer = {
      yield_to_ref_input_index: requireReferenceInputIndex(
        ctx,
        yieldRefInput,
        "remove-fraudulent-block yield",
      ),
      fraudulent_operator: fraudulentOperator,
      fraudulent_blocks_header_hash: fraudulentBlocksHeaderHash,
      slashing_approach: slashingApproach,
      fraud_proof_ref_input_index: fraudProofRefInputIndex,
    };

    if (kind === "remove-successor") {
      const fraudulentNodeOutputIndex = requireOutputIndexByUnit({
        outputs: ctx.outputs,
        address: contracts.stateQueueAddress,
        unit: continuedAnchorUnit,
        label: "remove-fraudulent-block continued fraud-proved node",
      });
      onLayout({
        ...commonLayout,
        fraudulentNodeOutputIndex,
      });
      return Data.to(
        {
          RemoveFraudulentBlockHeader: {
            ...commonRedeemer,
            block_removal_approach: {
              RemoveFraudulentBlocksLink: {
                fraudulent_node_input_outref: outputReferenceFromUTxO(
                  fraudulentNodeInput.utxo,
                ),
                fraudulent_node_output_index: fraudulentNodeOutputIndex,
              },
            },
          },
        } satisfies StateQueueRedeemerData,
        StateQueueRedeemer,
      );
    }

    const anchorElementOutputIndex = requireOutputIndexByUnit({
      outputs: ctx.outputs,
      address: contracts.stateQueueAddress,
      unit: continuedAnchorUnit,
      label: "remove-fraudulent-block continued predecessor anchor",
    });
    onLayout({
      ...commonLayout,
      anchorElementOutputIndex,
    });
    return Data.to(
      {
        RemoveFraudulentBlockHeader: {
          ...commonRedeemer,
          block_removal_approach: {
            RemoveLastFraudulentBlock: {
              anchor_element_input_outref: outputReferenceFromUTxO(anchor.utxo),
              anchor_element_output_index: anchorElementOutputIndex,
            },
          },
        },
      } satisfies StateQueueRedeemerData,
      StateQueueRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
