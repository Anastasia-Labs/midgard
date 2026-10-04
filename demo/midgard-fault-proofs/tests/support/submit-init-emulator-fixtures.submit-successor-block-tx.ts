import "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";

import { MIDGARD_CONSENSUS_PROFILE, outRefLabel } from "@al-ft/midgard-core";
import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  CORRECTION_LOCK_ASSET_NAME,
  encodeLinkedListNodeView,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  Header,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  type MidgardValidators,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  Emulator,
  Lucid,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  findStateQueueYieldReferenceScript,
  type OperatorLifecycleReferenceScripts,
} from "./emulator/reference-scripts.js";
import { firstWalletUtxo } from "./submit-init-emulator-shared.js";

export const submitSuccessorBlockTx = async ({
  lucid,
  emulator,
  contracts,
  anchorBlockUnit,
  header,
  hubOracle,
  scheduler,
  activeOperatorNode,
  activeOperatorNodeUnit,
  validFrom,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly emulator: Emulator;
  readonly contracts: MidgardValidators & {
    readonly operatorLifecycleReferenceScripts?: OperatorLifecycleReferenceScripts;
  };
  readonly anchorBlockUnit: string;
  readonly header: Header;
  readonly hubOracle: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly activeOperatorNodeUnit: string;
  /** Short live window for successors committed after lifecycle transactions. */
  readonly validFrom?: bigint;
}): Promise<{
  readonly continuedAnchorOutRef: string;
  readonly successorOutRef: string;
  readonly successorHeaderHash: string;
  readonly successorBlockUnit: string;
  readonly activeOperatorNode: UTxO;
}> => {
  const [anchorBlockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    anchorBlockUnit,
  );
  if (anchorBlockUtxo === undefined) {
    throw new Error("Expected live state-queue anchor block for successor");
  }
  const anchorBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(anchorBlockUtxo, contracts.stateQueue.policyId),
  );
  const successorHeaderHash = await Effect.runPromise(hashBlockHeader(header));
  const successorBlockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + successorHeaderHash,
  );
  const commitFeeInput = await firstWalletUtxo(
    lucid,
    "successor commit fee input",
  );
  const commitValidFrom = validFrom ?? header.startTime - 60_000n;
  const commitValidTo = header.endTime + 1n;
  // Each successor starts where its predecessor ends, so its commit window
  // opens one header length after the previous commit's. The first successor
  // finds the emulator already inside that window; a second one does not, and
  // the emulator rejects a lower bound ahead of its clock. Advance to the
  // window's first slot, which is still inside the predecessor's own window
  // and so keeps the next successor's contiguity check honest.
  const firstCommitSlot = lucid.unixTimeToSlot(Number(commitValidFrom));
  const slotsUntilCommitWindow = firstCommitSlot - lucid.currentSlot();
  if (slotsUntilCommitWindow > 0) {
    emulator.awaitSlot(slotsUntilCommitWindow);
  }
  expect(
    commitValidTo,
    "successor commit validTo must be later than the emulator clock before submission",
  ).toBeGreaterThan(BigInt(emulator.now()));
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time:
          commitValidTo -
          1n +
          BigInt(MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs),
        inactivity_strikes: 0n,
      },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorCommitRedeemer = ((ctx) =>
    Data.to(
      {
        UpdateBondHoldNewState: {
          active_operator: header.operatorVkey,
          active_node_input_index: requireInputIndex(
            ctx,
            activeOperatorNode,
            "successor commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[activeOperatorNodeUnit] ?? 0n) === 1n,
            "successor commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracle,
            "successor commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "successor commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const [confirmedStateRefInput] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    toUnit(contracts.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME),
  );
  if (confirmedStateRefInput === undefined) {
    throw new Error("successor commit found no confirmed-state root witness");
  }
  const confirmedState = await Effect.runPromise(
    utxoToStateQueueUTxO(confirmedStateRefInput, contracts.stateQueue.policyId),
  );
  const [correctionLockUtxo] = await lucid.utxosAtWithUnit(
    contracts.correctionLock.spendingScriptAddress,
    toUnit(contracts.hubOracle.policyId, CORRECTION_LOCK_ASSET_NAME),
  );
  if (correctionLockUtxo === undefined) {
    throw new Error("successor commit found no correction-lock witness");
  }
  if (confirmedState.datum.next === "Empty") {
    throw new Error("successor commit found an empty state-queue root");
  }
  const headHeaderHash = confirmedState.datum.next.Key.key;
  const anchorHeaderHash = anchorBlock.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  const headStateQueueNodeRefInput =
    headHeaderHash === anchorHeaderHash
      ? undefined
      : (
          await lucid.utxosAtWithUnit(
            contracts.stateQueue.spendingScriptAddress,
            toUnit(
              contracts.stateQueue.policyId,
              STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headHeaderHash,
            ),
          )
        )[0];
  if (
    headHeaderHash !== anchorHeaderHash &&
    headStateQueueNodeRefInput === undefined
  ) {
    throw new Error(
      `successor commit found no authenticated queue-head witness ${headHeaderHash}`,
    );
  }
  const commitYieldRef = await findStateQueueYieldReferenceScript({
    lucid,
    contracts,
    arm: "commit",
  });
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: anchorBlock,
        newHeader: header,
        additionalInputs: [commitFeeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: scheduler,
        correctionLockRefInput: {
          utxo: correctionLockUtxo,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        confirmedStateRefInput,
        ...(headStateQueueNodeRefInput === undefined
          ? {}
          : { headStateQueueNodeRefInput }),
        additionalRefInputs: [
          hubOracle,
          ...(contracts.operatorLifecycleReferenceScripts?.initial ?? [])
            .filter((r) => r.name === "state-queue minting")
            .map((r) => r.utxo),
          ...(contracts.operatorLifecycleReferenceScripts?.active ?? [])
            .filter((r) => r.name === "active-operators spending")
            .map((r) => r.utxo),
        ],
        activeOperatorInput: activeOperatorNode,
        activeOperatorSpendRedeemer: activeOperatorCommitRedeemer,
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorNode.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
        yieldWitness: {
          referenceInput: commitYieldRef,
          script: contracts.stateQueue.yields.commit.withdrawalScript,
        },
      },
    ),
  );
  const commitUnsigned = await commitTx.complete({ localUPLCEval: true });
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await lucid.awaitTx(await commitSigned.submit());

  const [continuedAnchorUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    anchorBlockUnit,
  );
  const [successorUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    successorBlockUnit,
  );
  const [continuedActiveOperatorNode] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit,
  );
  if (
    continuedAnchorUtxo === undefined ||
    successorUtxo === undefined ||
    continuedActiveOperatorNode === undefined
  ) {
    throw new Error("Successor commit did not preserve expected queue nodes");
  }
  const continuedAnchor = await Effect.runPromise(
    utxoToStateQueueUTxO(continuedAnchorUtxo, contracts.stateQueue.policyId),
  );
  await Effect.runPromise(getHeaderFromStateQueueDatum(continuedAnchor.datum));
  expect(continuedAnchor.datum.next).toEqual({
    Key: { key: successorHeaderHash },
  });

  return {
    continuedAnchorOutRef: outRefLabel(continuedAnchorUtxo),
    successorOutRef: outRefLabel(successorUtxo),
    successorHeaderHash,
    successorBlockUnit,
    activeOperatorNode: continuedActiveOperatorNode,
  };
};

export type SuccessorBlockFixture = Awaited<
  ReturnType<typeof submitSuccessorBlockTx>
> & {
  readonly header: Header;
};

/**
 * Publication labels for the four double-spend step validators, following the
 * `fraudProof<Family>StepNN` deployment-entry naming style. Local to the
 * emulator fixtures: the step references reach the submitters as explicit
 * `referenceScriptUtxo` parameters, not through the deployment manifest.
 */
export const DOUBLE_SPEND_STEP_REFERENCE_NAMES = [
  "fraudProofDoubleSpend",
  "fraudProofDoubleSpendStep02",
  "fraudProofDoubleSpendStep03",
  "fraudProofDoubleSpendStep04",
] as const;
