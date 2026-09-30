import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  CORRECTION_LOCK_ASSET_NAME,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  hashBlockHeader,
  type Header,
  HUB_ORACLE_ASSET_NAME,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  SCHEDULER_ASSET_NAME,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
} from "../../src/runtime.js";
import { findStateQueueYieldReferenceScript } from "./emulator/reference-scripts.js";
import {
  makeFaultProofEmulatorHarness,
  network as emulatorNetwork,
} from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// Harness, committed header, reference scripts, removal category
// ---------------------------------------------------------------------------

export const makeValueNotPreservedEmulatorHarness = async ({
  alwaysFraudProofCatalogue = true,
}: { readonly alwaysFraudProofCatalogue?: boolean } = {}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realValueNotPreserved: true,
      alwaysFraudProofCatalogue,
    },
  });
  const family = harness.contracts.valueNotPreserved;
  const category = harness.catalogue.categories.valueNotPreserved;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the value-not-preserved contracts/category",
    );
  }
  if (
    category.categoryId !== FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.valueNotPreserved
  ) {
    throw new Error("Unexpected value-not-preserved catalogue category id");
  }
  return { ...harness, family, category };
};

export type ValueNotPreservedHarness = Awaited<
  ReturnType<typeof makeValueNotPreservedEmulatorHarness>
>;

/**
 * Commits `header` appended after the queued anchor block — the same
 * production-shaped commit transaction the harness setup submits for its
 * first block, re-anchored at a node instead of the confirmed-state root.
 */
export const commitHeaderAfterAnchorBlock = async ({
  harness,
  anchorBlockOutRef,
  header,
}: {
  readonly harness: ValueNotPreservedHarness;
  readonly anchorBlockOutRef: string;
  readonly header: Header;
}): Promise<{
  readonly headerHash: string;
  readonly blockOutRef: string;
  readonly stateQueueBlockUnit: string;
}> => {
  const lucid = harness.funderLucid;
  const { contracts } = harness;
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const anchorUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(anchorBlockOutRef, "--anchor-block-out-ref"),
    label: "value-not-preserved anchor block UTxO",
  });
  const anchorStateQueueUtxo = await Effect.runPromise(
    utxoToStateQueueUTxO(anchorUtxo, contracts.stateQueue.policyId),
  );
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      emulatorNetwork,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    unit: toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME),
    label: "value-not-preserved commit hub oracle",
  });
  const schedulerUtxo = await requireSingletonUtxo({
    lucid,
    address: contracts.scheduler.spendingScriptAddress,
    unit: toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
    label: "value-not-preserved commit scheduler",
  });
  const correctionLockUtxo = await requireSingletonUtxo({
    lucid,
    address: contracts.correctionLock.spendingScriptAddress,
    unit: toUnit(contracts.hubOracle.policyId, CORRECTION_LOCK_ASSET_NAME),
    label: "value-not-preserved commit correction lock",
  });
  const activeOperatorNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + header.operatorVkey,
  );
  const activeOperatorNode = await requireSingletonUtxo({
    lucid,
    address: contracts.activeOperators.spendingScriptAddress,
    unit: activeOperatorNodeUnit,
    label: "value-not-preserved commit active-operator node",
  });
  const commitValidFrom = header.startTime - 60_000n;
  const commitValidTo = header.endTime + 1n;
  if (commitValidTo <= BigInt(harness.emulator.now())) {
    throw new Error(
      "value-not-preserved successor commit validTo expired before submission",
    );
  }
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
            "value-not-preserved commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[activeOperatorNodeUnit] ?? 0n) === 1n,
            "value-not-preserved commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            "value-not-preserved commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "value-not-preserved commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  // Mirrors the harness setup's own commit: the funder wallet's first UTxO
  // funds the fee (the funder is the emulator's operator party; its change
  // may legitimately carry operator-side units).
  const [feeInput] = (await lucid.wallet().getUtxos()).filter(
    (utxo) =>
      utxo.datum == null && utxo.datumHash == null && utxo.scriptRef == null,
  );
  if (feeInput === undefined) {
    throw new Error(
      "value-not-preserved second commit found no funder fee UTxO",
    );
  }
  const [confirmedStateRefInput] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    toUnit(contracts.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME),
  );
  if (confirmedStateRefInput === undefined) {
    throw new Error(
      "value-not-preserved second commit found no confirmed-state root witness",
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
        anchorUTxO: anchorStateQueueUtxo,
        newHeader: header,
        additionalInputs: [feeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: schedulerUtxo,
        correctionLockRefInput: {
          utxo: correctionLockUtxo,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        confirmedStateRefInput,
        additionalRefInputs: [
          hubOracleUtxo,
          ...contracts.operatorLifecycleReferenceScripts.initial
            .filter((r) => r.name === "state-queue minting")
            .map((r) => r.utxo),
          ...contracts.operatorLifecycleReferenceScripts.active
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
  const unsigned = await commitTx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const stateQueueBlockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const [blockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    stateQueueBlockUnit,
  );
  if (blockUtxo === undefined) {
    throw new Error(
      "value-not-preserved fraudulent block missing after the second commit",
    );
  }
  return {
    headerHash,
    blockOutRef: `${blockUtxo.txHash}#${blockUtxo.outputIndex.toString()}`,
    stateQueueBlockUnit,
  };
};
