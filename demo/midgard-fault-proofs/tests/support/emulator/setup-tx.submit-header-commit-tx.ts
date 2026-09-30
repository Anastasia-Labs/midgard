import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import {
  ActiveOperatorDatum,
  ActiveOperatorSpendRedeemer,
  CORRECTION_LOCK_ASSET_NAME,
  encodeLinkedListNodeView,
  getHeaderFromStateQueueDatum,
  Header,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  type MidgardValidators,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  SchedulerDatum,
  SchedulerSpendRedeemer,
  utxoToStateQueueUTxO,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { ledgerOrderedIndex } from "./catalogue.js";
import {
  firstWalletUtxo,
  requireUtxoWithUnit,
  runEmulatorLifecycleStage,
} from "./emulator-context.js";
import { SCHEDULER_APPOINTMENT_OUTPUT_INDEX } from "./header-fixtures.js";
import { findStateQueueYieldReferenceScript } from "./reference-scripts.js";
import {
  registerStateQueueYieldRewardAccounts,
  type SetupContracts,
  type SetupLucid,
  type SetupUnits,
} from "./setup-tx.setup-units.js";

/**
 * Transaction 3: appoint the activated operator as the scheduler's first
 * shift, returning the appointed scheduler UTxO.
 */
export const submitSchedulerAppointmentTx = async ({
  lucid,
  contracts,
  header,
  units,
  schedulerUtxo,
  activeOperatorNode,
  registeredOperatorsRoot,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: MidgardValidators;
  readonly header: Header;
  readonly units: SetupUnits;
  readonly schedulerUtxo: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly registeredOperatorsRoot: UTxO;
}): Promise<UTxO> => {
  const schedulerAppointmentFeeInput = await firstWalletUtxo(
    lucid,
    "scheduler appointment fee input",
  );
  const appointmentInputs = [schedulerAppointmentFeeInput, schedulerUtxo];
  const appointmentRefs = [activeOperatorNode, registeredOperatorsRoot];
  const schedulerAppointmentRedeemer: SchedulerSpendRedeemer = {
    scheduler_input_index: ledgerOrderedIndex(
      appointmentInputs,
      schedulerUtxo,
      "scheduler appointment input",
    ),
    scheduler_output_index: SCHEDULER_APPOINTMENT_OUTPUT_INDEX.scheduler,
    advancing_approach: {
      AppointFirstOperator: {
        new_shifts_operator_node_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          activeOperatorNode,
          "active-operator node appointment reference input",
        ),
        registered_element_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          registeredOperatorsRoot,
          "registered-operators root appointment reference input",
        ),
      },
    },
  };
  const appointmentUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-appointment.complete",
    () =>
      lucid
        .newTx()
        .collectFrom([schedulerAppointmentFeeInput])
        .collectFrom(
          [schedulerUtxo],
          Data.to(schedulerAppointmentRedeemer, SchedulerSpendRedeemer),
        )
        .readFrom(appointmentRefs)
        .pay.ToContract(
          contracts.scheduler.spendingScriptAddress,
          {
            kind: "inline",
            value: Data.to(
              {
                ActiveOperator: {
                  operator: header.operatorVkey,
                  start_time: header.startTime,
                },
              },
              SchedulerDatum,
            ),
          },
          schedulerUtxo.assets,
        )
        .attach.Script(contracts.scheduler.spendingScript)
        .validFrom(
          Math.max(0, lucid.slotToUnixTime(lucid.currentSlot()) - 60_000),
        )
        .validTo(Number(header.startTime + 1n))
        .complete({ localUPLCEval: true }),
  );
  const appointmentSigned = await appointmentUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-appointment", async () =>
    lucid.awaitTx(await appointmentSigned.submit()),
  );

  const appointedSchedulerUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
    "scheduler after the appointment transaction",
  );
  expect(Data.from(appointedSchedulerUtxo.datum!, SchedulerDatum)).toEqual({
    ActiveOperator: {
      operator: header.operatorVkey,
      start_time: header.startTime,
    },
  });
  return appointedSchedulerUtxo;
};

/**
 * Lower bound of a header commit's validity range: 60 s before the header
 * start. The upper bound is `header.endTime + 1`.
 */
export const headerCommitValidFrom = (header: Header): bigint =>
  header.startTime - 60_000n;

/**
 * Transaction 4: commit the (fraudulent) header onto the state queue behind
 * the root, holding the operator's bond.
 */
export const submitHeaderCommitTx = async ({
  lucid,
  contracts,
  header,
  headerHash,
  units,
  hubOracleUtxo,
  correctionLockUtxo,
  stateQueueRootUtxo,
  appointedSchedulerUtxo,
  activeOperatorNode,
  confirmedStateRefInput,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: SetupContracts;
  readonly header: Header;
  readonly headerHash: string;
  readonly units: SetupUnits;
  readonly hubOracleUtxo: UTxO;
  readonly correctionLockUtxo: UTxO;
  readonly stateQueueRootUtxo: UTxO;
  readonly appointedSchedulerUtxo: UTxO;
  readonly activeOperatorNode: UTxO;
  readonly confirmedStateRefInput?: UTxO;
}): Promise<{
  readonly fraudulentBlockUtxo: UTxO;
  readonly continuedActiveOperatorNode: UTxO;
}> => {
  await registerStateQueueYieldRewardAccounts(lucid, contracts);
  const commitYieldReference = await findStateQueueYieldReferenceScript({
    lucid,
    contracts,
    arm: "commit",
  });
  const stateQueueRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(stateQueueRootUtxo, contracts.stateQueue.policyId),
  );
  const commitFeeInput = await firstWalletUtxo(lucid, "commit fee input");
  const commitValidFrom = headerCommitValidFrom(header);
  const commitValidTo = header.endTime + 1n;
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
            "commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[units.activeOperatorNode] ?? 0n) === 1n,
            "commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            "commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: stateQueueRoot,
        newHeader: header,
        additionalInputs: [commitFeeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: appointedSchedulerUtxo,
        correctionLockRefInput: {
          utxo: correctionLockUtxo,
          datum: "Idle",
          assetName: CORRECTION_LOCK_ASSET_NAME,
        },
        ...(confirmedStateRefInput === undefined
          ? {}
          : { confirmedStateRefInput }),
        additionalRefInputs: [
          hubOracleUtxo,
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
          referenceInput: commitYieldReference,
          script: contracts.stateQueue.yields.commit.withdrawalScript,
        },
      },
    ),
  );
  const commitUnsigned = await runEmulatorLifecycleStage(
    "setup.header-commit.complete",
    () => commitTx.complete({ localUPLCEval: true }),
  );
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.header-commit", async () =>
    lucid.awaitTx(await commitSigned.submit()),
  );

  const fraudulentBlockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueBlock,
    "committed block after the header commit",
  );
  const continuedRootUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
    "state-queue root after the header commit",
  );
  const continuedActiveOperatorNode = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
    "active-operator node after the header commit",
  );
  const committedBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(fraudulentBlockUtxo, contracts.stateQueue.policyId),
  );
  const committedHeader = await Effect.runPromise(
    getHeaderFromStateQueueDatum(committedBlock.datum),
  );
  expect(committedHeader.transactionsRoot).toBe(header.transactionsRoot);
  const continuedRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(continuedRootUtxo, contracts.stateQueue.policyId),
  );
  expect(continuedRoot.datum.next).toEqual({
    Key: {
      key:
        confirmedStateRefInput === undefined
          ? headerHash
          : header.prevHeaderHash,
    },
  });
  return { fraudulentBlockUtxo, continuedActiveOperatorNode };
};
