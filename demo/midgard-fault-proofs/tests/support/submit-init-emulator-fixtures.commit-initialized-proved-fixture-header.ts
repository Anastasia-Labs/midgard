import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { requireUtxoWithUnit } from "./emulator/emulator-context.js";
import { withCommitSlot } from "./emulator/header-fixtures.js";
import { setupUnits } from "./emulator/setup-tx.setup-units.js";
import {
  submitHeaderCommitTx,
  submitSchedulerAppointmentTx,
} from "./emulator/setup-tx.submit-header-commit-tx.js";
import type { submitSetupTx } from "./emulator/setup-tx.submit-setup-tx.js";

export const commitInitializedProvedFixtureHeader = async ({
  lucid,
  contracts,
  header: requestedHeader,
  schedulerStartTime,
  bondPaddingLovelace,
}: {
  readonly schedulerStartTime: bigint;
  readonly bondPaddingLovelace?: bigint;
  readonly lucid: Parameters<typeof submitHeaderCommitTx>[0]["lucid"];
  readonly contracts: Parameters<typeof submitHeaderCommitTx>[0]["contracts"];
  readonly header: SDK.Header;
}): Promise<Awaited<ReturnType<typeof submitSetupTx>>> => {
  const header = withCommitSlot(contracts.l1SlotConfig, requestedHeader);
  await Effect.runPromise(
    SDK.validateHeaderTransitionCommitmentsProgram(header),
  );
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const units = setupUnits(contracts, header, headerHash);
  const snapshot = await Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts),
  );
  const get = (address: string, unit: string) =>
    requireUtxoWithUnit(lucid, address, unit, "canonical initialized setup");
  const hubOracle = snapshot.hubOracle.utxo;
  const scheduler = await get(
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
  );
  const correctionLock = await get(
    contracts.correctionLock.spendingScriptAddress,
    units.correctionLock,
  );
  const stateQueueRoot = await get(
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
  );
  const activeOperatorNode = await get(
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
  );
  const activeOperatorsRoot = await get(
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorsRoot,
  );
  const retiredOperatorsRoot = await get(
    contracts.retiredOperators.spendingScriptAddress,
    units.retiredOperatorsRoot,
  );
  const registeredOperatorsRoot = await get(
    contracts.registeredOperators.spendingScriptAddress,
    units.registeredOperatorsRoot,
  );
  const appointedScheduler = await submitSchedulerAppointmentTx({
    lucid,
    contracts,
    header: { ...header, startTime: schedulerStartTime },
    units,
    schedulerUtxo: scheduler,
    activeOperatorNode,
    registeredOperatorsRoot,
  });
  const committed = await submitHeaderCommitTx({
    lucid,
    contracts,
    header,
    headerHash,
    units,
    hubOracleUtxo: hubOracle,
    correctionLockUtxo: correctionLock,
    stateQueueRootUtxo: stateQueueRoot,
    appointedSchedulerUtxo: appointedScheduler,
    activeOperatorNode,
    bondPaddingLovelace,
  });
  return {
    header,
    fraudulentBlockOutRef: `${committed.fraudulentBlockUtxo.txHash}#${committed.fraudulentBlockUtxo.outputIndex}`,
    headerHash,
    stateQueueBlockUnit: units.stateQueueBlock,
    stateQueueRootUnit: units.stateQueueRoot,
    hubOracle,
    scheduler: appointedScheduler,
    activeOperatorsRoot,
    activeOperatorsRootUnit: units.activeOperatorsRoot,
    retiredOperatorsRoot,
    retiredOperatorsRootUnit: units.retiredOperatorsRoot,
    activeOperatorNode: committed.continuedActiveOperatorNode,
    activeOperatorNodeUnit: units.activeOperatorNode,
    registeredOperatorsRoot,
  };
};
