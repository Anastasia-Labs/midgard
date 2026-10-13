import {
  type FraudProofCatalogueDeploymentInfo,
  hashBlockHeader,
  Header,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { network } from "./blueprints.js";
import { requireUtxoWithUnit } from "./emulator-context.js";
import { withCommitSlot } from "./header-fixtures.js";
import {
  type MinAdaYieldReferenceScripts,
  publishMinAdaYieldReferenceScripts,
} from "./reference-scripts.js";
import { submitOperatorActivationTx } from "./setup-tx.onboard-emulator-operator.js";
import {
  type SetupContracts,
  type SetupLucid,
  setupUnits,
} from "./setup-tx.setup-units.js";
import {
  submitHeaderCommitTx,
  submitSchedulerAppointmentTx,
} from "./setup-tx.submit-header-commit-tx.js";
import { submitInitialMintTx } from "./setup-tx.submit-initial-mint-tx.js";

/**
 * The four-transaction setup journey every emulator suite starts from:
 * initial mint, operator activation, scheduler appointment, then the
 * fraudulent header commit.
 */
export const submitSetupTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  catalogue,
  header: requestedHeader,
  beforeHeaderCommit,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: SetupContracts;
  readonly nonceUtxo: UTxO;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  /**
   * The header to commit. It commits at block_time = endTime, so its
   * block_slot is stamped here; use the returned `header`.
   */
  readonly header: Header;
  /** Run actual event admission after hub creation, before committing this header. */
  readonly beforeHeaderCommit?: (hubOracle: UTxO) => Promise<void>;
}): Promise<{
  readonly header: Header;
  readonly fraudulentBlockOutRef: string;
  readonly headerHash: string;
  readonly stateQueueBlockUnit: string;
  readonly stateQueueRootUnit: string;
  readonly hubOracle: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorsRoot: UTxO;
  readonly activeOperatorsRootUnit: string;
  readonly retiredOperatorsRoot: UTxO;
  readonly retiredOperatorsRootUnit: string;
  readonly activeOperatorNode: UTxO;
  readonly activeOperatorNodeUnit: string;
  readonly registeredOperatorsRoot: UTxO;
  readonly minAdaYieldReferenceScripts?: MinAdaYieldReferenceScripts;
}> => {
  const header = withCommitSlot(contracts.l1SlotConfig, requestedHeader);
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const units = setupUnits(contracts, header, headerHash);

  await submitInitialMintTx({
    lucid,
    contracts,
    nonceUtxo,
    catalogue,
    header,
    units,
  });
  const minAdaYieldReferenceScripts =
    contracts.minAda === undefined
      ? undefined
      : (contracts.minAdaYieldReferenceScripts ??
        (await publishMinAdaYieldReferenceScripts({ lucid, contracts })));
  await submitOperatorActivationTx({ lucid, contracts, header, units });

  const hubOracleUtxo = await requireUtxoWithUnit(
    lucid,
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    units.hubOracle,
    "hub oracle after setup",
  );
  const stateQueueRootUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
    "state-queue root after setup",
  );
  const correctionLockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.correctionLock.spendingScriptAddress,
    units.correctionLock,
    "correction lock after setup",
  );
  const schedulerUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
    "scheduler after setup",
  );
  const activeOperatorNode = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
    "active-operator node after activation",
  );
  const activeOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorsRoot,
    "active-operators root after activation",
  );
  const retiredOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.retiredOperators.spendingScriptAddress,
    units.retiredOperatorsRoot,
    "retired-operators root after setup",
  );
  const registeredOperatorsRoot = await requireUtxoWithUnit(
    lucid,
    contracts.registeredOperators.spendingScriptAddress,
    units.registeredOperatorsRoot,
    "registered-operators root after setup",
  );

  const appointedSchedulerUtxo = await submitSchedulerAppointmentTx({
    lucid,
    contracts,
    header,
    units,
    schedulerUtxo,
    activeOperatorNode,
    registeredOperatorsRoot,
  });
  // Appointment must precede the header start. Admission may wait within the
  // header window, so perform that setup hook only after appointment.
  await beforeHeaderCommit?.(hubOracleUtxo);
  const { fraudulentBlockUtxo, continuedActiveOperatorNode } =
    await submitHeaderCommitTx({
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
    });

  return {
    header,
    fraudulentBlockOutRef: `${fraudulentBlockUtxo.txHash}#${fraudulentBlockUtxo.outputIndex.toString()}`,
    headerHash,
    stateQueueBlockUnit: units.stateQueueBlock,
    stateQueueRootUnit: units.stateQueueRoot,
    hubOracle: hubOracleUtxo,
    scheduler: appointedSchedulerUtxo,
    activeOperatorsRoot,
    activeOperatorsRootUnit: units.activeOperatorsRoot,
    retiredOperatorsRoot,
    retiredOperatorsRootUnit: units.retiredOperatorsRoot,
    activeOperatorNode: continuedActiveOperatorNode,
    activeOperatorNodeUnit: units.activeOperatorNode,
    registeredOperatorsRoot,
    ...(minAdaYieldReferenceScripts === undefined
      ? {}
      : { minAdaYieldReferenceScripts }),
  };
};

/**
 * Commit one additional authenticated header after `submitSetupTx` has
 * established the operator, scheduler, and state-queue root. Existing setup
 * callers retain the original single-header behavior; this helper only
 * advances when explicitly invoked by a multi-header lifecycle fixture.
 */
export const submitSecondHeaderTx = async ({
  lucid,
  contracts,
  header: requestedHeader,
}: {
  readonly lucid: SetupLucid;
  readonly contracts: SetupContracts;
  /** Stamped with its commit slot like `submitSetupTx`'s; use the returned one. */
  readonly header: Header;
}): Promise<{
  readonly header: Header;
  readonly blockOutRef: string;
  readonly headerHash: string;
}> => {
  const header = withCommitSlot(contracts.l1SlotConfig, requestedHeader);
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const units = setupUnits(contracts, header, headerHash);
  const hubOracleUtxo = await requireUtxoWithUnit(
    lucid,
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    units.hubOracle,
    "hub oracle before second header",
  );
  const correctionLockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.correctionLock.spendingScriptAddress,
    units.correctionLock,
    "correction lock before second header",
  );
  const stateQueueRootUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
    "state-queue root before second header",
  );
  const previousBlockUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.stateQueue.spendingScriptAddress,
    toUnit(
      contracts.stateQueue.policyId,
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header.prevHeaderHash,
    ),
    "previous block before second header",
  );
  const appointedSchedulerUtxo = await requireUtxoWithUnit(
    lucid,
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
    "scheduler before second header",
  );
  const activeOperatorNode = await requireUtxoWithUnit(
    lucid,
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
    "active operator before second header",
  );
  const committed = await submitHeaderCommitTx({
    lucid,
    contracts,
    header,
    headerHash,
    units,
    hubOracleUtxo,
    correctionLockUtxo,
    stateQueueRootUtxo: previousBlockUtxo,
    confirmedStateRefInput: stateQueueRootUtxo,
    appointedSchedulerUtxo,
    activeOperatorNode,
  });
  return {
    header,
    blockOutRef: `${committed.fraudulentBlockUtxo.txHash}#${committed.fraudulentBlockUtxo.outputIndex.toString()}`,
    headerHash,
  };
};
