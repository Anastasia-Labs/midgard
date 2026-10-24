/** A commit after the Open scene's blocks, for suites that watch what a
 * defending operator's transactions do to the operator's later commits: the
 * scene's operator commits a successor of the block it names, now, with
 * the operator's active node, the scheduler and the hub oracle read live. */
import * as SDK from "@al-ft/midgard-sdk";
import { toUnit, type UTxO } from "@lucid-evolution/lucid";

import { submitSuccessorBlockTx } from "./submit-init-emulator-fixtures.submit-successor-block-tx.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

/** A committed block: its header and hash. */
export type CommittedBlock = Readonly<{
  header: SDK.Header;
  headerHash: string;
}>;

const liveByUnit = async (
  scene: Scene,
  address: string,
  unit: string,
  label: string,
): Promise<UTxO> => {
  const [utxo] = await scene.h.funderLucid.utxosAtWithUnit(address, unit);
  if (utxo === undefined) throw new Error(`no live ${label}`);
  return utxo;
};

/** An operator that commits: its wallet, key and active-node unit. */
export type CommittingOperator = Readonly<{
  lucid: Scene["h"]["funderLucid"];
  vkey: string;
  nodeUnit: string;
}>;

/**
 * Commits a successor of `previous` (the scene's last block by default) by
 * `operator` (the scene's by default), which the scheduler must name, now.
 * Returns the committed block and the operator's active node the commit
 * continued.
 */
export const commitSceneSuccessor = async (
  scene: Scene,
  previous: CommittedBlock = scene.forced.evidence,
  operator: CommittingOperator = {
    lucid: scene.h.funderLucid,
    vkey: scene.operator.vkey,
    nodeUnit: scene.operator.nodeUnit,
  },
): Promise<CommittedBlock & Readonly<{ activeNode: UTxO }>> => {
  const { h } = scene;
  const { contracts, emulator } = h;
  const endTime = BigInt(emulator.now() + 60_000) - 1n;
  const committed = await submitSuccessorBlockTx({
    lucid: operator.lucid,
    emulator,
    contracts,
    anchorBlockUnit: toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + previous.headerHash,
    ),
    header: {
      ...previous.header,
      operatorVkey: operator.vkey,
      prevHeaderHash: previous.headerHash,
      prevUtxosRoot: previous.header.utxosRoot,
      startTime: previous.header.endTime,
      endTime,
    },
    hubOracle: await liveByUnit(
      scene,
      scene.hubOracle.address,
      toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
      "hub oracle",
    ),
    scheduler: await liveByUnit(
      scene,
      contracts.scheduler.spendingScriptAddress,
      toUnit(contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
      "scheduler",
    ),
    activeOperatorNode: await liveByUnit(
      scene,
      contracts.activeOperators.spendingScriptAddress,
      operator.nodeUnit,
      "active-operator node",
    ),
    activeOperatorNodeUnit: operator.nodeUnit,
    validFrom: BigInt(emulator.now()) - 1_000n,
  });
  return {
    header: committed.header,
    headerHash: committed.successorHeaderHash,
    activeNode: committed.activeOperatorNode,
  };
};
