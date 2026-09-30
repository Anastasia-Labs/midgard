import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  CORRECTION_LOCK_ASSET_NAME,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  Header,
  HUB_ORACLE_ASSET_NAME,
  type MidgardValidators,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  SCHEDULER_ASSET_NAME,
  scriptRewardAddress,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { Lucid, toUnit } from "@lucid-evolution/lucid";

import { network } from "./blueprints.js";
import { type MinAdaYieldReferenceScripts } from "./reference-scripts.js";
import { type OperatorLifecycleReferenceScripts } from "./reference-scripts.js";

export type SetupLucid = Awaited<ReturnType<typeof Lucid>>;

export type SetupContracts = MidgardValidators & {
  readonly operatorLifecycleReferenceScripts?: OperatorLifecycleReferenceScripts;
  readonly minAdaYieldReferenceScripts?: MinAdaYieldReferenceScripts;
  readonly validationTraceDispute?: {
    readonly yields: MidgardValidators["fraudProofContracts"]["validationTraceDispute"]["yields"];
  };
  readonly minAda?: {
    readonly yields: MidgardValidators["fraudProofContracts"]["minAda"]["yields"];
  };
};

/** Every asset unit the four setup transactions mint or track. */
export const setupUnits = (
  contracts: MidgardValidators,
  header: Header,
  headerHash: string,
) => ({
  hubOracle: toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME),
  correctionLock: toUnit(
    contracts.hubOracle.policyId,
    CORRECTION_LOCK_ASSET_NAME,
  ),
  fraudProofCatalogue: toUnit(
    contracts.fraudProofCatalogue.policyId,
    FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  ),
  stateQueueBlock: toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  ),
  stateQueueRoot: toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_ROOT_ASSET_NAME,
  ),
  scheduler: toUnit(contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
  activeOperatorsRoot: toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ),
  retiredOperatorsRoot: toUnit(
    contracts.retiredOperators.policyId,
    RETIRED_OPERATORS_ROOT_ASSET_NAME,
  ),
  activeOperatorNode: toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + header.operatorVkey,
  ),
  registeredOperatorsRoot: toUnit(
    contracts.registeredOperators.policyId,
    REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  ),
});

export type SetupUnits = ReturnType<typeof setupUnits>;

/**
 * Lovelace the correction lock is created with.
 *
 * `correction-lock.ak` requires every correction spend to preserve the lock's
 * value exactly (`lock_output.value == own_input.output.value`), so the UTxO
 * can never be topped up after it is minted: it has to be funded once, at
 * creation, for the largest datum it will ever carry. Lucid's automatic
 * min-Ada top-up sizes it for the 3-byte `Idle` datum instead (1,146,460
 * lovelace), which is 70 bytes -- 301,700 lovelace at 4,310 lovelace per byte
 * -- short of the 73-byte `Locked { target_header_hash: bytes(28),
 * correction_identity: FraudProof { bytes(32) } }` datum that the non-terminal
 * removal leg has to write. Under-funding makes every multi-transaction fraud
 * removal unbuildable: the lock output is silently bumped to its own min-Ada,
 * and the exact fraud-slash fee -- the operator bond minus the prover reward,
 * to the lovelace -- has no slack to absorb the difference, so the first
 * transaction fails balancing by exactly 301,700 lovelace. The round figure
 * here clears the 1,448,160-lovelace worst case with margin for datum drift.
 */
export const CORRECTION_LOCK_LOVELACE = 2_000_000n;

export const registerStateQueueYieldRewardAccounts = async (
  lucid: SetupLucid,
  contracts: MidgardValidators,
): Promise<void> => {
  const missing: string[] = [];
  for (const { withdrawalScript } of Object.values(
    contracts.stateQueue.yields,
  )) {
    const rewardAddress = scriptRewardAddress(network, withdrawalScript);
    if (!(await lucid.rewardAccountAt(rewardAddress)).registered) {
      missing.push(rewardAddress);
    }
  }
  if (missing.length === 0) return;
  let registration = lucid.newTx();
  for (const rewardAddress of missing) {
    registration = registration.register.Stake(rewardAddress);
  }
  const signed = await (
    await registration.complete({ localUPLCEval: true })
  ).sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await signed.submit());
};
