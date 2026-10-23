/**
 * The network-id workflow's direct reads of its own threads and record. Init,
 * Open and every conviction let any number of threads and records of one
 * header carry the same unit; another prover's, or one whose datum does not
 * decode under its step's schema, is no part of our workflow. Among ours (a
 * same-key twin's beside our own) the lowest out-ref decides, the same on
 * every runner: a Lucid read carries no creation slot, so the raw L1
 * derivation's newest-first rule cannot apply here. A read the chain makes
 * ambiguous fails as a derivation failure, which the orchestrator records
 * against this workflow alone.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  NetworkIdForcedScanDatum,
  NetworkIdForcedStepDatum,
  NetworkIdStep02Datum,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import { type Data, toUnit, type UTxO } from "@lucid-evolution/lucid";

import {
  datumProver,
  selectOwnFraudProofRecord,
} from "../fraud-proof-record-selection.js";
import { FraudProofRawL1DerivationError } from "../workflow/raw-l1-family-derivation.js";
import type { NetworkIdWorkflowAdapterConfig } from "./workflow-adapter.create-network-id-raw-l1-observation-port.js";
import { createNetworkIdForcedStepContract } from "./workflow-adapter.forced-step-contract.js";
import { CATEGORY } from "./workflow-adapter.prepared-from-artifact.js";

export const createNetworkIdOwnThreadReads = (
  config: NetworkIdWorkflowAdapterConfig,
) => {
  const { requireForcedStep } = createNetworkIdForcedStepContract(config);
  const requireForcedScan = () => {
    const forcedScan = config.contracts.forcedScan;
    if (forcedScan === undefined) {
      throw new Error(
        "network-id forced direction requires the deployed forced outputs scan",
      );
    }
    return forcedScan;
  };
  const unitOf = (policyId: string, headerHash: string) =>
    toUnit(policyId, `${config.category.categoryId}${headerHash}`);

  /** Our holder of `unit` at `address`, by its step's datum `schema`. */
  const ownThread = async (
    address: string,
    unit: string,
    schema: Parameters<typeof Data.from>[1],
  ): Promise<UTxO | undefined> =>
    (await config.lucid.utxosAtWithUnit(address, unit))
      .filter(
        (utxo) =>
          utxo.assets[unit] === 1n &&
          datumProver(utxo.datum, schema) === config.signer.paymentKeyHash,
      )
      .map((utxo) => ({ utxo, outRef: outRefLabel(utxo) }))
      .sort((left, right) =>
        left.outRef < right.outRef ? -1 : left.outRef > right.outRef ? 1 : 0,
      )[0]?.utxo;

  /**
   * Our scan thread, read directly for the same reason the forced door is:
   * the §10 loop self-loops at one address, so it is not a link the linear
   * raw-L1 family definition can walk.
   */
  const forcedScanThread = async (
    headerHash: string,
  ): Promise<UTxO | undefined> =>
    await ownThread(
      requireForcedScan().spendingScriptAddress,
      unitOf(config.contracts.computationThread.policyId, headerHash),
      NetworkIdForcedScanDatum,
    );

  /**
   * The forced door is deliberately not a link in the linear chain the raw-L1
   * family definition walks, so its occupancy is read directly. Nothing here
   * is trusted: `submitNetworkIdForcedBind` re-authenticates the thread token,
   * the handoff datum, the prover and the header before it spends this UTxO.
   */
  const forcedThreadOutRef = async (
    headerHash: string,
  ): Promise<string | undefined> => {
    const utxo = await ownThread(
      requireForcedStep().spendingScriptAddress,
      unitOf(config.contracts.computationThread.policyId, headerHash),
      NetworkIdForcedStepDatum,
    );
    return utxo === undefined ? undefined : outRefLabel(utxo);
  };

  const live = async (headerHash: string) => {
    const threadUnit = unitOf(
      config.contracts.computationThread.policyId,
      headerHash,
    );
    const proofUnit = unitOf(config.contracts.fraudProof.policyId, headerHash);
    const stateQueueUnit = toUnit(
      config.contracts.stateQueuePolicyId,
      `${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
    );
    const [step01, step02, proofs, stateQueue] = await Promise.all([
      ownThread(
        config.contracts.steps[0].spendingScriptAddress,
        threadUnit,
        FraudProofComputationThreadStepDatum,
      ),
      ownThread(
        config.contracts.steps[1].spendingScriptAddress,
        threadUnit,
        NetworkIdStep02Datum,
      ),
      config.lucid.utxosAtWithUnit(
        config.contracts.fraudProof.spendingScriptAddress,
        proofUnit,
      ),
      config.lucid.utxosAtWithUnit(config.stateQueueAddress, stateQueueUnit),
    ]);
    if (stateQueue.length > 1)
      throw new FraudProofRawL1DerivationError(
        CATEGORY,
        new Error("network-id workflow found duplicate state-queue UTxOs"),
      );
    return {
      threadUnit,
      proofUnit,
      step01,
      step02,
      proof: selectOwnFraudProofRecord({
        records: proofs.filter((utxo) => utxo.assets[proofUnit] === 1n),
        outRefOf: outRefLabel,
        datumOf: (utxo) => utxo.datum,
        prover: config.signer.paymentKeyHash,
      }),
      stateQueue: stateQueue[0],
    };
  };

  return { requireForcedScan, forcedScanThread, forcedThreadOutRef, live };
};
