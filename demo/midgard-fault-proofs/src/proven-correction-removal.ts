/**
 * Driving a fraud-proof correction from L1 alone. A record is never spent
 * (validators/fraud-proof.ak), and the state-queue validator starts a
 * removal from an Idle CorrectionLock, or continues one from a lock Locked
 * under the same correction, against any proven record of the target header
 * (validators/correction-lock.ak, RemoveFraudulentBlockHeader;
 * lib/midgard/correction-lock.ak, datum_transition_is_valid). So anyone who
 * reads the lock and the record can start or finish the removal with the
 * ordinary builder, holding no admitted decision and no journal. The
 * record's prover is paid, whoever submits.
 */
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type CorrectionLockDatum,
  fetchCorrectionLockUTxOProgram,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk/core";
import {
  type LucidEvolution,
  type Network,
  toUnit,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  fraudProofRecordProver,
  selectOwnFraudProofRecord,
} from "./fraud-proof-record-selection.js";
import { buildRemovalContracts } from "./remove-fraudulent-block.assemble-removal-contracts.js";
import { loadStateQueueTopology } from "./remove-fraudulent-block.load-state-queue-topology.js";
import type {
  StateQueueMutationLeaseCoordinator,
  SubmitRemoveFraudulentBlockResult,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import { submitRemoveFraudulentBlock } from "./remove-fraudulent-block.submit-remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "./runtime.js";

/** A fraud-proof correction: the queued header a proven record convicts,
 * and that record's asset name. */
export type ProvenFraudProofCorrection = Readonly<{
  targetHeaderHash: string;
  fraudProofAssetName: string;
}>;

/**
 * `unproven`: no decodable record of the correction is on L1, so nothing was
 * submitted. `finished`: nothing is left for it to do now: the live lock is
 * Locked under another correction, or Idle with its header no longer queued.
 * `proven`: the removal citing `fraudProofOutRef` is under way; `removal`
 * settles when it is confirmed or fails.
 */
export type ProvenCorrectionRemoval =
  | Readonly<{ kind: "unproven" }>
  | Readonly<{ kind: "finished" }>
  | Readonly<{
      kind: "proven";
      fraudProofOutRef: string;
      removal: Promise<SubmitRemoveFraudulentBlockResult>;
    }>;

const correctionCategory = (
  correction: ProvenFraudProofCorrection,
): FraudProofCatalogueCategoryName | undefined =>
  (
    Object.keys(
      FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
    ) as FraudProofCatalogueCategoryName[]
  ).find(
    (category) =>
      `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category]}${correction.targetHeaderHash}` ===
      correction.fraudProofAssetName,
  );

const namesCorrection = (
  datum: Exclude<CorrectionLockDatum, "Idle">,
  correction: ProvenFraudProofCorrection,
): boolean => {
  const identity = datum.Locked.correction_identity;
  return (
    datum.Locked.target_header_hash === correction.targetHeaderHash &&
    typeof identity === "object" &&
    "FraudProof" in identity &&
    identity.FraudProof.fraud_proof_asset_name ===
      correction.fraudProofAssetName
  );
};

/**
 * Starts the removal of a proven correction's header, or finishes the one a
 * lock Locked under it is waiting for, citing a proven record of it: the
 * signer's own when it holds one, else the lowest out-ref. Nothing is
 * submitted when no such record is on L1, the live lock is Locked under
 * another correction, or the lock is Idle and the header is no longer queued.
 */
export const driveProvenCorrectionRemoval = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  stateQueueMutationLeaseCoordinator,
  correction,
  validFrom,
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  readonly correction: ProvenFraudProofCorrection;
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
}): Promise<ProvenCorrectionRemoval> => {
  const category = correctionCategory(correction);
  if (category === undefined) return Object.freeze({ kind: "unproven" });
  const contracts = await buildRemovalContracts({
    blueprint,
    deploymentInfo,
    network,
    fraudCategory: category,
  });
  const unit = toUnit(
    contracts.fraudProofPolicyId,
    correction.fraudProofAssetName,
  );
  const records = (
    await lucid.utxosAtWithUnit(contracts.fraudProofAddress, unit)
  ).filter(
    (utxo) =>
      utxo.assets[unit] === 1n &&
      fraudProofRecordProver(utxo.datum) !== undefined,
  );
  const lowest = records
    .map(outRefLabel)
    .sort((left, right) => (left < right ? -1 : left > right ? 1 : 0))[0];
  const own = selectOwnFraudProofRecord({
    records,
    outRefOf: outRefLabel,
    datumOf: (utxo) => utxo.datum,
    prover: signer.paymentKeyHash,
  });
  const fraudProofOutRef = own === undefined ? lowest : outRefLabel(own);
  if (fraudProofOutRef === undefined)
    return Object.freeze({ kind: "unproven" });
  const live = await Effect.runPromise(
    fetchCorrectionLockUTxOProgram(lucid, {
      correctionLockAddress: contracts.correctionLockAddress,
      hubOraclePolicyId: contracts.hubOraclePolicyId,
    }),
  );
  if (live.datum === "Idle") {
    const topology = await loadStateQueueTopology({
      lucid,
      stateQueueAddress: contracts.stateQueueAddress,
      stateQueuePolicyId: contracts.stateQueuePolicyId,
    });
    if (!topology.nodeByHeaderHash.has(correction.targetHeaderHash))
      return Object.freeze({ kind: "finished" });
  } else if (!namesCorrection(live.datum, correction))
    return Object.freeze({ kind: "finished" });
  return Object.freeze({
    kind: "proven",
    fraudProofOutRef,
    removal: submitRemoveFraudulentBlock({
      lucid,
      blueprint,
      deploymentInfo,
      network,
      signer,
      fraudCategory: category,
      fraudulentHeaderHash: correction.targetHeaderHash,
      fraudProofOutRef,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      ...(stateQueueMutationLeaseCoordinator === undefined
        ? {}
        : { stateQueueMutationLeaseCoordinator }),
      ...(validFrom === undefined ? {} : { validFrom }),
      ...(validTo === undefined ? {} : { validTo }),
    }),
  });
};
