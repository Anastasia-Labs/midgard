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
} from "@al-ft/midgard-sdk";
import {
  CML,
  type LucidEvolution,
  type Network,
  toUnit,
  utxoToCore,
  type WalletApi,
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
import { balanceCbor } from "./workflow/funding-reservation-permit.apply-transition.js";

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
 * A signer whose wallet offers every coin except those `reservedOutRefs`
 * names, read afresh on each wallet read: an action outside any funding
 * reservation never spends, or sits as collateral on, a coin a reservation
 * holds. Signing and submission go to the original wallet.
 */
export const excludeReservedFunding = ({
  signer,
  reservedOutRefs,
}: {
  readonly signer: ResolvedProverSigner;
  readonly reservedOutRefs: () => Promise<readonly string[]>;
}): ResolvedProverSigner =>
  Object.freeze({
    ...signer,
    selectWallet: (lucid: LucidEvolution): void => {
      signer.selectWallet(lucid);
      const original = lucid.wallet();
      const unreserved = async () => {
        const reserved = new Set(await reservedOutRefs());
        return (await original.getUtxos()).filter(
          (utxo) => !reserved.has(outRefLabel(utxo)),
        );
      };
      const addressHex = CML.Address.from_bech32(signer.address).to_hex();
      const api: WalletApi = Object.freeze({
        getNetworkId: async () =>
          CML.Address.from_bech32(signer.address).to_raw_bytes()[0]! & 0x0f,
        getUtxos: async () =>
          (await unreserved()).map((utxo) => utxoToCore(utxo).to_cbor_hex()),
        getBalance: async () => balanceCbor(await unreserved()),
        getUsedAddresses: async () => [addressHex],
        getUnusedAddresses: async () => [],
        getChangeAddress: async () => addressHex,
        getRewardAddresses: async () => [],
        signTx: async (tx) =>
          (
            await original.signTx(CML.Transaction.from_cbor_hex(tx))
          ).to_cbor_hex(),
        signData: async (address, payload) =>
          await original.signMessage(
            CML.Address.from_hex(address).to_bech32(),
            payload,
          ),
        submitTx: async (tx) => await original.submitTx(tx),
        getCollateral: async () => [],
        experimental: Object.freeze({
          getCollateral: async () => [],
          on: () => undefined,
          off: () => undefined,
        }),
      });
      lucid.selectWallet.fromAPI(api);
    },
  });

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
