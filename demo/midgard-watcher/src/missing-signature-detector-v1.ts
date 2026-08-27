/**
 * Classification layer for the existing replay divergence that witnesses a
 * missing required signature.  This module does not rescan blocks: callers
 * hand it the canonical replay result and authenticated compact-transaction
 * material they already reconstructed.
 */
import {
  type MissingSignatureFindingV1,
  MissingSignatureProvabilityV1,
} from "@al-ft/midgard-fault-proofs";
import type { MidgardAddressWitness } from "@al-ft/midgard-sdk";
import {
  type EventKey,
  MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE_V1,
  missingSignatureVkeyHashV1,
} from "@al-ft/midgard-sdk";
import { type RejectCode, RejectCodes } from "@al-ft/midgard-validation/types";

export const WATCHER_MISSING_SIGNATURE_DETECTOR_V1_SCHEMA_VERSION =
  "midgard-watcher-missing-signature-detector-v1" as const;

export type WatcherMissingSignatureDetectionConfigV1 = {
  /** Safety coverage is default-on; this isolated switch is operational only. */
  readonly enabled: boolean;
};

export const WATCHER_MISSING_SIGNATURE_DETECTION_DEFAULTS_V1: WatcherMissingSignatureDetectionConfigV1 =
  Object.freeze({ enabled: true });

export type WatcherMissingSignatureVkeySourcesV1 = {
  /** Vkeys indexed from any committed L2 transaction, in deterministic order. */
  readonly committedL2Vkeys: readonly string[];
  /** Vkeys observed in L1 transaction witness sets, in deterministic order. */
  readonly observedL1Vkeys: readonly string[];
  /** Manual/operator-supplied preimage, tried last. */
  readonly operatorSuppliedVkey?: string;
};

export type WatcherMissingSignatureCandidateV1 = {
  readonly headerHash: string;
  readonly eventKey: EventKey;
  readonly fraudulentBlockOutRef: string;
  readonly txId: string;
  readonly nativeTxCompactCbor: string;
  readonly committedWitnessSetHash: string;
  /** True for a Normal accepted leaf or `ForcedTxValid`. */
  readonly committedAccepted: boolean;
  /** Canonical replay rejection, or null when replay agrees with acceptance. */
  readonly replayRejectCode: RejectCode | null;
  readonly requiredSignerHashes: readonly string[];
  readonly addrTxWits: readonly MidgardAddressWitness[];
  readonly vkeySources: WatcherMissingSignatureVkeySourcesV1;
};

export type WatcherMissingSignatureDetectionV1 = {
  readonly schemaVersion: typeof WATCHER_MISSING_SIGNATURE_DETECTOR_V1_SCHEMA_VERSION;
  readonly finding: MissingSignatureFindingV1;
};

const normalizeVkey = (vkey: string): string | null => {
  const normalized = vkey.toLowerCase();
  return /^[0-9a-f]{64}$/u.test(normalized) ? normalized : null;
};

/** Recovery order is part of the detector contract (§3.3). */
export const recoverMissingSignatureVkeyV1 = ({
  requiredSignerHash,
  sources,
}: {
  readonly requiredSignerHash: string;
  readonly sources: WatcherMissingSignatureVkeySourcesV1;
}): string | null => {
  const candidates = [
    ...sources.committedL2Vkeys,
    ...sources.observedL1Vkeys,
    ...(sources.operatorSuppliedVkey === undefined
      ? []
      : [sources.operatorSuppliedVkey]),
  ];
  for (const candidate of candidates) {
    const normalized = normalizeVkey(candidate);
    if (
      normalized !== null &&
      missingSignatureVkeyHashV1(normalized) === requiredSignerHash
    ) {
      return normalized;
    }
  }
  return null;
};

/**
 * Classify one replay divergence and emit the finding record consumed by the
 * proving core. Every non-provable class is still returned for journaling.
 */
export const detectMissingSignatureFindingV1 = ({
  candidate,
  config = WATCHER_MISSING_SIGNATURE_DETECTION_DEFAULTS_V1,
}: {
  readonly candidate: WatcherMissingSignatureCandidateV1;
  readonly config?: WatcherMissingSignatureDetectionConfigV1;
}): WatcherMissingSignatureDetectionV1 | null => {
  if (!config.enabled) return null;

  const witnessHashes = new Set(
    candidate.addrTxWits.map((witness) =>
      missingSignatureVkeyHashV1(witness.verification_key),
    ),
  );
  const accusedIndex = candidate.requiredSignerHashes.findIndex(
    (hash) => !witnessHashes.has(hash.toLowerCase()),
  );
  const accusedHash =
    accusedIndex < 0
      ? (candidate.requiredSignerHashes[0]?.toLowerCase() ?? "00".repeat(28))
      : candidate.requiredSignerHashes[accusedIndex]!.toLowerCase();

  let provability: MissingSignatureFindingV1["provability"];
  let resolvedVkey: string | null = null;
  if (!candidate.committedAccepted) {
    provability = MissingSignatureProvabilityV1.NotAFault;
  } else if (
    candidate.replayRejectCode === RejectCodes.MissingRequiredWitness &&
    accusedIndex >= 0
  ) {
    resolvedVkey = recoverMissingSignatureVkeyV1({
      requiredSignerHash: accusedHash,
      sources: candidate.vkeySources,
    });
    provability =
      resolvedVkey === null
        ? MissingSignatureProvabilityV1.UnknownVkeyPreimage
        : MissingSignatureProvabilityV1.MissingWitness;
  } else if (
    candidate.replayRejectCode === RejectCodes.InvalidSignature &&
    accusedIndex < 0
  ) {
    provability = MissingSignatureProvabilityV1.PresentButInvalid;
  } else {
    provability = MissingSignatureProvabilityV1.NotAFault;
  }

  return {
    schemaVersion: WATCHER_MISSING_SIGNATURE_DETECTOR_V1_SCHEMA_VERSION,
    finding: {
      headerHash: candidate.headerHash,
      eventKey: candidate.eventKey,
      fraudulentBlockOutRef: candidate.fraudulentBlockOutRef,
      txId: candidate.txId,
      nativeTxCompactCbor: candidate.nativeTxCompactCbor,
      accusedRequiredSignerIndex: BigInt(Math.max(0, accusedIndex)),
      accusedRequiredSignerHash: accusedHash,
      resolvedVkey,
      committedWitnessSetHash: candidate.committedWitnessSetHash,
      provability,
      estimatedThreadTxCount:
        5 +
        Math.floor(
          Math.max(0, candidate.addrTxWits.length - 1) /
            MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE_V1,
        ),
    },
  };
};

/** Default-on batch adapter with a journal callback for every classification. */
export const detectAndJournalMissingSignatureFindingsV1 = async ({
  candidates,
  config = WATCHER_MISSING_SIGNATURE_DETECTION_DEFAULTS_V1,
  journal,
}: {
  readonly candidates: readonly WatcherMissingSignatureCandidateV1[];
  readonly config?: WatcherMissingSignatureDetectionConfigV1;
  readonly journal: (
    detection: WatcherMissingSignatureDetectionV1,
  ) => void | Promise<void>;
}): Promise<readonly WatcherMissingSignatureDetectionV1[]> => {
  const detections: WatcherMissingSignatureDetectionV1[] = [];
  for (const candidate of candidates) {
    const detection = detectMissingSignatureFindingV1({ candidate, config });
    if (detection === null) continue;
    detections.push(detection);
    await journal(detection);
  }
  return detections;
};
