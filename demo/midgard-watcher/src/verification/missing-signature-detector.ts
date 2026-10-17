/**
 * Classification layer for the existing replay divergence that witnesses a
 * missing required signature.  This module does not rescan blocks: callers
 * hand it the canonical replay result and authenticated compact-transaction
 * material they already reconstructed.
 */
import {
  type MissingSignatureFinding,
  MissingSignatureProvability,
} from "@al-ft/midgard-fault-proofs";
import type { MidgardAddressWitness } from "@al-ft/midgard-sdk";
import {
  type EventKey,
  MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
  missingSignatureVkeyHash,
} from "@al-ft/midgard-sdk";
import { type RejectCode, RejectCodes } from "@al-ft/midgard-validation/types";

export const WATCHER_MISSING_SIGNATURE_DETECTOR_SCHEMA_VERSION =
  "midgard-watcher-missing-signature-detector-v1" as const;

export type WatcherMissingSignatureDetectionConfig = {
  /** Safety coverage is default-on; this isolated switch is operational only. */
  readonly enabled: boolean;
};

export const WATCHER_MISSING_SIGNATURE_DETECTION_DEFAULTS: WatcherMissingSignatureDetectionConfig =
  Object.freeze({ enabled: true });

export type WatcherMissingSignatureVkeySources = {
  /** Vkeys indexed from any committed L2 transaction, in deterministic order. */
  readonly committedL2Vkeys: readonly string[];
  /** Vkeys observed in L1 transaction witness sets, in deterministic order. */
  readonly observedL1Vkeys: readonly string[];
  /** Manual/operator-supplied preimage, tried last. */
  readonly operatorSuppliedVkey?: string;
};

export type WatcherMissingSignatureCandidate = {
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
  readonly vkeySources: WatcherMissingSignatureVkeySources;
};

export type WatcherMissingSignatureDetection = {
  readonly schemaVersion: typeof WATCHER_MISSING_SIGNATURE_DETECTOR_SCHEMA_VERSION;
  readonly finding: MissingSignatureFinding;
};

const normalizeVkey = (vkey: string): string | null => {
  const normalized = vkey.toLowerCase();
  return /^[0-9a-f]{64}$/u.test(normalized) ? normalized : null;
};

/** Recovery order is part of the detector contract (§3.3). */
export const recoverMissingSignatureVkey = ({
  requiredSignerHash,
  sources,
}: {
  readonly requiredSignerHash: string;
  readonly sources: WatcherMissingSignatureVkeySources;
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
      missingSignatureVkeyHash(normalized) === requiredSignerHash
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
export const detectMissingSignatureFinding = ({
  candidate,
  config = WATCHER_MISSING_SIGNATURE_DETECTION_DEFAULTS,
}: {
  readonly candidate: WatcherMissingSignatureCandidate;
  readonly config?: WatcherMissingSignatureDetectionConfig;
}): WatcherMissingSignatureDetection | null => {
  if (!config.enabled) return null;

  const witnessHashes = new Set(
    candidate.addrTxWits.map((witness) =>
      missingSignatureVkeyHash(witness.verification_key),
    ),
  );
  const accusedIndex = candidate.requiredSignerHashes.findIndex(
    (hash) => !witnessHashes.has(hash.toLowerCase()),
  );
  const accusedHash =
    accusedIndex < 0
      ? (candidate.requiredSignerHashes[0]?.toLowerCase() ?? "00".repeat(28))
      : candidate.requiredSignerHashes[accusedIndex]!.toLowerCase();

  let provability: MissingSignatureFinding["provability"];
  let resolvedVkey: string | null = null;
  if (!candidate.committedAccepted) {
    provability = MissingSignatureProvability.NotAFault;
  } else if (
    candidate.replayRejectCode === RejectCodes.MissingRequiredWitness &&
    accusedIndex >= 0
  ) {
    resolvedVkey = recoverMissingSignatureVkey({
      requiredSignerHash: accusedHash,
      sources: candidate.vkeySources,
    });
    provability =
      resolvedVkey === null
        ? MissingSignatureProvability.UnknownVkeyPreimage
        : MissingSignatureProvability.MissingWitness;
  } else if (
    candidate.replayRejectCode === RejectCodes.InvalidSignature &&
    accusedIndex < 0
  ) {
    provability = MissingSignatureProvability.PresentButInvalid;
  } else {
    provability = MissingSignatureProvability.NotAFault;
  }

  return {
    schemaVersion: WATCHER_MISSING_SIGNATURE_DETECTOR_SCHEMA_VERSION,
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
            MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
        ),
    },
  };
};

/** Default-on batch adapter with a journal callback for every classification. */
export const detectAndJournalMissingSignatureFindings = async ({
  candidates,
  config = WATCHER_MISSING_SIGNATURE_DETECTION_DEFAULTS,
  journal,
}: {
  readonly candidates: readonly WatcherMissingSignatureCandidate[];
  readonly config?: WatcherMissingSignatureDetectionConfig;
  readonly journal: (
    detection: WatcherMissingSignatureDetection,
  ) => void | Promise<void>;
}): Promise<readonly WatcherMissingSignatureDetection[]> => {
  const detections: WatcherMissingSignatureDetection[] = [];
  for (const candidate of candidates) {
    const detection = detectMissingSignatureFinding({ candidate, config });
    if (detection === null) continue;
    detections.push(detection);
    await journal(detection);
  }
  return detections;
};
