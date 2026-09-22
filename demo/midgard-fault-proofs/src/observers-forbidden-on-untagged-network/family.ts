import { EMPTY_NULL_ROOT } from "@al-ft/midgard-core";
import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  RejectionReasonSchema,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY =
  "observersForbiddenOnUntaggedNetwork" as const;
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_ID =
  "00000024" as const;
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FIELD_INDEX = 3 as const;
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_ID =
  "ObserversForbiddenOnUntaggedNetwork" as const;
export const MIDGARD_UNTAGGED_NETWORK_ID = 255 as const;
/**
 * The canonical absent `script_integrity_hash`. Phase-A script preconditions
 * reach the observer arm only past this value: the absent hash either needs no
 * Plutus evaluation or is rejected first as `ScriptIntegrityHashMissing`.
 */
export const MIDGARD_ABSENT_SCRIPT_INTEGRITY_HASH =
  EMPTY_NULL_ROOT.toString("hex");

export type ObserversForbiddenFinding = Readonly<{
  subject: VerdictSubject;
  networkId: 0 | 1 | 255;
  /** Body `script_integrity_hash`, 32 bytes of lowercase hex. */
  scriptIntegrityHash: string;
}>;

export const classifyObserversForbiddenFinding = (
  finding: ObserversForbiddenFinding,
): ObserversForbiddenFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    throw new Error("observersForbidden: verdict subject is not canonical");
  const direction = finding.subject.direction;
  if (![0, 1, 255].includes(finding.networkId))
    throw new Error("observersForbidden: network scalar changed");
  if (!/^[0-9a-f]{64}$/u.test(finding.scriptIntegrityHash))
    throw new Error("observersForbidden: script integrity hash changed");
  if (direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    const reason = finding.subject.rejection_reason;
    if (reason !== OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_ID)
      throw new Error("observersForbidden: typed rejection reason changed");
  } else if (
    direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  ) {
    throw new Error("observersForbidden: direction/reason polarity changed");
  }
  return Object.freeze(finding);
};

export type ObserversForbiddenEvidence = ObserversForbiddenFinding &
  Readonly<{
    observerCount: number;
    observerFieldPreimageCbor: string;
    observerFieldCommitment: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

export const prepareObserversForbiddenEvidence = ({
  finding: rawFinding,
  observerFieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: ObserversForbiddenFinding;
  readonly observerFieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): ObserversForbiddenEvidence => {
  const finding = classifyObserversForbiddenFinding(rawFinding);
  const actual = midgardFieldCommitment(observerFieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    throw new Error("observersForbidden: retained field 3 changed commitment");
  const observers = decodeMidgardFieldPreimage(observerFieldPreimage);
  if (observers.some((observer) => observer.length !== 28))
    throw new Error("observersForbidden: observer is not a 28-byte hash");
  return Object.freeze({
    ...finding,
    observerCount: observers.length,
    observerFieldPreimageCbor: Buffer.from(observerFieldPreimage).toString(
      "hex",
    ),
    observerFieldCommitment: actual,
    carriage: selectMidgardFieldCarriageTier(observerFieldPreimage.length),
  });
};

/**
 * Twin of `forbidden_observers_hold_v1`: the phase-A observer arm holds only
 * when Plutus evaluation is required and the integrity hash is present, which
 * collapses to a present hash; then observers present on scalar 255.
 */
export const observersForbiddenFaultHolds = (
  evidence: Pick<
    ObserversForbiddenEvidence,
    "observerCount" | "networkId" | "scriptIntegrityHash"
  >,
): boolean =>
  evidence.scriptIntegrityHash !== MIDGARD_ABSENT_SCRIPT_INTEGRITY_HASH &&
  evidence.observerCount > 0 &&
  evidence.networkId === MIDGARD_UNTAGGED_NETWORK_ID;

export const observersForbiddenEvidenceCloses = (
  evidence: ObserversForbiddenEvidence,
): boolean =>
  evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
    ? observersForbiddenFaultHolds(evidence)
    : !observersForbiddenFaultHolds(evidence);

export const ObserversForbiddenVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

export const ObserversForbiddenStateSchema = Data.Object({
  subject: ObserversForbiddenVerdictSubjectSchema,
  network_id: Data.Integer(),
});
