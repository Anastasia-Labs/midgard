import {
  decodeMidgardFieldPreimageV1,
  midgardFieldCommitmentV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  RejectionReasonV1Schema,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_V1 =
  "observersForbiddenOnUntaggedNetwork" as const;
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY_ID_V1 =
  "00000024" as const;
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FIELD_INDEX_V1 =
  3 as const;
export const OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_ID_V1 =
  "ObserversForbiddenOnUntaggedNetwork" as const;
export const MIDGARD_UNTAGGED_NETWORK_ID_V1 = 255 as const;

export type ObserversForbiddenFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  networkId: 0 | 1 | 255;
}>;

export const classifyObserversForbiddenFindingV1 = (
  finding: ObserversForbiddenFindingV1,
): ObserversForbiddenFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    throw new Error("observersForbidden: verdict subject is not canonical");
  const direction = finding.subject.direction;
  if (![0, 1, 255].includes(finding.networkId))
    throw new Error("observersForbidden: network scalar changed");
  if (direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1) {
    const reason = finding.subject.rejection_reason;
    if (reason !== OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_ID_V1)
      throw new Error("observersForbidden: typed rejection reason changed");
  } else if (
    direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  ) {
    throw new Error("observersForbidden: direction/reason polarity changed");
  }
  return Object.freeze(finding);
};

export type ObserversForbiddenEvidenceV1 = ObserversForbiddenFindingV1 &
  Readonly<{
    observerCount: number;
    observerFieldPreimageCbor: string;
    observerFieldCommitment: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

export const prepareObserversForbiddenEvidenceV1 = ({
  finding: rawFinding,
  observerFieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: ObserversForbiddenFindingV1;
  readonly observerFieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): ObserversForbiddenEvidenceV1 => {
  const finding = classifyObserversForbiddenFindingV1(rawFinding);
  const actual = midgardFieldCommitmentV1(observerFieldPreimage).toString(
    "hex",
  );
  if (actual !== committedFieldHashHex)
    throw new Error("observersForbidden: retained field 3 changed commitment");
  const observers = decodeMidgardFieldPreimageV1(observerFieldPreimage);
  if (observers.some((observer) => observer.length !== 28))
    throw new Error("observersForbidden: observer is not a 28-byte hash");
  return Object.freeze({
    ...finding,
    observerCount: observers.length,
    observerFieldPreimageCbor: Buffer.from(observerFieldPreimage).toString(
      "hex",
    ),
    observerFieldCommitment: actual,
    carriage: selectMidgardFieldCarriageTierV1(observerFieldPreimage.length),
  });
};

export const observersForbiddenFaultHoldsV1 = (
  evidence: Pick<ObserversForbiddenEvidenceV1, "observerCount" | "networkId">,
): boolean =>
  evidence.observerCount > 0 &&
  evidence.networkId === MIDGARD_UNTAGGED_NETWORK_ID_V1;

export const observersForbiddenEvidenceClosesV1 = (
  evidence: ObserversForbiddenEvidenceV1,
): boolean =>
  evidence.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ? observersForbiddenFaultHoldsV1(evidence)
    : !observersForbiddenFaultHoldsV1(evidence);

export const ObserversForbiddenVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});

export const ObserversForbiddenStateV1Schema = Data.Object({
  subject: ObserversForbiddenVerdictSubjectV1Schema,
  network_id: Data.Integer(),
});
