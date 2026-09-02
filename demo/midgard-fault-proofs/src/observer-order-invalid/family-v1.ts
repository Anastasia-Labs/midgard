import {
  decodeMidgardFieldPreimageV1,
  midgardFieldCommitmentV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
  type RejectionReasonV1,
  RejectionReasonV1Schema,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const OBSERVER_ORDER_INVALID_CATEGORY_V1 =
  "observerOrderInvalid" as const;
export const OBSERVER_ORDER_INVALID_CATEGORY_ID_V1 = "00000025" as const;
export const OBSERVER_ORDER_INVALID_FIELD_INDEX_V1 = 3 as const;
export const OBSERVER_ORDER_INVALID_ITEM_BUDGET_V1 = 24 as const;

const fail = (message: string): never => {
  throw new Error(`${OBSERVER_ORDER_INVALID_CATEGORY_V1}: ${message}`);
};
const natural = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};
const reasonIndex = (reason: RejectionReasonV1): number => {
  if (typeof reason === "string" || !("ObserverOrderInvalid" in reason))
    return fail("typed rejection reason is not ObserverOrderInvalid");
  return natural(
    Number(reason.ObserverOrderInvalid.observer_index),
    "reason observer index",
  );
};

export type ObserverOrderInvalidFindingV1 = Readonly<{
  subject: VerdictSubjectV1;
  observerIndex: number;
}>;
export const classifyObserverOrderInvalidFindingV1 = (
  finding: ObserverOrderInvalidFindingV1,
): ObserverOrderInvalidFindingV1 => {
  if (!verdictSubjectIsCanonicalV1(finding.subject))
    return fail("verdict subject is not canonical");
  const observerIndex = natural(finding.observerIndex, "observer index");
  if (observerIndex === 0)
    return fail("observer index must name the later item of an adjacent pair");
  if (
    finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    if (finding.subject.rejection_reason === null)
      return fail("wrongful rejection has no typed reason");
    if (reasonIndex(finding.subject.rejection_reason) !== observerIndex)
      return fail("typed reason observer coordinate changed");
  } else if (
    finding.subject.direction !==
      PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity is invalid");
  }
  return Object.freeze({ subject: finding.subject, observerIndex });
};

export type ObserverOrderInvalidScanResultV1 = Readonly<{
  violation: boolean;
  previousObserverHex: string;
  observerHex: string;
}>;
/** Exact first-offending-adjacent-pair twin of PhaseAScriptPreconditions. */
export const scanObserverOrderInvalidV1 = (
  items: readonly Uint8Array[],
  observerIndex: number,
): ObserverOrderInvalidScanResultV1 => {
  natural(observerIndex, "observer index");
  if (observerIndex === 0 || observerIndex >= items.length)
    return fail("observer coordinate is outside an adjacent field-3 pair");
  let previous = Buffer.from(items[0]!);
  if (previous.length !== 28) return fail("observer item is not 28 bytes");
  for (let index = 1; index <= observerIndex; index += 1) {
    const current = Buffer.from(items[index]!);
    if (current.length !== 28) return fail("observer item is not 28 bytes");
    const violation = Buffer.compare(previous, current) >= 0;
    if (index < observerIndex && violation)
      return fail("an earlier observer pair is already noncanonical");
    if (index === observerIndex)
      return Object.freeze({
        violation,
        previousObserverHex: previous.toString("hex"),
        observerHex: current.toString("hex"),
      });
    previous = current;
  }
  return fail("unreachable observer scan state");
};

export type ObserverOrderInvalidEvidenceV1 = ObserverOrderInvalidFindingV1 &
  ObserverOrderInvalidScanResultV1 &
  Readonly<{
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;
export const prepareObserverOrderInvalidEvidenceV1 = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: ObserverOrderInvalidFindingV1;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): ObserverOrderInvalidEvidenceV1 => {
  const finding = classifyObserverOrderInvalidFindingV1(rawFinding);
  if (!/^[0-9a-f]{64}$/u.test(committedFieldHashHex))
    return fail("field commitment is not 32-byte lowercase hex");
  const actual = midgardFieldCommitmentV1(fieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    return fail("retained field-3 bytes do not match the compact commitment");
  const items = decodeMidgardFieldPreimageV1(fieldPreimage);
  return Object.freeze({
    ...finding,
    ...scanObserverOrderInvalidV1(items, finding.observerIndex),
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    carriage: selectMidgardFieldCarriageTierV1(fieldPreimage.length),
  });
};
export const observerOrderInvalidEvidenceClosesV1 = (
  evidence: ObserverOrderInvalidEvidenceV1,
): boolean =>
  terminalVerdictContradictionV1(evidence.subject, evidence.violation);

export const ObserverOrderInvalidVerdictSubjectV1Schema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonV1Schema),
});
export const ObserverOrderInvalidBoundObserverV1Schema = Data.Object({
  subject: ObserverOrderInvalidVerdictSubjectV1Schema,
  observer_index: Data.Integer(),
});
export const ObserverOrderInvalidAuthenticationStateV1Schema = Data.Enum([
  Data.Object({
    Bound: Data.Object({ bound: ObserverOrderInvalidBoundObserverV1Schema }),
  }),
  Data.Object({
    Reserved: Data.Object({ bound: ObserverOrderInvalidBoundObserverV1Schema }),
  }),
]);
export const ObserverOrderInvalidScanStateV1Schema = Data.Object({
  subject: ObserverOrderInvalidVerdictSubjectV1Schema,
  observer_index: Data.Integer(),
  checkpoint_hash: Data.Bytes(),
  seen: Data.Integer(),
  previous_observer: Data.Bytes(),
  outcome: Data.Integer(),
});
export const ObserverOrderInvalidDecisionStateV1Schema = Data.Object({
  subject: ObserverOrderInvalidVerdictSubjectV1Schema,
  observer_index: Data.Integer(),
  violation: Data.Boolean(),
});
