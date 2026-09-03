import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  type RejectionReason,
  RejectionReasonSchema,
  terminalVerdictContradiction,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const OBSERVER_ORDER_INVALID_CATEGORY = "observerOrderInvalid" as const;
export const OBSERVER_ORDER_INVALID_CATEGORY_ID = "00000025" as const;
export const OBSERVER_ORDER_INVALID_FIELD_INDEX = 3 as const;
export const OBSERVER_ORDER_INVALID_ITEM_BUDGET = 24 as const;

const fail = (message: string): never => {
  throw new Error(`${OBSERVER_ORDER_INVALID_CATEGORY}: ${message}`);
};
const natural = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0)
    return fail(`${label} must be a non-negative safe integer`);
  return value;
};
const reasonIndex = (reason: RejectionReason): number => {
  if (typeof reason === "string" || !("ObserverOrderInvalid" in reason))
    return fail("typed rejection reason is not ObserverOrderInvalid");
  return natural(
    Number(reason.ObserverOrderInvalid.observer_index),
    "reason observer index",
  );
};

export type ObserverOrderInvalidFinding = Readonly<{
  subject: VerdictSubject;
  observerIndex: number;
}>;
export const classifyObserverOrderInvalidFinding = (
  finding: ObserverOrderInvalidFinding,
): ObserverOrderInvalidFinding => {
  if (!verdictSubjectIsCanonical(finding.subject))
    return fail("verdict subject is not canonical");
  const observerIndex = natural(finding.observerIndex, "observer index");
  if (observerIndex === 0)
    return fail("observer index must name the later item of an adjacent pair");
  if (finding.subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION) {
    if (finding.subject.rejection_reason === null)
      return fail("wrongful rejection has no typed reason");
    if (reasonIndex(finding.subject.rejection_reason) !== observerIndex)
      return fail("typed reason observer coordinate changed");
  } else if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE ||
    finding.subject.rejection_reason !== null
  ) {
    return fail("direction/rejection-reason polarity is invalid");
  }
  return Object.freeze({ subject: finding.subject, observerIndex });
};

export type ObserverOrderInvalidScanResult = Readonly<{
  violation: boolean;
  previousObserverHex: string;
  observerHex: string;
}>;
/** Exact first-offending-adjacent-pair twin of PhaseAScriptPreconditions. */
export const scanObserverOrderInvalid = (
  items: readonly Uint8Array[],
  observerIndex: number,
): ObserverOrderInvalidScanResult => {
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

export type ObserverOrderInvalidEvidence = ObserverOrderInvalidFinding &
  ObserverOrderInvalidScanResult &
  Readonly<{
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;
export const prepareObserverOrderInvalidEvidence = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: ObserverOrderInvalidFinding;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): ObserverOrderInvalidEvidence => {
  const finding = classifyObserverOrderInvalidFinding(rawFinding);
  if (!/^[0-9a-f]{64}$/u.test(committedFieldHashHex))
    return fail("field commitment is not 32-byte lowercase hex");
  const actual = midgardFieldCommitment(fieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    return fail("retained field-3 bytes do not match the compact commitment");
  const items = decodeMidgardFieldPreimage(fieldPreimage);
  return Object.freeze({
    ...finding,
    ...scanObserverOrderInvalid(items, finding.observerIndex),
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
  });
};
export const observerOrderInvalidEvidenceCloses = (
  evidence: ObserverOrderInvalidEvidence,
): boolean =>
  terminalVerdictContradiction(evidence.subject, evidence.violation);

export const ObserverOrderInvalidVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const ObserverOrderInvalidBoundObserverSchema = Data.Object({
  subject: ObserverOrderInvalidVerdictSubjectSchema,
  observer_index: Data.Integer(),
});
export const ObserverOrderInvalidAuthenticationStateSchema = Data.Enum([
  Data.Object({
    Bound: Data.Object({ bound: ObserverOrderInvalidBoundObserverSchema }),
  }),
  Data.Object({
    Reserved: Data.Object({ bound: ObserverOrderInvalidBoundObserverSchema }),
  }),
]);
export const ObserverOrderInvalidScanStateSchema = Data.Object({
  subject: ObserverOrderInvalidVerdictSubjectSchema,
  observer_index: Data.Integer(),
  checkpoint_hash: Data.Bytes(),
  seen: Data.Integer(),
  previous_observer: Data.Bytes(),
  outcome: Data.Integer(),
});
export const ObserverOrderInvalidDecisionStateSchema = Data.Object({
  subject: ObserverOrderInvalidVerdictSubjectSchema,
  observer_index: Data.Integer(),
  violation: Data.Boolean(),
});
