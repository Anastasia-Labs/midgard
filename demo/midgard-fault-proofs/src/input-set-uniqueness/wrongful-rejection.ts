import { encodeCbor } from "@al-ft/midgard-core";
import {
  encodeVerdictSubject,
  hashHexWithBlake2b,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  PROOF_THREAD_SOURCE_KIND_FORCED,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";

const DOMAIN = Buffer.from(
  "midgard/fraud-proofs/input-set-uniqueness/checkpoint-v1",
  "utf8",
);

export type BoundDuplicateInput = Readonly<{
  subject: VerdictSubject;
  first_field_index: bigint;
  first_item_index: bigint;
  second_field_index: bigint;
  second_item_index: bigint;
}>;

export type InputSetUniqueScanState = Readonly<{
  bound: BoundDuplicateInput;
  spend_count: bigint;
  reference_count: bigint;
  cursor: bigint;
  previous_item: string;
  next_expected_script_hash: string;
  checkpoint_hash: string;
}>;

const canonicalItem = (value: string, label: string): string => {
  if (!/^825820[0-9a-f]{64}19[0-9a-f]{4}$/u.test(value)) {
    throw new Error(`${label} is not a canonical 38-byte out-ref item`);
  }
  return value;
};

export const bindForcedDuplicateInput = (
  subject: VerdictSubject,
): BoundDuplicateInput => {
  if (
    subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION ||
    subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED ||
    subject.rejection_reason === null ||
    typeof subject.rejection_reason !== "object" ||
    !("DuplicateInput" in subject.rejection_reason)
  ) {
    throw new Error(
      "input-set-uniqueness forced subject does not carry DuplicateInput",
    );
  }
  return Object.freeze({ subject, ...subject.rejection_reason.DuplicateInput });
};

export const inputSetUnionIsStrictlyIncreasing = ({
  spendInputItemCbors,
  referenceInputItemCbors,
}: {
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
}): boolean => {
  const union = [...spendInputItemCbors, ...referenceInputItemCbors].map(
    (item, index) => canonicalItem(item, `input-set union item ${index}`),
  );
  return union.every(
    (item, index) => index === 0 || (union[index - 1] as string) < item,
  );
};

const cborBytes = (hex: string): Buffer => {
  const bytes = Buffer.from(hex, "hex");
  if (bytes.length < 24)
    return Buffer.concat([Buffer.from([0x40 + bytes.length]), bytes]);
  if (bytes.length <= 0xff)
    return Buffer.concat([Buffer.from([0x58, bytes.length]), bytes]);
  const head = Buffer.alloc(3);
  head[0] = 0x59;
  head.writeUInt16BE(bytes.length, 1);
  return Buffer.concat([head, bytes]);
};

export const inputSetUniquenessCheckpoint = ({
  bound,
  spendCount,
  referenceCount,
  cursor,
  previousItem,
  nextExpectedScriptHash,
}: {
  readonly bound: BoundDuplicateInput;
  readonly spendCount: bigint;
  readonly referenceCount: bigint;
  readonly cursor: bigint;
  readonly previousItem: string;
  readonly nextExpectedScriptHash: string;
}): string => {
  if (!/^[0-9a-f]{56}$/u.test(nextExpectedScriptHash)) {
    throw new Error("next expected script hash must be 28-byte lowercase hex");
  }
  if (previousItem !== "") canonicalItem(previousItem, "previous item");
  const integers = [
    bound.first_field_index,
    bound.first_item_index,
    bound.second_field_index,
    bound.second_item_index,
    spendCount,
    referenceCount,
    cursor,
  ].map((value) => Buffer.from(encodeCbor(value)));
  const material = Buffer.concat([
    DOMAIN,
    encodeVerdictSubject(bound.subject),
    ...integers,
    cborBytes(previousItem),
    cborBytes(nextExpectedScriptHash),
  ]).toString("hex");
  return Effect.runSync(hashHexWithBlake2b(material, 32));
};
