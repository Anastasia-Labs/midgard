import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";

import {
  exactLiteral,
  exactNatural,
  exactRecord,
  exactString,
  fail,
  type ParseBudget,
  sha256Bytes,
} from "./l1-adapter.exact-array.js";
import {
  HEX_28,
  HEX_32,
  LOWER_HEX_BYTES,
  WATCHER_L1_ADAPTER_BOUNDS,
  WATCHER_L1_REDEEMER_PURPOSES,
  WATCHER_L1_SCRIPT_LANGUAGES,
  type WatcherL1Datum,
  type WatcherL1PublicBytes,
  type WatcherL1Redeemer,
  type WatcherL1Script,
  type WatcherL1Utxo,
} from "./l1-adapter.watcher-local-node-query-transport.js";

const freezePublicBytes = (
  bytesHex: string,
  sha256: string,
): WatcherL1PublicBytes =>
  Object.freeze({
    bytesHex,
    sha256,
  });

export const parsePublicBytes = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1PublicBytes => {
  const record = exactRecord(value, path, ["bytesHex", "sha256"]);
  const bytesHex = exactString(
    record.bytesHex,
    `${path}.bytesHex`,
    LOWER_HEX_BYTES,
  );
  const byteLength = bytesHex.length / 2;
  if (byteLength > WATCHER_L1_ADAPTER_BOUNDS.publicBytes) {
    fail("out_of_bounds", `${path}.bytesHex`);
  }
  budget.publicBytes += byteLength;
  if (budget.publicBytes > WATCHER_L1_ADAPTER_BOUNDS.totalPublicBytes) {
    fail("out_of_bounds", "$.transactions");
  }
  const sha256 = exactString(record.sha256, `${path}.sha256`, HEX_32);
  if (sha256Bytes(Buffer.from(bytesHex, "hex")) !== sha256) {
    fail("content_digest_mismatch", `${path}.sha256`);
  }
  return freezePublicBytes(bytesHex, sha256);
};

export const makeWatcherL1PublicBytes = (
  bytesHex: string,
): WatcherL1PublicBytes => {
  if (
    !LOWER_HEX_BYTES.test(bytesHex) ||
    bytesHex.length / 2 > WATCHER_L1_ADAPTER_BOUNDS.publicBytes
  ) {
    fail("invalid_field", "$.bytesHex");
  }
  return freezePublicBytes(bytesHex, sha256Bytes(Buffer.from(bytesHex, "hex")));
};

export const parseScript = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1Script => {
  const record = exactRecord(value, path, ["scriptHash", "language", "bytes"]);
  return Object.freeze({
    scriptHash: exactString(record.scriptHash, `${path}.scriptHash`, HEX_28),
    language: exactLiteral(
      record.language,
      `${path}.language`,
      WATCHER_L1_SCRIPT_LANGUAGES,
    ),
    bytes: parsePublicBytes(record.bytes, `${path}.bytes`, budget),
  });
};

export const parseDatum = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1Datum => {
  const record = exactRecord(value, path, ["datumHash", "bytes"]);
  const bytes = parsePublicBytes(record.bytes, `${path}.bytes`, budget);
  const datumHash = exactString(record.datumHash, `${path}.datumHash`, HEX_32);
  if (
    computeHash32(Buffer.from(bytes.bytesHex, "hex")).toString("hex") !==
    datumHash
  ) {
    fail("identity_mismatch", `${path}.datumHash`);
  }
  return Object.freeze({ datumHash, bytes });
};

const parseOptionalDatum = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1Datum | null =>
  value === null ? null : parseDatum(value, path, budget);

const parseOptionalScript = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1Script | null =>
  value === null ? null : parseScript(value, path, budget);

export const parseRedeemer = (
  value: unknown,
  path: string,
  budget: ParseBudget,
): WatcherL1Redeemer => {
  const record = exactRecord(value, path, ["purpose", "index", "bytes"]);
  return Object.freeze({
    purpose: exactLiteral(
      record.purpose,
      `${path}.purpose`,
      WATCHER_L1_REDEEMER_PURPOSES,
    ),
    index: exactNatural(record.index, `${path}.index`),
    bytes: parsePublicBytes(record.bytes, `${path}.bytes`, budget),
  });
};

export const compareNaturalStrings = (left: string, right: string): number =>
  left.length !== right.length
    ? left.length - right.length
    : left < right
      ? -1
      : left > right
        ? 1
        : 0;

export const parseUtxo = (
  value: unknown,
  path: string,
  txHash: string,
  budget: ParseBudget,
): WatcherL1Utxo => {
  const record = exactRecord(value, path, [
    "outRef",
    "outputIndex",
    "output",
    "datum",
    "referenceScript",
  ]);
  const outputIndex = exactNatural(record.outputIndex, `${path}.outputIndex`);
  const outRef = exactString(
    record.outRef,
    `${path}.outRef`,
    /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u,
  );
  if (outRef !== `${txHash}#${outputIndex}`) {
    fail("identity_mismatch", `${path}.outRef`);
  }
  return Object.freeze({
    outRef,
    outputIndex,
    output: parsePublicBytes(record.output, `${path}.output`, budget),
    datum: parseOptionalDatum(record.datum, `${path}.datum`, budget),
    referenceScript: parseOptionalScript(
      record.referenceScript,
      `${path}.referenceScript`,
      budget,
    ),
  });
};

export const freezeSortedUnique = <T>(
  values: readonly T[],
  path: string,
  identity: (value: T) => string,
  compare: (left: T, right: T) => number = (left, right) =>
    identity(left) < identity(right)
      ? -1
      : identity(left) > identity(right)
        ? 1
        : 0,
): readonly T[] => {
  const sorted = [...values].sort(compare);
  for (let index = 1; index < sorted.length; index += 1) {
    if (identity(sorted[index - 1] as T) === identity(sorted[index] as T)) {
      fail("duplicate_identity", `${path}[${index.toString()}]`);
    }
  }
  return Object.freeze(sorted);
};

export const purposeOrder = new Map(
  WATCHER_L1_REDEEMER_PURPOSES.map((purpose, index) => [purpose, index]),
);

export const compareRedeemers = (
  left: WatcherL1Redeemer,
  right: WatcherL1Redeemer,
): number => {
  const purposeComparison =
    (purposeOrder.get(left.purpose) as number) -
    (purposeOrder.get(right.purpose) as number);
  return purposeComparison === 0
    ? compareNaturalStrings(left.index, right.index)
    : purposeComparison;
};

export const publicBytesFromCbor = (bytesHex: string): WatcherL1PublicBytes =>
  freezePublicBytes(bytesHex, sha256Bytes(Buffer.from(bytesHex, "hex")));

export type CmlWitnessScript = Readonly<{
  hash(): Readonly<{ to_hex(): string }>;
  to_canonical_cbor_hex(): string;
}>;

type CmlWitnessScriptList = Readonly<{
  get(index: number): CmlWitnessScript;
  len(): number;
}>;

export const collectWitnessScripts = (
  list: CmlWitnessScriptList | undefined,
  language: WatcherL1Script["language"],
): WatcherL1Script[] => {
  const scripts: WatcherL1Script[] = [];
  if (list === undefined) {
    return scripts;
  }
  for (let index = 0; index < list.len(); index += 1) {
    const script = list.get(index);
    scripts.push(
      Object.freeze({
        scriptHash: script.hash().to_hex(),
        language,
        bytes: publicBytesFromCbor(script.to_canonical_cbor_hex()),
      }),
    );
  }
  return scripts;
};

const redeemerPurpose = (
  tag: CML.RedeemerTag,
  path: string,
): WatcherL1Redeemer["purpose"] => {
  switch (tag) {
    case CML.RedeemerTag.Spend:
      return "spend";
    case CML.RedeemerTag.Mint:
      return "mint";
    case CML.RedeemerTag.Cert:
      return "certificate";
    case CML.RedeemerTag.Reward:
      return "withdrawal";
    case CML.RedeemerTag.Voting:
      return "vote";
    case CML.RedeemerTag.Proposing:
      return "propose";
    default:
      return fail("invalid_field", path);
  }
};

export const witnessRedeemer = (
  tag: CML.RedeemerTag,
  index: bigint,
  data: CML.PlutusData,
  path: string,
): WatcherL1Redeemer =>
  Object.freeze({
    purpose: redeemerPurpose(tag, path),
    index: index.toString(),
    bytes: publicBytesFromCbor(data.to_canonical_cbor_hex()),
  });
