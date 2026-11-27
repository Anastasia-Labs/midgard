import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { dirname, isAbsolute, resolve } from "node:path";
import { isDeepStrictEqual } from "node:util";

import {
  type AuthenticatedL1TxObservation,
  canonicalString,
  type ChainPoint,
  exactKeys,
  type JsonValue,
  lowerHex,
  nonNegativeInteger,
  positiveInteger,
  record,
} from "./e2e-state-correction-reconciliation.final-snapshot.js";

export const sha256Hex = (value: unknown, field: string): string =>
  lowerHex(value, 32, field);

export const canonicalAssetUnit = (value: unknown, field: string): string => {
  const parsed = canonicalString(value, field);
  if (!/^[0-9a-f]{56,120}$/u.test(parsed)) {
    throw new Error(`${field} must be a canonical Cardano asset unit`);
  }
  return parsed;
};

export const canonicalOutRef = (value: unknown, field: string): string => {
  const parsed = canonicalString(value, field);
  if (!/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(parsed)) {
    throw new Error(`${field} must be a canonical Cardano output reference`);
  }
  return parsed;
};

export const canonicalLovelace = (value: unknown, field: string): string => {
  const parsed = canonicalString(value, field);
  if (!/^(?:0|[1-9][0-9]*)$/u.test(parsed)) {
    throw new Error(`${field} must be canonical non-negative lovelace`);
  }
  return parsed;
};

export const stringArray = (
  value: unknown,
  field: string,
  parse: (entry: unknown, field: string) => string = canonicalString,
): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${field} must be an array`);
  return value.map((entry, index) =>
    parse(entry, `${field}[${index.toString()}]`),
  );
};

export const parseChainPoint = (value: unknown, field: string): ChainPoint => {
  const candidate = record(value, field);
  exactKeys(candidate, ["slot", "blockHash"], field);
  const slot = canonicalString(candidate.slot, `${field}.slot`);
  if (!/^(?:0|[1-9][0-9]*)$/u.test(slot)) {
    throw new Error(`${field}.slot must be a canonical non-negative integer`);
  }
  return {
    slot,
    blockHash: sha256Hex(candidate.blockHash, `${field}.blockHash`),
  };
};

export const parseObservedChainPoint = (
  value: unknown,
  field: string,
): ChainPoint & { readonly confirmationDepth: number } => {
  const candidate = record(value, field);
  exactKeys(candidate, ["slot", "blockHash", "confirmationDepth"], field);
  return {
    ...parseChainPoint(
      { slot: candidate.slot, blockHash: candidate.blockHash },
      field,
    ),
    confirmationDepth: positiveInteger(
      candidate.confirmationDepth,
      `${field}.confirmationDepth`,
    ),
  };
};

const normalizeJson = (value: unknown, field: string): JsonValue => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return value;
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value) || !Number.isSafeInteger(value)) {
      throw new Error(`${field} numbers must be finite safe integers`);
    }
    return value;
  }
  if (Array.isArray(value)) {
    return value.map((entry, index) =>
      normalizeJson(entry, `${field}[${index.toString()}]`),
    );
  }
  const candidate = record(value, field);
  return Object.fromEntries(
    Object.entries(candidate).map(([key, entry]) => [
      key,
      normalizeJson(entry, `${field}.${key}`),
    ]),
  );
};

const stableJson = (value: JsonValue): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(stableJson).join(",")}]`;
  return `{${Object.entries(value)
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([key, child]) => `${JSON.stringify(key)}:${stableJson(child)}`)
    .join(",")}}`;
};

export const sha256 = (value: string | Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

export const jsonDigest = (value: unknown): string =>
  sha256(stableJson(normalizeJson(value, "digest input")));

export const readJson = async (path: string): Promise<unknown> =>
  JSON.parse(await readFile(path, "utf8")) as unknown;

const referencedPath = (parentPath: string, childPath: string): string =>
  isAbsolute(childPath) ? childPath : resolve(dirname(parentPath), childPath);

export const readDigestCheckedJson = async ({
  parentPath,
  childPath,
  expectedSha256,
  field,
}: {
  readonly parentPath: string;
  readonly childPath: string;
  readonly expectedSha256: string;
  readonly field: string;
}): Promise<{ readonly path: string; readonly value: unknown }> => {
  const path = referencedPath(parentPath, childPath);
  const bytes = await readFile(path);
  assertEqual(sha256(bytes), expectedSha256, `${field} digest`);
  try {
    return { path, value: JSON.parse(bytes.toString("utf8")) as unknown };
  } catch (cause) {
    throw new Error(`${field} must contain JSON`, { cause });
  }
};

type ParsedKupoMatch = {
  readonly transactionId: string;
  readonly outputIndex: number;
  readonly createdAt: ChainPoint;
  readonly spentAt: unknown;
  readonly assets: Readonly<Record<string, unknown>>;
};

export const parseKupoMatches = (
  value: unknown,
  field: string,
): readonly ParsedKupoMatch[] => {
  if (!Array.isArray(value)) throw new Error(`${field} must be an array`);
  return value.map((entry, index) => {
    const itemField = `${field}[${index.toString()}]`;
    const item = record(entry, itemField);
    const createdAt = record(item.created_at, `${itemField}.created_at`);
    const rawSlot = nonNegativeInteger(
      createdAt.slot_no,
      `${itemField}.created_at.slot_no`,
    );
    const valueRecord = record(item.value, `${itemField}.value`);
    const assets = record(valueRecord.assets, `${itemField}.value.assets`);
    return {
      transactionId: sha256Hex(
        item.transaction_id,
        `${itemField}.transaction_id`,
      ),
      outputIndex: nonNegativeInteger(
        item.output_index,
        `${itemField}.output_index`,
      ),
      createdAt: {
        slot: rawSlot.toString(),
        blockHash: sha256Hex(
          createdAt.header_hash,
          `${itemField}.created_at.header_hash`,
        ),
      },
      spentAt: item.spent_at,
      assets,
    };
  });
};

const unwrapOgmiosResult = (value: unknown, field: string): unknown => {
  const candidate = record(value, field);
  return Object.hasOwn(candidate, "result") ? candidate.result : candidate;
};

type ParsedOgmiosBlock = {
  readonly point: ChainPoint;
  readonly height: number;
  readonly transactionIds: ReadonlySet<string>;
};

export const parseOgmiosBlock = (
  value: unknown,
  field: string,
): ParsedOgmiosBlock => {
  const result = record(unwrapOgmiosResult(value, field), `${field}.result`);
  const block = record(
    Object.hasOwn(result, "block") ? result.block : result,
    `${field}.block`,
  );
  if (!Array.isArray(block.transactions)) {
    throw new Error(`${field}.block.transactions must be an array`);
  }
  const transactionIds = new Set(
    block.transactions.map((transaction, index) =>
      sha256Hex(
        record(transaction, `${field}.block.transactions[${index.toString()}]`)
          .id,
        `${field}.block.transactions[${index.toString()}].id`,
      ),
    ),
  );
  return {
    point: {
      slot: nonNegativeInteger(block.slot, `${field}.block.slot`).toString(),
      blockHash: sha256Hex(block.id, `${field}.block.id`),
    },
    height: nonNegativeInteger(block.height, `${field}.block.height`),
    transactionIds,
  };
};

type ParsedOgmiosTip = ChainPoint & { readonly height: number };

export const parseOgmiosTip = (
  value: unknown,
  field: string,
): ParsedOgmiosTip => {
  const result = record(unwrapOgmiosResult(value, field), `${field}.result`);
  const tip = record(
    Object.hasOwn(result, "tip") ? result.tip : result,
    `${field}.tip`,
  );
  return {
    slot: nonNegativeInteger(tip.slot, `${field}.tip.slot`).toString(),
    blockHash: sha256Hex(tip.id, `${field}.tip.id`),
    height: nonNegativeInteger(tip.height, `${field}.tip.height`),
  };
};

export type DerivedL1Observation = {
  readonly observation: AuthenticatedL1TxObservation;
  readonly kupoOutputIndex: number;
  readonly inclusionHeight: number;
  readonly rawPaths: readonly string[];
};

export const assertEqual = (
  actual: unknown,
  expected: unknown,
  field: string,
): void => {
  if (!isDeepStrictEqual(actual, expected)) {
    throw new Error(
      `${field} mismatch: expected=${JSON.stringify(expected)} actual=${JSON.stringify(actual)}`,
    );
  }
};
