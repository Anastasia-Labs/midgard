import {
  admitFraudProofRawL1Point,
  admitFraudProofRawL1Utxo,
  type FraudProofL1ObservationDepth,
  type FraudProofRawL1Point,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";

export const LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE =
  "midgard-local-kupmios-fraud-proof-raw-source-v1" as const;

/** The unpublished capture must restart when its pinned provider head changes. */
export class LocalKupmiosCheckpointChangedError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "LocalKupmiosCheckpointChangedError";
  }
}

/**
 * Provider transport owned by the local node runtime. Every response remains
 * untrusted: this package parses the pages, binds both providers to one pinned
 * point, and admits the final canonical bytes.
 */
export interface LocalKupmiosFraudProofRawSource {
  /** Concrete local history lookup, including transactions whose outputs were spent. */
  resolveTransactionInclusion?(input: {
    readonly txHash: string;
  }): Promise<unknown>;
  pinBoundaryAtPoint?(input: {
    readonly point: FraudProofRawL1Point;
  }): Promise<unknown>;
  readOutRefsAtPoint?(input: {
    readonly point: FraudProofRawL1Point;
    readonly outRefs: readonly string[];
  }): Promise<unknown>;
  readonly sourceVersion: typeof LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE;
  readonly sourceId: string;
  readonly kupoHttpUrl: string;
  readonly ogmiosWebSocketUrl: string;
  readBoundary(input?: {
    readonly observationDepth?: FraudProofL1ObservationDepth;
  }): Promise<unknown>;
  /** Exact ordered raw block capture for independently authenticated readers. */
  readBlockAtPoint(input: {
    readonly point: FraudProofRawL1Point;
  }): Promise<unknown>;
  scanAddressPage(input: {
    readonly address: string;
    readonly throughPoint: FraudProofRawL1Point;
    readonly after: string | null;
  }): Promise<unknown>;
  scanUnitHistoryPage(input: {
    readonly unit: string;
    readonly fromGenesis: true;
    readonly throughPoint: FraudProofRawL1Point;
    readonly after: string | null;
  }): Promise<unknown>;
  readTransaction(input: {
    readonly txHash: string;
    readonly expectedInclusionPoint: FraudProofRawL1Point;
  }): Promise<unknown>;
  confirmCanonicalPoint(input: {
    readonly point: FraudProofRawL1Point;
  }): Promise<unknown>;
}

const sourceCaptures = new WeakMap<
  LocalKupmiosFraudProofRawSource,
  Promise<void>
>();

/** Own the source's mutable boundary through the final read and admission.
 * Call only at the outer capture boundary; nested reads use the held source.
 */
export const withLocalKupmiosSourceCapture = async <T>(
  source: LocalKupmiosFraudProofRawSource,
  capture: () => Promise<T>,
): Promise<T> => {
  const previous = sourceCaptures.get(source) ?? Promise.resolve();
  let release!: () => void;
  const held = new Promise<void>((resolve) => {
    release = resolve;
  });
  sourceCaptures.set(source, held);
  await previous;
  try {
    return await capture();
  } finally {
    release();
    if (sourceCaptures.get(source) === held) sourceCaptures.delete(source);
  }
};

/** A failed branch must not leave sibling reads using a released source. */
export const settleLocalKupmiosReads = async <T extends readonly unknown[]>(
  reads: T,
): Promise<{ -readonly [K in keyof T]: Awaited<T[K]> }> => {
  await Promise.allSettled(reads);
  return await Promise.all(reads);
};

export const MAX_PAGE_COUNT = 100_000;

const MAX_PAGE_ITEMS = 10_000;

const DIGEST = /^[0-9a-f]{64}$/u;

export const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

export const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

export const array = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value) || value.length > MAX_PAGE_ITEMS) {
    throw new Error(`${label} must be a bounded array`);
  }
  return value;
};

export const nonEmptyString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.trim() !== value
  ) {
    throw new Error(`${label} must be a canonical non-empty string`);
  }
  return value;
};

export const txHash = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!DIGEST.test(parsed)) throw new Error(`${label} must be 32-byte hex`);
  return parsed;
};

export const assertLoopback = (value: string, label: string): void => {
  const url = new URL(value);
  const hostname = url.hostname.toLowerCase();
  if (
    hostname !== "127.0.0.1" &&
    hostname !== "localhost" &&
    hostname !== "::1" &&
    hostname !== "[::1]"
  ) {
    throw new Error(`${label} must be a loopback local provider endpoint`);
  }
};

export const samePoint = (
  left: FraudProofRawL1Point,
  right: FraudProofRawL1Point,
): boolean =>
  left.slot === right.slot &&
  left.blockNo === right.blockNo &&
  left.blockHash === right.blockHash &&
  left.pointId === right.pointId;

export const parseBoundary = (
  value: unknown,
): {
  readonly kupoCheckpoint: FraudProofRawL1Point;
  readonly ogmiosTip: FraudProofRawL1Point;
} => {
  const parsed = exact(
    value,
    ["kupoCheckpoint", "ogmiosTip"],
    "Kupmios boundary",
  );
  return {
    kupoCheckpoint: admitFraudProofRawL1Point(
      parsed.kupoCheckpoint,
      "Kupmios Kupo checkpoint",
    ),
    ogmiosTip: admitFraudProofRawL1Point(
      parsed.ogmiosTip,
      "Kupmios Ogmios tip",
    ),
  };
};

export const parsePageTail = (
  parsed: Readonly<Record<string, unknown>>,
  label: string,
): { readonly nextCursor: string | null; readonly complete: boolean } => {
  const nextCursor =
    parsed.nextCursor === null
      ? null
      : nonEmptyString(parsed.nextCursor, `${label}.nextCursor`);
  if (typeof parsed.complete !== "boolean") {
    throw new Error(`${label}.complete must be boolean`);
  }
  if (parsed.complete !== (nextCursor === null)) {
    throw new Error(`${label} has a truncated or contradictory continuation`);
  }
  return { nextCursor, complete: parsed.complete };
};

export const scanAllAddressUtxos = async ({
  source,
  address,
  throughPoint,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly address: string;
  readonly throughPoint: FraudProofRawL1Point;
}): Promise<readonly FraudProofRawL1Utxo[]> => {
  const result: FraudProofRawL1Utxo[] = [];
  const seenCursors = new Set<string>();
  let after: string | null = null;
  for (let pageIndex = 0; pageIndex < MAX_PAGE_COUNT; pageIndex += 1) {
    const label = `Kupo address page ${pageIndex.toString()}`;
    const parsed = exact(
      await source.scanAddressPage({ address, throughPoint, after }),
      ["checkpoint", "utxos", "nextCursor", "complete"],
      label,
    );
    const checkpoint = admitFraudProofRawL1Point(
      parsed.checkpoint,
      `${label}.checkpoint`,
    );
    if (!samePoint(checkpoint, throughPoint)) {
      throw new Error(`${label} changed the pinned Kupo checkpoint`);
    }
    result.push(
      ...array(parsed.utxos, `${label}.utxos`).map((entry, index) =>
        admitFraudProofRawL1Utxo(entry, `${label}.utxos[${index.toString()}]`),
      ),
    );
    const tail = parsePageTail(parsed, label);
    if (tail.complete) {
      if (new Set(result.map((entry) => entry.outRef)).size !== result.length) {
        throw new Error(
          "Kupo address scan returned duplicate output references",
        );
      }
      return result;
    }
    if (tail.nextCursor === null || seenCursors.has(tail.nextCursor)) {
      throw new Error(`${label} repeated or omitted its continuation cursor`);
    }
    seenCursors.add(tail.nextCursor);
    after = tail.nextCursor;
  }
  throw new Error("Kupo address scan exceeded the page safety bound");
};

export type UnitHistoryTransaction = {
  readonly txHash: string;
  readonly inclusionPoint: FraudProofRawL1Point;
};
