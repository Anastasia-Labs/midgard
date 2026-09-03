import {
  admitFraudProofRawL1Point,
  admitFraudProofRawL1Snapshot,
  admitFraudProofRawL1Utxo,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  type FraudProofRawL1Point,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1SnapshotRequest,
  type FraudProofRawL1Transaction,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy-v1.js";

export const LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE =
  "midgard-local-kupmios-fraud-proof-raw-source-v1" as const;

/**
 * Provider transport owned by the local node runtime. Every response remains
 * untrusted: this package parses the pages, binds both providers to one pinned
 * point, and admits the final canonical bytes.
 */
export interface LocalKupmiosFraudProofRawSource {
  readonly sourceVersion: typeof LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE;
  readonly sourceId: string;
  readonly kupoHttpUrl: string;
  readonly ogmiosWebSocketUrl: string;
  readBoundary(): Promise<unknown>;
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

const MAX_PAGE_COUNT = 100_000;
const MAX_PAGE_ITEMS = 10_000;
const DIGEST = /^[0-9a-f]{64}$/u;

const record = (
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

const exact = (
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

const array = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value) || value.length > MAX_PAGE_ITEMS) {
    throw new Error(`${label} must be a bounded array`);
  }
  return value;
};

const nonEmptyString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.trim() !== value
  ) {
    throw new Error(`${label} must be a canonical non-empty string`);
  }
  return value;
};

const txHash = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!DIGEST.test(parsed)) throw new Error(`${label} must be 32-byte hex`);
  return parsed;
};

const assertLoopback = (value: string, label: string): void => {
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

const samePoint = (
  left: FraudProofRawL1Point,
  right: FraudProofRawL1Point,
): boolean =>
  left.slot === right.slot &&
  left.blockNo === right.blockNo &&
  left.blockHash === right.blockHash &&
  left.pointId === right.pointId;

const parseBoundary = (
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

const parsePageTail = (
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

const scanAllAddressUtxos = async ({
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

type UnitHistoryTransaction = {
  readonly txHash: string;
  readonly inclusionPoint: FraudProofRawL1Point;
};

const scanCompleteUnitHistory = async ({
  source,
  unit,
  throughPoint,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly unit: string;
  readonly throughPoint: FraudProofRawL1Point;
}): Promise<readonly UnitHistoryTransaction[]> => {
  const result: UnitHistoryTransaction[] = [];
  const seenCursors = new Set<string>();
  let after: string | null = null;
  for (let pageIndex = 0; pageIndex < MAX_PAGE_COUNT; pageIndex += 1) {
    const label = `Kupo unit-history page ${pageIndex.toString()}`;
    const parsed = exact(
      await source.scanUnitHistoryPage({
        unit,
        fromGenesis: true,
        throughPoint,
        after,
      }),
      ["checkpoint", "transactions", "nextCursor", "complete"],
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
      ...array(parsed.transactions, `${label}.transactions`).map(
        (entry, index) => {
          const transactionLabel = `${label}.transactions[${index.toString()}]`;
          const transaction = exact(
            entry,
            ["txHash", "inclusionPoint"],
            transactionLabel,
          );
          return {
            txHash: txHash(transaction.txHash, `${transactionLabel}.txHash`),
            inclusionPoint: admitFraudProofRawL1Point(
              transaction.inclusionPoint,
              `${transactionLabel}.inclusionPoint`,
            ),
          };
        },
      ),
    );
    const tail = parsePageTail(parsed, label);
    if (tail.complete) {
      if (new Set(result.map((entry) => entry.txHash)).size !== result.length) {
        throw new Error(
          "Kupo unit-history scan returned duplicate transactions",
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
  throw new Error("Kupo unit-history scan exceeded the page safety bound");
};

const readCrossCheckedTransaction = async ({
  source,
  expected,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly expected: UnitHistoryTransaction;
}): Promise<FraudProofRawL1Transaction> => {
  const parsed = exact(
    await source.readTransaction({
      txHash: expected.txHash,
      expectedInclusionPoint: expected.inclusionPoint,
    }),
    ["kupo", "ogmios"],
    `Kupmios transaction ${expected.txHash}`,
  );
  const kupo = exact(
    parsed.kupo,
    ["txHash", "inclusionPoint"],
    `Kupo transaction ${expected.txHash}`,
  );
  const kupoTxHash = txHash(kupo.txHash, "Kupo transaction hash");
  const kupoPoint = admitFraudProofRawL1Point(
    kupo.inclusionPoint,
    "Kupo transaction inclusion point",
  );
  const ogmios = record(parsed.ogmios, `Ogmios transaction ${expected.txHash}`);
  const ogmiosTxHash = txHash(ogmios.txHash, "Ogmios transaction hash");
  const ogmiosPoint = admitFraudProofRawL1Point(
    ogmios.inclusionPoint,
    "Ogmios transaction inclusion point",
  );
  if (
    kupoTxHash !== expected.txHash ||
    ogmiosTxHash !== expected.txHash ||
    !samePoint(kupoPoint, expected.inclusionPoint) ||
    !samePoint(ogmiosPoint, expected.inclusionPoint)
  ) {
    throw new Error(
      `Kupo and Ogmios disagree about transaction ${expected.txHash}`,
    );
  }
  return parsed.ogmios as FraudProofRawL1Transaction;
};

const confirmPinnedPoint = async ({
  source,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<void> => {
  const parsed = exact(
    await source.confirmCanonicalPoint({ point }),
    ["canonical", "point"],
    "Kupmios canonical-point confirmation",
  );
  const confirmed = admitFraudProofRawL1Point(
    parsed.point,
    "Kupmios confirmed point",
  );
  if (parsed.canonical !== true || !samePoint(confirmed, point)) {
    throw new Error("pinned Kupo point rolled back during snapshot capture");
  }
};

export const createLocalKupmiosFraudProofRawL1SnapshotAuthority = ({
  source,
  releaseFinality,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): FraudProofRawL1SnapshotAuthority => {
  if (source.sourceVersion !== LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE) {
    throw new Error("local Kupmios raw source has an unsupported version");
  }
  const sourceId = nonEmptyString(source.sourceId, "local Kupmios sourceId");
  assertLoopback(source.kupoHttpUrl, "Kupo URL");
  assertLoopback(source.ogmiosWebSocketUrl, "Ogmios URL");
  return {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: async (request: FraudProofRawL1SnapshotRequest) => {
      const boundary = parseBoundary(await source.readBoundary());
      const scopes = await Promise.all(
        request.scopes.map(async (scope) => ({
          ...scope,
          utxos: await scanAllAddressUtxos({
            source,
            address: scope.address,
            throughPoint: boundary.kupoCheckpoint,
          }),
        })),
      );
      const histories = await Promise.all(
        request.historyUnits.map(async (unit) => ({
          unit,
          transactions: await scanCompleteUnitHistory({
            source,
            unit,
            throughPoint: boundary.kupoCheckpoint,
          }),
        })),
      );
      const inclusionByHash = new Map<string, FraudProofRawL1Point>();
      for (const history of histories) {
        for (const transaction of history.transactions) {
          const previous = inclusionByHash.get(transaction.txHash);
          if (
            previous !== undefined &&
            !samePoint(previous, transaction.inclusionPoint)
          ) {
            throw new Error(
              `Kupo unit histories disagree about transaction ${transaction.txHash}`,
            );
          }
          inclusionByHash.set(transaction.txHash, transaction.inclusionPoint);
        }
      }
      const transactions = await Promise.all(
        [...inclusionByHash.entries()]
          .sort(([left], [right]) => left.localeCompare(right))
          .map(([transactionHash, inclusionPoint]) =>
            readCrossCheckedTransaction({
              source,
              expected: { txHash: transactionHash, inclusionPoint },
            }),
          ),
      );
      await confirmPinnedPoint({ source, point: boundary.kupoCheckpoint });
      const confirmationDepth =
        BigInt(boundary.ogmiosTip.blockNo) -
        BigInt(boundary.kupoCheckpoint.blockNo) +
        1n;
      if (
        confirmationDepth <= 0n ||
        confirmationDepth > BigInt(Number.MAX_SAFE_INTEGER)
      ) {
        throw new Error("Kupmios boundary has an invalid confirmation depth");
      }
      const snapshot = {
        schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
        deploymentIdentityDigest: request.deploymentIdentityDigest,
        releaseIdentityDigest: request.releaseIdentityDigest,
        finalityPolicyDigest: request.finalityPolicyDigest,
        headerHash: request.headerHash,
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId,
          grade: "security",
          sourceMode: "local_kupo_ogmios",
          kupoCheckpoint: boundary.kupoCheckpoint,
          ogmiosTip: boundary.ogmiosTip,
        },
        cursor: {
          point: boundary.kupoCheckpoint,
          tip: boundary.ogmiosTip,
          confirmationDepth: Number(confirmationDepth),
          rollbackCursor: computeFraudProofRawL1RollbackCursor({
            deploymentIdentityDigest: request.deploymentIdentityDigest,
            releaseIdentityDigest: request.releaseIdentityDigest,
            finalityPolicyDigest: request.finalityPolicyDigest,
            sourceId,
            pointId: boundary.kupoCheckpoint.pointId,
          }),
        },
        scopes,
        historyUnits: [...request.historyUnits],
        history: histories.map((history) => ({
          unit: history.unit,
          fromGenesis: true as const,
          completeThroughPointId: boundary.kupoCheckpoint.pointId,
          transactionHashes: history.transactions.map(
            (transaction) => transaction.txHash,
          ),
        })),
        transactions,
      };
      return admitFraudProofRawL1Snapshot({
        value: snapshot,
        request,
        releaseFinality,
      });
    },
  };
};
