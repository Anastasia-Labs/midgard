import {
  admitFraudProofRawL1PointV1,
  admitFraudProofRawL1SnapshotV1,
  admitFraudProofRawL1UtxoV1,
  computeFraudProofRawL1RollbackCursorV1,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_V1_SCHEMA_VERSION,
  type FraudProofRawL1PointV1,
  type FraudProofRawL1SnapshotAuthorityV1,
  type FraudProofRawL1SnapshotRequestV1,
  type FraudProofRawL1TransactionV1,
  type FraudProofRawL1UtxoV1,
} from "./raw-l1-snapshot-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicyV1 } from "./release-finality-policy-v1.js";

export const LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE_V1 =
  "midgard-local-kupmios-fraud-proof-raw-source-v1" as const;

/**
 * Provider transport owned by the local node runtime. Every response remains
 * untrusted: this package parses the pages, binds both providers to one pinned
 * point, and admits the final canonical bytes.
 */
export interface LocalKupmiosFraudProofRawSourceV1 {
  readonly sourceVersion: typeof LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE_V1;
  readonly sourceId: string;
  readonly kupoHttpUrl: string;
  readonly ogmiosWebSocketUrl: string;
  readBoundary(): Promise<unknown>;
  /** Exact ordered raw block capture for independently authenticated readers. */
  readBlockAtPoint(input: {
    readonly point: FraudProofRawL1PointV1;
  }): Promise<unknown>;
  scanAddressPage(input: {
    readonly address: string;
    readonly throughPoint: FraudProofRawL1PointV1;
    readonly after: string | null;
  }): Promise<unknown>;
  scanUnitHistoryPage(input: {
    readonly unit: string;
    readonly fromGenesis: true;
    readonly throughPoint: FraudProofRawL1PointV1;
    readonly after: string | null;
  }): Promise<unknown>;
  readTransaction(input: {
    readonly txHash: string;
    readonly expectedInclusionPoint: FraudProofRawL1PointV1;
  }): Promise<unknown>;
  confirmCanonicalPoint(input: {
    readonly point: FraudProofRawL1PointV1;
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
  left: FraudProofRawL1PointV1,
  right: FraudProofRawL1PointV1,
): boolean =>
  left.slot === right.slot &&
  left.blockNo === right.blockNo &&
  left.blockHash === right.blockHash &&
  left.pointId === right.pointId;

const parseBoundary = (
  value: unknown,
): {
  readonly kupoCheckpoint: FraudProofRawL1PointV1;
  readonly ogmiosTip: FraudProofRawL1PointV1;
} => {
  const parsed = exact(
    value,
    ["kupoCheckpoint", "ogmiosTip"],
    "Kupmios boundary",
  );
  return {
    kupoCheckpoint: admitFraudProofRawL1PointV1(
      parsed.kupoCheckpoint,
      "Kupmios Kupo checkpoint",
    ),
    ogmiosTip: admitFraudProofRawL1PointV1(
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
  readonly source: LocalKupmiosFraudProofRawSourceV1;
  readonly address: string;
  readonly throughPoint: FraudProofRawL1PointV1;
}): Promise<readonly FraudProofRawL1UtxoV1[]> => {
  const result: FraudProofRawL1UtxoV1[] = [];
  const seenCursors = new Set<string>();
  let after: string | null = null;
  for (let pageIndex = 0; pageIndex < MAX_PAGE_COUNT; pageIndex += 1) {
    const label = `Kupo address page ${pageIndex.toString()}`;
    const parsed = exact(
      await source.scanAddressPage({ address, throughPoint, after }),
      ["checkpoint", "utxos", "nextCursor", "complete"],
      label,
    );
    const checkpoint = admitFraudProofRawL1PointV1(
      parsed.checkpoint,
      `${label}.checkpoint`,
    );
    if (!samePoint(checkpoint, throughPoint)) {
      throw new Error(`${label} changed the pinned Kupo checkpoint`);
    }
    result.push(
      ...array(parsed.utxos, `${label}.utxos`).map((entry, index) =>
        admitFraudProofRawL1UtxoV1(
          entry,
          `${label}.utxos[${index.toString()}]`,
        ),
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
  readonly inclusionPoint: FraudProofRawL1PointV1;
};

const scanCompleteUnitHistory = async ({
  source,
  unit,
  throughPoint,
}: {
  readonly source: LocalKupmiosFraudProofRawSourceV1;
  readonly unit: string;
  readonly throughPoint: FraudProofRawL1PointV1;
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
    const checkpoint = admitFraudProofRawL1PointV1(
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
            inclusionPoint: admitFraudProofRawL1PointV1(
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
  readonly source: LocalKupmiosFraudProofRawSourceV1;
  readonly expected: UnitHistoryTransaction;
}): Promise<FraudProofRawL1TransactionV1> => {
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
  const kupoPoint = admitFraudProofRawL1PointV1(
    kupo.inclusionPoint,
    "Kupo transaction inclusion point",
  );
  const ogmios = record(parsed.ogmios, `Ogmios transaction ${expected.txHash}`);
  const ogmiosTxHash = txHash(ogmios.txHash, "Ogmios transaction hash");
  const ogmiosPoint = admitFraudProofRawL1PointV1(
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
  return parsed.ogmios as FraudProofRawL1TransactionV1;
};

const confirmPinnedPoint = async ({
  source,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSourceV1;
  readonly point: FraudProofRawL1PointV1;
}): Promise<void> => {
  const parsed = exact(
    await source.confirmCanonicalPoint({ point }),
    ["canonical", "point"],
    "Kupmios canonical-point confirmation",
  );
  const confirmed = admitFraudProofRawL1PointV1(
    parsed.point,
    "Kupmios confirmed point",
  );
  if (parsed.canonical !== true || !samePoint(confirmed, point)) {
    throw new Error("pinned Kupo point rolled back during snapshot capture");
  }
};

export const createLocalKupmiosFraudProofRawL1SnapshotAuthorityV1 = ({
  source,
  releaseFinality,
}: {
  readonly source: LocalKupmiosFraudProofRawSourceV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
}): FraudProofRawL1SnapshotAuthorityV1 => {
  if (source.sourceVersion !== LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE_V1) {
    throw new Error("local Kupmios raw source has an unsupported version");
  }
  const sourceId = nonEmptyString(source.sourceId, "local Kupmios sourceId");
  assertLoopback(source.kupoHttpUrl, "Kupo URL");
  assertLoopback(source.ogmiosWebSocketUrl, "Ogmios URL");
  return {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1,
    capture: async (request: FraudProofRawL1SnapshotRequestV1) => {
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
      const inclusionByHash = new Map<string, FraudProofRawL1PointV1>();
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
        schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_V1_SCHEMA_VERSION,
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
          rollbackCursor: computeFraudProofRawL1RollbackCursorV1({
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
      return admitFraudProofRawL1SnapshotV1({
        value: snapshot,
        request,
        releaseFinality,
      });
    },
  };
};
