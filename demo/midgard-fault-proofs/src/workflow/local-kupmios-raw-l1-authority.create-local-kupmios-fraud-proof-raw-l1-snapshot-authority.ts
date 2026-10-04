import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import {
  array,
  assertLoopback,
  exact,
  LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE,
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
  MAX_PAGE_COUNT,
  nonEmptyString,
  parseBoundary,
  parsePageTail,
  record,
  samePoint,
  scanAllAddressUtxos,
  settleLocalKupmiosReads,
  txHash,
  type UnitHistoryTransaction,
  withLocalKupmiosSourceCapture,
} from "./local-kupmios-raw-l1-authority.scan-all-address-utxos.js";
import { withLocalKupmiosReadOperation } from "./local-kupmios-read-operation.js";
import {
  admitFraudProofRawL1Point,
  admitFraudProofRawL1Snapshot,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  type FraudProofL1ObservationDepth,
  type FraudProofRawL1Point,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1SnapshotRequest,
  type FraudProofRawL1Transaction,
} from "./raw-l1-snapshot.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";

export interface LocalKupmiosFraudProofRawL1SnapshotAuthority
  extends FraudProofRawL1SnapshotAuthority {
  capture(
    request: FraudProofRawL1SnapshotRequest,
    options?: Readonly<{ scope: DaAvailabilityReadScope }>,
  ): Promise<unknown>;
}

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
  if (typeof parsed.canonical !== "boolean") {
    throw new Error(
      "Kupmios canonical-point confirmation has an invalid verdict",
    );
  }
  if (!parsed.canonical) {
    throw new Error("pinned Kupo point rolled back during snapshot capture");
  }
  if (!samePoint(confirmed, point)) {
    throw new Error(
      "Kupmios canonical-point confirmation substituted the pinned point",
    );
  }
};

export const createLocalKupmiosFraudProofRawL1SnapshotAuthority = ({
  source,
  releaseFinality,
  observationDepth = "release_finality",
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
  readonly observationDepth?: FraudProofL1ObservationDepth;
}): LocalKupmiosFraudProofRawL1SnapshotAuthority => {
  if (source.sourceVersion !== LOCAL_KUPMIOS_FRAUD_PROOF_RAW_SOURCE) {
    throw new Error("local Kupmios raw source has an unsupported version");
  }
  const sourceId = nonEmptyString(source.sourceId, "local Kupmios sourceId");
  assertLoopback(source.kupoHttpUrl, "Kupo URL");
  assertLoopback(source.ogmiosWebSocketUrl, "Ogmios URL");
  const captureOnce = async (request: FraudProofRawL1SnapshotRequest) => {
    const boundary = parseBoundary(
      await source.readBoundary({ observationDepth }),
    );
    const scopes = await settleLocalKupmiosReads(
      request.scopes.map(async (scope) => ({
        ...scope,
        utxos: await scanAllAddressUtxos({
          source,
          address: scope.address,
          throughPoint: boundary.kupoCheckpoint,
        }),
      })),
    );
    const histories = await settleLocalKupmiosReads(
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
    const transactions = await settleLocalKupmiosReads(
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
      blueprintHash: request.blueprintHash,
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
          blueprintHash: request.blueprintHash,
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
      observationDepth,
    });
  };
  return {
    authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
    capture: (
      request: FraudProofRawL1SnapshotRequest,
      options?: Readonly<{ scope: DaAvailabilityReadScope }>,
    ) => {
      // Share the existing two checkpoint retries across transport retries too;
      // no nested retry resets this allowance or the operation deadline.
      let checkpointRetries = 0;
      const capture = async () => {
        // Each readBoundary discards source caches and pins a fresh authenticated
        // boundary. Failed attempt data never escapes this exclusive capture.
        for (;;) {
          try {
            return await captureOnce(request);
          } catch (error) {
            if (
              !(error instanceof LocalKupmiosCheckpointChangedError) ||
              checkpointRetries >= 2
            )
              throw error;
            checkpointRetries += 1;
          }
        }
      };
      return options === undefined
        ? withLocalKupmiosSourceCapture(source, capture)
        : withLocalKupmiosReadOperation(
            source,
            async (attempt) => {
              attempt.assertCurrent();
              const result = await capture();
              attempt.assertCurrent();
              return result;
            },
            options,
          );
    },
  };
};
