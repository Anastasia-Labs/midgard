import "./local-kupmios-http-ogmios-source.acquire-ogmios-session.js";

import { CML } from "@lucid-evolution/lucid";

import { sameRawPoint } from "./local-kupmios-http-ogmios-source.open-ogmios-session.js";
import {
  cbor,
  digest,
  exactKeys,
  naturalNumber,
  record,
} from "./local-kupmios-http-ogmios-source.parse-ogmios-block.js";
import {
  admittedHttpOgmiosSourceDetails,
  admittedHttpOgmiosSources,
  admittedPredecessorReaders,
  admittedReferenceBodyReaders,
  LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
  type LocalKupmiosAdmittedBoundary,
  type LocalKupmiosAdmittedPredecessorPoint,
  type LocalKupmiosRawBlockAtPoint,
  type LocalKupmiosReferenceBodiesAtPoint,
  MAX_MATCHES,
  OGMIOS_RAW_TRANSACTION_CBOR_FLAG,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import {
  LocalKupmiosCheckpointChangedError,
  type LocalKupmiosFraudProofRawSource,
} from "./local-kupmios-raw-l1-authority.js";
import {
  admitFraudProofRawL1Point,
  type FraudProofL1ObservationDepth,
  type FraudProofRawL1Point,
} from "./raw-l1-snapshot.js";

export const requireOgmiosRawTransactionCbor = ({
  value,
  expectedTxHash,
  label,
}: {
  readonly value: unknown;
  readonly expectedTxHash: string;
  readonly label: string;
}): string => {
  const parsed = record(value, label);
  const reported = digest(parsed.id, `${label}.id`);
  if (reported !== expectedTxHash) {
    throw new Error(`${label}.id disagrees with the requested transaction`);
  }
  if (!("cbor" in parsed)) {
    throw new Error(
      `${label}.cbor is missing; Ogmios must run with ${OGMIOS_RAW_TRANSACTION_CBOR_FLAG}`,
    );
  }
  const transactionCbor = cbor(parsed.cbor, `${label}.cbor`);
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  } catch (cause) {
    throw new Error(
      `${label}.cbor is not a Cardano transaction: ${String(cause)}`,
    );
  }
  const body = transaction.body();
  const bodyHash = CML.hash_transaction(body);
  try {
    if (bodyHash.to_hex() !== expectedTxHash) {
      throw new Error(`${label}.cbor hashes to a different transaction`);
    }
    return transactionCbor;
  } finally {
    bodyHash.free();
    body.free();
    transaction.free();
  }
};

const admitLocalKupmiosRawBlockAtPoint = ({
  value,
  source,
  requestedPoint,
}: {
  readonly value: unknown;
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly requestedPoint: FraudProofRawL1Point;
}): LocalKupmiosRawBlockAtPoint => {
  const parsed = exactKeys(
    value,
    [
      "schemaVersion",
      "sourceId",
      "point",
      "parentBlockHash",
      "kupoCheckpoint",
      "transactions",
    ],
    [],
    "local Kupmios raw block",
  );
  if (parsed.schemaVersion !== LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT) {
    throw new Error("local Kupmios raw block schema changed");
  }
  if (parsed.sourceId !== source.sourceId) {
    throw new Error("local Kupmios raw block changed its admitted source");
  }
  const point = admitFraudProofRawL1Point(
    parsed.point,
    "local Kupmios raw block point",
  );
  const parentBlockHash =
    parsed.parentBlockHash === null
      ? null
      : digest(parsed.parentBlockHash, "local Kupmios raw block parent hash");
  if (!sameRawPoint(point, requestedPoint)) {
    throw new Error("local Kupmios raw block changed the requested point");
  }
  const checkpoint = exactKeys(
    parsed.kupoCheckpoint,
    ["slot", "blockHash"],
    [],
    "local Kupmios raw block Kupo checkpoint",
  );
  const kupoCheckpoint = Object.freeze({
    slot: naturalNumber(
      checkpoint.slot,
      "local Kupmios raw block Kupo checkpoint slot",
    ),
    blockHash: digest(
      checkpoint.blockHash,
      "local Kupmios raw block Kupo checkpoint hash",
    ),
  });
  if (
    kupoCheckpoint.slot !== Number(point.slot) ||
    kupoCheckpoint.blockHash !== point.blockHash
  ) {
    throw new Error(
      "local Kupmios raw block Kupo checkpoint differs from Ogmios point",
    );
  }
  if (
    !Array.isArray(parsed.transactions) ||
    parsed.transactions.length > MAX_MATCHES
  ) {
    throw new Error("local Kupmios raw block transactions are not bounded");
  }
  const transactions = Object.freeze(
    parsed.transactions.map((value, index) => {
      const transaction = exactKeys(
        value,
        ["txHash", "transactionCbor"],
        [],
        `local Kupmios raw block transaction ${index.toString()}`,
      );
      const transactionHash = digest(
        transaction.txHash,
        `local Kupmios raw block transaction ${index.toString()} hash`,
      );
      return Object.freeze({
        txHash: transactionHash,
        transactionCbor: requireOgmiosRawTransactionCbor({
          value: {
            id: transactionHash,
            cbor: transaction.transactionCbor,
          },
          expectedTxHash: transactionHash,
          label: `local Kupmios raw block transaction ${index.toString()}`,
        }),
      });
    }),
  );
  if (
    new Set(transactions.map(({ txHash }) => txHash)).size !==
    transactions.length
  ) {
    throw new Error(
      "local Kupmios raw block contains duplicate transaction ids",
    );
  }
  return Object.freeze({
    schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
    sourceId: source.sourceId,
    point,
    parentBlockHash,
    kupoCheckpoint,
    transactions,
  });
};

/**
 * Reads an exact raw block only from a source minted by the concrete loopback
 * HTTP/WS constructor, then independently re-admits its point and every ordered
 * transaction CBOR. Structural test doubles cannot cross this boundary.
 */
export const readAdmittedLocalKupmiosRawBlockAtPoint = async ({
  source,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<LocalKupmiosRawBlockAtPoint> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "exact raw block read requires the admitted local Kupo/Ogmios source",
    );
  }
  const requestedPoint = admitFraudProofRawL1Point(
    point,
    "requested local Kupmios raw block point",
  );
  return admitLocalKupmiosRawBlockAtPoint({
    value: await source.readBlockAtPoint({ point: requestedPoint }),
    source,
    requestedPoint,
  });
};

/** Reads a direct predecessor through the concrete source's captured readers. */
export const readAdmittedLocalKupmiosPredecessorPoint = async ({
  source,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<LocalKupmiosAdmittedPredecessorPoint> => {
  const read = admittedPredecessorReaders.get(source);
  if (read === undefined) {
    throw new Error(
      "predecessor point read requires the admitted local Kupo/Ogmios source",
    );
  }
  return await read(
    admitFraudProofRawL1Point(point, "requested local Kupmios child point"),
  );
};

/** Captured exact-target reference preimages; no resolved-input or native handle. */
export const readAdmittedLocalKupmiosReferenceBodiesAtPoint = async ({
  source,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<LocalKupmiosReferenceBodiesAtPoint> => {
  const read = admittedReferenceBodyReaders.get(source);
  if (read === undefined)
    throw new Error(
      "reference bodies require the admitted local Kupo/Ogmios source",
    );
  return await read(
    admitFraudProofRawL1Point(point, "requested reference target point"),
  );
};

/**
 * A boundary capture pins Kupo's head at its first response. Kupo indexing a
 * block while the finality bracket is still being searched changes that head,
 * and the source reports the change as a typed checkpoint change instead of a
 * boundary. That is ordinary chain progress, not divergence, and readBoundary
 * discards every cache of the abandoned capture, so the read starts over on a
 * fresh pin: at most this many complete captures, as signed recovery allows.
 */
const BOUNDARY_CAPTURE_ATTEMPTS = 3;

const captureBoundary = async (
  source: LocalKupmiosFraudProofRawSource,
  observationDepth: FraudProofL1ObservationDepth,
): Promise<Awaited<ReturnType<typeof source.readBoundary>>> => {
  for (let attempt = 1; ; attempt += 1) {
    try {
      return await source.readBoundary({ observationDepth });
    } catch (cause) {
      if (
        !(cause instanceof LocalKupmiosCheckpointChangedError) ||
        attempt >= BOUNDARY_CAPTURE_ATTEMPTS
      )
        throw cause;
    }
  }
};

/** Authenticates a fresh boundary; stable evidence remains the default. */
export const readAdmittedLocalKupmiosBoundary = async ({
  source,
  observationDepth = "release_finality",
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly observationDepth?: FraudProofL1ObservationDepth;
}): Promise<LocalKupmiosAdmittedBoundary> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "release boundary read requires the admitted local Kupo/Ogmios source",
    );
  }
  const value = exactKeys(
    await captureBoundary(source, observationDepth),
    ["kupoCheckpoint", "ogmiosTip"],
    [],
    "local Kupmios release boundary",
  );
  const kupoCheckpoint = admitFraudProofRawL1Point(
    value.kupoCheckpoint,
    "local Kupmios release Kupo checkpoint",
  );
  const ogmiosTip = admitFraudProofRawL1Point(
    value.ogmiosTip,
    "local Kupmios release Ogmios tip",
  );
  const confirmationDepth =
    Number(ogmiosTip.blockNo) - Number(kupoCheckpoint.blockNo) + 1;
  const details = admittedHttpOgmiosSourceDetails.get(source)!;
  if (
    !Number.isSafeInteger(confirmationDepth) ||
    confirmationDepth <
      (observationDepth === "inclusion" ? 1 : details.confirmationDepth) ||
    confirmationDepth > details.automaticRecoveryMaxDepth
  ) {
    throw new Error("local Kupmios boundary is outside release finality");
  }
  return Object.freeze({ kupoCheckpoint, ogmiosTip, confirmationDepth });
};
