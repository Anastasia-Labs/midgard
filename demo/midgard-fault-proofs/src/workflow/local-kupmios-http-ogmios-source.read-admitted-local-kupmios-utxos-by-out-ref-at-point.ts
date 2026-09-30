import { CML } from "@lucid-evolution/lucid";

import { sameRawPoint } from "./local-kupmios-http-ogmios-source.open-ogmios-session.js";
import {
  digest,
  exactKeys,
} from "./local-kupmios-http-ogmios-source.parse-ogmios-block.js";
import {
  admittedHistoricalPageReaders,
  admittedHttpOgmiosSourceDetails,
  admittedHttpOgmiosSources,
  type LocalKupmiosAdmittedUnitHistory,
  MAX_MATCHES,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import { type LocalKupmiosFraudProofRawSource } from "./local-kupmios-raw-l1-authority.js";
import {
  admitFraudProofRawL1Point,
  admitFraudProofRawL1Utxo,
  type FraudProofRawL1Point,
  type FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";

/** Reads complete unit history at the active or an admitted historical point. */
export const readAdmittedLocalKupmiosUnitHistoryAtPoint = async ({
  source,
  unit,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly unit: string;
  readonly point: FraudProofRawL1Point;
}): Promise<LocalKupmiosAdmittedUnitHistory> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "unit-history read requires the admitted local Kupo/Ogmios source",
    );
  }
  if (!/^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(unit)) {
    throw new Error("unit-history read requires a canonical Cardano unit");
  }
  const checkpoint = admitFraudProofRawL1Point(
    point,
    "requested local Kupmios unit-history point",
  );
  const page = exactKeys(
    await admittedHistoricalPageReaders.get(source)!.history({
      unit,
      fromGenesis: true,
      throughPoint: checkpoint,
      after: null,
    }),
    ["checkpoint", "transactions", "nextCursor", "complete"],
    [],
    "local Kupmios unit-history page",
  );
  const returnedCheckpoint = admitFraudProofRawL1Point(
    page.checkpoint,
    "local Kupmios unit-history checkpoint",
  );
  if (
    returnedCheckpoint.pointId !== checkpoint.pointId ||
    page.nextCursor !== null ||
    page.complete !== true ||
    !Array.isArray(page.transactions)
  ) {
    throw new Error("local Kupmios unit history is incomplete or substituted");
  }
  const details = admittedHttpOgmiosSourceDetails.get(source)!;
  if (page.transactions.length > details.automaticRecoveryMaxDepth) {
    throw new Error("local Kupmios unit history exceeds its release bound");
  }
  const transactions = page.transactions.map((entry, index) => {
    const parsed = exactKeys(
      entry,
      ["txHash", "inclusionPoint"],
      [],
      `local Kupmios unit-history transaction ${index.toString()}`,
    );
    const txHash = digest(
      parsed.txHash,
      `local Kupmios unit-history transaction ${index.toString()} hash`,
    );
    const inclusionPoint = admitFraudProofRawL1Point(
      parsed.inclusionPoint,
      `local Kupmios unit-history transaction ${index.toString()} point`,
    );
    if (Number(inclusionPoint.blockNo) > Number(checkpoint.blockNo)) {
      throw new Error("local Kupmios unit history crosses its checkpoint");
    }
    return Object.freeze({ txHash, inclusionPoint });
  });
  if (
    new Set(transactions.map(({ txHash }) => txHash)).size !==
    transactions.length
  ) {
    throw new Error(
      "local Kupmios unit history contains duplicate transactions",
    );
  }
  return Object.freeze({
    checkpoint: returnedCheckpoint,
    transactions: Object.freeze(transactions),
  });
};

/**
 * Reads the exact unspent address set at one admitted historical point from
 * the same concrete local Kupo/Ogmios source. This is the compaction anchor
 * for restart recovery; caller-authored UTxO snapshots cannot cross it.
 */
export const readAdmittedLocalKupmiosAddressUtxosAtPoint = async ({
  source,
  address,
  point,
}: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly address: string;
  readonly point: FraudProofRawL1Point;
}): Promise<readonly FraudProofRawL1Utxo[]> => {
  if (!admittedHttpOgmiosSources.has(source)) {
    throw new Error(
      "exact address snapshot requires the admitted local Kupo/Ogmios source",
    );
  }
  const canonicalAddress = CML.Address.from_bech32(address).to_bech32();
  if (canonicalAddress !== address) {
    throw new Error("exact address snapshot requires a canonical address");
  }
  const throughPoint = admitFraudProofRawL1Point(
    point,
    "requested local Kupmios address point",
  );
  const page = exactKeys(
    await admittedHistoricalPageReaders.get(source)!.address({
      address,
      throughPoint,
      after: null,
    }),
    ["checkpoint", "utxos", "nextCursor", "complete"],
    [],
    "local Kupmios exact address snapshot",
  );
  if (
    !sameRawPoint(
      admitFraudProofRawL1Point(
        page.checkpoint,
        "local Kupmios address checkpoint",
      ),
      throughPoint,
    ) ||
    page.nextCursor !== null ||
    page.complete !== true ||
    !Array.isArray(page.utxos) ||
    page.utxos.length > MAX_MATCHES
  ) {
    throw new Error("local Kupmios address snapshot is incomplete");
  }
  const utxos = page.utxos.map((value, index) =>
    admitFraudProofRawL1Utxo(
      value,
      `local Kupmios address UTxO ${index.toString()}`,
    ),
  );
  if (new Set(utxos.map(({ outRef }) => outRef)).size !== utxos.length) {
    throw new Error(
      "local Kupmios address snapshot contains duplicate outrefs",
    );
  }
  return Object.freeze(utxos);
};

/**
 * An exact outref Kupo reports spent at or below the read point, kept only
 * when its raw consuming transaction passes {@link transactionConsumesOutRef}.
 */
export type LocalKupmiosVerifiedSpend = Readonly<{
  outRef: string;
  spendingTxHash: string;
  spendPoint: FraudProofRawL1Point;
}>;

export type LocalKupmiosOutRefsAtPoint = Readonly<{
  /** The requested outrefs still unspent at the point. */
  outputs: readonly FraudProofRawL1Utxo[];
  /** The requested outrefs spent at or below the point, verified. */
  spends: readonly LocalKupmiosVerifiedSpend[];
}>;

/**
 * Exact resolved transaction read from the same concrete loopback source as
 * the admitted raw-block path. Both provider claims and every resolved input
 * are re-admitted before the result crosses the package boundary. An outref
 * reported in neither list is missing with no verified spend.
 */
export const readAdmittedLocalKupmiosUtxosByOutRefAtPoint = async (input: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
  readonly outRefs: readonly string[];
}): Promise<LocalKupmiosOutRefsAtPoint> => {
  if (
    !admittedHttpOgmiosSources.has(input.source) ||
    input.source.readOutRefsAtPoint === undefined
  ) {
    throw new Error(
      "Exact outref reads require admitted local Kupo/Ogmios authority",
    );
  }
  const point = admitFraudProofRawL1Point(
    input.point,
    "exact outref checkpoint",
  );
  if (
    input.outRefs.length > MAX_MATCHES ||
    new Set(input.outRefs).size !== input.outRefs.length ||
    input.outRefs.some((ref) => !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(ref))
  )
    throw new Error("Invalid exact outref request");
  const value = exactKeys(
    await input.source.readOutRefsAtPoint({
      point,
      outRefs: input.outRefs,
    }),
    ["outputs", "spends"],
    [],
    "exact outref read",
  );
  if (
    !Array.isArray(value.outputs) ||
    !Array.isArray(value.spends) ||
    value.outputs.length + value.spends.length > input.outRefs.length
  )
    throw new Error("Exact outref source returned an invalid set");
  const outputs = value.outputs.map((output, index) =>
    admitFraudProofRawL1Utxo(output, `exact outref ${index}`),
  );
  const spends = value.spends.map((entry, index): LocalKupmiosVerifiedSpend => {
    const spend = exactKeys(
      entry,
      ["outRef", "spendingTxHash", "spendPoint"],
      [],
      `exact outref spend ${index}`,
    );
    const spendPoint = admitFraudProofRawL1Point(
      spend.spendPoint,
      `exact outref spend ${index} point`,
    );
    if (
      typeof spend.outRef !== "string" ||
      typeof spend.spendingTxHash !== "string" ||
      !/^[0-9a-f]{64}$/u.test(spend.spendingTxHash) ||
      BigInt(spendPoint.slot) > BigInt(point.slot) ||
      BigInt(spendPoint.blockNo) > BigInt(point.blockNo)
    )
      throw new Error("Exact outref source returned an invalid spend");
    return Object.freeze({
      outRef: spend.outRef,
      spendingTxHash: spend.spendingTxHash,
      spendPoint,
    });
  });
  const reported = [...outputs, ...spends].map(({ outRef }) => outRef);
  if (
    new Set(reported).size !== reported.length ||
    reported.some((outRef) => !input.outRefs.includes(outRef))
  ) {
    throw new Error("Exact outref source substituted the requested set");
  }
  return Object.freeze({
    outputs: Object.freeze(outputs),
    spends: Object.freeze(spends),
  });
};

export const pinAdmittedLocalKupmiosBoundaryAtPoint = async (input: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly point: FraudProofRawL1Point;
}): Promise<void> => {
  if (
    !admittedHttpOgmiosSources.has(input.source) ||
    input.source.pinBoundaryAtPoint === undefined
  ) {
    throw new Error(
      "Boundary pinning requires admitted local Kupo/Ogmios authority",
    );
  }
  const point = admitFraudProofRawL1Point(
    input.point,
    "requested exact boundary",
  );
  const returned = admitFraudProofRawL1Point(
    await input.source.pinBoundaryAtPoint({ point }),
    "pinned exact boundary",
  );
  if (!sameRawPoint(point, returned))
    throw new Error("Local source substituted the requested boundary");
};

export const readAdmittedLocalKupmiosTransactionInclusion = async (input: {
  readonly source: LocalKupmiosFraudProofRawSource;
  readonly txHash: string;
}): Promise<FraudProofRawL1Point | null> => {
  if (
    !admittedHttpOgmiosSources.has(input.source) ||
    input.source.resolveTransactionInclusion === undefined
  ) {
    throw new Error(
      "Transaction inclusion requires admitted local Kupo/Ogmios history",
    );
  }
  const value = await input.source.resolveTransactionInclusion({
    txHash: digest(input.txHash, "requested inclusion transaction"),
  });
  return value === null
    ? null
    : admitFraudProofRawL1Point(value, "local transaction inclusion");
};
