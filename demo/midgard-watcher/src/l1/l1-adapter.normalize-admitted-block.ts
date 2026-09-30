import "./l1-adapter.exact-tls-endpoint.js";

import { deriveTransaction } from "./l1-adapter.derive-transaction.js";
import { contentJson } from "./l1-adapter.establish-watcher-local-node-query-transport.js";
import {
  digestCanonicalJson,
  exactLiteral,
  exactNatural,
  exactRecord,
  exactString,
  fail,
  type ParseBudget,
  preflightTransactionCollections,
  watcherL1TransportAttestationDetails,
} from "./l1-adapter.exact-array.js";
import {
  parseTransaction,
  providerJson,
} from "./l1-adapter.parse-authenticated-provider.js";
import { publicBytesFromCbor } from "./l1-adapter.parse-utxo.js";
import {
  HEX_32,
  NETWORKS,
  normalizationSessionStates,
  normalizedBlockProvenance,
  PROVIDER_ID,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  WATCHER_NORMALIZED_L1_BLOCK_SCHEMA_VERSION,
  type WatcherL1Network,
  type WatcherL1NormalizationSession,
  type WatcherL1NormalizationSessionState,
  type WatcherL1TransportAttestationContext,
  type WatcherL1TransportAttestationDetails,
  type WatcherNormalizedAuthenticatedL1Provider,
  type WatcherNormalizedL1Block,
} from "./l1-adapter.watcher-local-node-query-transport.js";
import {
  readWatcherLocalHistoricalCapture,
  type WatcherLocalHistoricalCaptureReceipt,
} from "./local-historical-capture.js";

// Only this module supplies the admitted provider and private session state.
export const normalizeAdmittedBlock = (
  provider: WatcherNormalizedAuthenticatedL1Provider,
  observationInput: unknown,
  session?: WatcherL1NormalizationSessionState,
): WatcherNormalizedL1Block => {
  const observation = exactRecord(observationInput, "$", [
    "schemaVersion",
    "network",
    "providerId",
    "chainPoint",
    "transactions",
  ]);
  if (
    observation.schemaVersion !== WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION
  ) {
    fail("unsupported_schema", "$.schemaVersion");
  }
  const network = exactLiteral(observation.network, "$.network", NETWORKS);
  if (network !== provider.network) {
    fail("network_mismatch", "$.network");
  }
  const providerId = exactString(
    observation.providerId,
    "$.providerId",
    PROVIDER_ID,
  );
  if (providerId !== provider.providerId) {
    fail("provider_mismatch", "$.providerId");
  }
  const point = exactRecord(observation.chainPoint, "$.chainPoint", [
    "blockHash",
    "parentBlockHash",
    "slot",
    "blockNo",
    "depth",
  ]);
  const blockHash = exactString(
    point.blockHash,
    "$.chainPoint.blockHash",
    HEX_32,
  );
  const parentBlockHash =
    point.parentBlockHash === null
      ? null
      : exactString(
          point.parentBlockHash,
          "$.chainPoint.parentBlockHash",
          HEX_32,
        );
  const slot = exactNatural(point.slot, "$.chainPoint.slot");
  const blockNo = exactNatural(point.blockNo, "$.chainPoint.blockNo");
  const depth = exactNatural(point.depth, "$.chainPoint.depth");
  const budget: ParseBudget = { collectionMembers: 0, publicBytes: 0 };
  const transactionInputs = preflightTransactionCollections(
    observation.transactions,
    budget,
  );
  const transactions = transactionInputs.map((transaction, index) =>
    parseTransaction(
      transaction,
      `$.transactions[${index.toString()}]`,
      budget,
      session,
    ),
  );
  const transactionHashes = new Set<string>();
  for (let index = 0; index < transactions.length; index += 1) {
    const transaction = transactions[index]!;
    if (transactionHashes.has(transaction.txHash)) {
      fail("duplicate_identity", `$.transactions[${index.toString()}]`);
    }
    if (
      transaction.transactionIndex !== undefined &&
      transaction.transactionIndex !== index.toString()
    ) {
      fail(
        "identity_mismatch",
        `$.transactions[${index.toString()}].transactionIndex`,
      );
    }
    transactionHashes.add(transaction.txHash);
  }
  Object.freeze(transactions);
  const pointDigest = digestCanonicalJson({
    network,
    blockHash,
    parentBlockHash,
    slot,
    blockNo,
  });
  const chainPointId = digestCanonicalJson({
    pointDigest,
    depth,
    provider: providerJson(provider),
  });
  const chainPoint = Object.freeze({
    chainPointId,
    pointDigest,
    blockHash,
    parentBlockHash,
    slot,
    blockNo,
    depth,
  });
  const blockContentDigest = digestCanonicalJson(
    contentJson({
      network,
      pointDigest,
      blockHash,
      parentBlockHash,
      slot,
      blockNo,
      transactions,
    }),
  );
  const observationDigest = digestCanonicalJson({
    schemaVersion: WATCHER_NORMALIZED_L1_BLOCK_SCHEMA_VERSION,
    provider: providerJson(provider),
    chainPoint,
    blockContentDigest,
  });
  const normalized = Object.freeze({
    schemaVersion: WATCHER_NORMALIZED_L1_BLOCK_SCHEMA_VERSION,
    network,
    provider,
    chainPoint,
    transactions,
    blockContentDigest,
    observationDigest,
  });
  return normalized;
};

export const normalizeWatcherL1Block = (
  transportAttestationContext: WatcherL1TransportAttestationContext,
  observationInput: unknown,
  normalizationSession?: WatcherL1NormalizationSession,
): WatcherNormalizedL1Block => {
  /*
   * Trust boundary: the opaque context proves the configured live transport
   * location and peer identity; it does not claim that this arbitrary JS value
   * was itself read from the socket. The watcher-owned transport adapter must
   * call this function only with bytes it decoded from that connection. This is
   * an in-process capability boundary, not a remotely callable receipt API:
   * untrusted serialized callers cannot construct the WeakMap-backed context,
   * and detached/closed contexts fail below. A future wire adapter may issue
   * per-frame receipts once its framing protocol is fixed, but manufacturing a
   * receipt here would falsely attest provenance that this layer cannot see.
   */
  const session =
    normalizationSession === undefined
      ? undefined
      : (normalizationSessionStates.get(normalizationSession) ??
        fail("invalid_field", "$.normalizationSession"));
  const attestation = watcherL1TransportAttestationDetails(
    transportAttestationContext,
  );
  if (attestation === null) {
    fail("invalid_field", "$.transportAttestationContext");
  }
  const provider = (attestation as WatcherL1TransportAttestationDetails)
    .provider;
  const normalized = normalizeAdmittedBlock(
    provider,
    observationInput,
    session,
  );
  normalizedBlockProvenance.set(
    normalized,
    transportAttestationContext as WatcherL1TransportAttestationContext,
  );
  return normalized;
};

/**
 * Builds a normalized observation from exact full transaction bytes read by
 * a watcher-owned transport adapter. Full/body/witness encodings are retained;
 * output and witness views are canonical projections derived locally. All
 * fields pass through the ordinary strict normalizer; callers cannot supply
 * hashes, witnesses, outputs, datums, scripts or redeemers independently of
 * the transaction bytes.
 */
export const normalizeWatcherL1BlockFromTransactionCbors = (
  transportAttestationContext: WatcherL1TransportAttestationContext,
  input: Readonly<{
    network: WatcherL1Network;
    chainPoint: Readonly<{
      blockHash: string;
      parentBlockHash: string | null;
      slot: string;
      blockNo: string;
      depth: string;
    }>;
    transactionCbors: readonly string[];
  }>,
  normalizationSession?: WatcherL1NormalizationSession,
): WatcherNormalizedL1Block => {
  const attestation = watcherL1TransportAttestationDetails(
    transportAttestationContext,
  );
  if (attestation === null) {
    fail("invalid_field", "$.transportAttestationContext");
  }
  const providerId = (attestation as WatcherL1TransportAttestationDetails)
    .provider.providerId;
  const transactions = observationsFromTransactionCbors(input.transactionCbors);
  return normalizeWatcherL1Block(
    transportAttestationContext,
    {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
      network: input.network,
      providerId,
      chainPoint: input.chainPoint,
      transactions,
    },
    normalizationSession,
  );
};

export const observationsFromTransactionCbors = (
  transactionCbors: readonly string[],
) => {
  return transactionCbors.map((cborHex, index) => {
    const fullTransaction = publicBytesFromCbor(cborHex);
    const derived = deriveTransaction(
      fullTransaction,
      `$.transactionCbors[${index.toString()}]`,
      undefined,
    );
    return Object.freeze({
      txHash: derived.txHash,
      transactionIndex: index.toString(),
      fullTransaction: derived.fullTransaction,
      body: derived.body,
      witnessSet: derived.witnessSet,
      utxos: derived.utxos,
      scripts: derived.scripts,
      datums: derived.datums,
      redeemers: derived.redeemers,
    });
  });
};

export const localBackfillObservationBrand = Symbol(
  "local-backfill-observation",
);

export type WatcherLocalBackfillObservationReceipt = Readonly<{
  [localBackfillObservationBrand]: true;
}>;

type LocalBackfillObservationRead = Readonly<{
  capture: ReturnType<typeof readWatcherLocalHistoricalCapture>;
  native: WatcherNormalizedL1Block;
  ogmios: WatcherNormalizedL1Block;
  kupo: WatcherNormalizedL1Block;
  sourceIdentityDigest: string;
  acquisitionDigest: string;
}>;

export const localBackfillObservations = new WeakMap<
  WatcherLocalBackfillObservationReceipt,
  Readonly<{
    capture: WatcherLocalHistoricalCaptureReceipt;
    value: LocalBackfillObservationRead;
  }>
>();

export const localBackfillCaptures: WeakMap<
  WatcherLocalHistoricalCaptureReceipt,
  WatcherLocalBackfillObservationReceipt
> = new WeakMap();

/** Returns descriptive immutable data only while the concrete capture is live. */
export const readWatcherLocalBackfillObservation = (
  receipt: WatcherLocalBackfillObservationReceipt,
): LocalBackfillObservationRead => {
  const owner = localBackfillObservations.get(receipt);
  if (
    owner === undefined ||
    readWatcherLocalHistoricalCapture(owner.capture) !== owner.value.capture
  )
    throw new Error("local backfill observation receipt is absent or stale");
  return owner.value;
};
