import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import { digestCanonicalJson } from "./l1-adapter.exact-array.js";
import {
  localBackfillCaptures,
  localBackfillObservationBrand,
  localBackfillObservations,
  normalizeAdmittedBlock,
  observationsFromTransactionCbors,
  readWatcherLocalBackfillObservation,
  type WatcherLocalBackfillObservationReceipt,
} from "./l1-adapter.normalize-admitted-block.js";
import { parseAuthenticatedProvider } from "./l1-adapter.parse-authenticated-provider.js";
import {
  WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
} from "./l1-adapter.watcher-local-node-query-transport.js";
import {
  readWatcherLocalHistoricalCapture,
  type WatcherLocalHistoricalCaptureReceipt,
} from "./local-historical-capture.js";

/** No arbitrary provider data or transport authority is accepted or returned. */
export const admitWatcherLocalBackfillObservation = (
  receipt: WatcherLocalHistoricalCaptureReceipt,
): WatcherLocalBackfillObservationReceipt => {
  const capture = readWatcherLocalHistoricalCapture(receipt);
  const existing = localBackfillCaptures.get(receipt);
  if (existing !== undefined) {
    readWatcherLocalBackfillObservation(existing);
    return existing;
  }
  const binding = capture.sourceBinding;
  const sourceIdentityDigest = digestCanonicalJson({
    deploymentIdentityDigest: capture.deploymentIdentityDigest,
    blueprintHash: capture.blueprintHash,
    sourceId: capture.sourceId,
    sourceBinding: binding,
  });
  const acquisitionDigest = digestCanonicalJson({
    sourceIdentityDigest,
    predecessor: capture.predecessorPoint,
    target: capture.point,
    rawBlock: {
      ...capture.rawBlock,
      kupoCheckpoint: {
        ...capture.rawBlock.kupoCheckpoint,
        slot: capture.rawBlock.kupoCheckpoint.slot.toString(),
      },
    },
    nativeAuthorityDigest: capture.nativeAuthorityDigest,
    nativeStartupDigest: capture.nativeStartupDigest,
    targetEventDigest: capture.targetEventDigest,
    observedNativeTip: capture.observedNativeTip,
    depthAtObservedTip: capture.depthAtObservedTip,
    startedAtMonotonicMs: capture.startedAtMonotonicMs.toString(),
  });
  const normalize = (surface: "chain_sync" | "ogmios" | "kupo") => {
    const service = binding.queryServices.find(({ kind }) => kind === surface);
    if (surface !== "chain_sync" && service === undefined)
      throw new Error("local backfill query surface is absent");
    const provider = parseAuthenticatedProvider({
      schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
      network: capture.network,
      providerId: service?.providerId ?? binding.authorityNodeId,
      source: {
        sourceMode: "local_node",
        authorityNodeId: binding.authorityNodeId,
        surface,
      },
      authentication:
        surface === "chain_sync"
          ? {
              kind: "cardano_node_genesis_v1",
              publicIdentitySha256: binding.genesisIdentitySha256,
            }
          : {
              kind: "local_capture_identity_v1",
              publicIdentitySha256: digestCanonicalJson({
                sourceIdentityDigest,
                surface,
                service:
                  service === undefined
                    ? null
                    : {
                        kind: service.kind,
                        providerId: service.providerId,
                        endpoint: service.endpoint,
                        admittedSourceUrl: service.admittedSourceUrl,
                      },
              }),
            },
    });
    return normalizeAdmittedBlock(provider, {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
      network: capture.network,
      providerId: provider.providerId,
      chainPoint: {
        blockHash:
          surface === "kupo"
            ? capture.rawBlock.kupoCheckpoint.blockHash
            : capture.point.blockHash,
        parentBlockHash: capture.rawBlock.parentBlockHash,
        slot:
          surface === "kupo"
            ? capture.rawBlock.kupoCheckpoint.slot.toString()
            : capture.point.slot,
        blockNo: capture.point.blockNo,
        depth: capture.depthAtObservedTip,
      },
      transactions: observationsFromTransactionCbors(
        surface === "chain_sync"
          ? capture.nativeBlock.transactionCbors
          : surface === "ogmios"
            ? capture.rawBlock.transactions.map(
                ({ transactionCbor }) => transactionCbor,
              )
            : [],
      ),
    });
  };
  const native = normalize("chain_sync");
  const ogmios = normalize("ogmios");
  const kupo = normalize("kupo");
  if (
    native.blockContentDigest !== ogmios.blockContentDigest ||
    native.chainPoint.pointDigest !== kupo.chainPoint.pointDigest ||
    !watcherSameCanonicalJson(
      native.transactions.map(({ txHash }) => txHash),
      capture.rawBlock.transactions.map(({ txHash }) => txHash),
    )
  )
    throw new Error(
      "local backfill normalized native/query evidence disagrees",
    );
  const value = Object.freeze({
    capture,
    native,
    ogmios,
    kupo,
    sourceIdentityDigest,
    acquisitionDigest,
  });
  if (readWatcherLocalHistoricalCapture(receipt) !== capture)
    throw new Error("local backfill capture changed during normalization");
  const observation = Object.freeze({
    [localBackfillObservationBrand]: true as const,
  });
  localBackfillObservations.set(
    observation,
    Object.freeze({ capture: receipt, value }),
  );
  localBackfillCaptures.set(receipt, observation);
  return observation;
};
