/**
 * The L1 origin a first finalized deployment manifest records: the point the
 * nonce command recorded in run state before it signed the hub-oracle nonce,
 * proved on the local node's chain and before the nonce's block. A manifest
 * already written keeps its own, so its manifestId never changes.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { formatL1Origin, type L1Origin } from "@al-ft/midgard-core/l1-origin";
import { findOrigin, type FindOriginResult } from "@al-ft/midgard-l1-follower";
import { Effect, type Scope } from "effect";

import { NATIVE_LEDGER_SETTING_NAMES } from "../services/native-ledger.js";
import {
  type LocalNodeConfig,
  openLocalNodeTransport,
} from "./prepare-hub-oracle-nonce.l1-origin.js";

/**
 * Follows the local node's chain from `origin` to the block that holds the
 * hub-oracle nonce tx `nonceTxHash`.
 */
export type ManifestL1OriginProver = (
  origin: L1Origin,
  nonceTxHash: string,
) => Promise<FindOriginResult>;

const unproved = (
  origin: L1Origin,
  nonceTxHash: string,
  result: Exclude<FindOriginResult, { kind: "found" }>,
): string => {
  const point = formatL1Origin(origin);
  switch (result.kind) {
    case "from_not_on_chain":
      return `the recorded L1 origin ${point} is not on the local node's chain`;
    case "not_found":
      return `the local node's chain after the recorded L1 origin ${point} does not hold the hub-oracle nonce tx ${nonceTxHash}, so the origin does not lie before the nonce's block`;
    case "no_preceding_point":
      return `the hub-oracle nonce tx ${nonceTxHash} is in the chain's first block, which no recorded L1 origin can precede`;
  }
};

/**
 * Proves `recordedOrigin` for a first finalized manifest: it must be on the
 * local node's chain and lie before the block holding the hub-oracle nonce.
 * Refuses, with no manifest written, when no origin was recorded, when no
 * local node is configured, or when the chain does not prove it.
 */
export const provenManifestL1Origin = ({
  recordedOrigin,
  nonceTxHash,
  prover,
}: Readonly<{
  recordedOrigin: L1Origin | undefined;
  nonceTxHash: string;
  prover: ManifestL1OriginProver | undefined;
}>): Effect.Effect<L1Origin, Error> =>
  Effect.gen(function* () {
    const txHash = nonceTxHash.toLowerCase();
    if (recordedOrigin === undefined)
      return yield* Effect.fail(
        new Error(
          `Cannot finalize the deployment manifest: the deployment run state records no L1 origin for the hub-oracle nonce ${txHash} (the nonce command records it before it signs)`,
        ),
      );
    if (prover === undefined)
      return yield* Effect.fail(
        new Error(
          `Cannot finalize the deployment manifest: no local node is configured (${NATIVE_LEDGER_SETTING_NAMES.join(", ")}) to prove the recorded L1 origin ${formatL1Origin(recordedOrigin)} on its chain`,
        ),
      );
    const result = yield* Effect.tryPromise({
      try: () => prover(recordedOrigin, txHash),
      catch: (cause) =>
        new Error(
          `Cannot finalize the deployment manifest: failed to follow the local node's chain from the recorded L1 origin ${formatL1Origin(recordedOrigin)}: ${formatUnknownError(cause)}`,
          { cause },
        ),
    });
    if (result.kind !== "found")
      return yield* Effect.fail(
        new Error(
          `Cannot finalize the deployment manifest: ${unproved(recordedOrigin, txHash, result)}`,
        ),
      );
    return recordedOrigin;
  });

/**
 * The prover over the node's configured local node, open for the scope, or
 * undefined when no local node is configured.
 */
export const localNodeManifestL1OriginProver = (
  config: LocalNodeConfig,
): Effect.Effect<ManifestL1OriginProver | undefined, Error, Scope.Scope> =>
  Effect.map(
    openLocalNodeTransport(config),
    (transport): ManifestL1OriginProver | undefined =>
      transport === undefined
        ? undefined
        : (origin, nonceTxHash) =>
            findOrigin({ transport, txHash: nonceTxHash, from: origin }),
  );
