import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
  type LocalKupmiosFraudProofRawSource,
  localKupmiosHttpOgmiosRawSourceDetails,
  type LocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
  settleLocalKupmiosReads,
} from "@al-ft/midgard-fault-proofs";

import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
} from "../runtime/deployment-identity.js";
import { watcherL1TransportAttestationDetails } from "./l1-adapter.js";
import {
  assertWatcherLocalKupmiosNativeObservation,
  type WatcherLocalKupmiosNativeObservation,
} from "./local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "./native-block-admission.js";

export const WATCHER_RESOLVED_BLOCK_OBSERVATION_SCHEMA_VERSION =
  "midgard-watcher-resolved-block-observation-v1" as const;

/** A live source capability; copied fields cannot grant resolution authority. */
export type WatcherResolvedBlockObservation = Readonly<{
  schemaVersion: typeof WATCHER_RESOLVED_BLOCK_OBSERVATION_SCHEMA_VERSION;
}>;

export type WatcherResolvedBlockObservationSource = Readonly<{
  observe(input: {
    readonly nativeBlock: WatcherNativeBlockAdmission;
    readonly localObservation: WatcherLocalKupmiosNativeObservation;
  }): Promise<WatcherResolvedBlockObservation>;
}>;

export type WatcherResolvedBlockObservationEvidence = Readonly<{
  deploymentIdentityDigest: string;
  sourceId: string;
  finalityDepth: string;
  /** Stable release minimum, independent of the tip depth of this observation. */
  minimumConfirmationDepth: string;
  /** Complete native-ordered transaction bytes, before any consumer filter. */
  rawBlock: LocalKupmiosRawBlockAtPoint;
}>;

type ResolvedBlockRuntime = Readonly<{
  evidence: WatcherResolvedBlockObservationEvidence;
  nativeBlock: WatcherNativeBlockAdmission;
  localObservation: WatcherLocalKupmiosNativeObservation;
  rawSource: LocalKupmiosFraudProofRawSource;
  minimumConfirmationDepth: number;
  sourceDetails: NonNullable<
    ReturnType<typeof localKupmiosHttpOgmiosRawSourceDetails>
  >;
}>;

const admittedObservations = new WeakMap<
  WatcherResolvedBlockObservation,
  ResolvedBlockRuntime
>();

const normalizedEndpoint = (value: string): string => {
  const parsed = new URL(value);
  if (parsed.protocol === "ws:") parsed.protocol = "http:";
  if (parsed.protocol === "wss:") parsed.protocol = "https:";
  parsed.hash = "";
  return parsed.toString().replace(/\/$/u, "");
};

const assertLiveObservation = ({
  nativeBlock,
  localObservation,
  sourceDetails,
  minimumConfirmationDepth,
}: Pick<
  ResolvedBlockRuntime,
  | "nativeBlock"
  | "localObservation"
  | "sourceDetails"
  | "minimumConfirmationDepth"
>): void => {
  assertWatcherLocalKupmiosNativeObservation(localObservation, nativeBlock);
  const transportDetails = localObservation.transportAttestations
    .map(watcherL1TransportAttestationDetails)
    .filter((value) => value !== null);
  const kupo = transportDetails.find(
    ({ provider }) =>
      provider.source.sourceMode === "local_node" &&
      provider.source.surface === "kupo",
  );
  const ogmios = transportDetails.find(
    ({ provider }) =>
      provider.source.sourceMode === "local_node" &&
      provider.source.surface === "ogmios",
  );
  if (kupo === undefined || ogmios === undefined) {
    const missing = [
      kupo === undefined ? "kupo" : null,
      ogmios === undefined ? "ogmios" : null,
    ].filter((surface) => surface !== null);
    throw new Error(
      `resolved block transport authority is closed or absent: ${missing.join(", ")}`,
    );
  }
  if (
    normalizedEndpoint(kupo.transportEndpoint) !== sourceDetails.kupoHttpUrl ||
    normalizedEndpoint(ogmios.transportEndpoint) !== sourceDetails.ogmiosUrl
  ) {
    throw new Error(
      "resolved block source differs from admitted watcher transports",
    );
  }
  const point = localObservation.block.chainPoint;
  if (
    point.blockHash !== nativeBlock.blockHash ||
    point.slot !== nativeBlock.slot ||
    point.blockNo !== nativeBlock.blockNo ||
    BigInt(point.depth) < BigInt(minimumConfirmationDepth)
  ) {
    throw new Error(
      "resolved block chain point/finality differs from native admission",
    );
  }
};

const requireObservation = (
  observation: WatcherResolvedBlockObservation,
): ResolvedBlockRuntime => {
  const runtime = admittedObservations.get(observation);
  if (runtime === undefined) {
    throw new Error("resolved block observation was not admitted");
  }
  assertLiveObservation(runtime);
  return runtime;
};

export const readWatcherResolvedBlockObservation = (
  observation: WatcherResolvedBlockObservation,
): WatcherResolvedBlockObservationEvidence =>
  requireObservation(observation).evidence;

/** Resolves only a consumer's selected subset of the already complete block. */
export const resolveWatcherBlockObservationTransactions = async (
  observation: WatcherResolvedBlockObservation,
  transactionHashes: readonly string[],
): Promise<readonly FraudProofRawL1Transaction[]> => {
  const runtime = requireObservation(observation);
  const hashes = [...transactionHashes];
  if (
    new Set(hashes).size !== hashes.length ||
    hashes.some((hash) => !runtime.nativeBlock.transactionIds.includes(hash))
  ) {
    throw new Error(
      "resolved block transaction selection is not a unique subset",
    );
  }
  const resolved = await settleLocalKupmiosReads(
    hashes.map(async (txHash) => {
      const raw = await readAdmittedLocalKupmiosRawTransaction({
        source: runtime.rawSource,
        txHash,
        expectedInclusionPoint: runtime.evidence.rawBlock.point,
        minimumConfirmationDepth: runtime.minimumConfirmationDepth,
      });
      const transactionIndex =
        runtime.nativeBlock.transactionIds.indexOf(txHash);
      const normalized =
        runtime.localObservation.block.transactions[transactionIndex];
      const point = runtime.evidence.rawBlock.point;
      if (
        raw.txHash !== txHash ||
        raw.inclusionPoint.blockHash !== point.blockHash ||
        raw.inclusionPoint.slot !== point.slot ||
        raw.inclusionPoint.blockNo !== point.blockNo ||
        raw.confirmationDepth < runtime.minimumConfirmationDepth
      ) {
        throw new Error(
          "resolved transaction differs from the admitted block point",
        );
      }
      if (
        normalized === undefined ||
        normalized.txHash !== raw.txHash ||
        normalized.body.bytesHex !== raw.bodyCbor ||
        normalized.witnessSet.bytesHex !== raw.witnessSetCbor
      ) {
        throw new Error(
          "resolved transaction bytes differ from the admitted watcher block",
        );
      }
      // Raw-source admission verifies the exact spent/reference rosters. Copy
      // their records so later mutations cannot alter the captured evidence.
      return Object.freeze({
        ...raw,
        inclusionPoint: Object.freeze({ ...raw.inclusionPoint }),
        resolvedInputs: Object.freeze(
          raw.resolvedInputs.map((input) => Object.freeze({ ...input })),
        ),
        resolvedReferenceInputs: Object.freeze(
          raw.resolvedReferenceInputs.map((input) =>
            Object.freeze({ ...input }),
          ),
        ),
      });
    }),
  );
  assertLiveObservation(runtime);
  return Object.freeze(resolved);
};

export const createWatcherResolvedBlockObservationSource = ({
  deploymentIdentity,
  rawSource,
  minimumConfirmationDepth: requestedConfirmationDepth,
}: {
  /** Inclusion depth 1, or the source's release depth (the default). */
  readonly minimumConfirmationDepth?: number;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly rawSource: LocalKupmiosFraudProofRawSource;
}): WatcherResolvedBlockObservationSource => {
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const sourceDetails = localKupmiosHttpOgmiosRawSourceDetails(rawSource);
  if (
    sourceDetails === null ||
    sourceDetails.deploymentIdentityDigest !== deploymentIdentity.manifestId ||
    sourceDetails.blueprintHash !== deploymentIdentity.blueprintHash
  ) {
    throw new Error("raw block source is not bound to the verified deployment");
  }
  const minimumConfirmationDepth =
    requestedConfirmationDepth ?? sourceDetails.confirmationDepth;
  if (
    minimumConfirmationDepth !== 1 &&
    minimumConfirmationDepth !== sourceDetails.confirmationDepth
  ) {
    throw new Error(
      "resolved block depth must be inclusion or the release confirmation depth",
    );
  }
  const source: WatcherResolvedBlockObservationSource = Object.freeze({
    async observe({ nativeBlock, localObservation }) {
      if (this !== source) {
        throw new Error("resolved block observation source was not admitted");
      }
      const live = {
        nativeBlock,
        localObservation,
        sourceDetails,
        minimumConfirmationDepth,
      };
      assertLiveObservation(live);
      const point = Object.freeze({
        blockHash: nativeBlock.blockHash,
        blockNo: nativeBlock.blockNo,
        slot: nativeBlock.slot,
        pointId: computeFraudProofRawL1PointId({
          blockHash: nativeBlock.blockHash,
          blockNo: nativeBlock.blockNo,
          slot: nativeBlock.slot,
        }),
      });
      const raw = await readAdmittedLocalKupmiosRawBlockAtPoint({
        source: rawSource,
        point,
      });
      if (
        raw.parentBlockHash !==
          (nativeBlock.prevHash.length === 0 ? null : nativeBlock.prevHash) ||
        raw.transactions.length !== nativeBlock.transactionIds.length ||
        raw.transactions.some(
          (transaction, index) =>
            transaction.txHash !== nativeBlock.transactionIds[index] ||
            transaction.transactionCbor !== nativeBlock.transactionCbors[index],
        )
      ) {
        throw new Error("raw resolved block differs from native admission");
      }
      const rawBlock = Object.freeze({
        ...raw,
        point: Object.freeze({ ...raw.point }),
        kupoCheckpoint: Object.freeze({ ...raw.kupoCheckpoint }),
        transactions: Object.freeze(
          raw.transactions.map((transaction) =>
            Object.freeze({ ...transaction }),
          ),
        ),
      });
      assertLiveObservation(live);
      const observation = Object.freeze({
        schemaVersion: WATCHER_RESOLVED_BLOCK_OBSERVATION_SCHEMA_VERSION,
      });
      admittedObservations.set(
        observation,
        Object.freeze({
          ...live,
          rawSource,
          evidence: Object.freeze({
            deploymentIdentityDigest: deploymentIdentity.manifestId,
            sourceId: sourceDetails.sourceId,
            finalityDepth: localObservation.block.chainPoint.depth,
            minimumConfirmationDepth: minimumConfirmationDepth.toString(),
            rawBlock,
          }),
        }),
      );
      return observation;
    },
  });
  return source;
};
