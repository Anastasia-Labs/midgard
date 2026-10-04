import {
  computeFraudProofRawL1PointId,
  type LocalKupmiosFraudProofRawSource,
  localKupmiosHttpOgmiosRawSourceDetails,
  type LocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawBlockAtPoint,
} from "@al-ft/midgard-fault-proofs";

import { parseWatcherConfig, type WatcherConfig } from "../runtime/config.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseFinalityPolicy,
} from "../runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  closeWatcherL1TransportAttestationContext,
  establishWatcherLocalNodeAuthorityTransport,
  establishWatcherLocalNodeQueryTransport,
  normalizeWatcherL1BlockFromTransactionCbors,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
  type WatcherNormalizedL1Block,
} from "./l1-adapter.js";
import { captureExactBlockWithKupoLag } from "./local-kupmios-native-observation.capture-exact-block-with-kupo-lag.js";
import { createWatcherLocalKupmiosRawSource } from "./local-kupmios-raw-source.js";
import { evaluateWatcherMultiProviderConsistency } from "./multi-provider-consistency.js";
import type { WatcherNativeBlockAdmission } from "./native-block-admission.js";
import {
  type WatcherNativeChainSyncAuthority,
  watcherNativeChainSyncAuthorityDetails,
} from "./native-chain-sync.js";

export { captureExactBlockWithKupoLag } from "./local-kupmios-native-observation.capture-exact-block-with-kupo-lag.js";

export const WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_SCHEMA_VERSION =
  "midgard-watcher-local-kupmios-native-observation-v1" as const;

const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type WatcherLocalKupmiosNativeObservation = Readonly<{
  schemaVersion: typeof WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_SCHEMA_VERSION;
  block: WatcherNormalizedL1Block;
  ogmiosBlock: WatcherNormalizedL1Block;
  kupoCheckpoint: WatcherNormalizedL1Block;
  observations: readonly WatcherNormalizedL1Block[];
  transportAttestations: readonly WatcherL1TransportAttestationContext[];
  consistency: ReturnType<typeof evaluateWatcherMultiProviderConsistency>;
}>;

export type WatcherLocalKupmiosNativeObservationRuntime = Readonly<{
  /** Opaque, deployment-bound source shared with downstream exact L1 readers. */
  rawSource: LocalKupmiosFraudProofRawSource;
  observe(input: {
    readonly block: WatcherNativeBlockAdmission;
    readonly depth: string;
  }): Promise<WatcherLocalKupmiosNativeObservation>;
  close(): void;
}>;

type NativeObservationAdmission = Readonly<{
  nativeBinding: string;
  assertCurrent?: () => void;
}>;

const nativeBindingByLocalObservation = new WeakMap<
  object,
  NativeObservationAdmission
>();

const nativeObservationBinding = (block: WatcherNativeBlockAdmission): string =>
  watcherSha256CanonicalJson({
    schemaVersion: block.schemaVersion,
    blockType: block.blockType,
    protocolMajor: block.protocolMajor,
    blockHash: block.blockHash,
    prevHash: block.prevHash,
    slot: block.slot,
    blockNo: block.blockNo,
    rawBlockCbor: block.rawBlockCbor,
    rawHeaderCbor: block.rawHeaderCbor,
    transactionIds: block.transactionIds,
    transactionCbors: block.transactionCbors,
  });

/**
 * Proves this exact observation was produced after native/Kupo/Ogmios byte and
 * point agreement for the supplied native block. Structural copies reject.
 */
const readNativeObservationAdmission = (
  observation: WatcherLocalKupmiosNativeObservation,
  nativeBlock: WatcherNativeBlockAdmission,
): NativeObservationAdmission => {
  const admission = nativeBindingByLocalObservation.get(observation);
  if (
    admission === undefined ||
    admission.nativeBinding !== nativeObservationBinding(nativeBlock)
  ) {
    throw new Error(
      "watcher local Kupo/Ogmios observation is not admitted for the native block",
    );
  }
  admission.assertCurrent?.();
  return admission;
};

export const assertWatcherLocalKupmiosNativeObservation = (
  observation: WatcherLocalKupmiosNativeObservation,
  nativeBlock: WatcherNativeBlockAdmission,
): void => {
  readNativeObservationAdmission(observation, nativeBlock);
};

/** Derives a generation-fenced capability only from an admitted native observation. */
export const guardWatcherLocalKupmiosNativeObservation = ({
  observation,
  nativeBlock,
  assertCurrent,
}: {
  readonly observation: WatcherLocalKupmiosNativeObservation;
  readonly nativeBlock: WatcherNativeBlockAdmission;
  readonly assertCurrent: () => void;
}): WatcherLocalKupmiosNativeObservation &
  Readonly<{ assertCurrent: () => void }> => {
  const admission = readNativeObservationAdmission(observation, nativeBlock);
  const guard = () => {
    admission.assertCurrent?.();
    assertCurrent();
  };
  guard();
  const guarded = Object.freeze({ ...observation, assertCurrent: guard });
  nativeBindingByLocalObservation.set(
    guarded,
    Object.freeze({
      nativeBinding: admission.nativeBinding,
      assertCurrent: guard,
    }),
  );
  return guarded;
};

const sameStrings = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

/**
 * This comparison runs only after the fault-proof package has re-admitted the
 * opaque local-Kupmios source result. Keeping it separate makes the exact
 * native-vs-Ogmios byte/order invariant directly testable without creating a
 * second source-authority implementation in the watcher.
 */
const assertNativeKupmiosAgreement = (
  native: WatcherNativeBlockAdmission,
  raw: LocalKupmiosRawBlockAtPoint,
): void => {
  const transactionIds = raw.transactions.map(({ txHash }) => txHash);
  const transactionCbors = raw.transactions.map(
    ({ transactionCbor }) => transactionCbor,
  );
  if (
    raw.point.blockHash !== native.blockHash ||
    raw.point.slot !== native.slot ||
    raw.point.blockNo !== native.blockNo ||
    raw.kupoCheckpoint.blockHash !== native.blockHash ||
    raw.kupoCheckpoint.slot.toString() !== native.slot ||
    !sameStrings(transactionIds, native.transactionIds) ||
    !sameStrings(transactionCbors, native.transactionCbors)
  ) {
    throw new Error(
      "local Kupo/Ogmios observation differs from the native chain-sync block",
    );
  }
};

/** Test-only direct exercise of the deterministic comparison above. */
/** Kupo indexes a block a little after the native chain-sync delivers it. */
const KUPO_LAG_BUDGET_MS = 180_000;

export const unsafeAssertNativeKupmiosAgreementForTest = (
  native: WatcherNativeBlockAdmission,
  raw: LocalKupmiosRawBlockAtPoint,
): void => assertNativeKupmiosAgreement(native, raw);

const tcpTransport = (
  service: Extract<
    WatcherConfig["l1"]["source"],
    { readonly sourceMode: "local_node" }
  >["queryServices"][number],
  timeoutMs: number,
) =>
  Object.freeze({
    transportKind: "tcp" as const,
    providerId: service.identity,
    surface: service.kind,
    endpoint: service.endpoint,
    connectTimeoutMs: timeoutMs,
  });

/**
 * Constructs the live W01 observation authority from one native NtC session
 * and the exact local Kupo/Ogmios topology admitted by watcher config. Kupo
 * contributes only its authenticated point; Ogmios must reproduce the native
 * transaction vector byte-for-byte.
 */
export const createWatcherLocalKupmiosNativeObservationRuntime = async (
  input: Readonly<{
    watcherConfig: unknown;
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    nativeAuthority: WatcherNativeChainSyncAuthority;
    rawSource?: LocalKupmiosFraudProofRawSource;
  }>,
): Promise<WatcherLocalKupmiosNativeObservationRuntime> => {
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  const nativeDetails = watcherNativeChainSyncAuthorityDetails(
    input.nativeAuthority,
  );
  if (
    watcherConfig.mode !== "acceptance" ||
    (watcherConfig.targetNetwork !== "Preprod" &&
      watcherConfig.targetNetwork !== "Custom") ||
    input.deploymentIdentity.network !== watcherConfig.targetNetwork ||
    watcherConfig.l1.source.sourceMode !== "local_node" ||
    nativeDetails === null ||
    nativeDetails.network !== watcherConfig.targetNetwork ||
    nativeDetails.authorityNodeId !== watcherConfig.l1.source.authorityNodeId ||
    nativeDetails.genesisIdentitySha256 !==
      watcherConfig.l1.source.chainSync.genesisIdentitySha256 ||
    nativeDetails.socketPath !== watcherConfig.l1.source.chainSync.socketPath
  ) {
    throw new Error(
      "local Kupo/Ogmios runtime differs from the admitted native authority",
    );
  }
  const source = watcherConfig.l1.source;
  if (source.sourceMode !== "local_node") {
    throw new Error(
      "local Kupo/Ogmios runtime source changed during admission",
    );
  }
  if (
    watcherConfig.l1.finality.depth !==
      watcherDeploymentReleaseFinalityPolicy(input.deploymentIdentity).policy
        .confirmationDepth ||
    watcherConfig.l1.finality.rollback.postFinalityRecoveryMaxDepth !== 2160
  ) {
    throw new Error(
      "local Kupo/Ogmios runtime finality differs from the release policy",
    );
  }
  const kupo = source.queryServices.find(({ kind }) => kind === "kupo");
  const ogmios = source.queryServices.find(({ kind }) => kind === "ogmios");
  if (kupo === undefined || ogmios === undefined) {
    throw new Error("local Kupo/Ogmios runtime omitted a required service");
  }
  if (source.queryServices.some(({ kind }) => kind === "db_sync")) {
    throw new Error(
      "configured db-sync requires a concrete authenticated watcher query adapter",
    );
  }

  const authorityContext = establishWatcherLocalNodeAuthorityTransport(
    input.nativeAuthority,
  );
  const queryContexts: WatcherL1TransportAttestationContext[] = [];
  try {
    const kupoContext = await establishWatcherLocalNodeQueryTransport(
      authorityContext,
      tcpTransport(kupo, watcherConfig.l1.requestTimeoutMs),
    );
    queryContexts.push(kupoContext);
    const ogmiosContext = await establishWatcherLocalNodeQueryTransport(
      authorityContext,
      tcpTransport(ogmios, watcherConfig.l1.requestTimeoutMs),
    );
    queryContexts.push(ogmiosContext);
    // Use the same constructor for the expected namespace and normalized URLs.
    // Construction performs no network I/O; a supplied source still has to carry
    // the constructor's private admission and the exact configured identity.
    const configuredRawSource = createWatcherLocalKupmiosRawSource({
      watcherConfig,
      deploymentIdentity: input.deploymentIdentity,
    });
    const expectedSourceDetails =
      localKupmiosHttpOgmiosRawSourceDetails(configuredRawSource);
    const rawSource = input.rawSource ?? configuredRawSource;
    const rawSourceDetails = localKupmiosHttpOgmiosRawSourceDetails(rawSource);
    if (
      rawSourceDetails === null ||
      expectedSourceDetails === null ||
      rawSourceDetails.sourceId !== expectedSourceDetails.sourceId ||
      rawSourceDetails.deploymentIdentityDigest !==
        input.deploymentIdentity.manifestId ||
      rawSourceDetails.blueprintHash !==
        input.deploymentIdentity.blueprintHash ||
      rawSourceDetails.kupoHttpUrl !== expectedSourceDetails.kupoHttpUrl ||
      rawSourceDetails.ogmiosUrl !== expectedSourceDetails.ogmiosUrl ||
      rawSourceDetails.finalityPolicyDigest !==
        expectedSourceDetails.finalityPolicyDigest
    ) {
      throw new Error(
        "local Kupo/Ogmios raw source differs from the admitted native topology",
      );
    }
    let closed = false;
    let observationSource = createWatcherLocalKupmiosRawSource({
      watcherConfig,
      deploymentIdentity: input.deploymentIdentity,
    });
    return Object.freeze({
      rawSource,
      observe: async ({ block, depth }) => {
        if (closed) throw new Error("local Kupo/Ogmios runtime is closed");
        if (!NATURAL.test(depth)) {
          throw new Error("local Kupo/Ogmios observation depth is invalid");
        }
        const point = Object.freeze({
          blockHash: block.blockHash,
          blockNo: block.blockNo,
          slot: block.slot,
          pointId: computeFraudProofRawL1PointId({
            blockHash: block.blockHash,
            blockNo: block.blockNo,
            slot: block.slot,
          }),
        });
        const recreateSource = () => {
          observationSource = createWatcherLocalKupmiosRawSource({
            watcherConfig,
            deploymentIdentity: input.deploymentIdentity,
          });
        };
        const raw = await captureExactBlockWithKupoLag({
          read: () =>
            readAdmittedLocalKupmiosRawBlockAtPoint({
              source: observationSource,
              point,
            }),
          recreateSource,
          isClosed: () => closed,
          lagBudgetMs: KUPO_LAG_BUDGET_MS,
        });
        assertNativeKupmiosAgreement(block, raw);
        for (const [name, context] of [
          ["native", authorityContext],
          ["kupo", kupoContext],
          ["ogmios", ogmiosContext],
        ] as const) {
          if (watcherL1TransportAttestationDetails(context) === null)
            throw new Error(
              `Local ${name} transport closed during exact block observation`,
            );
        }
        const chainPoint = Object.freeze({
          blockHash: block.blockHash,
          parentBlockHash: block.prevHash.length === 0 ? null : block.prevHash,
          slot: block.slot,
          blockNo: block.blockNo,
          depth,
        });
        const blockObservation = normalizeWatcherL1BlockFromTransactionCbors(
          authorityContext,
          {
            network: watcherConfig.targetNetwork,
            chainPoint,
            transactionCbors: block.transactionCbors,
          },
        );
        const ogmiosBlock = normalizeWatcherL1BlockFromTransactionCbors(
          ogmiosContext,
          {
            network: watcherConfig.targetNetwork,
            chainPoint,
            transactionCbors: raw.transactions.map(
              ({ transactionCbor }) => transactionCbor,
            ),
          },
        );
        const kupoCheckpoint = normalizeWatcherL1BlockFromTransactionCbors(
          kupoContext,
          {
            network: watcherConfig.targetNetwork,
            chainPoint,
            transactionCbors: [],
          },
        );
        const configuredSource = Object.freeze({
          sourceMode: "local_node" as const,
          network: watcherConfig.targetNetwork,
          authorityNodeId: source.authorityNodeId,
          genesisIdentitySha256: source.chainSync.genesisIdentitySha256,
          chainSyncSocketPath: source.chainSync.socketPath,
          queryServices: Object.freeze(
            [kupo, ogmios]
              .map((service) =>
                Object.freeze({
                  kind: service.kind,
                  providerId: service.identity,
                  endpoint: service.endpoint,
                }),
              )
              .sort((left, right) =>
                left.providerId.localeCompare(right.providerId),
              ),
          ),
        });
        const consistency = evaluateWatcherMultiProviderConsistency(
          configuredSource,
          [blockObservation, kupoCheckpoint, ogmiosBlock],
          [authorityContext, kupoContext, ogmiosContext],
        );
        if (
          consistency.status !== "agreed" ||
          consistency.protocolDecision !== "allowed" ||
          consistency.agreement?.blockContentDigest !==
            blockObservation.blockContentDigest ||
          consistency.localQueryServiceBindings.some(
            ({ observationStatus }) => observationStatus !== "aligned",
          )
        ) {
          throw new Error(
            "local Kupo/Ogmios observations did not agree with native chain-sync",
          );
        }
        const observations = Object.freeze([
          blockObservation,
          kupoCheckpoint,
          ogmiosBlock,
        ]);
        const observation = Object.freeze({
          schemaVersion:
            WATCHER_LOCAL_KUPMIOS_NATIVE_OBSERVATION_SCHEMA_VERSION,
          block: blockObservation,
          ogmiosBlock,
          kupoCheckpoint,
          observations,
          transportAttestations: Object.freeze([
            authorityContext,
            kupoContext,
            ogmiosContext,
          ]),
          consistency,
        });
        nativeBindingByLocalObservation.set(
          observation,
          Object.freeze({ nativeBinding: nativeObservationBinding(block) }),
        );
        return observation;
      },
      close: () => {
        if (closed) return;
        closed = true;
        for (const context of queryContexts) {
          closeWatcherL1TransportAttestationContext(context);
        }
        closeWatcherL1TransportAttestationContext(authorityContext);
      },
    });
  } catch (error) {
    for (const context of queryContexts) {
      closeWatcherL1TransportAttestationContext(context);
    }
    closeWatcherL1TransportAttestationContext(authorityContext);
    throw error;
  }
};
