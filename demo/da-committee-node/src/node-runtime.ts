import type { DaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";

import { deferredAvailabilityResponder } from "./availability/deferred-responder.js";
import { availabilityResponderFromConfig } from "./availability/factory.js";
import { committeePromiseAdmission } from "./availability/promise-admission.js";
import { assertCommitteePromiseEnrollment } from "./availability/promise-profile-selection.js";
import type { AvailabilityResponderReport } from "./availability/responder.js";
import type { AvailabilityResponseLoopEnforcement } from "./availability-response-loop.js";
import { CommitteeService } from "./committee-service.js";
import type { LoadedCommitteeConfig } from "./config.js";
import { onChainCoordinatorFromConfig } from "./coordinator/factory.js";
import type { DaSubmitterFundingCheck } from "./coordinator/lucid-submitter.js";
import { createDaBondPoolWiring } from "./coordinator/pool-monitor.js";
import { SubmitterReconciler } from "./coordinator/submitter-reconciler.js";
import {
  createDaLibp2pAttestationGossipHandlers,
  createDaLibp2pAttestationRequestHandlers,
  createDaLibp2pPayloadRequestHandlers,
  createDaLibp2pProofRequestHandlers,
  DaLibp2pAttestationExchange,
  DaLibp2pNode,
  DaLibp2pPayloadSource,
  type DaPeerRegistry,
  StoreBackedDaAttestationProtocol,
} from "./da/libp2p/index.js";
import { followerDaAttestationReader } from "./l1/da-attestation-reader.js";
import {
  type CommitteeL1Readiness,
  startCommitteeL1Follower,
  untilCommitteeL1SourceReady,
} from "./l1/follower/l1-follower.js";
import { PeerSignatureCoordinator } from "./peer/coordinator.js";
import { PeerSignaturePoller } from "./peer/poller.js";
import { daAvailabilityCommitmentAuthorityFromConfig } from "./peer/signatures.js";
import { resolveRemoteDaAttestationTargets } from "./peer/targets.js";
import type {
  DaCommitteeValidation,
  DaSigner,
  DaSignerValidation,
} from "./signer.js";
import { openCommitteeStore } from "./store/factory.js";
import type { PostgresStoreInstanceLockEvents } from "./store/postgres.instance-lock.js";

/** What the node checked and loaded before touching any dependency. */
export type CommitteeNodeLocalSetup = {
  readonly config: LoadedCommitteeConfig;
  readonly signer?: DaSigner;
  readonly committeeValidation: DaCommitteeValidation;
  readonly signerValidation?: DaSignerValidation;
  readonly daIdentity: DaLibp2pIdentity;
  readonly daPeerRegistry: DaPeerRegistry;
  readonly libp2pPrivateKeySource: string;
};

/** The availability responder a running node drains, and its promise hooks. */
export type CommitteeAvailabilityRuntime = Readonly<{
  responder: Readonly<{ drain: () => Promise<AvailabilityResponderReport> }>;
  close: () => void;
  promiseLoopEnforcement?: AvailabilityResponseLoopEnforcement;
  compactRetainedPromises?: () => Promise<readonly string[]>;
}>;

export type CommitteeNodeRuntime = Awaited<
  ReturnType<typeof openCommitteeNodeRuntime>
>;

/**
 * Opens every dependency of a running committee node (store, its L1
 * follower, coordinators, libp2p) and initializes the service. One attempt of the
 * startup retry: on any failure it closes what it opened and rethrows, so the
 * next attempt starts clean.
 *
 * An adopted promise profile needs the follower ready before its bootstrap
 * scan: while the follower holds the committee (catching up, or a reason
 * only an operator clears, such as `rollback_beyond_k`), the attempt waits
 * and reports the reasons to `onL1Held` on every poll, the process up. The
 * non-adopted responder is built on its first drain the follower is ready for.
 */
export const openCommitteeNodeRuntime = async (
  local: CommitteeNodeLocalSetup,
  storeLockEvents: PostgresStoreInstanceLockEvents,
  onL1Held?: (reasons: readonly CommitteeL1Readiness[]) => void,
) => {
  const { config, signer, committeeValidation, signerValidation } = local;
  const { daIdentity, daPeerRegistry } = local;
  const closers: (() => Promise<void> | void)[] = [];
  const close = async (): Promise<void> => {
    for (const closer of closers.splice(0).reverse()) {
      await Promise.resolve()
        .then(closer)
        .catch(() => undefined);
    }
  };
  try {
    const store = await openCommitteeStore(config.localState, storeLockEvents);
    closers.push(() => store.close?.());
    await assertCommitteePromiseEnrollment(config, store);
    // The committee's L1 follower, held on the store's instance lock
    // session: it writes only while this process holds the store.
    const follower = await startCommitteeL1Follower(
      config,
      async () => store.instanceLock.followerWriterLease(),
      (line) => process.stderr.write(`${line}\n`),
    );
    closers.push(() => follower.stop());
    const daChainReader =
      follower.store === null
        ? undefined
        : followerDaAttestationReader(follower.store, config);
    const availabilityCommitmentAuthority =
      daAvailabilityCommitmentAuthorityFromConfig(config);
    const daAttestationProtocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint: config.deploymentFingerprint,
      localPeerId: daIdentity.peerId,
      committeeValidation,
      availabilityCommitmentAuthority,
      store,
    });
    const requestHandlers = new Map([
      ...createDaLibp2pPayloadRequestHandlers({
        deploymentFingerprint: config.deploymentFingerprint,
        store,
        limits: config.daTransport.limits,
      }),
      ...createDaLibp2pProofRequestHandlers({
        deploymentFingerprint: config.deploymentFingerprint,
        store,
        limits: config.daTransport.limits,
        accessPolicy: { kind: "manifest_roles", registry: daPeerRegistry },
      }),
      ...createDaLibp2pAttestationRequestHandlers({
        deploymentFingerprint: config.deploymentFingerprint,
        protocol: daAttestationProtocol,
        limits: config.daTransport.limits,
      }),
    ]);
    const daLibp2pNode = new DaLibp2pNode({
      config: config.daTransport,
      registry: daPeerRegistry,
      privateKeySource: local.libp2pPrivateKeySource,
      requestHandlers,
      gossipHandlers: createDaLibp2pAttestationGossipHandlers({
        deploymentFingerprint: config.deploymentFingerprint,
        registry: daPeerRegistry,
        protocol: daAttestationProtocol,
        committeeValidation,
        store,
      }),
      onGossipMessageError: (error) => {
        process.stderr.write(
          `rejected DA gossip message: ${
            error instanceof Error ? error.message : String(error)
          }\n`,
        );
      },
    });
    const payloadSource = new DaLibp2pPayloadSource({
      deploymentFingerprint: config.deploymentFingerprint,
      node: daLibp2pNode,
      registry: daPeerRegistry,
      limits: config.daTransport.limits,
    });
    const daAttestationPeers = resolveRemoteDaAttestationTargets({
      peers: config.daTransport.peers,
      localPeerId: daIdentity.peerId,
      signerIndex: config.signerIndex,
    }).remotePeers;
    const attestationExchange = new DaLibp2pAttestationExchange({
      deploymentFingerprint: config.deploymentFingerprint,
      localPeerId: daIdentity.peerId,
      node: daLibp2pNode,
      registry: daPeerRegistry,
      protocol: daAttestationProtocol,
      committeeValidation,
      store,
      requestTimeoutMs: config.peerRequestTimeoutMs,
    });
    // The latest check of the submitter's plain ADA against the next round.
    let l1SubmitterFunding: DaSubmitterFundingCheck | undefined;
    // The pooled DA bond as last read; transitions are JSON events on stderr.
    const daBondPool = createDaBondPoolWiring({
      writeEvent: (event) => {
        process.stderr.write(`${JSON.stringify(event)}\n`);
      },
    });
    const onChainCoordinator = config.l1SubmissionEnabled
      ? await onChainCoordinatorFromConfig(
          config,
          follower.lucid,
          daChainReader,
          store,
          undefined,
          {
            recordSubmitterFunding: (check) => {
              l1SubmitterFunding = check;
            },
            ...daBondPool.coordinatorHooks,
          },
        )
      : undefined;
    const peerPoller =
      onChainCoordinator !== undefined && daAttestationPeers.length > 0
        ? new PeerSignaturePoller({
            deploymentFingerprint: config.deploymentFingerprint,
            peers: daAttestationPeers,
            localPeerId: daIdentity.peerId,
            attestationExchange,
            signerValidation: committeeValidation,
            availabilityCommitmentAuthority,
            store,
            requestTimeoutMs: config.peerRequestTimeoutMs,
          })
        : undefined;
    const submitterReconciler =
      onChainCoordinator === undefined
        ? undefined
        : new SubmitterReconciler({
            deploymentFingerprint: config.deploymentFingerprint,
            committeeValidation,
            store,
            coordinator: onChainCoordinator,
            peerPoller,
            availabilityCommitmentAuthority,
            submitterId: config.l1SubmitterId,
          });
    const coordinator =
      signer !== undefined &&
      signerValidation !== undefined &&
      config.signerIndex !== undefined &&
      (daAttestationPeers.length > 0 || onChainCoordinator !== undefined)
        ? new PeerSignatureCoordinator({
            deploymentFingerprint: config.deploymentFingerprint,
            peers: daAttestationPeers,
            localPeerId: daIdentity.peerId,
            signer,
            signerIndex: config.signerIndex,
            signerValidation,
            availabilityCommitmentAuthority,
            store,
            attestationExchange,
            requestTimeoutMs: config.peerRequestTimeoutMs,
            retryInitialDelayMs: config.peerRetryInitialDelayMs,
            retryMaxDelayMs: config.peerRetryMaxDelayMs,
            retryMaxAttempts: config.peerRetryMaxAttempts,
            onChainCoordinator,
          })
        : undefined;
    let actuationReady = config.availabilityPromiseAdoption === undefined;
    let promiseAdmission:
      | ReturnType<typeof committeePromiseAdmission>
      | undefined;
    const service = new CommitteeService({
      config,
      store,
      l1: follower.source,
      payloadSource,
      get signer() {
        return actuationReady ? signer : undefined;
      },
      get signerValidation() {
        return actuationReady ? signerValidation : undefined;
      },
      get promiseAdmission() {
        return promiseAdmission;
      },
      get coordinator() {
        return actuationReady ? coordinator : undefined;
      },
      get submitterReconciler() {
        return actuationReady ? submitterReconciler : undefined;
      },
      daChainReader,
      daLibp2pNode,
      daPeerRegistry,
    });
    await service.initialize();
    if (!actuationReady) {
      await untilCommitteeL1SourceReady(follower.source, {
        ...(onL1Held === undefined ? {} : { onHeld: onL1Held }),
      });
      const bootstrap = await service.tick();
      if (bootstrap.errors.length !== 0 || service.latestL1View() === undefined)
        throw new Error(
          "Adopted committee startup requires a complete healthy unsigned scan",
        );
    }
    const adopted =
      config.l1SubmissionEnabled &&
      config.availabilityPromiseAdoption !== undefined
        ? await availabilityResponderFromConfig(config, store, follower)
        : undefined;
    if (adopted !== undefined) closers.push(() => adopted.close());
    adopted?.bindRetirementOperationalPins?.(() =>
      service.readRetirementOperationalPins(),
    );
    await adopted?.compactRetainedPromises?.();
    if (config.availabilityPromiseAdoption !== undefined) {
      if (signerValidation === undefined || adopted === undefined)
        throw new Error(
          "Adopted committee startup has no signing admission authority",
        );
      promiseAdmission = committeePromiseAdmission({
        config,
        store,
        signerValidation,
        source: adopted.promiseAdmissionSource,
      });
    }
    const deferred =
      config.l1SubmissionEnabled && adopted === undefined
        ? deferredAvailabilityResponder(follower.source, () =>
            availabilityResponderFromConfig(config, store, follower),
          )
        : undefined;
    if (deferred !== undefined) closers.push(() => deferred.close());
    const availabilityRuntime: CommitteeAvailabilityRuntime | undefined =
      adopted ?? deferred;
    actuationReady = true;
    closers.push(() => daLibp2pNode.stop());
    await daLibp2pNode.start();
    return {
      store,
      service,
      l1: follower.source,
      l1Lucid: follower.lucid,
      daLibp2pNode,
      onChainCoordinator,
      availabilityRuntime,
      daBondPool,
      submitterFunding: () => l1SubmitterFunding,
      close,
    };
  } catch (error) {
    await close();
    throw error;
  }
};
