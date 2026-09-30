import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import {
  type LocalKupmiosFraudProofRawSource,
  settleLocalKupmiosReads,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Data,
  Lucid,
  paymentCredentialOf,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import { WatcherLocalKupmios } from "../l1/native-reward-account.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import {
  loadWatcherSecretText,
  type WatcherProcessConfig,
} from "../runtime/process-config.js";
import {
  bindWatcherL1AvailabilityPayloadSource,
  createWatcherRetainedDaRuntime,
} from "../storage/retained-da-runtime.js";
import {
  orderWatcherAvailabilityActions,
  selectWatcherAvailabilityActions,
  type WatcherAvailabilityAction,
  watcherAvailabilityOpenDeadlineMissed,
  watcherAvailabilityQueueStatus,
} from "./action.js";
import { createWatcherAvailabilityDeployment } from "./deployment.js";
import { createWatcherAvailabilityObservation } from "./observation.js";
import {
  deriveWatcherDaBondPoolObservation,
  type WatcherDaBondPoolObservation,
} from "./pool-observation.js";
import { createWatcherL1AvailabilityPayloadSource } from "./published-payload.js";
import {
  DA_CHALLENGE_WINDOW_MS,
  releaseWatcherAvailabilityWorkflows,
  required,
  WatcherAvailabilityCapitalShortfall,
  type WatcherAvailabilityRuntime,
  type WatcherAvailabilityStatus,
  type WatcherAvailabilityStatusTransition,
  WatcherAvailabilityTimeoutPoolUnavailable,
  type WatcherAvailabilityWorkflowRefusal,
  watcherAvailabilityWorkflowRefusal,
} from "./runtime.release-watcher-availability-workflows.js";
import {
  buildAdmittedWatcherAvailabilityOperation,
  selectWatcherAvailabilityFunding,
  watcherAvailabilityTimeoutCollateralLovelace,
} from "./runtime.select-watcher-availability-funding.js";

/** Concrete independent actor: signed release, public DA, exact L1 intake and durable executor. */
export const createWatcherAvailabilityRuntime = async (input: {
  config: WatcherProcessConfig;
  identity: VerifiedWatcherDeploymentIdentity;
  rawSource: LocalKupmiosFraudProofRawSource;
  /** Read-only payload reconstruction may follow reversible fault-proof inclusion. */
  faultProofObservation?: Readonly<{
    rawSource: LocalKupmiosFraudProofRawSource;
    currentObservation(): WatcherAuthenticatedStateQueueObservation;
  }>;
  proverWalletAddress: string;
  onStatusTransition?: (event: WatcherAvailabilityStatusTransition) => void;
  /**
   * Called with the pool readout after each successful pool read, once per
   * reconciliation. An error it throws propagates from `reconcile` and never
   * becomes an availability failure. Required, so a composition root cannot
   * drop the pool readout silently; production passes
   * `watcherDaBondPoolReporter`.
   */
  onDaBondPool: (pool: WatcherDaBondPoolObservation) => void;
  /**
   * Called with the cause after each failed pool read, once per
   * reconciliation, in place of `onDaBondPool`. Required for the same reason;
   * production passes `watcherDaBondPoolReadFailureReporter`.
   */
  onDaBondPoolReadFailure: (error: string) => void;
}): Promise<WatcherAvailabilityRuntime> => {
  const source = input.config.watcherConfig.l1.source;
  if (source.sourceMode !== "local_node")
    throw new Error("Availability actuation requires local-node authority");
  const kupo = required(
    source.queryServices.find(({ kind }) => kind === "kupo"),
    "Kupo source",
  );
  const ogmios = required(
    source.queryServices.find(({ kind }) => kind === "ogmios"),
    "Ogmios source",
  );
  const lucid = await Lucid(
    new WatcherLocalKupmios(kupo.endpoint, ogmios.endpoint, {
      watcherConfig: input.config.watcherConfig,
      binaryPath: input.config.nativeChainSyncBinaryPath,
      timeoutMs: input.config.watcherConfig.l1.requestTimeoutMs,
    }),
    input.identity.network,
    {
      evaluator: createScalusEvaluator(),
      slotConfig: input.config.watcherConfig.customNetwork?.slotConfig,
    },
  );
  const secret = await loadWatcherSecretText(
    input.config.availability.keySource,
  );
  if (secret.startsWith("ed25519_sk"))
    lucid.selectWallet.fromPrivateKey(secret);
  else lucid.selectWallet.fromSeed(secret, { addressType: "Enterprise" });
  const walletAddress = await lucid.wallet().address();
  if (
    paymentCredentialOf(walletAddress).hash ===
    paymentCredentialOf(input.proverWalletAddress).hash
  ) {
    throw new Error(
      "Availability and fault-proof actors must use independent payment keys",
    );
  }
  const actor = paymentCredentialOf(walletAddress).hash;
  const deployment = await createWatcherAvailabilityDeployment(
    lucid,
    input.identity,
  );
  const intake = createWatcherAvailabilityObservation({
    identity: input.identity,
    source: input.rawSource,
    deployment,
  });
  const publicDa = await createWatcherRetainedDaRuntime({
    watcherConfig: input.config.watcherConfig,
    deploymentIdentity: input.identity,
  });
  let journal: ReturnType<typeof openAvailabilityOperationJournal>;
  try {
    journal = openAvailabilityOperationJournal(
      input.config.availability.journalPath,
    );
  } catch (error) {
    await publicDa.close();
    throw error;
  }
  let generation = 0;
  let closed = false;
  let current: WatcherAuthenticatedStateQueueObservation | null = null;
  const l1PayloadSource = createWatcherL1AvailabilityPayloadSource({
    identity: input.identity,
    deployment,
    rawSource: input.faultProofObservation?.rawSource ?? input.rawSource,
    lucid,
    currentObservation:
      input.faultProofObservation?.currentObservation ?? (() => current),
  });
  const l1PayloadBinding = bindWatcherL1AvailabilityPayloadSource({
    deploymentIdentity: input.identity,
    source: l1PayloadSource,
  });
  let pending = new Set<string>();
  let report: WatcherAvailabilityStatus = {
    phase: "waiting",
    pendingHeaders: [],
  };
  let lastPool: WatcherDaBondPoolObservation | undefined;
  let lastPoolReadFailure: string | undefined;
  let serial: Promise<void> = Promise.resolve();
  let lastBlockedStatus: string | undefined;
  const reportTransition = (
    observation: WatcherAuthenticatedStateQueueObservation,
    startedAt: number,
  ): void => {
    if (input.onStatusTransition === undefined) return;
    // A new finalized point alone must not repeat the same blocked diagnostic.
    const blockedStatus =
      report.phase === "blocked"
        ? JSON.stringify({
            ...report,
            pendingHeaders: [...report.pendingHeaders].sort(),
          })
        : undefined;
    if (blockedStatus === lastBlockedStatus) return;
    lastBlockedStatus = blockedStatus;
    input.onStatusTransition(
      Object.freeze({
        status: Object.freeze({
          ...report,
          pendingHeaders: Object.freeze([...report.pendingHeaders]),
          ...(report.detail === undefined
            ? {}
            : {
                // Preserve the concise cause, never an embedded transaction payload.
                detail: report.detail
                  .replace(/[a-fA-F0-9]{128,}/g, "[hex omitted]")
                  .slice(0, 2048),
              }),
        }),
        observationDigest: observation.observationDigest,
        nativePoint: Object.freeze({ ...observation.nativePoint }),
        elapsedMs: Math.max(0, performance.now() - startedAt),
        observedAt: new Date().toISOString(),
      }),
    );
  };
  const validity = () => {
    const now = BigInt(Date.now());
    return { validFrom: now - 30_000n, validTo: now + 60_000n };
  };
  const assertCurrent = (epoch: number): void => {
    if (closed || epoch !== generation || current === null)
      throw new Error("Availability actuation generation was revoked");
    journal.assertRunning();
  };
  const commitmentOf = async (
    observation: WatcherAuthenticatedStateQueueObservation,
    snapshot: SDK.DaAvailabilityChallengeSnapshot,
  ): Promise<SDK.DaAvailabilityCommitment | undefined> => {
    const status = watcherAvailabilityQueueStatus(snapshot);
    if (
      status === undefined ||
      status === "Unattested" ||
      "Published" in status
    )
      return undefined;
    // The snapshot authenticated the record against the Challenged status.
    if ("Challenged" in status) return snapshot.recordDatum?.commitment;
    // E1: an Attested node carries only the hash; the preimage is the DAAT
    // datum its Apply spent, recovered and checked against that hash.
    return await intake.attestedCommitment(
      observation,
      snapshot.headerHash,
      status.Attested.commitment_hash,
    );
  };
  const publicPayloadAvailable = async (
    headerHash: string,
    commitment: SDK.DaAvailabilityCommitment,
  ): Promise<boolean> => {
    for (const source of publicDa.sources) {
      const result = await source.fetchPayloadByHeaderHash(headerHash);
      if (
        result.ok &&
        SDK.verifyDaAvailabilityPayloadCommitment({
          commitment,
          payload: result.payloadEnvelopeCbor,
        })
      )
        return true;
    }
    return false;
  };
  const minimumChange = (
    protocol: NonNullable<
      ReturnType<typeof lucid.config>["protocolParameters"]
    >,
  ): bigint =>
    calculateMinLovelaceFromUTxO(protocol.coinsPerUtxoByte, {
      address: walletAddress,
      assets: { lovelace: 2_000_000n },
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
  // Open spends one exact coin: challenger bond + record lovelace + the fee,
  // which is pinned to the Open fee ceiling.
  const openingLovelace =
    deployment.parameters.challenger_bond_lovelace +
    deployment.parameters.challenge_record_lovelace +
    deployment.parameters.max_open_fee_lovelace;
  const build = async (
    snapshot: SDK.DaAvailabilityChallengeSnapshot,
    action: WatcherAvailabilityAction,
    observation: WatcherAuthenticatedStateQueueObservation,
    commitment: SDK.DaAvailabilityCommitment | undefined,
  ): Promise<{
    action: string;
    completesWorkflow?: boolean;
    build(): Promise<TxSignBuilder | SDK.DaAvailabilityOperationBuild>;
  }> => {
    const parameters = deployment.parameters;
    const feeLovelace =
      action.action === "open"
        ? parameters.max_open_fee_lovelace
        : action.action === "settle"
          ? parameters.max_settlement_fee_lovelace
          : action.action === "close"
            ? parameters.max_close_fee_lovelace
            : parameters.max_timeout_fee_lovelace;
    const protocol = required(
      lucid.config().protocolParameters,
      "live protocol parameters",
    );
    const liveQueue =
      action.action === "timeout"
        ? await Effect.runPromise(
            SDK.fetchSortedStateQueueUTxOsProgram(lucid, {
              stateQueueAddress:
                deployment.contracts.stateQueue.spendingScriptAddress,
              stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
            }),
          )
        : undefined;
    // Anyone may TopUp the one shared pool and every Timeout spends it, so a
    // Timeout reads it at the tip rather than from the finalized snapshot,
    // which lags the tip. Without an authentic pool there the Timeout is
    // skipped this reconciliation, never built against the snapshot's pool.
    let livePool: Awaited<ReturnType<typeof SDK.fetchDaBondPool>> | undefined;
    if (action.action === "timeout") {
      try {
        livePool = await SDK.fetchDaBondPool(lucid, {
          policyId: deployment.contracts.daBondPool.policyId,
          address: deployment.contracts.daBondPool.spendingScriptAddress,
        });
      } catch (cause) {
        throw new WatcherAvailabilityTimeoutPoolUnavailable(
          cause instanceof Error ? cause.message : String(cause),
        );
      }
    }
    const minChange = minimumChange(protocol);
    const removalReserve =
      BigInt(liveQueue?.length ?? observation.finalizedHeaders.length + 1) *
        parameters.max_timeout_fee_lovelace +
      minChange;
    const configured = BigInt(input.config.availability.minimumFundingLovelace);
    const requiredWorking =
      action.action === "open"
        ? openingLovelace + removalReserve + parameters.max_open_fee_lovelace
        : action.action === "timeout" ||
            action.action === "prune" ||
            action.action === "remove"
          ? removalReserve
          : 0n;
    // G9: an Open is only worth taking when its Timeout stays reachable, and
    // the Timeout's fee includes the slashed penalty.
    const collateralRequired =
      action.action === "open" || action.action === "timeout"
        ? watcherAvailabilityTimeoutCollateralLovelace({
            parameters,
            collateralPercentage: protocol.collateralPercentage,
            minimumReturnLovelace: minChange,
          })
        : (feeLovelace * BigInt(protocol.collateralPercentage) + 99n) / 100n +
          minChange;
    // An Open (and its preparation) is taken only while the wallet also holds
    // the queue-bounded removal reserve and one Timeout collateral set, once
    // per wallet: removals are serialized by the correction lock and every
    // Timeout removes the head and prunes its descendants, so all live
    // challenges share one removal path. Bonds already opened sit on chain.
    const funds = selectWatcherAvailabilityFunding({
      utxos: await lucid.wallet().getUtxos(),
      reservedOutRefs: new Set(journal.reservedOutRefs(actor)),
      collateralLovelace: collateralRequired,
      openingLovelace,
      requiredWorkingLovelace:
        action.action === "open" && configured > requiredWorking
          ? configured
          : requiredWorking,
    });
    if (action.action === "open" && funds.exactOpening === undefined) {
      const preparing =
        openingLovelace + parameters.max_open_fee_lovelace + minChange;
      if (funds.funding.assets.lovelace < preparing) {
        throw new WatcherAvailabilityCapitalShortfall(
          "Availability funding needs one input large enough to prepare the exact challenger bond",
          preparing,
          funds.funding.assets.lovelace,
        );
      }
      return {
        action: "prepare",
        build: () =>
          SDK.buildDaAvailabilityFundingPreparationTx(lucid, {
            fundingInput: funds.funding,
            outputLovelace: openingLovelace,
            feeLovelace: parameters.max_open_fee_lovelace,
            ...validity(),
          }),
      };
    }
    return {
      action: action.action,
      ...(action.action === "timeout"
        ? { completesWorkflow: snapshot.descendant === undefined }
        : {}),
      build: async () => {
        const resources = {
          collateralInputs: funds.collateral,
          feeLovelace,
          ...validity(),
        };
        if (action.action === "open") {
          // The Open's inclusive upper bound must stay before the header's
          // end_time + da_challenge_window_ms.
          const node = Data.castFrom(
            required(snapshot.queue, "queue").datum.data,
            SDK.StateQueueNode,
          );
          const deadline = node.header.endTime + DA_CHALLENGE_WINDOW_MS;
          return (
            await Effect.runPromise(
              SDK.buildOpenDaAvailabilityChallengeTxProgram(lucid, deployment, {
                ...resources,
                validTo:
                  resources.validTo < deadline ? resources.validTo : deadline,
                commitment: required(commitment, "attested commitment"),
                queue: required(snapshot.queue, "queue").utxo,
                challengerFunding: required(
                  funds.exactOpening,
                  "exact challenger funding",
                ),
                challenger: actor,
                daChallengeWindowMs: DA_CHALLENGE_WINDOW_MS,
              }),
            )
          ).tx;
        }
        if (action.action === "settle") {
          const tranche = required(action.tranche, "next tranche");
          return (
            await Effect.runPromise(
              SDK.buildSettleDaAvailabilityTrancheTxProgram(lucid, deployment, {
                ...resources,
                record: required(snapshot.record, "challenge record"),
                terminal: required(snapshot.terminal, "terminal accumulator"),
                thread: tranche.utxo,
                ...(tranche.carrier === undefined
                  ? {}
                  : { carrier: tranche.carrier }),
              }),
            )
          ).tx;
        }
        if (action.action === "close")
          return (
            await Effect.runPromise(
              SDK.buildCloseDaAvailabilityChallengeTxProgram(
                lucid,
                deployment,
                {
                  ...resources,
                  record: required(snapshot.record, "challenge record"),
                  terminal: required(snapshot.terminal, "terminal accumulator"),
                  queue: required(snapshot.queue, "queue").utxo,
                },
              ),
            )
          ).tx;
        const removalTarget = {
          collateralInputs: resources.collateralInputs,
          validFrom: resources.validFrom,
          validTo: resources.validTo,
          queue: required(snapshot.queue, "queue").utxo,
          confirmedState: snapshot.confirmedState.utxo,
          correctionLock: snapshot.correctionLock,
          ...(snapshot.descendant === undefined
            ? {}
            : { descendant: snapshot.descendant.utxo }),
          challengeAssetName: required(
            action.challengeAssetName,
            "challenge identity",
          ),
          headerHash: snapshot.headerHash,
          rentRefundAddress: input.proverWalletAddress,
        };
        if (action.action === "timeout") {
          // The record, terminal and pool pay the exact fee
          // min(penalty, taken) + c, so no wallet coin funds it (E2). The
          // pool is the one read at the tip for this step.
          const built = await Effect.runPromise(
            SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
              lucid,
              deployment,
              {
                ...removalTarget,
                fundingQueueTailRefInput: required(
                  liveQueue?.at(-1),
                  "live funding queue tail",
                ).utxo,
                record: required(snapshot.record, "challenge record"),
                terminal: required(snapshot.terminal, "terminal accumulator"),
                pool: required(livePool, "tip DA bond pool").utxo,
              },
            ),
          );
          return {
            tx: built.tx,
            timeoutFeePartLovelace: required(
              built.timeoutFeePartLovelace,
              "timeout fee part",
            ),
          };
        }
        const removal = {
          ...removalTarget,
          feeLovelace,
          feeFunding: funds.funding,
        };
        return (
          await Effect.runPromise(
            action.action === "prune"
              ? SDK.buildPruneDaUnavailableBlockDescendantTxProgram(
                  lucid,
                  deployment,
                  removal,
                )
              : SDK.buildRemoveDaUnavailableHeadTxProgram(
                  lucid,
                  deployment,
                  removal,
                ),
          )
        ).tx;
      },
    };
  };
  const reconcile = (
    observation: WatcherAuthenticatedStateQueueObservation,
    actuate: boolean,
  ): Promise<void> => {
    const epoch = generation;
    const work = serial.then(async () => {
      if (closed || epoch !== generation) return;
      assertWatcherStateQueueObservation(observation);
      const startedAt = performance.now();
      current = observation;
      pending = new Set(
        observation.finalizedHeaders
          .filter(
            ({ daAvailability }) =>
              daAvailability !== "Unattested" &&
              !("Published" in daAvailability),
          )
          .map(({ headerHash }) => headerHash),
      );
      report = { phase: "waiting", pendingHeaders: [...pending] };
      let poolRead: WatcherDaBondPoolObservation | undefined;
      let poolReadFailure: string | undefined;
      try {
        assertCurrent(epoch);
        // E5: the pool is read on every reconciliation, pending headers or
        // not, and only reported: neither its state nor a failed read changes
        // the phase or holds back an Open, Settle, Close, Timeout or prune.
        try {
          const read = deriveWatcherDaBondPoolObservation({
            pool: await intake.pool(observation),
            policyId: deployment.contracts.daBondPool.policyId,
            parameters: deployment.parameters,
            nowMs: BigInt(Date.now()),
          });
          assertCurrent(epoch);
          lastPool = poolRead = read;
          lastPoolReadFailure = undefined;
        } catch (cause) {
          // A revoked generation still aborts the whole reconciliation.
          assertCurrent(epoch);
          lastPoolReadFailure = poolReadFailure =
            cause instanceof Error ? cause.message : String(cause);
        }
        const context: SDK.DaAvailabilityOperationContext = {
          deploymentIdentity: input.identity.manifestId,
          actor,
          journal,
          stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
          minimumConfirmationDepth: intake.confirmationDepth,
          transactionLimits: SDK.daAvailabilityOperationLimits(
            lucid,
            deployment.parameters,
          ),
          assertActuationCurrent: () => assertCurrent(epoch),
          observe: (intent) => intake.operation(observation, intent),
          submit: (cbor) =>
            required(lucid.config().provider, "local provider").submitTx(cbor),
        };
        const recovered = await SDK.reconcileDaAvailabilityOperations(context);
        // P20: after our own steps reconcile and before any admission, rows
        // whose challenge someone else's terminal step ended are released,
        // in every deployment of this actor.
        const workflowRelease = await releaseWatcherAvailabilityWorkflows(
          journal,
          actor,
          (openIntent, headerHash) =>
            intake.workflowRelease(observation, openIntent, headerHash),
          () => assertCurrent(epoch),
        );
        const unresolved = recovered.find(
          ({ status }) =>
            status !== "confirmed" &&
            status !== "expired" &&
            status !== "included",
        );
        if (unresolved !== undefined) {
          report = {
            phase: unresolved.status === "conflict" ? "blocked" : "waiting",
            pendingHeaders: [...pending],
            txHash: unresolved.txHash,
          };
        }
        const snapshots = await settleLocalKupmiosReads(
          observation.finalizedHeaders
            .filter(({ headerHash }) => pending.has(headerHash))
            .map(({ headerHash }) => intake.snapshot(observation, headerHash)),
        );
        const now = BigInt(Date.now());
        const openWindow = {
          // The earliest upper bound any Open built now could carry.
          inclusiveValidityUpper: now,
          daChallengeWindowMs: DA_CHALLENGE_WINDOW_MS,
        };
        const commitments = new Map<string, SDK.DaAvailabilityCommitment>();
        const candidates: {
          snapshot: SDK.DaAvailabilityChallengeSnapshot;
          publiclyAvailable: boolean;
        }[] = [];
        const missedOpenDeadlines: string[] = [];
        for (const snapshot of snapshots) {
          const status = watcherAvailabilityQueueStatus(snapshot);
          const commitment = await commitmentOf(observation, snapshot);
          if (commitment !== undefined)
            commitments.set(snapshot.headerHash, commitment);
          const attested = typeof status === "object" && "Attested" in status;
          const available =
            attested &&
            commitment !== undefined &&
            (await publicPayloadAvailable(snapshot.headerHash, commitment));
          if (attested && available) pending.delete(snapshot.headerHash);
          if (
            watcherAvailabilityOpenDeadlineMissed(
              snapshot,
              available,
              openWindow,
            )
          )
            missedOpenDeadlines.push(snapshot.headerHash);
          candidates.push({ snapshot, publiclyAvailable: available });
        }
        // E3: every header is selected independently; no live challenge
        // suppresses another header's Open, including a withheld descendant of
        // a Challenged header (if that ancestor settles, the descendant would
        // otherwise merge unchallenged). Opens go first, earliest deadline
        // first, since a missed Open deadline cannot be recovered.
        const ordered = orderWatcherAvailabilityActions(
          selectWatcherAvailabilityActions(
            candidates,
            validity().validFrom,
            openWindow,
          ),
        );
        // E5: missed-deadline, refused-Open, deferred-Timeout,
        // refused-workflow and workflow-release alerts are reported, never
        // blocking.
        let alerts: Pick<
          WatcherAvailabilityStatus,
          | "missedOpenDeadlines"
          | "openRefused"
          | "timeoutsDeferred"
          | "workflowRefused"
          | "workflowReleased"
          | "workflowReleaseDeferred"
        > = {
          ...(workflowRelease.released.length === 0
            ? {}
            : {
                workflowReleased: Object.freeze([...workflowRelease.released]),
              }),
          ...(workflowRelease.deferred.length === 0
            ? {}
            : {
                workflowReleaseDeferred: Object.freeze([
                  ...workflowRelease.deferred,
                ]),
              }),
          ...(missedOpenDeadlines.length === 0
            ? {}
            : { missedOpenDeadlines: Object.freeze(missedOpenDeadlines) }),
        };
        assertCurrent(epoch);
        report =
          unresolved === undefined
            ? { phase: "ready", pendingHeaders: [...pending], ...alerts }
            : {
                phase: unresolved.status === "conflict" ? "blocked" : "waiting",
                pendingHeaders: [...pending],
                txHash: unresolved.txHash,
                ...alerts,
              };
        if (!actuate || unresolved !== undefined) return;
        // The journal keeps one workflow per header and admits every header of
        // this deployment. A step it refuses, or an Open the wallet cannot
        // fund, must not starve a live challenge's own settle, close or
        // Timeout, so take the first step that is admitted and builds.
        const workflowRefused: WatcherAvailabilityWorkflowRefusal[] = [];
        const { selected, openRefused, timeoutsDeferred } =
          await buildAdmittedWatcherAvailabilityOperation(
            ordered,
            (headerHash, action) => {
              const detail = watcherAvailabilityWorkflowRefusal(journal, {
                actor,
                deploymentIdentity: input.identity.manifestId,
                headerHash,
                action,
                nowMs: Date.now(),
              });
              if (detail === undefined) return true;
              workflowRefused.push({ headerHash, action, detail });
              return false;
            },
            (step) =>
              build(
                step.snapshot,
                step.action,
                observation,
                commitments.get(step.snapshot.headerHash),
              ),
          );
        alerts = {
          ...alerts,
          ...(openRefused.length === 0
            ? {}
            : { openRefused: Object.freeze(openRefused) }),
          ...(timeoutsDeferred.length === 0
            ? {}
            : { timeoutsDeferred: Object.freeze(timeoutsDeferred) }),
          ...(workflowRefused.length === 0
            ? {}
            : { workflowRefused: Object.freeze(workflowRefused) }),
        };
        report = { ...report, ...alerts };
        if (selected === undefined) return;
        const { operation } = selected;
        const result = await SDK.runDaAvailabilityOperation(context, {
          headerHash: selected.step.snapshot.headerHash,
          ...operation,
        });
        report = {
          phase: result.status === "conflict" ? "blocked" : "waiting",
          pendingHeaders: [...pending],
          action: operation.action,
          txHash: result.txHash,
          ...alerts,
        };
      } catch (cause) {
        if (epoch !== generation || closed) return;
        report = {
          phase: "blocked",
          pendingHeaders: [...pending],
          detail: cause instanceof Error ? cause.message : String(cause),
        };
      } finally {
        // Diagnostics describe the completed reconciliation, not its temporary
        // waiting state. A revoked observation cannot emit a recovery signal.
        if (epoch === generation && !closed) {
          try {
            if (poolRead !== undefined) input.onDaBondPool(poolRead);
            else if (poolReadFailure !== undefined)
              input.onDaBondPoolReadFailure(poolReadFailure);
          } finally {
            reportTransition(observation, startedAt);
          }
        }
      }
    });
    serial = work.catch(() => undefined);
    return work;
  };
  return {
    reconcile,
    pendingAvailabilityHeaders: async (observation) => {
      assertWatcherStateQueueObservation(observation);
      const epoch = generation;
      const assertCurrentClassification = () => {
        const inclusion = input.faultProofObservation?.currentObservation();
        if (
          closed ||
          epoch !== generation ||
          current === null ||
          observation.deploymentIdentityDigest !== input.identity.manifestId ||
          (current.observationDigest !== observation.observationDigest &&
            inclusion?.observationDigest !== observation.observationDigest)
        )
          throw new Error(
            "Availability pending state requires a current authenticated observation",
          );
      };
      assertCurrentClassification();
      // Classification follows the inclusion overlay; this read never changes
      // the finalized observation or authorizes an availability transaction.
      const result = new Set<string>();
      for (const {
        headerHash,
        daAvailability,
      } of observation.finalizedHeaders) {
        if (daAvailability === "Unattested" || "Published" in daAvailability)
          continue;
        if ("Challenged" in daAvailability || pending.has(headerHash)) {
          result.add(headerHash);
          continue;
        }
        if (
          current?.finalizedHeaders.some(
            (header) => header.headerHash === headerHash,
          )
        )
          continue;
        // A newly included attestation can precede public DA propagation. Only
        // ordinary unavailability defers classification: malformed/rejected
        // data still reaches the classifier's mandatory evidence verification.
        let unavailable = true;
        for (const source of publicDa.sources) {
          const payload = await source.fetchPayloadByHeaderHash(headerHash);
          if (payload.ok) {
            unavailable = false;
            break;
          }
          if (
            payload.attempts.some(
              ({ status }) =>
                status !== "not_found" &&
                status !== "transport_error" &&
                status !== "timeout",
            )
          )
            unavailable = false;
        }
        if (unavailable) result.add(headerHash);
      }
      assertCurrentClassification();
      return result;
    },
    invalidateForRollback: (point) => {
      generation += 1;
      if (
        current !== null &&
        point !== undefined &&
        (point.kind === "origin" ||
          BigInt(point.slot) < BigInt(current.nativePoint.slot) ||
          (point.slot === current.nativePoint.slot &&
            point.blockHash !== current.nativePoint.blockHash))
      ) {
        journal.halt(
          "Finalized availability observation rolled back; authenticated recovery required",
        );
      }
      current = null;
      pending = new Set();
      report = { phase: "waiting", pendingHeaders: [] };
    },
    invalidateForShutdown: () => {
      generation += 1;
      closed = true;
    },
    status: () => ({
      ...report,
      ...(lastPool === undefined ? {} : { pool: lastPool }),
      ...(lastPoolReadFailure === undefined
        ? {}
        : { poolReadFailure: lastPoolReadFailure }),
    }),
    close: async () => {
      generation += 1;
      closed = true;
      await serial;
      l1PayloadBinding.close();
      await publicDa.close();
      journal.close();
      report = { phase: "closed", pendingHeaders: [] };
    },
  };
};
