import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import {
  type LocalKupmiosFraudProofRawSource,
  settleLocalKupmiosReads,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Lucid,
  paymentCredentialOf,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
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
  selectWatcherAvailabilityAction,
  type WatcherAvailabilityAction,
} from "./action.js";
import { createWatcherAvailabilityDeployment } from "./deployment.js";
import { createWatcherAvailabilityObservation } from "./observation.js";
import { createWatcherL1AvailabilityPayloadSource } from "./published-payload.js";

export type WatcherAvailabilityStatus = Readonly<{
  phase: "ready" | "waiting" | "blocked" | "closed";
  pendingHeaders: readonly string[];
  action?: string;
  txHash?: string;
  detail?: string;
}>;

export type WatcherAvailabilityStatusTransition = Readonly<{
  status: WatcherAvailabilityStatus;
  observationDigest: string;
  nativePoint: WatcherAuthenticatedStateQueueObservation["nativePoint"];
  elapsedMs: number;
  observedAt: string;
}>;

export type WatcherAvailabilityRuntime = Readonly<{
  reconcile(
    observation: WatcherAuthenticatedStateQueueObservation,
    actuate: boolean,
  ): Promise<void>;
  pendingAvailabilityHeaders(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<ReadonlySet<string>>;
  invalidateForRollback(point?: WatcherNativeChainSyncPoint): void;
  invalidateForShutdown(): void;
  status(): WatcherAvailabilityStatus;
  /** True while a header still needs attestation or the last attempt was blocked. */
  close(): Promise<void>;
}>;

const required = <T>(value: T | undefined, label: string): T => {
  if (value === undefined)
    throw new Error(`Availability action requires ${label}`);
  return value;
};

const plainAda = (utxo: UTxO): boolean =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).length === 1 &&
  (utxo.assets.lovelace ?? 0n) > 0n;

export const selectWatcherAvailabilityFunding = (input: {
  utxos: readonly UTxO[];
  collateralLovelace: bigint;
  openingLovelace: bigint;
  requiredWorkingLovelace: bigint;
  reservedOutRefs?: ReadonlySet<string>;
}): Readonly<{ collateral: UTxO; funding: UTxO; exactOpening?: UTxO }> => {
  const candidates = input.utxos
    .filter(plainAda)
    .sort((left, right) =>
      left.assets.lovelace === right.assets.lovelace
        ? `${left.txHash}#${left.outputIndex}`.localeCompare(
            `${right.txHash}#${right.outputIndex}`,
          )
        : left.assets.lovelace < right.assets.lovelace
          ? -1
          : 1,
    );
  const collateral = candidates.find(
    (utxo) =>
      utxo.assets.lovelace >= input.collateralLovelace &&
      utxo.assets.lovelace !== input.openingLovelace,
  );
  if (collateral === undefined)
    throw new Error(
      "Availability wallet needs a separate plain-ADA collateral output",
    );
  const spending = candidates.filter(
    (utxo) =>
      utxo !== collateral &&
      !input.reservedOutRefs?.has(`${utxo.txHash}#${utxo.outputIndex}`),
  );
  if (
    spending.reduce((sum, utxo) => sum + utxo.assets.lovelace, 0n) <
    input.requiredWorkingLovelace
  ) {
    throw new Error(
      "Availability wallet cannot fund the challenger bond and reachable timeout removal path",
    );
  }
  const funding = spending.at(-1);
  if (funding === undefined)
    throw new Error("Availability wallet has no independent fee funding");
  const exactOpening = spending.find(
    (utxo) => utxo.assets.lovelace === input.openingLovelace,
  );
  return {
    collateral,
    funding,
    ...(exactOpening === undefined ? {} : { exactOpening }),
  };
};

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
  const publicPayloadAvailable = async (
    snapshot: SDK.DaAvailabilityChallengeSnapshot,
  ): Promise<boolean> => {
    const datum = snapshot.bondDatum;
    if (datum === undefined) return false;
    const commitment =
      "Available" in datum
        ? datum.Available.commitment
        : datum.ChallengedBond.commitment;
    for (const source of publicDa.sources) {
      const result = await source.fetchPayloadByHeaderHash(snapshot.headerHash);
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
  const requireReachableRemovalCapital = async (
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<void> => {
    const protocol = required(
      lucid.config().protocolParameters,
      "live protocol parameters",
    );
    const minChange = calculateMinLovelaceFromUTxO(protocol.coinsPerUtxoByte, {
      address: walletAddress,
      assets: { lovelace: 2_000_000n },
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
    const fee = deployment.parameters.max_timeout_fee_lovelace;
    try {
      selectWatcherAvailabilityFunding({
        utxos: await lucid.wallet().getUtxos(),
        reservedOutRefs: new Set(journal.reservedOutRefs(actor)),
        collateralLovelace:
          (fee * BigInt(protocol.collateralPercentage) + 99n) / 100n +
          minChange,
        openingLovelace:
          deployment.parameters.challenger_bond_lovelace +
          deployment.parameters.max_open_fee_lovelace,
        requiredWorkingLovelace:
          BigInt(observation.finalizedHeaders.length + 1) * fee + minChange,
      });
    } catch (cause) {
      throw new Error(
        "Additional availability funding is required for the current descendant removal path",
        { cause },
      );
    }
  };
  const build = async (
    snapshot: SDK.DaAvailabilityChallengeSnapshot,
    action: WatcherAvailabilityAction,
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<{
    action: string;
    completesWorkflow?: boolean;
    build(): Promise<TxSignBuilder>;
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
    const minChange = calculateMinLovelaceFromUTxO(protocol.coinsPerUtxoByte, {
      address: walletAddress,
      assets: { lovelace: 2_000_000n },
      txHash: "00".repeat(32),
      outputIndex: 0,
    });
    const removalReserve =
      BigInt(liveQueue?.length ?? observation.finalizedHeaders.length + 1) *
        parameters.max_timeout_fee_lovelace +
      minChange;
    const openingLovelace =
      parameters.challenger_bond_lovelace + parameters.max_open_fee_lovelace;
    const configured = BigInt(input.config.availability.minimumFundingLovelace);
    const requiredWorking =
      action.action === "open"
        ? openingLovelace + removalReserve + parameters.max_open_fee_lovelace
        : action.action === "timeout" ||
            action.action === "prune" ||
            action.action === "remove"
          ? removalReserve
          : 0n;
    const collateralRequired =
      (feeLovelace * BigInt(protocol.collateralPercentage) + 99n) / 100n +
      minChange;
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
      if (
        funds.funding.assets.lovelace <
        openingLovelace + parameters.max_open_fee_lovelace + minChange
      ) {
        throw new Error(
          "Availability funding needs one input large enough to prepare the exact challenger bond",
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
          collateralInputs: [funds.collateral],
          feeLovelace,
          ...validity(),
        };
        if (action.action === "open")
          return (
            await Effect.runPromise(
              SDK.buildOpenDaAvailabilityChallengeTxProgram(lucid, deployment, {
                ...resources,
                bond: required(snapshot.bond, "bond"),
                queue: required(snapshot.queue, "queue").utxo,
                challengerFunding: required(
                  funds.exactOpening,
                  "exact challenger funding",
                ),
                challenger: actor,
              }),
            )
          ).tx;
        if (action.action === "settle") {
          const tranche = required(action.tranche, "next tranche");
          return (
            await Effect.runPromise(
              SDK.buildSettleDaAvailabilityTrancheTxProgram(lucid, deployment, {
                ...resources,
                bond: required(snapshot.bond, "bond"),
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
                  bond: required(snapshot.bond, "bond"),
                  terminal: required(snapshot.terminal, "terminal accumulator"),
                  queue: required(snapshot.queue, "queue").utxo,
                },
              ),
            )
          ).tx;
        const removal = {
          ...resources,
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
          feeFunding: funds.funding,
        };
        if (action.action === "timeout")
          return (
            await Effect.runPromise(
              SDK.buildTimeoutDaAvailabilityChallengeTxProgram(
                lucid,
                deployment,
                {
                  ...removal,
                  fundingQueueTailRefInput: required(
                    liveQueue?.at(-1),
                    "live funding queue tail",
                  ).utxo,
                  bond: required(snapshot.bond, "bond"),
                  terminal: required(snapshot.terminal, "terminal accumulator"),
                },
              ),
            )
          ).tx;
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
      try {
        assertCurrent(epoch);
        const context: SDK.DaAvailabilityOperationContext = {
          deploymentIdentity: input.identity.manifestId,
          actor,
          journal,
          stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
          minimumConfirmationDepth: 30,
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
        const hasChallenge =
          snapshots.some(
            ({ bondDatum }) =>
              bondDatum !== undefined && "ChallengedBond" in bondDatum,
          ) || observation.finalizedCorrectionLock?.datum !== "Idle";
        let selected:
          | {
              snapshot: SDK.DaAvailabilityChallengeSnapshot;
              action: WatcherAvailabilityAction;
            }
          | undefined;
        for (const snapshot of snapshots) {
          const available = await publicPayloadAvailable(snapshot);
          if (
            available &&
            snapshot.bondDatum !== undefined &&
            "Available" in snapshot.bondDatum
          )
            pending.delete(snapshot.headerHash);
          const action = selectWatcherAvailabilityAction(
            snapshot,
            available,
            validity().validFrom,
          );
          if (
            action === null &&
            snapshot.bondDatum !== undefined &&
            "ChallengedBond" in snapshot.bondDatum &&
            snapshot.bondDatum.ChallengedBond.response_deadline >
              validity().validFrom
          ) {
            await requireReachableRemovalCapital(observation);
          }
          if (
            action !== null &&
            !(action.action === "open" && hasChallenge) &&
            selected === undefined
          )
            selected = { snapshot, action };
        }
        assertCurrent(epoch);
        report =
          unresolved === undefined
            ? { phase: "ready", pendingHeaders: [...pending] }
            : {
                phase: unresolved.status === "conflict" ? "blocked" : "waiting",
                pendingHeaders: [...pending],
                txHash: unresolved.txHash,
              };
        if (!actuate || selected === undefined || unresolved !== undefined)
          return;
        const operation = await build(
          selected.snapshot,
          selected.action,
          observation,
        );
        const result = await SDK.runDaAvailabilityOperation(context, {
          headerHash: selected.snapshot.headerHash,
          ...operation,
        });
        report = {
          phase: result.status === "conflict" ? "blocked" : "waiting",
          pendingHeaders: [...pending],
          action: operation.action,
          txHash: result.txHash,
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
        if (epoch === generation && !closed)
          reportTransition(observation, startedAt);
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
    status: () => report,
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
