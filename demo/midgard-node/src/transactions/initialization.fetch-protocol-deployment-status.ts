import { existsSync } from "node:fs";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  LucidEvolution,
  type Network,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either, Schedule } from "effect";

import {
  configuredContractDeploymentInfoPath,
  readDeploymentManifestFile,
} from "../commands/contract-deployment-info.build-deployment-manifest.js";
import {
  defaultDeploymentRunStatePath,
  loadDeploymentRunState,
  recordedInitProtocolAttempts,
  recordInitProtocolSubmission,
} from "../e2e/run-state.js";
import { NodeConfig } from "../services/config.js";
import { type IntentJournal, openPlan } from "../services/intent-journal.js";
import { Lucid } from "../services/lucid.js";
import {
  type ContractDeploymentIdentityValue,
  MidgardContracts,
} from "../services/midgard-contracts.js";
import { loadRealBlueprint } from "../services/midgard-contracts.load-reference-script-auth-validator.js";
import { ensureAvailabilityChallengeRewardAccountsRegisteredProgram } from "./availability-challenge-registration.js";
import {
  type AtomicProtocolInitReferenceScripts,
  buildFraudProofCatalogueDeploymentInfo,
  DEPLOYMENT_VISIBILITY_REFRESH_DELAY,
  DEPLOYMENT_VISIBILITY_REFRESH_MAX_RETRIES,
  fraudProofsToIndexedValidators,
} from "./initialization.atomic-protocol-init-reference-scripts-from-publications.js";
import {
  deriveOperatorDaParams,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "./initialization.derive-operator-da-params.js";
import {
  completeAndSubmit,
  fetchConfiguredNonceUtxo,
  resolveDefaultDeploymentDeadline,
  resolveDeploymentValidityBounds,
} from "./initialization.fetch-configured-nonce-utxo.js";
import {
  hubOracleWitnessAmong,
  initOutputOutRefs,
} from "./initialization.fetch-hub-oracle-witness.js";
import { ensurePhasMembershipRewardAccountRegisteredProgram } from "./phas-membership-registration.js";
import {
  ensureEventHistoryRewardAccountsRegisteredProgram,
  ensureRuntimeRewardAccountsRegisteredProgram,
} from "./script-reward-registration.js";

export type ProtocolDeploymentKeys = {
  /** The configured hub-oracle one-shot nonce. */
  readonly nonce: { readonly txHash: string; readonly outputIndex: number };
  /**
   * Every initialization recorded for that nonce. One recorded without a
   * validity bound (the manifest's) never expires.
   */
  readonly initAttempts: readonly {
    readonly txHash: string;
    readonly invalidHereafterSlot?: number;
  }[];
};

/** What the deployment status reads through. */
export type ProtocolDeploymentReader = {
  readonly lucid: LucidEvolution;
  /** The slot of the point reads answer at now, or a later one. */
  readonly viewSlot: Effect.Effect<number, Error>;
  /** The hub-oracle policy, resolved only once the nonce is spent. */
  readonly hubOraclePolicyId: Effect.Effect<string, Error>;
};

export type ProtocolDeploymentStatus = {
  /**
   * `complete` once an output of a recorded initialization carries the
   * hub-oracle NFT; `submitted` while one is recorded, not visible, and
   * still valid; `empty` otherwise.
   */
  readonly status: "empty" | "submitted" | "complete";
  readonly nonce: "live" | "spent";
  readonly hubOracleWitness: UTxO | null;
  /**
   * Recorded initializations that never landed and whose validity bound the
   * read view has passed: no block can carry them any more.
   */
  readonly expiredInitTxHashes: readonly string[];
};

const outRefText = (outRef: ProtocolDeploymentKeys["nonce"]): string =>
  `${outRef.txHash}#${outRef.outputIndex.toString()}`;

/**
 * The hub-oracle policy of the deployment the nonce parameterizes, derived
 * from the blueprint without the rest of the contracts.
 */
export const hubOraclePolicyOfNonce = (
  nonce: ProtocolDeploymentKeys["nonce"],
): Effect.Effect<string, Error> =>
  Effect.gen(function* () {
    const blueprint = yield* loadRealBlueprint();
    return yield* Effect.try({
      try: () =>
        SDK.buildHubOracleMintingValidator({
          blueprint,
          oneShotOutRef: nonce,
        }).policyId,
      catch: (cause) =>
        new Error("Failed to derive the hub-oracle policy", { cause }),
    });
  });

/** A reader over the node's Lucid service and its loaded contracts. */
export const contractsDeploymentReader = (
  lucid: Lucid,
  contracts: SDK.MidgardValidators,
): ProtocolDeploymentReader => ({
  lucid: lucid.api,
  viewSlot: Effect.suspend(() => lucid.viewSlot()),
  hubOraclePolicyId: Effect.sync(() => contracts.hubOracle.policyId),
});

/**
 * Queries the deployment state of the protocol from the configured nonce and
 * the recorded initialization outputs, in one exact-outref read. The
 * hub-oracle output can never be spent and its policy is bound to the nonce,
 * so its presence at a recorded initialization output proves the atomic
 * deployment landed, whatever the protocol later spent. A recorded attempt
 * spends the nonce, so while the nonce is live none landed, and one whose
 * validity bound the view has passed never will.
 */
export const fetchProtocolDeploymentStatus = (
  reader: ProtocolDeploymentReader,
  keys: ProtocolDeploymentKeys,
): Effect.Effect<ProtocolDeploymentStatus, SDK.LucidError> =>
  Effect.gen(function* () {
    const readFailure = (message: string) => (cause: unknown) =>
      new SDK.LucidError({ message, cause });
    // Read before the outputs, so they answer at this slot or a later one.
    const viewSlot = keys.initAttempts.some(
      ({ invalidHereafterSlot }) => invalidHereafterSlot !== undefined,
    )
      ? yield* reader.viewSlot.pipe(
          Effect.mapError(readFailure("Failed to read the L1 view slot")),
        )
      : undefined;
    const initTxHashes = keys.initAttempts.map(({ txHash }) => txHash);
    const live = yield* Effect.tryPromise({
      try: () =>
        reader.lucid.utxosByOutRef([
          keys.nonce,
          ...initTxHashes.flatMap(initOutputOutRefs),
        ]),
      catch: readFailure("Failed to read the protocol deployment outputs"),
    });
    const nonceLive = live.some(
      (utxo) =>
        utxo.txHash === keys.nonce.txHash &&
        utxo.outputIndex === keys.nonce.outputIndex,
    );
    if (nonceLive) {
      const expired = keys.initAttempts.filter(
        ({ invalidHereafterSlot }) =>
          invalidHereafterSlot !== undefined &&
          viewSlot !== undefined &&
          viewSlot >= invalidHereafterSlot,
      );
      return {
        status:
          expired.length < keys.initAttempts.length ? "submitted" : "empty",
        nonce: "live",
        hubOracleWitness: null,
        expiredInitTxHashes: expired.map(({ txHash }) => txHash),
      };
    }
    const hubOracleWitness = hubOracleWitnessAmong(
      live,
      initTxHashes,
      yield* reader.hubOraclePolicyId.pipe(
        Effect.mapError(readFailure("Failed to resolve the hub-oracle policy")),
      ),
    );
    if (hubOracleWitness === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: `Hub-oracle nonce ${outRefText(keys.nonce)} is spent and no recorded initialization transaction is visible`,
          cause: `recorded_init_txs=[${initTxHashes.join(",")}]`,
        }),
      );
    }
    return {
      status: "complete",
      nonce: "spent",
      hubOracleWitness,
      expiredInitTxHashes: [],
    };
  });

/**
 * The configured deployment manifest when it belongs to `nonce`; null when
 * there is none or it cannot be read (its own verification reports that).
 */
const configuredManifestOfNonce = (nonce: ProtocolDeploymentKeys["nonce"]) => {
  const manifestPath = configuredContractDeploymentInfoPath();
  const manifest = existsSync(manifestPath)
    ? Either.getOrNull(
        Either.try(() => readDeploymentManifestFile(manifestPath)),
      )
    : null;
  return manifest !== null &&
    manifest.hubOracleOneShot.txHash === nonce.txHash &&
    manifest.hubOracleOneShot.outputIndex === nonce.outputIndex
    ? manifest
    : null;
};

/**
 * The configured nonce and the initialization transactions recorded for it:
 * the configured manifest's `initProtocol` transaction when that manifest
 * belongs to the nonce, and every attempt recorded in the run state.
 */
export const configuredProtocolDeploymentKeys = (
  runStatePath: string = defaultDeploymentRunStatePath(),
): Effect.Effect<ProtocolDeploymentKeys, SDK.LucidError, NodeConfig> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const nonce = {
      txHash: nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH,
      outputIndex: nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
    };
    const manifestInitTxHash =
      configuredManifestOfNonce(nonce)?.steps.initProtocol.txHash;
    const recorded = yield* Effect.tryPromise({
      try: async () =>
        recordedInitProtocolAttempts(
          await loadDeploymentRunState(runStatePath),
          outRefText(nonce),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to read the deployment run state at ${runStatePath}`,
          cause,
        }),
    });
    return {
      nonce,
      initAttempts: [
        ...(manifestInitTxHash === undefined ||
        recorded.some(({ txHash }) => txHash === manifestInitTxHash)
          ? []
          : [{ txHash: manifestInitTxHash }]),
        ...recorded,
      ],
    };
  });

const waitForAtomicInitializationVisibility = (
  reader: ProtocolDeploymentReader,
  keys: ProtocolDeploymentKeys,
): Effect.Effect<UTxO, SDK.LucidError> =>
  Effect.gen(function* () {
    const status = yield* fetchProtocolDeploymentStatus(reader, keys);
    if (status.hubOracleWitness !== null) {
      return status.hubOracleWitness;
    }
    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Atomic initialization transaction is confirmed but not yet fully visible through the provider",
        cause: `status=${status.status}`,
      }),
    );
  }).pipe(
    Effect.retry(
      Schedule.intersect(
        Schedule.fixed(DEPLOYMENT_VISIBILITY_REFRESH_DELAY),
        Schedule.recurs(DEPLOYMENT_VISIBILITY_REFRESH_MAX_RETRIES),
      ),
    ),
  );

export const buildAtomicProtocolInitTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  nodeConfig: {
    HUB_ORACLE_ONE_SHOT_TX_HASH: string;
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
    L1_OPERATOR_SEED_PHRASE: string;
    NETWORK: Network;
    DA_COMMITTEE_HEX?: string;
    DA_THRESHOLD?: bigint | null;
    DA_COSIGNER_SEED_PHRASE?: string;
    DA_OWNERS_HEX?: string;
  },
  fraudProofCatalogueMerkleRoot: string,
  validTo: bigint | undefined,
  referenceScripts: AtomicProtocolInitReferenceScripts,
  consensusProfile: ContractDeploymentIdentityValue["consensusProfile"] = MIDGARD_CONSENSUS_PROFILE,
): Effect.Effect<
  TxBuilder,
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.UnspecifiedNetworkError
  | SDK.HashingError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const validityRange = resolveDeploymentValidityBounds(lucid, validTo);
    const nonceUtxo = yield* fetchConfiguredNonceUtxo(lucid, nodeConfig);
    const daParams = yield* deriveOperatorDaParams(nodeConfig);
    return yield* SDK.incompleteInitializationTxProgram(lucid, {
      midgardValidators: contracts,
      consensusProfile,
      fraudProofCatalogueMerkleRoot,
      daParams,
      oneShotNonceUTxO: nonceUtxo,
      validityRange,
      referenceScripts,
    });
  });

/**
 * End-to-end protocol initialization program.
 *
 * The flow performs exactly one atomic bootstrap: canonical Init validators
 * depend on the hub-oracle NFT being minted in the same transaction as every
 * protocol root. Each attempt is recorded in the run state at
 * `runStatePath`, with its validity bound, before it is submitted. A recorded
 * attempt that is not visible is attempted again: every attempt spends the
 * same nonce, so at most one lands. Over a landed deployment it registers the
 * reward accounts and returns that deployment's initialization transaction.
 */
export const program = (
  runStatePath: string = defaultDeploymentRunStatePath(),
): Effect.Effect<
  string,
  unknown,
  Lucid | MidgardContracts | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;

    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;

    const indexedFraudProofs = fraudProofsToIndexedValidators(
      contracts.fraudProofs,
    );
    const fraudProofCatalogueDeploymentInfo =
      yield* buildFraudProofCatalogueDeploymentInfo(indexedFraudProofs);
    yield* Effect.logInfo(
      `Fraud proof catalogue root prepared for initialization: ${fraudProofCatalogueDeploymentInfo.root}`,
    );

    // S5: the plan opens before the deployment-status read the init rests on.
    const plan = yield* openPlan;
    const reader = contractsDeploymentReader(lucidService, contracts);
    const keys = yield* configuredProtocolDeploymentKeys(runStatePath);
    const status = yield* fetchProtocolDeploymentStatus(reader, keys);
    if (status.hubOracleWitness !== null) {
      yield* ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
        lucid,
        contracts,
      );
      const phasRegistration =
        yield* ensurePhasMembershipRewardAccountRegisteredProgram(lucid);
      yield* ensureRuntimeRewardAccountsRegisteredProgram(lucid, contracts);
      yield* Effect.logInfo(
        `PHAS membership reward-account registration status: status=${phasRegistration.status},scriptHash=${phasRegistration.scriptHash},rewardAddress=${phasRegistration.rewardAddress},txHash=${phasRegistration.txHash ?? "already-registered"}`,
      );
      return status.hubOracleWitness.txHash;
    }

    const referenceScripts =
      yield* ensureAtomicProtocolInitReferenceScriptsProgram(
        lucidService.referenceScriptsApi,
        contracts,
        lucid,
        lucidService.referenceScriptsAddress,
      );
    yield* ensureEventHistoryRewardAccountsRegisteredProgram(
      lucidService.referenceScriptsApi,
      contracts,
    );
    const initDeadline = resolveDefaultDeploymentDeadline(lucid);
    const txHash = yield* completeAndSubmit(
      lucid,
      yield* buildAtomicProtocolInitTxProgram(
        lucid,
        contracts,
        nodeConfig,
        fraudProofCatalogueDeploymentInfo.root,
        initDeadline,
        referenceScripts,
        contracts.consensusProfile,
      ),
      "Failed to build atomic real protocol initialization transaction",
      keys.nonce,
      plan,
      (attempt) =>
        Effect.tryPromise({
          try: () =>
            recordInitProtocolSubmission(runStatePath, {
              network: nodeConfig.NETWORK,
              nonce: outRefText(keys.nonce),
              attempt,
            }),
          catch: (cause) =>
            new SDK.LucidError({
              message: `Failed to record the initialization transaction in the run state at ${runStatePath}`,
              cause,
            }),
        }).pipe(Effect.asVoid),
    );
    yield* Effect.logInfo(
      `Atomic real protocol initialization submitted: txHash=${txHash}`,
    );
    yield* waitForAtomicInitializationVisibility(reader, {
      nonce: keys.nonce,
      initAttempts: [{ txHash }],
    });
    yield* ensureAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid,
      contracts,
    );
    const phasRegistration =
      yield* ensurePhasMembershipRewardAccountRegisteredProgram(lucid);
    yield* ensureRuntimeRewardAccountsRegisteredProgram(lucid, contracts);
    yield* Effect.logInfo(
      `PHAS membership reward-account registration status: status=${phasRegistration.status},scriptHash=${phasRegistration.scriptHash},rewardAddress=${phasRegistration.rewardAddress},txHash=${phasRegistration.txHash ?? "already-registered"}`,
    );
    return txHash;
  });
