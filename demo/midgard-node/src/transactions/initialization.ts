import { Effect, Schedule } from "effect";
import { Lucid } from "@/services/lucid.js";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import { NodeConfig } from "@/services/config.js";
import {
  fetchStateQueueTopologyProgram,
  type StateQueueTopology,
} from "@/services/state-queue-topology.js";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
  UTxO,
  LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { ensureNodeRuntimeReferenceScriptsProgram } from "@/transactions/reference-scripts.js";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "@/lucid-time.js";

import { MidgardMpt, MptError } from "@/workers/utils/mpt.js";
import { BatchDBOp } from "@ethereumjs/util";

/**
 * Deployment helpers for the protocol's initial on-chain contract state.
 *
 * Canonical real deployment is atomic: hub-oracle, scheduler, state-queue,
 * operator-set roots, and fraud-proof catalogue are minted in one transaction.
 */

/**
 * Converts a fraud-proof catalogue index into the fixed-width key used by the
 * catalogue MPT.
 */
export const uint32ToFraudProofID = (index: number): Buffer => {
  const buf = Buffer.alloc(4);
  buf.writeUInt32BE(index);
  return buf;
};

/**
 * Assigns deterministic integer keys to the fraud-proof validator set.
 */
export const fraudProofsToIndexedValidators = (
  fraudProofs: SDK.FraudProofs,
): [Buffer, SDK.SpendingValidator][] => {
  return Object.entries(fraudProofs).map(
    ([_fraudProofTitle, fraudProofValidator], i) => [
      uint32ToFraudProofID(i),
      fraudProofValidator,
    ],
  );
};

/**
 * Builds the Merkle Patricia Trie used as the fraud-proof catalogue root.
 */
export const createFraudProofCatalogueMpt = (
  indexedFraudProofs: [Buffer, SDK.SpendingValidator][],
): Effect.Effect<MidgardMpt, MptError> =>
  Effect.gen(function* () {
    const batchOps = indexedFraudProofs.map(
      ([i, fraudProofValidator]): BatchDBOp => ({
        type: "put",
        key: i,
        value: Buffer.from(fraudProofValidator.spendingScriptHash, "hex"),
      }),
    );
    const trie = yield* MidgardMpt.create("fraud_proof_catalogue");
    yield* trie.batch(batchOps);
    return trie;
  });

/**
 * Formats a UTxO as `txHash#outputIndex` for diagnostics.
 */
export const outRefLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex}`;
const DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS = 7n * 60n * 1000n;
const DEPLOYMENT_VISIBILITY_REFRESH_MAX_RETRIES = 12;
const DEPLOYMENT_VISIBILITY_REFRESH_DELAY = "2 seconds";

export type AtomicProtocolInitReferenceScripts =
  SDK.AtomicProtocolInitReferenceScripts;

type ReferenceScriptPublicationLike = {
  readonly name: string;
  readonly utxo: UTxO;
};

const requireReferenceScriptPublication = (
  publications: readonly ReferenceScriptPublicationLike[],
  name: string,
): UTxO => {
  const publication = publications.find((candidate) => candidate.name === name);
  if (publication === undefined) {
    throw new Error(`Missing published reference script ${name}`);
  }
  return publication.utxo;
};

export const atomicProtocolInitReferenceScriptsFromPublications = (
  publications: readonly ReferenceScriptPublicationLike[],
): AtomicProtocolInitReferenceScripts => ({
  hubOracleMinting: requireReferenceScriptPublication(
    publications,
    "hub-oracle minting",
  ),
  schedulerMinting: requireReferenceScriptPublication(
    publications,
    "scheduler minting",
  ),
  stateQueueMinting: requireReferenceScriptPublication(
    publications,
    "state-queue minting",
  ),
  registeredOperatorsMinting: requireReferenceScriptPublication(
    publications,
    "registered-operators minting",
  ),
  activeOperatorsMinting: requireReferenceScriptPublication(
    publications,
    "active-operators minting",
  ),
  retiredOperatorsMinting: requireReferenceScriptPublication(
    publications,
    "retired-operators minting",
  ),
  fraudProofCatalogueMinting: requireReferenceScriptPublication(
    publications,
    "fraud-proof-catalogue minting",
  ),
});

export const ensureAtomicProtocolInitReferenceScriptsProgram = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
): Effect.Effect<
  AtomicProtocolInitReferenceScripts,
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  ensureNodeRuntimeReferenceScriptsProgram(
    referenceScriptsLucid,
    contracts,
    fundingLucid,
  ).pipe(Effect.map(atomicProtocolInitReferenceScriptsFromPublications));

/**
 * Fetches the hub-oracle witness UTxO if it exists.
 */
export const fetchHubOracleWitness = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<UTxO | null, SDK.LucidError> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Failed to resolve network for hub-oracle witness lookup",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const hubOracleAddress = credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    );
    const hubOracleUnit = toUnit(
      contracts.hubOracle.policyId,
      SDK.HUB_ORACLE_ASSET_NAME,
    );
    const utxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAtWithUnit(hubOracleAddress, hubOracleUnit),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch hub-oracle witness UTxO(s)",
          cause,
        }),
    });
    if (utxos.length > 1) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Expected at most one hub-oracle witness UTxO",
          cause: utxos.map((utxo) => outRefLabel(utxo)).join(","),
        }),
      );
    }
    return utxos[0] ?? null;
  });

/**
 * Returns whether a node-set validator already has at least one initialized
 * on-chain UTxO.
 */
export const isNodeSetInitialized = (
  lucid: LucidEvolution,
  validator: SDK.AuthenticatedValidator,
): Effect.Effect<boolean, SDK.LucidError> =>
  SDK.utxosAtByNFTPolicyId(
    lucid,
    validator.spendingScriptAddress,
    validator.policyId,
  ).pipe(
    Effect.map((utxos) => utxos.length > 0),
    Effect.mapError(
      (cause) =>
        new SDK.LucidError({
          message: `Failed to query node-set initialization for policy=${validator.policyId}`,
          cause,
        }),
    ),
  );

/**
 * Returns whether the scheduler witness UTxO is already present on-chain.
 */
export const isSchedulerInitialized = (
  lucid: LucidEvolution,
  scheduler: SDK.AuthenticatedValidator,
): Effect.Effect<boolean, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () => {
      const schedulerUnit = toUnit(scheduler.policyId, SDK.SCHEDULER_ASSET_NAME);
      const schedulerUtxos = await lucid.utxosAtWithUnit(
        scheduler.spendingScriptAddress,
        schedulerUnit,
      );
      return schedulerUtxos.length > 0;
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to query scheduler initialization state",
        cause,
      }),
  });

/**
 * Resolves the configured one-shot hub-oracle nonce UTxO from the operator
 * wallet.
 */
export const fetchConfiguredNonceUtxo = (
  lucid: LucidEvolution,
  nodeConfig: {
    HUB_ORACLE_ONE_SHOT_TX_HASH: string;
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: number;
  },
): Effect.Effect<UTxO, SDK.LucidError> =>
  Effect.gen(function* () {
    const walletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch operator wallet UTxOs for initialization",
          cause,
        }),
    });
    const configuredNonceUtxoLabel = `${nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH}#${nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX}`;
    const nonceUtxo = walletUtxos.find(
      (utxo) =>
        utxo.txHash === nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH &&
        utxo.outputIndex === nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX,
    );
    if (nonceUtxo === undefined) {
      const availableWalletUtxos = walletUtxos.map((utxo) => outRefLabel(utxo));
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Configured one-shot hub oracle UTxO is not available in the operator wallet",
          cause: `required=${configuredNonceUtxoLabel}, available=[${availableWalletUtxos.join(", ")}]`,
        }),
      );
    }
    return nonceUtxo;
  });

/**
 * Completes, signs, and submits a transaction builder with local UPLC
 * evaluation enforced.
 */
export const completeAndSubmit = (
  lucid: LucidEvolution,
  txBuilder: any,
  failureMessage: string,
): Effect.Effect<
  string,
  SDK.LucidError | TxConfirmError | TxSignError | TxSubmitError
> =>
  Effect.gen(function* () {
    const unsignedTx = yield* Effect.tryPromise({
      try: () => txBuilder.complete({ localUPLCEval: true }),
      catch: (cause) =>
        new SDK.LucidError({
          message: `${failureMessage}: ${cause}`,
          cause,
        }),
    });
    return yield* handleSignSubmit(lucid, unsignedTx as TxSignBuilder);
  });

/**
 * Produces a conservative default validity start time for deployment
 * transactions.
 */
const resolveDeploymentStartTime = (lucid?: LucidEvolution): bigint => {
  if (lucid !== undefined && lucid.config().network === "Custom") {
    const provider = lucid.config().provider as {
      time?: number;
    };
    if (typeof provider.time === "number") {
      return BigInt(provider.time);
    }
  }
  return BigInt(Date.now());
};

const resolveDefaultDeploymentDeadline = (lucid?: LucidEvolution): bigint => {
  const targetTime = Number(
    resolveDeploymentStartTime(lucid) + DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS,
  );
  if (lucid === undefined) {
    return BigInt(targetTime);
  }
  const targetSlot = lucid.unixTimeToSlot(targetTime);
  const alignedTime = slotToUnixTimeForLucidOrEmulatorFallback(
    lucid,
    targetSlot,
  );
  if (alignedTime >= targetTime) {
    return BigInt(alignedTime);
  }
  return BigInt(
    slotToUnixTimeForLucidOrEmulatorFallback(lucid, targetSlot + 1),
  );
};

const resolveDeploymentValidityBounds = (
  lucid?: LucidEvolution,
  validTo?: bigint,
): { validFrom: bigint; validTo: bigint } => {
  if (validTo !== undefined) {
    return {
      validFrom: validTo - DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS,
      validTo,
    };
  }
  const upperBound = resolveDefaultDeploymentDeadline(lucid);
  return {
    validFrom: resolveDeploymentStartTime(lucid),
    validTo: upperBound,
  };
};

const makePartialProtocolDeploymentError = (
  status: ProtocolDeploymentStatus,
): SDK.LucidError =>
  new SDK.LucidError({
    message:
      "Real protocol deployment is partial and cannot be completed in-place",
    cause: `missing_components=[${status.missingComponents.join(",")}]; the canonical Init validators require one atomic bootstrap transaction that mints the hub-oracle NFT and protocol root NFTs together; use a fresh one-shot hub-oracle nonce/deployment`,
  });

export type ProtocolDeploymentStatus = {
  readonly hubOracleWitness: UTxO | null;
  readonly stateQueueTopology: StateQueueTopology;
  readonly schedulerInitialized: boolean;
  readonly registeredOperatorsInitialized: boolean;
  readonly activeOperatorsInitialized: boolean;
  readonly retiredOperatorsInitialized: boolean;
  readonly fraudProofCatalogueInitialized: boolean;
  readonly complete: boolean;
  readonly empty: boolean;
  readonly missingComponents: readonly string[];
};

/**
 * Queries the current deployment state of the protocol contracts.
 */
export const fetchProtocolDeploymentStatus = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<ProtocolDeploymentStatus, SDK.LucidError> =>
  Effect.gen(function* () {
    const hubOracleWitness = yield* fetchHubOracleWitness(lucid, contracts);
    const stateQueueTopology = yield* fetchStateQueueTopologyProgram(
      lucid,
      contracts.stateQueue,
    );
    const schedulerInitialized = yield* isSchedulerInitialized(
      lucid,
      contracts.scheduler,
    );
    const registeredOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.registeredOperators,
    );
    const activeOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.activeOperators,
    );
    const retiredOperatorsInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.retiredOperators,
    );
    const fraudProofCatalogueInitialized = yield* isNodeSetInitialized(
      lucid,
      contracts.fraudProofCatalogue,
    );
    const missingComponents = [
      ...(hubOracleWitness === null ? ["hub-oracle"] : []),
      ...(!stateQueueTopology.initialized ? ["state-queue"] : []),
      ...(!schedulerInitialized ? ["scheduler"] : []),
      ...(!registeredOperatorsInitialized ? ["registered-operators"] : []),
      ...(!activeOperatorsInitialized ? ["active-operators"] : []),
      ...(!retiredOperatorsInitialized ? ["retired-operators"] : []),
      ...(!fraudProofCatalogueInitialized ? ["fraud-proof-catalogue"] : []),
    ] as const;
    const complete =
      hubOracleWitness !== null &&
      stateQueueTopology.initialized &&
      stateQueueTopology.healthy &&
      schedulerInitialized &&
      registeredOperatorsInitialized &&
      activeOperatorsInitialized &&
      retiredOperatorsInitialized &&
      fraudProofCatalogueInitialized;
    const empty =
      hubOracleWitness === null &&
      !stateQueueTopology.initialized &&
      !schedulerInitialized &&
      !registeredOperatorsInitialized &&
      !activeOperatorsInitialized &&
      !retiredOperatorsInitialized &&
      !fraudProofCatalogueInitialized;

    return {
      hubOracleWitness,
      stateQueueTopology,
      schedulerInitialized,
      registeredOperatorsInitialized,
      activeOperatorsInitialized,
      retiredOperatorsInitialized,
      fraudProofCatalogueInitialized,
      complete,
      empty,
      missingComponents,
    };
  });

const waitForAtomicInitializationVisibility = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<ProtocolDeploymentStatus, SDK.LucidError> =>
  Effect.gen(function* () {
    const status = yield* fetchProtocolDeploymentStatus(lucid, contracts);
    if (status.complete) {
      return status;
    }
    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Atomic initialization transaction is confirmed but not yet fully visible through the provider",
        cause: `missing_components=[${status.missingComponents.join(",")}]`,
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
  },
  fraudProofCatalogueMerkleRoot: string,
  validTo?: bigint,
  referenceScripts?: AtomicProtocolInitReferenceScripts,
): Effect.Effect<
  TxBuilder,
  SDK.LucidError | SDK.Bech32DeserializationError | SDK.UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const validityRange = resolveDeploymentValidityBounds(lucid, validTo);
    const nonceUtxo = yield* fetchConfiguredNonceUtxo(lucid, nodeConfig);
    return yield* SDK.unsignedInitializationTxProgram(lucid, {
      midgardValidators: contracts,
      fraudProofCatalogueMerkleRoot,
      oneShotNonceUTxO: nonceUtxo,
      validityRange,
      referenceScripts,
    });
  });

/**
 * End-to-end protocol initialization program.
 *
 * The flow performs exactly one atomic bootstrap. Partial real deployment is
 * fatal because canonical Init validators depend on the hub-oracle NFT being
 * minted in the same transaction as every protocol root.
 */
export const program: Effect.Effect<
  string,
  unknown,
  Lucid | MidgardContracts | NodeConfig
> = Effect.gen(function* () {
  const lucidService = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;

  yield* lucidService.switchToOperatorsMainWallet;
  const lucid = lucidService.api;

  const indexedFraudProofs = fraudProofsToIndexedValidators(
    contracts.fraudProofs,
  );
  const fpMPT = yield* createFraudProofCatalogueMpt(indexedFraudProofs);
  const fraudProofCatalogueMerkleRoot = yield* fpMPT.getRootHex();
  yield* Effect.logInfo(
    `Fraud proof catalogue root prepared for initialization: ${fraudProofCatalogueMerkleRoot}`,
  );

  const status = yield* fetchProtocolDeploymentStatus(lucid, contracts);
  if (status.complete) {
    return "already-initialized";
  }

  if (!status.empty) {
    return yield* Effect.fail(makePartialProtocolDeploymentError(status));
  }

  const referenceScripts = yield* ensureAtomicProtocolInitReferenceScriptsProgram(
    lucidService.referenceScriptsApi,
    contracts,
    lucid,
  );
  const initDeadline = resolveDefaultDeploymentDeadline(lucid);
  const txHash = yield* completeAndSubmit(
    lucid,
    yield* buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      nodeConfig,
      fraudProofCatalogueMerkleRoot,
      initDeadline,
      referenceScripts,
    ),
    "Failed to build atomic real protocol initialization transaction",
  );
  yield* Effect.logInfo(
    `Atomic real protocol initialization submitted: txHash=${txHash}`,
  );
  yield* waitForAtomicInitializationVisibility(lucid, contracts);
  return txHash;
});
