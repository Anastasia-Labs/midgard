import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data as LucidData,
  LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  UTxO,
  validatorToScriptHash,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { slotToUnixTimeForLucidOrEmulatorFallback } from "@/lucid-time.js";
import {
  loadPhasMembershipWithdrawalScript,
  phasMembershipRewardAddress,
} from "@/phas-membership.js";
import { NodeConfig } from "@/services/config.js";
import { Lucid } from "@/services/lucid.js";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import {
  fetchStateQueueTopologyProgram,
  type StateQueueTopology,
} from "@/services/state-queue-topology.js";
import { ensurePhasMembershipRewardAccountRegisteredProgram } from "@/transactions/phas-membership-registration.js";
import { ensureNodeRuntimeReferenceScriptsProgram } from "@/transactions/reference-scripts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import { outRefLabel } from "@/tx-context.js";
import { MidgardMpf, MpfBatchOp, MpfError } from "@/workers/utils/mpf.js";

/**
 * Deployment helpers for the protocol's initial on-chain contract state.
 *
 * Canonical real deployment is atomic: hub-oracle, scheduler, state-queue,
 * operator-set roots, and fraud-proof catalogue are minted in one transaction.
 */

/**
 * Converts a fraud-proof catalogue index into the fixed-width key used by the
 * catalogue MPF.
 */
export const uint32ToFraudProofID = (index: number): Buffer => {
  const buf = Buffer.alloc(4);
  buf.writeUInt32BE(index);
  return buf;
};

const FraudProofCatalogueIdSchema = LucidData.Bytes({
  minLength: SDK.FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: SDK.FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});
type LucidDataSchema = Parameters<typeof LucidData.to>[1];

type IndexedFraudProof = readonly [
  categoryId: Buffer,
  validator: SDK.SpendingValidator,
  categoryName: SDK.FraudProofCatalogueCategoryName,
];

/**
 * Assigns deterministic integer keys to the fraud-proof validator set.
 */
export const fraudProofsToIndexedValidators = (
  fraudProofs: SDK.FraudProofs,
): IndexedFraudProof[] => {
  return SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((fraudProofTitle, i) => [
    uint32ToFraudProofID(i),
    fraudProofs[fraudProofTitle],
    fraudProofTitle,
  ]);
};

const encodeFraudProofCatalogueKey = (categoryId: Buffer): Buffer =>
  Buffer.from(
    LucidData.to(
      categoryId.toString("hex"),
      FraudProofCatalogueIdSchema as unknown as LucidDataSchema,
    ),
    "hex",
  );

const encodeFraudProofCatalogueValue = (
  fraudProofValidator: SDK.SpendingValidator,
): Buffer =>
  Buffer.from(
    LucidData.to(
      fraudProofValidator.spendingScriptHash,
      SDK.ScriptHashSchema as unknown as LucidDataSchema,
    ),
    "hex",
  );

/**
 * Builds the Merkle Patricia Forestry root used as the fraud-proof catalogue.
 */
export const createFraudProofCatalogueMpf = (
  indexedFraudProofs: readonly IndexedFraudProof[],
): Effect.Effect<MidgardMpf, MpfError> =>
  Effect.gen(function* () {
    const batchOps = indexedFraudProofs.map(
      ([i, fraudProofValidator]): MpfBatchOp => ({
        type: "insert",
        key: encodeFraudProofCatalogueKey(i),
        value: encodeFraudProofCatalogueValue(fraudProofValidator),
      }),
    );
    const mpf = yield* MidgardMpf.createScratch("fraud_proof_catalogue");
    yield* mpf.applyBatch(batchOps);
    return mpf;
  });

export const buildFraudProofCatalogueDeploymentInfo = (
  indexedFraudProofs: readonly IndexedFraudProof[],
): Effect.Effect<SDK.FraudProofCatalogueDeploymentInfo, MpfError> =>
  Effect.gen(function* () {
    const mpf = yield* createFraudProofCatalogueMpf(indexedFraudProofs);
    const root = yield* mpf.rootHex();
    const categories: Partial<
      Record<
        SDK.FraudProofCatalogueCategoryName,
        SDK.FraudProofCatalogueCategoryDeploymentInfo
      >
    > = {};

    for (const [categoryId, validator, categoryName] of indexedFraudProofs) {
      const key = encodeFraudProofCatalogueKey(categoryId);
      const proof = yield* mpf.prove(key);
      categories[categoryName] = {
        categoryId: categoryId.toString("hex"),
        scriptHash: validator.spendingScriptHash,
        membershipProofCbor: proof.cbor.toString("hex"),
      };
    }

    return {
      root,
      categories:
        categories as SDK.FraudProofCatalogueDeploymentInfo["categories"],
    };
  });

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
  daParamsGovernorMinting: requireReferenceScriptPublication(
    publications,
    "da-params-governor minting",
  ),
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

export const deriveOperatorDaParams = (
  nodeConfig: {
    readonly L1_OPERATOR_SEED_PHRASE: string;
    readonly NETWORK: Network;
    readonly DA_COMMITTEE_HEX?: string;
    readonly DA_THRESHOLD?: bigint | null;
  },
): Effect.Effect<SDK.DaParamsDatum, SDK.HashingError> =>
  Effect.gen(function* () {
    const wallet = walletFromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE, {
      network: nodeConfig.NETWORK,
    });
    const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
    const publicKey = privateKey.to_public();
    const configuredCommittee = (nodeConfig.DA_COMMITTEE_HEX ?? "").trim();
    const committee =
      configuredCommittee.length > 0
        ? yield* validateConfiguredDaCommittee(configuredCommittee)
        : Buffer.from(publicKey.to_raw_bytes()).toString("hex");
    const committeeLength = BigInt(committee.length / 64);
    const daThreshold = nodeConfig.DA_THRESHOLD ?? 1n;
    if (daThreshold <= 0n || daThreshold > committeeLength) {
      return yield* Effect.fail(
        new SDK.HashingError({
          message: "Invalid DA threshold configuration",
          cause: `threshold=${daThreshold.toString()},committee_members=${committeeLength.toString()}`,
        }),
      );
    }
    return {
      committee,
      committee_signers_hash: yield* SDK.hashHexWithBlake2b(committee, 32),
      da_threshold: daThreshold,
      owners: [publicKey.hash().to_hex()],
      update_threshold: 1n,
    };
  });

const validateConfiguredDaCommittee = (
  committee: string,
): Effect.Effect<string, SDK.HashingError> =>
  Effect.try({
    try: () => {
      const normalized = committee.trim().toLowerCase();
      if (!/^[0-9a-f]*$/.test(normalized) || normalized.length % 64 !== 0) {
        throw new Error(
          "DA_COMMITTEE_HEX must be packed 32-byte verification keys as hex",
        );
      }
      if (normalized.length === 0) {
        throw new Error("DA_COMMITTEE_HEX cannot be empty when configured");
      }
      return normalized;
    },
    catch: (cause) =>
      new SDK.HashingError({
        message: "Invalid DA committee configuration",
        cause,
      }),
  });

export const ensureAtomicProtocolInitReferenceScriptsProgram = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
  referenceScriptsAddress?: string,
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
    contracts.referenceScriptAuth,
    fundingLucid,
    referenceScriptsAddress,
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
      const schedulerUnit = toUnit(
        scheduler.policyId,
        SDK.SCHEDULER_ASSET_NAME,
      );
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
 * Returns whether the canonical DA params UTxO is already present on-chain.
 */
export const isDaParamsInitialized = (
  lucid: LucidEvolution,
  daParamsGovernor: SDK.AuthenticatedValidator,
): Effect.Effect<boolean, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () => {
      const daParamsUtxos = await lucid.utxosAtWithUnit(
        daParamsGovernor.spendingScriptAddress,
        SDK.daParamsUnit(daParamsGovernor),
      );
      return daParamsUtxos.length > 0;
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to query DA params initialization state",
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
    L1_OPERATOR_SEED_PHRASE: string;
    NETWORK: Network;
    DA_COMMITTEE_HEX?: string;
    DA_THRESHOLD?: bigint | null;
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
  readonly daParamsInitialized: boolean;
  readonly schedulerInitialized: boolean;
  readonly registeredOperatorsInitialized: boolean;
  readonly activeOperatorsInitialized: boolean;
  readonly retiredOperatorsInitialized: boolean;
  readonly fraudProofCatalogueInitialized: boolean;
  readonly phasMembershipRewardAddress: string;
  readonly phasMembershipScriptHash: string;
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
    const daParamsInitialized = yield* isDaParamsInitialized(
      lucid,
      contracts.daParamsGovernor,
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
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Failed to resolve network while building PHAS deployment identity",
          cause: "lucid.config().network is undefined",
        }),
      );
    }
    const phasMembershipScript = loadPhasMembershipWithdrawalScript();
    const phasMembershipReward = {
      rewardAddress: phasMembershipRewardAddress(network, phasMembershipScript),
      scriptHash: validatorToScriptHash(phasMembershipScript),
    };
    const missingComponents = [
      ...(hubOracleWitness === null ? ["hub-oracle"] : []),
      ...(!daParamsInitialized ? ["da-params"] : []),
      ...(!stateQueueTopology.initialized ? ["state-queue"] : []),
      ...(!schedulerInitialized ? ["scheduler"] : []),
      ...(!registeredOperatorsInitialized ? ["registered-operators"] : []),
      ...(!activeOperatorsInitialized ? ["active-operators"] : []),
      ...(!retiredOperatorsInitialized ? ["retired-operators"] : []),
      ...(!fraudProofCatalogueInitialized ? ["fraud-proof-catalogue"] : []),
    ] as const;
    const complete =
      hubOracleWitness !== null &&
      daParamsInitialized &&
      stateQueueTopology.initialized &&
      stateQueueTopology.healthy &&
      schedulerInitialized &&
      registeredOperatorsInitialized &&
      activeOperatorsInitialized &&
      retiredOperatorsInitialized &&
      fraudProofCatalogueInitialized;
    const empty =
      hubOracleWitness === null &&
      !daParamsInitialized &&
      !stateQueueTopology.initialized &&
      !schedulerInitialized &&
      !registeredOperatorsInitialized &&
      !activeOperatorsInitialized &&
      !retiredOperatorsInitialized &&
      !fraudProofCatalogueInitialized;

    return {
      hubOracleWitness,
      stateQueueTopology,
      daParamsInitialized,
      schedulerInitialized,
      registeredOperatorsInitialized,
      activeOperatorsInitialized,
      retiredOperatorsInitialized,
      fraudProofCatalogueInitialized,
      phasMembershipRewardAddress: phasMembershipReward.rewardAddress,
      phasMembershipScriptHash: phasMembershipReward.scriptHash,
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
    L1_OPERATOR_SEED_PHRASE: string;
    NETWORK: Network;
  },
  fraudProofCatalogueMerkleRoot: string,
  validTo?: bigint,
  referenceScripts?: AtomicProtocolInitReferenceScripts,
): Effect.Effect<
  TxBuilder,
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.UnspecifiedNetworkError
  | SDK.HashingError
> =>
  Effect.gen(function* () {
    const validityRange = resolveDeploymentValidityBounds(lucid, validTo);
    const nonceUtxo = yield* fetchConfiguredNonceUtxo(lucid, nodeConfig);
    const daParams = yield* deriveOperatorDaParams(nodeConfig);
    return yield* SDK.incompleteInitializationTxProgram(lucid, {
      midgardValidators: contracts,
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
  const fraudProofCatalogueDeploymentInfo =
    yield* buildFraudProofCatalogueDeploymentInfo(indexedFraudProofs);
  yield* Effect.logInfo(
    `Fraud proof catalogue root prepared for initialization: ${fraudProofCatalogueDeploymentInfo.root}`,
  );

  const status = yield* fetchProtocolDeploymentStatus(lucid, contracts);
  if (status.complete) {
    return "already-initialized";
  }

  if (!status.empty) {
    return yield* Effect.fail(makePartialProtocolDeploymentError(status));
  }

  const referenceScripts =
    yield* ensureAtomicProtocolInitReferenceScriptsProgram(
      lucidService.referenceScriptsApi,
      contracts,
      lucid,
      lucidService.referenceScriptsAddress,
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
    ),
    "Failed to build atomic real protocol initialization transaction",
  );
  yield* Effect.logInfo(
    `Atomic real protocol initialization submitted: txHash=${txHash}`,
  );
  yield* waitForAtomicInitializationVisibility(lucid, contracts);
  const phasRegistration =
    yield* ensurePhasMembershipRewardAccountRegisteredProgram(lucid);
  yield* Effect.logInfo(
    `PHAS membership reward-account registration status: status=${phasRegistration.status},scriptHash=${phasRegistration.scriptHash},rewardAddress=${phasRegistration.rewardAddress},txHash=${phasRegistration.txHash ?? "already-registered"}`,
  );
  return txHash;
});
