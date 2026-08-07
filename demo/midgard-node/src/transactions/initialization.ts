import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data as LucidData,
  LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import {
  type DaLocalSigner,
  daLocalSigners,
  isStrictlyAscending,
  splitPackedHex,
  VERIFICATION_KEY_HASH_HEX_LENGTH,
  VERIFICATION_KEY_HEX_LENGTH,
} from "@/da/local-signers.js";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "@/lucid-time.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import { NodeConfig } from "@/services/config.js";
import { Lucid } from "@/services/lucid.js";
import {
  type ContractDeploymentIdentityValue,
  MidgardContracts,
} from "@/services/midgard-contracts.js";
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

type IndexedFraudProof<
  CategoryName extends
    SDK.FraudProofCatalogueCategoryName = SDK.FraudProofCatalogueCategoryName,
> = readonly [
  categoryId: Buffer,
  validator: SDK.SpendingValidator,
  categoryName: CategoryName,
];

/**
 * Assigns deterministic integer keys to the fraud-proof validator set.
 */
export const fraudProofsToIndexedValidators = (
  fraudProofs: SDK.FraudProofs,
): IndexedFraudProof<SDK.FraudProofCatalogueCategoryName>[] => {
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
export const createFraudProofCatalogueMpf = <
  CategoryName extends SDK.FraudProofCatalogueCategoryName,
>(
  indexedFraudProofs: readonly IndexedFraudProof<CategoryName>[],
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

export const buildFraudProofCatalogueDeploymentInfo = <
  CategoryName extends SDK.FraudProofCatalogueCategoryName,
>(
  indexedFraudProofs: readonly IndexedFraudProof<CategoryName>[],
): Effect.Effect<
  SDK.FraudProofCatalogueDeploymentInfo<CategoryName>,
  MpfError
> =>
  Effect.gen(function* () {
    const mpf = yield* createFraudProofCatalogueMpf(indexedFraudProofs);
    const root = yield* mpf.rootHex();
    const categories: Partial<
      Record<CategoryName, SDK.FraudProofCatalogueCategoryDeploymentInfo>
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
        categories as SDK.FraudProofCatalogueDeploymentInfo<CategoryName>["categories"],
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

/**
 * Builds the `DaParamsDatum` this node writes at protocol initialization.
 *
 * Q63 (F04 §4) gave the governor threshold floors — `da_threshold >= max(2,
 * ceil(2*committee_len/3))` and `update_threshold >= max(2,
 * ceil(2*owner_len/3))` — which make a 1-of-1 committee and a lone owner
 * unrepresentable. Every bound below is evaluated by the SDK twins
 * (`SDK.governedThresholdFloor`, `SDK.daParamsFloorViolations`) so the
 * arithmetic lives in exactly one place and cannot drift from the validator.
 *
 * Both sets are also emitted sorted-unique, because `valid_datum` measures
 * `committee` and `owners` with its `sorted_unique_*` walkers, which fail the
 * script outright on an out-of-order or duplicate element.
 *
 * The function fails closed. There is no fallback to the operator's own single
 * key: a deployment that has not been told about a second committee member and
 * a second owner cannot produce params the governor accepts, and saying so here
 * is far cheaper than a cryptic on-chain script failure.
 */
export const deriveOperatorDaParams = (nodeConfig: {
  readonly L1_OPERATOR_SEED_PHRASE: string;
  readonly NETWORK: Network;
  readonly DA_COMMITTEE_HEX?: string;
  readonly DA_THRESHOLD?: bigint | null;
  readonly DA_COSIGNER_SEED_PHRASE?: string;
  readonly DA_OWNERS_HEX?: string;
}): Effect.Effect<SDK.DaParamsDatum, SDK.HashingError> =>
  Effect.gen(function* () {
    // Seed decoding is the one step here that can throw on caller-supplied
    // input, so it is surfaced as this function's declared error rather than as
    // an unhandled defect.
    const signers = yield* Effect.try({
      try: () => daLocalSigners(nodeConfig),
      catch: (cause) =>
        new SDK.HashingError({
          message: "Invalid DA signer configuration",
          cause,
        }),
    });
    const committee = yield* resolveDaCommittee(nodeConfig, signers);
    const committeeLength = committee.length / VERIFICATION_KEY_HEX_LENGTH;
    const owners = yield* resolveDaOwners(nodeConfig, signers);
    const daThreshold =
      nodeConfig.DA_THRESHOLD ??
      BigInt(SDK.governedThresholdFloor(committeeLength));
    const updateThreshold = BigInt(SDK.governedThresholdFloor(owners.length));

    const violations = SDK.daParamsFloorViolations({
      committeeLength,
      daThreshold: Number(daThreshold),
      ownerCount: owners.length,
      updateThreshold: Number(updateThreshold),
    });
    if (violations.length > 0) {
      return yield* Effect.fail(
        new SDK.HashingError({
          message: "Invalid DA threshold configuration",
          cause:
            `${violations.join(",")}; ` +
            `threshold=${daThreshold.toString()},committee_members=${committeeLength.toString()},` +
            `update_threshold=${updateThreshold.toString()},owners=${owners.length.toString()}`,
        }),
      );
    }

    return {
      committee,
      committee_signers_hash: yield* SDK.hashHexWithBlake2b(committee, 32),
      da_threshold: daThreshold,
      owners,
      update_threshold: updateThreshold,
    };
  });

const resolveDaCommittee = (
  nodeConfig: {
    readonly DA_COMMITTEE_HEX?: string;
  },
  signers: readonly DaLocalSigner[],
): Effect.Effect<string, SDK.HashingError> =>
  Effect.try({
    try: () => {
      const configured = (nodeConfig.DA_COMMITTEE_HEX ?? "").trim();
      if (configured.length > 0) {
        return validatedPackedSet(
          configured,
          VERIFICATION_KEY_HEX_LENGTH,
          "DA_COMMITTEE_HEX",
          "packed 32-byte verification keys",
        ).join("");
      }
      const keys = [
        ...new Set(signers.map((signer) => signer.verificationKeyHex)),
      ].sort();
      if (keys.length < SDK.MIN_DA_OWNER_COUNT) {
        throw new Error(
          `DA committee needs at least ${SDK.MIN_DA_OWNER_COUNT.toString()} members to satisfy the governed threshold floor; ` +
            "set DA_COMMITTEE_HEX to the packed committee, or set DA_COSIGNER_SEED_PHRASE to a second locally held key",
        );
      }
      return keys.join("");
    },
    catch: (cause) =>
      new SDK.HashingError({
        message: "Invalid DA committee configuration",
        cause,
      }),
  });

const resolveDaOwners = (
  nodeConfig: {
    readonly DA_OWNERS_HEX?: string;
  },
  signers: readonly DaLocalSigner[],
): Effect.Effect<string[], SDK.HashingError> =>
  Effect.try({
    try: () => {
      const configured = (nodeConfig.DA_OWNERS_HEX ?? "").trim();
      if (configured.length > 0) {
        return validatedPackedSet(
          configured,
          VERIFICATION_KEY_HASH_HEX_LENGTH,
          "DA_OWNERS_HEX",
          "packed 28-byte payment key hashes",
        );
      }
      const hashes = [
        ...new Set(signers.map((signer) => signer.keyHashHex)),
      ].sort();
      if (hashes.length < SDK.MIN_DA_OWNER_COUNT) {
        throw new Error(
          `DA owner set needs at least ${SDK.MIN_DA_OWNER_COUNT.toString()} members to satisfy the governed threshold floor; ` +
            "set DA_OWNERS_HEX to the packed owner key hashes, or set DA_COSIGNER_SEED_PHRASE to a second locally held key",
        );
      }
      return hashes;
    },
    catch: (cause) =>
      new SDK.HashingError({
        message: "Invalid DA owner configuration",
        cause,
      }),
  });

/**
 * Parses a packed hex set and enforces what `valid_datum` needs of it: correct
 * element width, strictly ascending order (its sorted-unique encoding), and at
 * least `MIN_DA_OWNER_COUNT` elements so a governed threshold floor of two is
 * satisfiable at all.
 *
 * Order is rejected rather than repaired. Committee position *is* the signer
 * index every attestation witness and attested-signer bit is keyed on, so
 * silently reordering a configured committee would desynchronise this node from
 * its peers.
 */
const validatedPackedSet = (
  packed: string,
  chunkHexLength: number,
  fieldName: string,
  shape: string,
): string[] => {
  let elements: readonly string[];
  try {
    elements = splitPackedHex(packed, chunkHexLength, fieldName);
  } catch {
    throw new Error(`${fieldName} must be ${shape} as hex`);
  }
  if (elements.length < SDK.MIN_DA_OWNER_COUNT) {
    throw new Error(
      `${fieldName} must list at least ${SDK.MIN_DA_OWNER_COUNT.toString()} entries to satisfy the governed threshold floor, received ${elements.length.toString()}`,
    );
  }
  if (!isStrictlyAscending(elements)) {
    throw new Error(
      `${fieldName} must be sorted ascending with no duplicates, matching the governor's sorted-unique encoding`,
    );
  }
  return [...elements];
};

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
  if (lucid === undefined) {
    return BigInt(Date.now());
  }
  return BigInt(
    slotToUnixTimeForLucidOrEmulatorFallback(lucid, lucid.currentSlot()),
  );
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
    const phasMembershipReward = SDK.phasMembershipIdentity(
      network,
      phasMembershipScript,
    );
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
    DA_COMMITTEE_HEX?: string;
    DA_THRESHOLD?: bigint | null;
    DA_COSIGNER_SEED_PHRASE?: string;
    DA_OWNERS_HEX?: string;
  },
  fraudProofCatalogueMerkleRoot: string,
  validTo?: bigint,
  referenceScripts?: AtomicProtocolInitReferenceScripts,
  consensusProfile: ContractDeploymentIdentityValue["consensusProfile"] = MIDGARD_CONSENSUS_PROFILE_V1,
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
    const phasRegistration =
      yield* ensurePhasMembershipRewardAccountRegisteredProgram(lucid);
    yield* Effect.logInfo(
      `PHAS membership reward-account registration status: status=${phasRegistration.status},scriptHash=${phasRegistration.scriptHash},rewardAddress=${phasRegistration.rewardAddress},txHash=${phasRegistration.txHash ?? "already-registered"}`,
    );
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
      contracts.consensusProfile,
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
