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
 * Committee size at or above which the single-key attestation warning is not
 * emitted.
 *
 * Advice, not a bound. The governor represents a committee of one — the
 * 2026-08-11 owner ruling accepted the single-key attest loop *with a
 * rate-limited explanatory log* and named two-key committees the standing
 * configuration — so nothing here refuses a committee below this size.
 */
const MIN_RECOMMENDED_DA_COMMITTEE_SIZE = 2;

/**
 * Owner-set size at or above which the single-key governance warning is not
 * emitted.
 *
 * Advice, not a bound, and deliberately a separate constant from
 * {@link MIN_RECOMMENDED_DA_COMMITTEE_SIZE}: one covers who can attest, the
 * other who can rotate the parameters, and a future change to either must not
 * silently move the other. The 2026-08-13 owner ruling made a lone owner
 * representable, so this drives a warning and nothing else.
 */
const MIN_RECOMMENDED_DA_OWNER_COUNT = 2;

/**
 * Builds the `DaParamsDatum` this node writes at protocol initialization.
 *
 * Q63 (F04 §4) gave the governor threshold floors — `da_threshold >=
 * ceil(2*committee_len/3)` and `update_threshold >= ceil(2*owner_len/3)`. Every
 * bound below is evaluated by the SDK twins (`SDK.governedThresholdFloor`,
 * `SDK.daParamsFloorViolations`) so the arithmetic lives in exactly one place
 * and cannot drift from the validator.
 *
 * Both sets are also emitted sorted-unique, because `valid_datum` measures
 * `committee` and `owners` with its `sorted_unique_*` walkers, which fail the
 * script outright on an out-of-order or duplicate element.
 *
 * ## A fully single-key deployment warns; it is no longer refused
 *
 * F04 §4 carried a `max(2, …)` clamp and the governor carried an owner-set
 * minimum of two, so both a 1-of-1 committee and a lone owner were unmintable
 * and this function refused each. Two owner rulings retired both: 2026-08-11
 * (ruling 4) for the committee floor, and 2026-08-13 (in-session, recorded on
 * #602) for the owner-set minimum.
 *
 * A node holding nothing but its own key therefore bootstraps end to end. It
 * emits a one-member committee and a one-member owner set, both of which the
 * governor admits, and this function emits one explanatory warning for each —
 * single-key attestation, and single-key governance. Neither is an error.
 *
 * The warnings are per-bootstrap rather than time-limited, because this is a
 * one-shot path: it runs once per deployment initialisation, so it cannot flood
 * a log the way a loop can. The rate limiter that the ruling asks for lives
 * where the repetition is — the attest loop in
 * `demo/da-committee-node/src/coordinator/on-chain.ts`.
 *
 * What still fails closed is genuinely malformed configuration: a set that is
 * not hex, not the right element width, empty, or not sorted-unique. Those
 * would be rejected by `valid_datum` on-chain, and saying so here is far
 * cheaper than a cryptic script failure.
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
    if (committeeLength < MIN_RECOMMENDED_DA_COMMITTEE_SIZE) {
      // The 2026-08-11 owner ruling 4's explanatory warning, at the one place a
      // deployment's committee size is decided. Warn, do not refuse: the
      // governor admits this datum, and refusing it here would reinstate the
      // prohibition the ruling lifted.
      yield* Effect.logWarning(
        `Single-key DA configuration: this deployment initialises a ${committeeLength.toString()}-member DA committee, ` +
          `so every block's data availability is attested by one key with no independent corroboration and no liveness redundancy. ` +
          `F04 §4 (amended 2026-08-11) permits it — the governed floor is ceil(2*${committeeLength.toString()}/3) = ` +
          `${SDK.governedThresholdFloor(committeeLength).toString()} — but two-key committees are the standing configuration. ` +
          `Set DA_COMMITTEE_HEX to the packed peer committee, or DA_COSIGNER_SEED_PHRASE to a second locally held key.`,
      );
    }
    const owners = yield* resolveDaOwners(nodeConfig, signers);
    if (owners.length < MIN_RECOMMENDED_DA_OWNER_COUNT) {
      // The 2026-08-13 owner ruling's explanatory warning, symmetric with the
      // committee one above and emitted at the one place a deployment's owner
      // set is decided. Warn, do not refuse: the governor admits this datum.
      yield* Effect.logWarning(
        `Single-key DA governance: this deployment initialises a ${owners.length.toString()}-member DA owner set, ` +
          `so one key can rotate the DA committee and both governed thresholds with no second approval. ` +
          `F04 §4 (amended 2026-08-13) permits it — the governed floor is ceil(2*${owners.length.toString()}/3) = ` +
          `${SDK.governedThresholdFloor(owners.length).toString()} — but multi-owner governance is the standing configuration. ` +
          `Set DA_OWNERS_HEX to the packed owner key hashes, or DA_COSIGNER_SEED_PHRASE to a second locally held key.`,
      );
    }
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
      // No arity refusal. A locally derived committee of one is the single-key
      // attest loop the 2026-08-11 owner ruling accepted; `deriveOperatorDaParams`
      // warns about it rather than failing closed here.
      return [...new Set(signers.map((signer) => signer.verificationKeyHex))]
        .sort()
        .join("");
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
      // No arity refusal, symmetric with the committee path. The 2026-08-13
      // owner ruling dropped the governor's owner-set minimum to one, so a
      // locally derived owner set of one produces a datum the governor admits;
      // `deriveOperatorDaParams` warns about it rather than failing closed.
      return [...new Set(signers.map((signer) => signer.keyHashHex))].sort();
    },
    catch: (cause) =>
      new SDK.HashingError({
        message: "Invalid DA owner configuration",
        cause,
      }),
  });

/**
 * Parses a packed hex set and enforces what `valid_datum` needs of it: correct
 * element width and strictly ascending order (its sorted-unique encoding).
 *
 * There is no longer an arity check. This function carried one — first against
 * a shared `MIN_DA_OWNER_COUNT` of two, then against a per-call minimum once
 * the 2026-08-11 ruling let a committee have one member. The 2026-08-13 ruling
 * dropped the owner-set minimum to one as well, so both call sites bottom out
 * at a single element, and at one the check could not fail: `splitPackedHex`
 * already rejects an empty field. Deleting it rather than passing a vacuous
 * minimum keeps the guard structure honest — the non-emptiness refusal lives in
 * exactly one place, and it is one that actually fires.
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

/** Fetches and authenticates the deployment correction-lock singleton. */
export const fetchCorrectionLockWitness = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
): Effect.Effect<SDK.CorrectionLockUTxO | null, SDK.LucidError> =>
  Effect.gen(function* () {
    const utxos = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.correctionLock.spendingScriptAddress,
          SDK.correctionLockUnit(contracts.hubOracle.policyId),
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch correction-lock witness UTxO(s)",
          cause,
        }),
    });
    const authentic = yield* SDK.utxosToCorrectionLockUTxOs(
      utxos,
      contracts.hubOracle.policyId,
    );
    if (authentic.length > 1) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Expected at most one authentic correction-lock UTxO",
          cause: authentic.map(({ utxo }) => outRefLabel(utxo)).join(","),
        }),
      );
    }
    return authentic[0] ?? null;
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
  readonly correctionLockWitness: SDK.CorrectionLockUTxO | null;
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
    const correctionLockWitness = yield* fetchCorrectionLockWitness(
      lucid,
      contracts,
    );
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
      ...(correctionLockWitness === null ? ["correction-lock"] : []),
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
      correctionLockWitness !== null &&
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
      correctionLockWitness === null &&
      !daParamsInitialized &&
      !stateQueueTopology.initialized &&
      !schedulerInitialized &&
      !registeredOperatorsInitialized &&
      !activeOperatorsInitialized &&
      !retiredOperatorsInitialized &&
      !fraudProofCatalogueInitialized;

    return {
      hubOracleWitness,
      correctionLockWitness,
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
