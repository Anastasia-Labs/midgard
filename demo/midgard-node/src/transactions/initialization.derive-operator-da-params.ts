import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type DaLocalSigner,
  daLocalSigners,
  VERIFICATION_KEY_HASH_HEX_LENGTH,
  VERIFICATION_KEY_HEX_LENGTH,
} from "../da/local-signers.js";
import { outRefLabel } from "../tx-context.js";
import {
  type AtomicProtocolInitReferenceScripts,
  atomicProtocolInitReferenceScriptsFromPublications,
  MIN_RECOMMENDED_DA_COMMITTEE_SIZE,
  MIN_RECOMMENDED_DA_OWNER_COUNT,
  resolveDaCommittee,
  validatedPackedSet,
} from "./initialization.atomic-protocol-init-reference-scripts-from-publications.js";
import { ensureNodeRuntimeReferenceScriptsProgram } from "./reference-scripts.js";
import { TxConfirmError, TxSignError, TxSubmitError } from "./utils.js";

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
