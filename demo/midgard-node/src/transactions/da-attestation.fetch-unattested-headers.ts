import * as SDK from "@al-ft/midgard-sdk";
import type { SqlClient } from "@effect/sql";
import {
  calculateMinLovelaceFromUTxO,
  Data,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { committeeSignerIndex, daLocalSigners } from "../da/local-signers.js";
import { landedStateQueueUTxOs } from "../services/landed-state-queue.js";
import { outRefLabel } from "../tx-context.js";
import { type OperatorDaConfig } from "./da-attestation.fetch-da-attestation-reference-scripts.js";

/** The landed queue's blocks without a DA attestation (P1, never L1). */
export const fetchUnattestedHeaders = (
  contracts: SDK.MidgardValidators,
  headerHash?: string,
): Effect.Effect<
  readonly SDK.DaAttestationStateQueueTarget[],
  SDK.DataCoercionError | SDK.HashingError | SDK.StateQueueError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const stateQueueUtxos = yield* landedStateQueueUTxOs(
      contracts.stateQueue,
      "DA attestation",
    );
    const matches: SDK.DaAttestationStateQueueTarget[] = [];
    for (const stateQueueUtxo of stateQueueUtxos) {
      if (stateQueueUtxo.datum.key === "Empty") {
        continue;
      }
      const node = yield* SDK.getStateQueueNodeFromStateQueueDatum(
        stateQueueUtxo.datum,
      );
      if (node.da_attestation !== SDK.NO_DA_ATTESTATION) {
        continue;
      }
      const recomputedHeaderHash = yield* SDK.hashBlockHeader(node.header);
      const datumHeaderHash = stateQueueUtxo.datum.key.Key.key;
      if (recomputedHeaderHash !== datumHeaderHash) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Failed to select DA attestation target: state-queue key/hash mismatch",
            cause: `outRef=${outRefLabel(stateQueueUtxo.utxo)},datumKey=${datumHeaderHash},computed=${recomputedHeaderHash}`,
          }),
        );
      }
      if (headerHash !== undefined && recomputedHeaderHash !== headerHash) {
        continue;
      }
      matches.push({
        stateQueueUtxo,
        stateQueueNode: node,
        headerHash: recomputedHeaderHash,
      });
    }
    if (headerHash !== undefined && matches.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Requested state-queue header is not available for DA attestation",
          cause: `header=${headerHash}`,
        }),
      );
    }
    return matches;
  });

/**
 * Every attestation witness this process can produce for `headerHash`.
 *
 * Since Q63 the governed floor puts `da_threshold` at two or more, so a single
 * operator signature can never reach threshold alone. A node holding more than
 * one DA key (dev and emulator bootstrap, via `DA_COSIGNER_SEED_PHRASE`)
 * contributes one genuine Ed25519 signature per key here; a production node
 * holds one key and the remaining witnesses arrive from peers over libp2p.
 *
 * The signer index is looked up in the on-chain committee rather than assumed,
 * because the committee is emitted sorted-unique and the operator's key is not
 * necessarily first. Keys absent from the committee are skipped: they cannot be
 * indexed, and the attestation validator would reject them.
 */
export const localDaSignatureWitnesses = (
  availabilityCommitment: SDK.DaAvailabilityCommitment,
  nodeConfig: OperatorDaConfig,
  committeeHex: string,
): readonly SDK.DaAttestationSignatureWitness[] => {
  const message = Buffer.from(
    SDK.daAvailabilityAttestationMessage(availabilityCommitment),
  );
  return daLocalSigners(nodeConfig)
    .flatMap((signer) => {
      const signerIndex = committeeSignerIndex(
        committeeHex,
        signer.verificationKeyHex,
      );
      return signerIndex === null
        ? []
        : [{ signerIndex, signatureHex: signer.sign(message) }];
    })
    .sort((left, right) => left.signerIndex - right.signerIndex);
};

/**
 * The widest `attestation_count` an attestation can reach: a committee has at
 * most 256 signers, and 256 is the first count whose CBOR integer takes three
 * bytes.
 */
const DA_ATTESTATION_WIDEST_COUNT = 256n;

/**
 * The lovelace the attestation Init output locks: the min-UTxO of that output
 * with `attestation_count` at its widest. Add-signatures carries the value
 * unchanged while the count grows, so sizing at the Init's own count (zero)
 * would let a later add-signatures output fall below the ledger minimum. The
 * attestation carries no bond: Apply (or Rescue) refunds its whole value to
 * `rescue_beneficiary`.
 */
export const daAttestationInitOutputLovelace = (
  lucid: Pick<LucidEvolution, "config">,
  {
    attestationAddress,
    attestationUnit,
    headerHash,
    availabilityCommitment,
    daParamsDatum,
    rescueBeneficiary,
  }: {
    readonly attestationAddress: string;
    readonly attestationUnit: string;
    readonly headerHash: string;
    readonly availabilityCommitment: SDK.DaAvailabilityCommitment;
    readonly daParamsDatum: SDK.DaParamsDatum;
    readonly rescueBeneficiary: SDK.AddressData;
  },
): Effect.Effect<bigint, SDK.LucidError> =>
  Effect.gen(function* () {
    const coinsPerUtxoByte =
      lucid.config().protocolParameters?.coinsPerUtxoByte;
    if (coinsPerUtxoByte === undefined) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Missing protocol parameters for the DA attestation min-UTxO",
          cause: "coinsPerUtxoByte is undefined",
        }),
      );
    }
    const widestDatum: SDK.DaAttestationDatum = {
      header_hash: headerHash,
      availability_commitment: availabilityCommitment,
      da_threshold: daParamsDatum.da_threshold,
      committee_signers_hash: daParamsDatum.committee_signers_hash,
      rescue_beneficiary: rescueBeneficiary,
      attested_signers: SDK.EMPTY_ATTESTED_SIGNER_BITMAP,
      attestation_count: DA_ATTESTATION_WIDEST_COUNT,
    };
    return calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: attestationAddress,
      assets: { lovelace: 0n, [attestationUnit]: 1n },
      datum: Data.to(widestDatum as never, SDK.DaAttestationDatum as never),
    });
  });

/** The Apply refusals that mean "the DA bond pool cannot back this now". */
export type DaBondPoolAttestationSkipReason = Extract<
  SDK.DaAttestationBuildFailureReason,
  "pool-under-backed" | "pool-unavailable" | "pool-withdrawing"
>;

const DA_BOND_POOL_SKIP_REASONS: ReadonlySet<SDK.DaAttestationBuildFailureReason> =
  new Set<DaBondPoolAttestationSkipReason>([
    "pool-under-backed",
    "pool-unavailable",
    "pool-withdrawing",
  ]);

export const isDaBondPoolAttestationSkip = (
  error: unknown,
): error is SDK.DaAttestationBuildError & {
  readonly reason: DaBondPoolAttestationSkipReason;
} =>
  error instanceof SDK.DaAttestationBuildError &&
  DA_BOND_POOL_SKIP_REASONS.has(error.reason);

/** The `event` log annotation every pool skip carries. */
export const DA_ATTESTATION_POOL_SKIP_EVENT = "da_attestation_skipped_pool";

/**
 * Reads the DA bond pool and checks that it can back one attestation, with
 * the refusals the SDK Apply builder uses: an unreadable pool is
 * `pool-unavailable`, a `Withdrawing` pool is `pool-withdrawing`, and backing
 * above the floor below `da_bond_lovelace` is `pool-under-backed`. Returns
 * the pool outref, so a failed Apply can tell whether the pool moved under it.
 *
 * The round runs it before Init as well as around Apply, so a pool that
 * cannot back the attestation never leaves an Init output waiting on it.
 */
export const daBondPoolBackingAttestationProgram = (
  lucid: LucidEvolution,
  contracts: Pick<SDK.MidgardValidators, "daBondPool">,
  parameters: SDK.DaAvailabilityParameters,
): Effect.Effect<string, SDK.DaAttestationBuildError> =>
  Effect.gen(function* () {
    const pool = yield* Effect.tryPromise({
      try: () =>
        SDK.fetchDaBondPool(lucid, {
          policyId: contracts.daBondPool.policyId,
          address: contracts.daBondPool.spendingScriptAddress,
          parameters,
        }),
      catch: (cause) =>
        new SDK.DaAttestationBuildError({
          reason: "pool-unavailable",
          message: "Could not fetch the authentic DA bond pool",
          cause,
        }),
    });
    if (pool.datum !== "Bonded") {
      return yield* Effect.fail(
        new SDK.DaAttestationBuildError({
          reason: "pool-withdrawing",
          message:
            "The DA bond pool is withdrawing and backs no new attestation until the withdrawal is cancelled",
          cause: `pool=${outRefLabel(pool.utxo)},unlock_at=${pool.datum.Withdrawing.unlock_at.toString()}`,
        }),
      );
    }
    const backing = pool.backing ?? 0n;
    if (backing < parameters.da_bond_lovelace) {
      return yield* Effect.fail(
        new SDK.DaAttestationBuildError({
          reason: "pool-under-backed",
          message:
            "The DA bond pool backs less than one DA bond above its floor",
          cause: `pool=${outRefLabel(pool.utxo)},backing=${backing.toString()},da_bond=${parameters.da_bond_lovelace.toString()}`,
        }),
      );
    }
    return outRefLabel(pool.utxo);
  });

/** Apply attempts when a pool top-up, slash or withdrawal step races it. */
export const DA_ATTESTATION_APPLY_POOL_CHURN_ATTEMPTS = 3;

/**
 * Runs `apply` (build, sign, submit) with a bounded retry on pool outref
 * churn (decision G8). Apply reads the pool as a reference input, and any
 * TopUp, Slash or withdrawal step spends the pool and invalidates a built
 * Apply. A failed attempt is retried only when the pool's outref changed
 * since the attempt read it, that is, when the pool moved rather than the
 * attestation or the state-queue node.
 *
 * A pool that stops backing between attempts, or an Apply build refusal for
 * the pool, fails with the pool's `DaAttestationBuildError`, which the round
 * turns into a logged skip. Every other failure propagates unchanged.
 */
export const applyWithDaBondPoolChurnRetry = <A, E, R, R2>({
  headerHash,
  readPoolOutRef,
  apply,
  maxAttempts = DA_ATTESTATION_APPLY_POOL_CHURN_ATTEMPTS,
}: {
  readonly headerHash: string;
  readonly readPoolOutRef: Effect.Effect<
    string,
    SDK.DaAttestationBuildError,
    R2
  >;
  readonly apply: Effect.Effect<A, E, R>;
  readonly maxAttempts?: number;
}): Effect.Effect<A, E | SDK.DaAttestationBuildError, R | R2> =>
  Effect.gen(function* () {
    for (let attempt = 1; ; attempt += 1) {
      const poolBefore = yield* readPoolOutRef;
      const outcome = yield* Effect.either(apply);
      if (outcome._tag === "Right") {
        return outcome.right;
      }
      if (isDaBondPoolAttestationSkip(outcome.left)) {
        return yield* Effect.fail(outcome.left);
      }
      const poolAfter = yield* readPoolOutRef;
      if (poolAfter === poolBefore || attempt >= maxAttempts) {
        return yield* Effect.fail(outcome.left);
      }
      yield* Effect.logWarning(
        `DA attestation apply for header ${headerHash} failed while the DA bond pool moved (${poolBefore} -> ${poolAfter}); rebuilding against the new pool outref, attempt ${(attempt + 1).toString()}/${maxAttempts.toString()}.`,
      );
    }
  });
