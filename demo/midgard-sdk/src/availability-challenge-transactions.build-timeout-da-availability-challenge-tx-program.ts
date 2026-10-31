import {
  CML,
  getAddressDetails,
  type LucidEvolution,
} from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import {
  type BuiltDaAvailabilityTransaction,
  type DaAvailabilityDeployment,
  type DaAvailabilityRemovalParams,
  DaAvailabilityTransactionError,
  effect,
  fail,
  type TimeoutDaAvailabilityChallengeParams,
} from "./availability-challenge-transactions.at.js";
import { authenticPool } from "./availability-challenge-transactions.build-close-da-availability-challenge-tx-program.js";
import { daAvailabilityLedgerMinFee } from "./availability-challenge-transactions.complete.js";
import {
  daAvailabilityTimeoutChallengerFee,
  protocolParameters,
  role,
} from "./availability-challenge-transactions.plan-da-availability-timeout.js";
import { removal } from "./availability-challenge-transactions.removal.js";
import { planDaBondPoolSlash } from "./da-bond-pool.js";

const isCompletionFailure = (cause: unknown) =>
  cause instanceof DaAvailabilityTransactionError &&
  cause.reason === "completion-failed";

/** Distinct key hashes that must sign: key-address inputs, collateral, required signers. */
const vkeyWitnessCount = (built: BuiltDaAvailabilityTransaction): number => {
  const keys = new Set<string>();
  for (const u of [...built.spentOutRefs, ...built.collateralOutRefs]) {
    const credential = getAddressDetails(u.address).paymentCredential;
    if (credential?.type === "Key") keys.add(credential.hash);
  }
  const signers = built.tx.toTransaction().body().required_signers();
  for (let i = 0; i < (signers?.len() ?? 0); i++)
    keys.add(signers!.get(i).to_hex());
  return keys.size;
};

/**
 * Times out an expired challenge (spec #685 E2): burns the record and terminal
 * accumulator, removes the challenged block (the head, or its immediate
 * descendant first) under an Idle correction lock, and slashes the pooled DA
 * bond in the same transaction.
 *
 * Outputs, in order: the continued queue node (0), the correction lock (1),
 * the ONE challenger output `remaining - c + challenge_record_lovelace +
 * payout` (2), the pool continuing with `pool_in - taken` beside its NFT and
 * its datum unchanged (3), the removed node's rent (4). The inputs pay the
 * outputs and the fee exactly, so no wallet coin is spent; the wallet only
 * backs collateral.
 *
 * The fee is exactly `feePart + c`, `feePart = min(penalty, taken)`. `c` is
 * the least the ledger needs:
 * 1. with a slashed penalty (`feePart > 0`), `c = 0` is tried first; a full
 *    pool's penalty covers any timeout's fee;
 * 2. otherwise the transaction is built at `c = max_timeout_fee`, which also
 *    proves the cap suffices, and its ledger minimum fee is measured
 *    (`daAvailabilityLedgerMinFee`);
 * 3. `c = max(0, measured - feePart)` is rebuilt; should Lucid still refuse
 *    that fee, the capped build stands.
 * A `c` above the cap is refused (`timeout-challenger-fee-cap`).
 */
export const buildTimeoutDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: TimeoutDaAvailabilityChallengeParams,
) =>
  effect(async () => {
    Availability.assertCanonicalDaAvailabilityParameters(d.parameters);
    authenticPool(d, p.pool);
    const { feePart } = planDaBondPoolSlash({
      poolLovelace: p.pool.assets.lovelace ?? 0n,
      parameters: d.parameters,
    });
    const { record, terminal, pool, challengerFeeLovelace, ...rest } = p;
    const attempt = (c: bigint) =>
      removal(
        lucid,
        d,
        { ...rest, feeLovelace: feePart + c },
        { record, terminal, pool, feePart },
      );
    if (challengerFeeLovelace !== undefined)
      return attempt(challengerFeeLovelace);
    if (feePart > 0n) {
      try {
        return await attempt(0n);
      } catch (cause) {
        if (!isCompletionFailure(cause)) throw cause;
      }
    }
    const cap = d.parameters.max_timeout_fee_lovelace;
    let capped: BuiltDaAvailabilityTransaction;
    try {
      capped = await attempt(cap);
    } catch (cause) {
      if (!isCompletionFailure(cause)) throw cause;
      return fail(
        `Timeout does not complete even at c = max_timeout_fee_lovelace ${cap}: ${(cause as Error).message}`,
        "timeout-challenger-fee-cap",
      );
    }
    const referenceScriptBytes = [
      ...capped.referenceOutRefs,
      ...capped.spentOutRefs,
    ].reduce(
      (total, u) =>
        total + (u.scriptRef ? BigInt(u.scriptRef.script.length / 2) : 0n),
      0n,
    );
    const measured = daAvailabilityLedgerMinFee({
      unsignedCbor: capped.unsignedCbor,
      protocolParameters: protocolParameters(lucid),
      referenceScriptBytes,
      vkeyWitnessCount: vkeyWitnessCount(capped),
    });
    const c = daAvailabilityTimeoutChallengerFee({
      feePartLovelace: feePart,
      requiredFeeLovelace: measured,
      parameters: d.parameters,
    });
    if (c >= cap) return capped;
    try {
      return await attempt(c);
    } catch (cause) {
      if (!isCompletionFailure(cause)) throw cause;
      return capped;
    }
  });

export const buildPruneDaUnavailableBlockDescendantTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
) =>
  effect(() => {
    if (!p.descendant) fail("Pruning requires an immediate descendant");
    return removal(lucid, d, p);
  });

export const buildRemoveDaUnavailableHeadTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
) =>
  effect(() => {
    if (p.descendant) fail("Head removal cannot include a descendant");
    return removal(lucid, d, p);
  });

export const assertDaAvailabilityReferenceScript = role;

export type RecoverDaAvailabilityCommitmentParams = {
  readonly applyTxHash: string;
  /** The DA attestation policy whose DAAT token the Apply transaction burns. */
  readonly daAttestationPolicyId: string;
  /**
   * A transaction's CBOR by id, or `undefined` when unknown. Called for the
   * Apply transaction and for the transactions that produced its inputs. Lucid
   * providers expose no transaction fetch, so the caller wires one (Blockfrost
   * `/txs/{hash}/cbor`, an Ogmios/chain-sync archive, or the CBOR it submitted
   * itself).
   */
  readonly fetchTransactionCbor: (
    txHash: string,
  ) => Promise<string | undefined>;
  /** When given, the recovered commitment must hash to it. */
  readonly expectedCommitmentHash?: string;
};

export type RecoveredDaAvailabilityCommitment = {
  readonly commitment: Availability.DaAvailabilityCommitment;
  readonly commitmentHash: string;
  readonly headerHash: string;
  /** The spent DAAT UTxO the commitment was read from. */
  readonly attestationOutRef: {
    readonly txHash: string;
    readonly outputIndex: number;
  };
  /**
   * `inline-datum`: the DAAT output's inline datum in its producing
   * transaction (the normal case: attestations carry inline datums).
   * `witness-datum`: a hashed DAAT datum resolved from the Apply transaction's
   * witness datums. `provider-datum`: a hashed DAAT datum the provider's datum
   * table resolves.
   */
  readonly source: "inline-datum" | "witness-datum" | "provider-datum";
};

export const plutusDataHash = (cbor: string) => {
  const data = CML.PlutusData.from_cbor_hex(cbor);
  try {
    return CML.hash_plutus_data(data).to_hex();
  } finally {
    data.free();
  }
};

export const cborTransaction = async (
  fetch: RecoverDaAvailabilityCommitmentParams["fetchTransactionCbor"],
  txHash: string,
) => {
  const cbor = await fetch(txHash);
  if (cbor === undefined) return undefined;
  const tx = CML.Transaction.from_cbor_hex(cbor);
  if (CML.hash_transaction(tx.body()).to_hex() !== txHash) {
    tx.free();
    return fail(
      `Fetched transaction does not hash to ${txHash}`,
      "apply-commitment-unrecoverable",
    );
  }
  return tx;
};
