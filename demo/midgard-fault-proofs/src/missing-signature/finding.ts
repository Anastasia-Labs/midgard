/**
 * The `missing-signature` finding record (offchain plan §3.4): the typed
 * CONTRACT between detection and proving. It is the sole input the proving
 * core (§4.3) accepts, and it is deliberately self-contained — everything
 * needed to start (or resume) a thread is derivable from it plus chain
 * state, so the same record drives the watcher's autonomous path and an
 * operator's manual CLI invocation alike.
 */
import type { EventKey } from "@al-ft/midgard-sdk";
import { missingSignatureVkeyHash } from "@al-ft/midgard-sdk";

import { missingSignatureSubmitError } from "./submit-common.js";

/**
 * The §3.2 provability classification. Only `MissingWitness` is provable;
 * the other three are refused at the proving core's API boundary regardless
 * of policy — the classification, not the consumer, is the gate (§3.4).
 */
export const MissingSignatureProvability = Object.freeze({
  /**
   * A required signer's witness is absent and a vkey preimage for its hash
   * is known (§3.3): provable end-to-end.
   */
  MissingWitness: "missingWitness",
  /**
   * The required signer's witness is present but its signature fails
   * verification — `invalidSignature`'s fault (Q15), not this family's;
   * step-04's fold would find the key present and refuse (§7.3, D6).
   */
  PresentButInvalid: "presentButInvalid",
  /**
   * The §7.2 corner: the witness is absent but no vkey preimage of the
   * accused hash is recoverable, so no single-party prover can walk
   * step-03. Journaled, never routed to proving; the interactive
   * `validationTraceDispute` machine covers the rule preimage-free (D4).
   */
  UnknownVkeyPreimage: "unknownVkeyPreimage",
  /** Every required signer is witnessed — the commitment is honest. */
  NotAFault: "notAFault",
} as const);
export type MissingSignatureProvability =
  (typeof MissingSignatureProvability)[keyof typeof MissingSignatureProvability];

/** The provability classes the proving core accepts. */
export const MISSING_SIGNATURE_PROVABLE_CLASSES: readonly MissingSignatureProvability[] =
  [MissingSignatureProvability.MissingWitness];

export type MissingSignatureFinding = {
  /** The faulted block's 28-byte header hash, lowercase hex. */
  readonly headerHash: string;
  /** The authenticated block event whose accepted transaction diverged. */
  readonly eventKey: EventKey;
  /** `txHash#index` of the faulted block's state-queue UTxO. */
  readonly fraudulentBlockOutRef: string;
  /** The committed accepted transaction's 32-byte native id, lowercase hex. */
  readonly txId: string;
  /** Canonical compact transaction bytes authenticated by the counted root. */
  readonly nativeTxCompactCbor: string;
  /**
   * The accused required signer's ordinal in body field 4 (fixed 28-byte
   * stride; any one absent signer suffices — detection chooses the first).
   */
  readonly accusedRequiredSignerIndex: bigint;
  /** The accused required signer hash (28 bytes, lowercase hex). */
  readonly accusedRequiredSignerHash: string;
  /**
   * The §3.3-recovered verification-key preimage of the accused hash, or
   * `null` for the `UnknownVkeyPreimage` class. The finding carries the
   * resolved vkey so the proving core never searches.
   */
  readonly resolvedVkey: string | null;
  /**
   * The `witness_set_hash` read off the committed compact structure — the
   * second half of the §2.5 anchor step-01 writes into thread state.
   */
  readonly committedWitnessSetHash: string;
  readonly provability: MissingSignatureProvability;
  /** §6 plan-time estimate of the thread's total L1 transaction count. */
  readonly estimatedThreadTxCount: number;
};

/**
 * The §3.2 boundary gate plus the record's own structural coherence.
 * Refusals here are classification refusals — no policy can override them.
 */
export const assertMissingSignatureFindingProvable = (
  finding: MissingSignatureFinding,
): void => {
  if (!MISSING_SIGNATURE_PROVABLE_CLASSES.includes(finding.provability)) {
    throw missingSignatureSubmitError(
      `finding class "${finding.provability}" is not provable by this family (§3.2/§7.2/§7.3) — it is journaled or routed, never proven here.`,
    );
  }
  if (!/^[0-9a-f]{64}$/u.test(finding.txId)) {
    throw missingSignatureSubmitError(
      "finding txId must be 32 bytes of lowercase hex.",
    );
  }
  if (
    !/^[0-9a-f]+$/u.test(finding.nativeTxCompactCbor) ||
    finding.nativeTxCompactCbor.length % 2 !== 0
  ) {
    throw missingSignatureSubmitError(
      "finding nativeTxCompactCbor must be non-empty lowercase hex.",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(finding.headerHash)) {
    throw missingSignatureSubmitError(
      "finding headerHash must be 28 bytes of lowercase hex.",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(finding.accusedRequiredSignerHash)) {
    throw missingSignatureSubmitError(
      "finding accusedRequiredSignerHash must be 28 bytes of lowercase hex.",
    );
  }
  if (!/^[0-9a-f]{64}$/u.test(finding.committedWitnessSetHash)) {
    throw missingSignatureSubmitError(
      "finding committedWitnessSetHash must be 32 bytes of lowercase hex.",
    );
  }
  if (finding.accusedRequiredSignerIndex < 0n) {
    throw missingSignatureSubmitError(
      `accused required-signer ordinal ${finding.accusedRequiredSignerIndex.toString()} is negative — field 4 ordinals abort on-chain outside the domain.`,
    );
  }
  if (finding.resolvedVkey === null) {
    throw missingSignatureSubmitError(
      "a provable finding must carry the §3.3-resolved verification key — step-03 demands the preimage.",
    );
  }
  // Step-03's exact check, made before anything is paid for: the resolved
  // vkey must be the accused hash's preimage under blake2b-224.
  const derived = missingSignatureVkeyHash(finding.resolvedVkey);
  if (derived !== finding.accusedRequiredSignerHash) {
    throw missingSignatureSubmitError(
      `resolved vkey hashes to ${derived}, not the accused required signer ${finding.accusedRequiredSignerHash}.`,
    );
  }
};
