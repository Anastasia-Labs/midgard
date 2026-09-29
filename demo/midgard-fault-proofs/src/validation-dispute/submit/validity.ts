import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile";
import {
  slotAlignedUpperBoundAtOrBefore,
  type SlotClock,
} from "@al-ft/midgard-sdk";
import { type Script, type TxSigned, type UTxO } from "@lucid-evolution/lucid";

import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../../workflow/transaction-boundary.js";

export const VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS = 60_000;
export const VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS = 60_000;
export const MAX_L1_VALIDATION_PROOF_TRANSACTION_BYTES = 16 * 1024;
export const VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE =
  "V1 validation-trace dispute";

/**
 * Build-time delivery cost heuristic for a tier-1 complete item (#619/#621).
 *
 * "direct" carries the §5.1 preimage in the observe redeemer; "reference"
 * routes it through a §8 proof-item publication and delivers by reference.
 * Since Option B the committed evidence is transition-only, so either route
 * can deliver any tier-1 item: this pin steers cost — one transaction versus
 * two — never soundness, and a stale pin degrades fees and latency, not
 * liveness. `maxReliableDirectCompleteItemBytes` is an owner-signed consensus
 * measurement; re-measuring the direct frontier and rebinding this heuristic
 * is #622's owner table, so the number is read here and never changed here.
 */
export const selectValidationCompleteItemCarriage = (
  itemBytes: number,
): "direct" | "reference" => {
  if (
    !Number.isSafeInteger(itemBytes) ||
    itemBytes < 0 ||
    itemBytes > MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes
  ) {
    throw new Error(
      "Complete validation proof item exceeds the measured single-publication envelope",
    );
  }
  return itemBytes <=
    MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes
    ? "direct"
    : "reference";
};

export type ValidationDisputeValidityRange = {
  readonly validFrom: number;
  readonly validTo: number;
};

/**
 * Optional Q51 pre-submit boundary (workflow ruling R5). When a durable
 * production workflow supplies `boundary`, the fully signed and locally
 * evaluated transaction is handed to it immediately before provider I/O so
 * the workflow can persist preflight/intent (or capture the transaction and
 * become the sole submit authority). When unset, behaviour is byte-identical
 * to the historical direct-submit path.
 */
export const reachOptionalPreSubmitBoundary = async ({
  signed,
  boundary,
  referenceScriptCandidates = [],
}: {
  readonly signed: TxSigned;
  readonly boundary: FraudProofPreSubmitBoundary | undefined;
  readonly referenceScriptCandidates?: readonly {
    readonly role: string;
    readonly utxo: UTxO | undefined;
    readonly expectedScript?: Script;
  }[];
}): Promise<void> => {
  if (boundary === undefined) return;
  await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: referenceScriptCandidates,
    }),
    boundary,
  });
};

export const safeUnsignedNumber = (value: bigint, field: string): number => {
  const number = Number(value);
  if (!Number.isSafeInteger(number) || number < 0) {
    throw new Error(`${field} must be a non-negative safe integer`);
  }
  return number;
};

export const validationDisputeValidityRange = (
  now: number,
): ValidationDisputeValidityRange => {
  if (
    !Number.isSafeInteger(now) ||
    now < VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS
  ) {
    throw new Error(
      "Validation-dispute current time must be a safe POSIX time",
    );
  }
  return {
    validFrom: now - VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS,
    validTo: now + VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS,
  };
};

export const validationDisputeTimeoutValidityRange = (
  now: number,
  responseDeadline: number,
): ValidationDisputeValidityRange => {
  const ordinary = validationDisputeValidityRange(now);
  if (!Number.isSafeInteger(responseDeadline) || responseDeadline < 0) {
    throw new Error(
      "Validation-dispute response deadline must be a non-negative safe integer",
    );
  }
  if (now <= responseDeadline) {
    throw new Error("Validation-dispute response deadline has not passed");
  }
  return requireValidityRange({
    validFrom: Math.max(ordinary.validFrom, responseDeadline + 1),
    validTo: ordinary.validTo,
  });
};

export const requireValidityRange = (
  range: ValidationDisputeValidityRange | null | undefined,
): ValidationDisputeValidityRange => {
  if (
    range == null ||
    !Number.isSafeInteger(range.validFrom) ||
    !Number.isSafeInteger(range.validTo) ||
    range.validFrom < 0 ||
    range.validTo <= range.validFrom ||
    range.validTo - range.validFrom >
      VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS +
        VALIDATION_DISPUTE_VALIDITY_LEEWAY_MS
  ) {
    throw new Error(
      "Validation-dispute validity range must yield a non-empty, non-negative closed range no longer than 120 seconds",
    );
  }
  return range;
};

export const refreshExpiredValidationDisputeValidityRange = ({
  range,
  currentLedgerTime,
}: {
  readonly range: ValidationDisputeValidityRange;
  readonly currentLedgerTime: number;
}): ValidationDisputeValidityRange => {
  const checked = requireValidityRange(range);
  if (!Number.isSafeInteger(currentLedgerTime) || currentLedgerTime < 0) {
    throw new Error(
      "Validation-dispute current ledger time must be a non-negative safe integer",
    );
  }
  if (currentLedgerTime < checked.validTo) {
    return checked;
  }
  const width = checked.validTo - checked.validFrom;
  const backoff = Math.min(VALIDATION_DISPUTE_VALIDITY_BACKOFF_MS, width - 1);
  const validFrom = Math.max(0, currentLedgerTime - backoff);
  return requireValidityRange({
    validFrom,
    validTo: validFrom + width,
  });
};

export const inclusiveValidityUpperBound = (
  range: ValidationDisputeValidityRange,
): number => range.validTo - 1;

/**
 * Open, VerifySource and Reveal record the inclusive upper bound in their
 * output datum, and the validators require it to equal the bound the ledger
 * presents. Lucid floors `validTo` to its slot, so a wall-clock `validTo`
 * would record a bound up to one slot later than the one the validator sees.
 * Moving `validTo` onto that slot boundary first makes
 * `inclusiveValidityUpperBound` exact.
 */
export const ledgerPresentedValidationDisputeValidityRange = (
  slotClock: SlotClock,
  range: ValidationDisputeValidityRange,
): ValidationDisputeValidityRange => {
  const checked = requireValidityRange(range);
  return requireValidityRange({
    validFrom: checked.validFrom,
    validTo: Number(
      slotAlignedUpperBoundAtOrBefore(slotClock, BigInt(checked.validTo)),
    ),
  });
};
