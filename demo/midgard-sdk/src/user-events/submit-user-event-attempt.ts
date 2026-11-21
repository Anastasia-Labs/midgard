/** A user submission's intent commitment and recorded-attempt checks. */
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  CML,
  Data,
  datumToHash,
  getAddressDetails,
  type LucidEvolution,
  type OutRef,
  type TransactionStatus,
} from "@lucid-evolution/lucid";

import { AddressData } from "../common.js";
import {
  CardanoDatum,
  WithdrawalBody,
  WithdrawalSignature,
} from "../ledger-state.js";
import { type SubmitDepositConfig } from "./deposit.js";
import { type UserSubmissionAttempt } from "./submit-user-event-types.js";
import {
  type SubmitWithdrawalConfig,
  type WithdrawalBodyInput,
  type WithdrawalBuildMetadata,
  type WithdrawalRefundDatumInput,
} from "./withdrawal.js";

export type UserDepositIntent = Pick<
  SubmitDepositConfig,
  | "l2Address"
  | "l2Datum"
  | "lovelace"
  | "additionalAssets"
  | "structuralLovelace"
>;

export type UserWithdrawalIntent = WithdrawalBodyInput &
  WithdrawalRefundDatumInput &
  Pick<SubmitWithdrawalConfig, "signature" | "refundAddress" | "lovelace">;

const depositIntentHash = (intent: UserDepositIntent): string =>
  datumToHash(
    Data.to([
      intent.l2Address === ""
        ? ""
        : getAddressDetails(intent.l2Address).address.hex,
      intent.l2Datum === null
        ? []
        : [aikenSerialisedPlutusDataCborPreservingMapOrder(intent.l2Datum)],
      intent.lovelace,
      Object.entries(intent.additionalAssets)
        .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
        .map(([unit, amount]) => [unit, amount]),
      intent.structuralLovelace === undefined
        ? []
        : [intent.structuralLovelace],
    ]),
  );

const withdrawalIntentHash = (intent: UserWithdrawalIntent): string => {
  if (
    (intent.body !== undefined && intent.bodyCbor !== undefined) ||
    (intent.refundDatum !== undefined && intent.refundDatumCbor !== undefined)
  )
    throw new Error(
      "Withdrawal intent must have exactly one encoding source per datum",
    );
  const body = intent.bodyCbor ?? Data.to(intent.body, WithdrawalBody);
  const refund =
    intent.refundDatumCbor ??
    Data.to(intent.refundDatum ?? "NoDatum", CardanoDatum);
  Data.from(body, WithdrawalBody);
  Data.from(refund, CardanoDatum);
  return datumToHash(
    Data.to([
      aikenSerialisedPlutusDataCborPreservingMapOrder(body),
      Data.from(Data.to(intent.signature, WithdrawalSignature)),
      Data.from(Data.to(intent.refundAddress, AddressData)),
      aikenSerialisedPlutusDataCborPreservingMapOrder(refund),
      intent.lovelace === undefined ? [] : [intent.lovelace],
    ]),
  );
};

/** Commits the exact semantic request, before any nonce is chosen, so a
 * record can refuse a rerun that asks for something else. */
export const userSubmissionIntentHash: {
  (kind: "Deposit", intent: UserDepositIntent): string;
  (kind: "Withdrawal", intent: UserWithdrawalIntent): string;
} = (
  kind: "Deposit" | "Withdrawal",
  intent: UserDepositIntent | UserWithdrawalIntent,
): string =>
  kind === "Deposit"
    ? depositIntentHash(intent as UserDepositIntent)
    : withdrawalIntentHash(intent as UserWithdrawalIntent);

export const withdrawalEventIdFromBuildMetadata = (
  metadata: WithdrawalBuildMetadata,
): string => metadata.withdrawalEventIdCbor;

const HASH_PATTERN = /^[0-9a-f]{64}$/u;

const isOutRef = (value: unknown): value is OutRef =>
  typeof value === "object" &&
  value !== null &&
  typeof (value as OutRef).txHash === "string" &&
  HASH_PATTERN.test((value as OutRef).txHash) &&
  Number.isSafeInteger((value as OutRef).outputIndex) &&
  (value as OutRef).outputIndex >= 0;

const inputLabels = (inputs: CML.TransactionInputList | undefined) => {
  const labels = new Set<string>();
  for (let index = 0; index < (inputs?.len() ?? 0); index++) {
    const input = inputs!.get(index);
    labels.add(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return labels;
};

/**
 * Throws, naming the field, unless the attempt is internally consistent: its
 * hash is its body's hash, its TTL is the body's TTL, an admission spends its
 * nonce (and reads its publication), and a publication does not.
 */
export const assertUserSubmissionAttempt = (
  attempt: UserSubmissionAttempt,
): void => {
  if (!Number.isSafeInteger(attempt.attempt) || attempt.attempt < 1)
    throw new Error("attempt must be a positive integer");
  if (attempt.phase !== "Publication" && attempt.phase !== "Admission")
    throw new Error("phase must be Publication or Admission");
  if (!isOutRef(attempt.nonce))
    throw new Error("nonce must be an exact output reference");
  if (attempt.publication !== null && !isOutRef(attempt.publication))
    throw new Error("publication must be null or an exact output reference");
  if (attempt.phase === "Publication" && attempt.publication !== null)
    throw new Error("a publication attempt cannot reference a publication");
  if (!HASH_PATTERN.test(attempt.txHash))
    throw new Error("txHash must be 32 lowercase hex bytes");
  if (!Number.isSafeInteger(attempt.ttlSlot) || attempt.ttlSlot < 0)
    throw new Error("ttlSlot must be a non-negative integer");
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(attempt.signedTxCbor);
  } catch (cause) {
    throw new Error(`signedTxCbor is not a transaction: ${String(cause)}`);
  }
  const body = transaction.body();
  if (CML.hash_transaction(body).to_hex() !== attempt.txHash)
    throw new Error("txHash is not the hash of signedTxCbor");
  const ttl = body.ttl();
  if (ttl === undefined)
    throw new Error(
      "signedTxCbor has no TTL, so it could never be proven dead",
    );
  if (ttl !== BigInt(attempt.ttlSlot))
    throw new Error("ttlSlot is not the TTL of signedTxCbor");
  const spendsNonce = inputLabels(body.inputs()).has(
    outRefLabel(attempt.nonce),
  );
  if (attempt.phase === "Admission" && !spendsNonce)
    throw new Error("the admission does not spend its nonce");
  if (attempt.phase === "Publication" && spendsNonce)
    throw new Error("the publication spends the event nonce");
  if (
    attempt.publication !== null &&
    !inputLabels(body.reference_inputs()).has(outRefLabel(attempt.publication))
  )
    throw new Error("the admission does not read its publication");
};

/** The provider's exact-hash answer, refused when it describes another hash. */
export const observeUserSubmissionAttempt = async (
  lucid: LucidEvolution,
  txHash: string,
): Promise<TransactionStatus> => {
  const status = await lucid.transactionStatus(txHash);
  if (
    status.txHash !== txHash ||
    (status.status === "confirmed" && status.confirmation.txHash !== txHash)
  )
    throw new Error("Provider returned the status of a different transaction");
  return status;
};
