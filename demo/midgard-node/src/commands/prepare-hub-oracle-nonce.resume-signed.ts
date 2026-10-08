import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { isSpentInputSubmitRejection } from "@al-ft/midgard-core/ogmios-json-rpc-error";
import { CML, type LucidEvolution, type OutRef } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseOutsideValidityIntervalDetails } from "../transactions/utils.js";

export class SignedNonceConflictError extends Error {
  override readonly name = "SignedNonceConflictError";
}

/** The ledger refused the recorded bytes while their inputs are unspent. */
export class SignedNonceRejectedError extends Error {
  override readonly name = "SignedNonceRejectedError";
}

/** Ogmios reports every ledger refusal of a submitted transaction as 3000–3999. */
const ogmiosSubmitFailureCode = (error: unknown): number | undefined => {
  const seen = new Set<unknown>();
  const search = (value: unknown): number | undefined => {
    if (typeof value !== "object" || value === null || seen.has(value))
      return undefined;
    seen.add(value);
    const { code } = value as { code?: unknown };
    if (typeof code === "number" && code >= 3000 && code <= 3999) return code;
    for (const key of ["error", "cause"])
      if (key in value) {
        const found = search((value as Record<string, unknown>)[key]);
        if (found !== undefined) return found;
      }
    return undefined;
  };
  return search(error);
};

/**
 * A resubmission that fails because the same transaction already consumed the
 * inputs (in a mempool, or on chain since the check) or has yet to enter its
 * validity interval can still land; any other ledger refusal is final for
 * these bytes. Transport failures carry no ledger verdict.
 */
const isDefiniteLedgerRejection = (error: unknown) => {
  if (ogmiosSubmitFailureCode(error) === undefined) return false;
  if (isSpentInputSubmitRejection(error)) return false;
  const validity = parseOutsideValidityIntervalDetails(error);
  return !(
    validity !== null && validity.currentSlot < validity.invalidBeforeSlot
  );
};

const spendingInputs = (txHash: string, signedTxCbor: string): OutRef[] => {
  const body = CML.Transaction.from_cbor_hex(signedTxCbor).body();
  if (CML.hash_transaction(body).to_hex() !== txHash)
    throw new Error(
      `Recorded signed hub-oracle nonce transaction does not hash to ${txHash}`,
    );
  const inputs = body.inputs();
  return Array.from({ length: inputs.len() }, (_, index) => ({
    txHash: inputs.get(index).transaction_id().to_hex(),
    outputIndex: Number(inputs.get(index).index()),
  }));
};

const landed = (lucid: LucidEvolution, txHash: string) =>
  Effect.tryPromise({
    try: async () => (await lucid.transactionStatus(txHash)).status,
    catch: (cause) =>
      new Error(
        `Failed to read the status of hub-oracle nonce transaction ${txHash}: ${formatUnknownError(cause)}`,
      ),
  }).pipe(Effect.map((status) => status === "confirmed"));

/**
 * Resumes the nonce transaction recorded before its first submission. A
 * transaction that has not landed is only ever resubmitted byte for byte, so
 * a resume can never create a second nonce; if another transaction spent its
 * inputs the recorded nonce can no longer land and the resume fails closed.
 */
export const resumeSignedHubOracleNonceTransaction = ({
  lucid,
  txHash,
  signedTxCbor,
}: {
  readonly lucid: LucidEvolution;
  readonly txHash: string;
  readonly signedTxCbor: string;
}): Effect.Effect<"landed" | "resubmitted", Error> =>
  Effect.gen(function* () {
    const inputs = yield* Effect.try({
      try: () => spendingInputs(txHash, signedTxCbor),
      catch: (cause) =>
        cause instanceof Error ? cause : new Error(String(cause)),
    });
    if (yield* landed(lucid, txHash)) return "landed";
    const live = yield* Effect.tryPromise({
      try: () => lucid.utxosByOutRef(inputs),
      catch: (cause) =>
        new Error(
          `Failed to read the inputs of hub-oracle nonce transaction ${txHash}: ${formatUnknownError(cause)}`,
        ),
    });
    const spent = inputs
      .filter(
        (input) =>
          !live.some(
            (utxo) =>
              utxo.txHash === input.txHash &&
              utxo.outputIndex === input.outputIndex,
          ),
      )
      .map((input) => `${input.txHash}#${input.outputIndex.toString()}`);
    if (spent.length > 0) {
      // The transaction may have landed between the two reads.
      if (yield* landed(lucid, txHash)) return "landed";
      return yield* Effect.fail(
        new SignedNonceConflictError(
          [
            `Hub-oracle nonce transaction ${txHash} was signed and recorded (run-state step hubOracleNonceSigned) but has not landed,`,
            `and another transaction spent its inputs ${spent.join(", ")}, so it can never land.`,
            "Keep the run state; pass --fresh-redeploy --fresh-redeploy-reason <reason> only when replacing the deployment identity is intentional.",
          ].join(" "),
        ),
      );
    }
    const provider = lucid.config().provider;
    if (provider === undefined)
      return yield* Effect.fail(
        new Error("No L1 provider is available to resubmit the nonce"),
      );
    // Same bytes as the first submission: a node that already has it rejects
    // the duplicate, so only a definite ledger refusal is fatal.
    const submission = yield* Effect.either(
      Effect.tryPromise({
        try: () => provider.submitTx(signedTxCbor),
        catch: (cause) => cause,
      }),
    );
    if (submission._tag === "Right") return "resubmitted";
    const reason = formatUnknownError(submission.left, { includeCause: true });
    if (!isDefiniteLedgerRejection(submission.left)) {
      yield* Effect.logWarning(
        `Resubmitting recorded hub-oracle nonce transaction ${txHash} was inconclusive: ${reason}`,
      );
      return "resubmitted";
    }
    if (yield* landed(lucid, txHash)) return "landed";
    return yield* Effect.fail(
      new SignedNonceRejectedError(
        [
          `Hub-oracle nonce transaction ${txHash} was signed and recorded (run-state step hubOracleNonceSigned) and its inputs are unspent,`,
          `but the ledger rejected it, so these bytes can never land: ${reason}.`,
          "Keep the run state; pass --fresh-redeploy --fresh-redeploy-reason <reason> only when replacing the deployment identity is intentional.",
        ].join(" "),
      ),
    );
  });
