import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { CML, type LucidEvolution, type OutRef } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export class SignedNonceConflictError extends Error {
  override readonly name = "SignedNonceConflictError";
}

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
    // the duplicate, so a failed resubmission is inconclusive, never fatal.
    yield* Effect.tryPromise(() => provider.submitTx(signedTxCbor)).pipe(
      Effect.catchAll((cause) =>
        Effect.logWarning(
          `Resubmitting recorded hub-oracle nonce transaction ${txHash} was inconclusive: ${formatUnknownError(cause, { includeCause: true })}`,
        ),
      ),
    );
    return "resubmitted";
  });
