import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  LucidEvolution,
  OutRef,
  TxSignBuilder,
  UTxO,
  coreToTxOutput,
  fromHex,
} from "@lucid-evolution/lucid";
import { Data, Effect, Schedule } from "effect";
import { BlocksTxsDB } from "@/database/index.js";
import { Database } from "@/services/index.js";
import { ImmutableDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";

const RETRY_ATTEMPTS = 1;

const INIT_RETRY_AFTER_MILLIS = 2_000;

const PAUSE_DURATION = "5 seconds";

type SignSubmitContext = {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly walletAddress: string;
};

/**
 * Formats an outref into a stable map key.
 */
const outRefToKey = (txHash: string, outputIndex: number): string =>
  `${txHash}#${outputIndex.toString()}`;

/**
 * Reconciles Lucid's local wallet view after a confirmed transaction.
 *
 * Removes spent inputs and adds new wallet outputs locally, so subsequent
 * transactions don't collide with already-spent UTxOs before the provider
 * refreshes.
 */
const reconcileWalletUtxosFromSignedTx = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
): Effect.Effect<void, never> =>
  Effect.gen(function* () {
    const wallet = lucid.wallet() as {
      getUtxos: () => Promise<UTxO[]>;
      overrideUTxOs?: (utxos: UTxO[]) => void;
    };
    if (typeof wallet.overrideUTxOs !== "function") {
      return;
    }
    const tx = CML.Transaction.from_cbor_hex(submission.signedTxCbor);
    const txInputs = tx.body().inputs();
    const spentOutRefs = new Set<string>();
    for (let index = 0; index < txInputs.len(); index += 1) {
      const input = txInputs.get(index);
      spentOutRefs.add(
        outRefToKey(input.transaction_id().to_hex(), Number(input.index())),
      );
    }
    const walletUtxos = yield* Effect.tryPromise({
      try: () => wallet.getUtxos(),
      catch: () => [] as UTxO[],
    });
    const filteredWalletUtxos = walletUtxos.filter(
      (utxo) => !spentOutRefs.has(outRefToKey(utxo.txHash, utxo.outputIndex)),
    );
    const txOutputs = tx.body().outputs();
    const localWalletOutputs: UTxO[] = [];
    for (let outputIndex = 0; outputIndex < txOutputs.len(); outputIndex += 1) {
      const output = coreToTxOutput(txOutputs.get(outputIndex));
      if (output.address !== submission.walletAddress) {
        continue;
      }
      localWalletOutputs.push({
        txHash: submission.txHash,
        outputIndex,
        address: output.address,
        assets: output.assets,
        datumHash: output.datumHash ?? undefined,
        datum: output.datum ?? undefined,
        scriptRef: output.scriptRef ?? undefined,
      });
    }
    const reconciled = new Map<string, UTxO>();
    for (const utxo of filteredWalletUtxos) {
      reconciled.set(outRefToKey(utxo.txHash, utxo.outputIndex), utxo);
    }
    for (const utxo of localWalletOutputs) {
      reconciled.set(outRefToKey(utxo.txHash, utxo.outputIndex), utxo);
    }
    wallet.overrideUTxOs(Array.from(reconciled.values()));
  }).pipe(Effect.catchAll(() => Effect.void));

/**
 * Handle the signing and submission of a transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmit = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const submission = yield* signSubmitHelper(lucid, signBuilder);
    const txHash = submission.txHash;
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    yield* Effect.tryPromise({
      try: () => lucid.awaitTx(txHash, 10_000),
      catch: (e) =>
        new TxConfirmError({
          message: `Failed to confirm transaction`,
          txHash,
          cause: e,
        }),
    });
    yield* reconcileWalletUtxosFromSignedTx(lucid, submission);
    yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
    yield* Effect.logInfo(`⌛ Pausing for ${PAUSE_DURATION}...`);
    yield* Effect.sleep(PAUSE_DURATION);
    yield* Effect.logInfo("✅ Pause ended.");
    return txHash;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
  );

/**
 * Handle the signing and submission of a transaction without waiting for the
 * transaction to be confirmed.
 *
 * @param lucid - The LucidEvolution instance. Here it's only used for logging the signer's address.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmitNoConfirmation = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, TxSignError | TxSubmitError> =>
  Effect.gen(function* () {
    const submission = yield* signSubmitHelper(lucid, signBuilder);
    return submission.txHash;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
  );

const signSubmitHelper = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<SignSubmitContext, TxSubmitError | TxSignError> =>
  Effect.gen(function* () {
    const walletAddr = yield* Effect.tryPromise(() =>
      lucid.wallet().address(),
    ).pipe(Effect.catchAll((_e) => Effect.succeed("<unknown>")));
    yield* Effect.logInfo(`✍  Signing tx with ${walletAddr}`);
    const txHash = signBuilder.toHash();
    const signedProgram = signBuilder.sign
      .withWallet()
      .completeProgram()
      .pipe(
        Effect.tapError((e) => Effect.logError(e)),
        Effect.mapError(
          (e) =>
            new TxSignError({
              message: `Failed to sign transaction`,
              cause: e,
              txHash,
            }),
        ),
      );
    const signed = yield* signedProgram;
    const signedTxCbor = signed.toCBOR();
    yield* Effect.logInfo(`Signed tx CBOR is:
${signedTxCbor}
`);
    yield* Effect.logInfo("✉️  Submitting transaction...");
    yield* signed.submitProgram().pipe(
      Effect.retry(
        Schedule.compose(
          Schedule.exponential(INIT_RETRY_AFTER_MILLIS),
          Schedule.recurs(RETRY_ATTEMPTS),
        ),
      ),
      Effect.tapError((e) => Effect.logError(e)),
      Effect.mapError(
        (e) =>
          new TxSubmitError({
            message: `Failed to submit transaction`,
            cause: e,
            txHash,
          }),
      ),
    );
    yield* Effect.logInfo(`🚀 Transaction submitted: ${txHash}`);
    return { txHash, signedTxCbor, walletAddress: walletAddr };
  });

/**
 * Fetch transactions of the first block by querying BlocksTxsDB and
 * ImmutableDB.
 *
 * If the given `StateQueueUTxO` is root, it'll return the transactions of the
 * latest merged block. Therefore, Genesis UTxO will return:
 *   { txs: [], headerHash: GENESIS_HEADER_HASH }
 *
 * @param firstBlockUTxO - UTxO of the first block in queue.
 * @returns An Effect that resolves to an array of transactions, and block's
 *          header hash.
 */
export const fetchFirstBlockTxs = (
  firstBlockUTxO: SDK.StateQueueUTxO,
): Effect.Effect<
  { txs: readonly Buffer[]; headerHash: Buffer },
  SDK.DataCoercionError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const headerHashHex =
      yield* SDK.headerHashFromStateQueueUTxO(firstBlockUTxO);
    const headerHash: Buffer = Buffer.from(fromHex(headerHashHex));
    const txHashes =
      yield* BlocksTxsDB.retrieveTxHashesByHeaderHash(headerHash);
    const txs: readonly Buffer[] =
      headerHashHex === SDK.GENESIS_HEADER_HASH
        ? []
        : yield* ImmutableDB.retrieveTxCborsByHashes(txHashes);
    return { txs, headerHash };
  });

export const utxoToOutRef = (utxo: UTxO): OutRef => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
});

export const outRefsAreEqual = (outRef0: OutRef, outRef1: OutRef): boolean => {
  return (
    outRef0.txHash === outRef1.txHash &&
    outRef0.outputIndex === outRef1.outputIndex
  );
};

export class TxSignError extends Data.TaggedError("TxSignError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class TxSubmitError extends Data.TaggedError("TxSubmitError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class TxConfirmError extends Data.TaggedError("TxConfirmError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class GenesisDepositError extends Data.TaggedError(
  "GenesisDepositError",
)<SDK.GenericErrorFields> {}
