import {
  CML,
  coreToTxOutput,
  type Emulator,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

/** One output as the emulator ledger holds it, in plain comparable form. */
export type ObservedOutput = Readonly<{
  txHash: string;
  outputIndex: number;
  address: string;
  assets: Readonly<Record<string, bigint>>;
  datum?: string;
  datumHash?: string;
  hasReferenceScript: boolean;
}>;

export const outputObservation = (output: UTxO): ObservedOutput => ({
  txHash: output.txHash,
  outputIndex: output.outputIndex,
  address: output.address,
  assets: { ...output.assets },
  ...(output.datum == null ? {} : { datum: output.datum }),
  ...(output.datumHash == null ? {} : { datumHash: output.datumHash }),
  hasReferenceScript: output.scriptRef != null,
});

const refs = (inputs: CML.TransactionInputList | undefined) =>
  Array.from({ length: inputs?.len() ?? 0 }, (_, index) => {
    const input = inputs!.get(index);
    return {
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    };
  });

/** Signs and submits `unsigned`, waits for it to confirm, and returns what the
 * emulator ledger holds for it. Successful emulator submission supplies the
 * validity disposition; this adapter makes no canonical-chain claim. */
export const submitObservedTransaction = async (
  lucid: LucidEvolution,
  unsigned: TxSignBuilder,
) => {
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const tx = CML.Transaction.from_cbor_hex(signedCbor);
  const body = tx.body();
  const references = refs(body.reference_inputs());
  const historical = await lucid.utxosByOutRef(references);
  expect(historical).toHaveLength(references.length);
  const txHash = await signed.submit();
  expect(txHash).toBe(signed.toHash());
  expect(await lucid.awaitTx(txHash)).toBe(true);
  return observeConfirmedTransaction(lucid, signedCbor, historical);
};

const observeConfirmedTransaction = async (
  lucid: LucidEvolution,
  signedCbor: string,
  historical: readonly UTxO[],
) => {
  const tx = CML.Transaction.from_cbor_hex(signedCbor);
  const body = tx.body();
  const txHash = CML.hash_transaction(body).to_hex();
  expect((await lucid.transactionStatus(txHash)).status).toBe("confirmed");
  expect(tx.is_valid()).toBe(true);

  const outputs = Array.from(
    { length: body.outputs().len() },
    (_, outputIndex) => ({
      ...coreToTxOutput(body.outputs().get(outputIndex)),
      txHash,
      outputIndex,
    }),
  );
  const actualOutputs = await lucid.utxosByOutRef(outputs);
  expect(actualOutputs.map(outputObservation)).toEqual(
    outputs.map(outputObservation),
  );
  const flat = tx.witness_set().redeemers()?.to_flat_format();
  let executionMemory = 0n;
  let executionSteps = 0n;
  for (let i = 0; i < (flat?.len() ?? 0); i++) {
    const redeemer = flat!.get(i);
    executionMemory += redeemer.ex_units().mem();
    executionSteps += redeemer.ex_units().steps();
  }
  return {
    transaction: { txHash, inputs: refs(body.inputs()), outputs },
    signedCbor,
    historical: historical.map(outputObservation),
    measurement: {
      completeSignedBytes: signedCbor.length / 2,
      fee: body.fee(),
      executionMemory,
      executionSteps,
    },
  };
};

export type ConfirmedTransactionObservation = Awaited<
  ReturnType<typeof submitObservedTransaction>
>;

/** What an observer has seen so far, as plain data: a restored emulator
 * (see `openPublishedLifecycle`) resumes observing from it. */
export type ObservationState = {
  readonly pending: ReadonlyMap<
    string,
    { readonly signedCbor: string; readonly historical: readonly UTxO[] }
  >;
  readonly observed: ReadonlySet<string>;
};

/** Observe the pipeline's own submissions and confirmations without advancing
 * its clock or changing signed bytes. Exact references are captured before
 * submission, while the referenced frontier still exists. */
export const captureConfirmedTransactions = (
  lucid: LucidEvolution,
  emulator: Emulator,
  onConfirmed: (
    observations: readonly ConfirmedTransactionObservation[],
  ) => Promise<void>,
  initial?: ObservationState,
) => {
  const submit = emulator.submitTx.bind(emulator);
  const awaitTx = emulator.awaitTx.bind(emulator);
  const pending = new Map<string, { signedCbor: string; historical: UTxO[] }>(
    [...(initial?.pending ?? [])].map(([hash, item]) => [
      hash,
      structuredClone({ ...item, historical: [...item.historical] }),
    ]),
  );
  const observed = new Set<string>(initial?.observed);
  const flush = async () => {
    const confirmed: ConfirmedTransactionObservation[] = [];
    for (const [hash, item] of pending) {
      if ((await lucid.transactionStatus(hash)).status !== "confirmed") break;
      confirmed.push(
        await observeConfirmedTransaction(
          lucid,
          item.signedCbor,
          item.historical,
        ),
      );
    }
    if (confirmed.length === 0) return;
    await onConfirmed(confirmed);
    for (const { transaction } of confirmed) {
      pending.delete(transaction.txHash);
      observed.add(transaction.txHash);
    }
  };
  emulator.submitTx = async (signedCbor) => {
    await flush();
    const transaction = CML.Transaction.from_cbor_hex(signedCbor);
    const references = refs(transaction.body().reference_inputs());
    const historical = await lucid.utxosByOutRef(references);
    expect(historical).toHaveLength(references.length);
    const hash = await submit(signedCbor);
    expect(hash).toBe(CML.hash_transaction(transaction.body()).to_hex());
    if (!observed.has(hash)) pending.set(hash, { signedCbor, historical });
    return hash;
  };
  emulator.awaitTx = async (hash) => {
    const confirmed = await awaitTx(hash);
    await flush();
    return confirmed;
  };
  return {
    flush,
    pendingCount: () => pending.size,
    /** Stop waiting for a submission the test dropped from the emulator
     * before any block included it; it will never confirm. */
    forgetDropped: (hash: string) => {
      if (!pending.delete(hash))
        throw new Error(`Submission ${hash} is not pending observation`);
    },
    restore: () => {
      emulator.submitTx = submit;
      emulator.awaitTx = awaitTx;
    },
    state: (): ObservationState => structuredClone({ pending, observed }),
  };
};
