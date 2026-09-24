import {
  CML,
  coreToTxOutput,
  type Emulator,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { decodeHistoryChainTransaction } from "../../src/l1-event-history-transaction.js";
import type { LedgerSnapshotOutput } from "../../src/l1-ledger-snapshot.js";

export const historyOutputObservation = (
  output: UTxO,
): LedgerSnapshotOutput => ({
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

const rawRef = (ref: { txHash: string; outputIndex: number }) => ({
  transaction: { id: ref.txHash },
  index: ref.outputIndex,
});

const rawValue = (assets: Readonly<Record<string, bigint>>) => {
  const value: Record<string, Record<string, bigint>> = {
    ada: { lovelace: assets.lovelace ?? 0n },
  };
  for (const [unit, quantity] of Object.entries(assets)) {
    if (unit !== "lovelace")
      (value[unit.slice(0, 56)] ??= {})[unit.slice(56)] = quantity;
  }
  return value;
};

/** CBOR supplies transaction fields; successful emulator submission supplies
 * the validity disposition. This adapter supplies no canonical-chain claim. */
export const submitHistoryObservation = async (
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
  return observeConfirmedHistoryTransaction(lucid, signedCbor, historical);
};

const observeConfirmedHistoryTransaction = async (
  lucid: LucidEvolution,
  signedCbor: string,
  historical: readonly UTxO[],
) => {
  const tx = CML.Transaction.from_cbor_hex(signedCbor);
  const body = tx.body();
  const txHash = CML.hash_transaction(body).to_hex();
  const references = refs(body.reference_inputs());
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
  expect(actualOutputs.map(historyOutputObservation)).toEqual(
    outputs.map(historyOutputObservation),
  );
  const mint: Record<string, Record<string, bigint>> = {};
  const minted = body.mint();
  const policies = minted?.keys();
  for (let i = 0; i < (policies?.len() ?? 0); i++) {
    const policy = policies!.get(i);
    const assets = minted!.get_assets(policy)!;
    const names = assets.keys();
    mint[policy.to_hex()] = {};
    for (let j = 0; j < names.len(); j++) {
      const name = names.get(j);
      mint[policy.to_hex()]![name.to_hex()] = assets.get(name)!;
    }
  }
  const withdrawals: Record<string, { ada: { lovelace: bigint } }> = {};
  const rewards = body.withdrawals();
  const accounts = rewards?.keys();
  for (let i = 0; i < (accounts?.len() ?? 0); i++) {
    const account = accounts!.get(i);
    withdrawals[account.to_address().to_bech32()] = {
      ada: { lovelace: rewards!.get(account)! },
    };
  }
  const purposes = ["spend", "mint", "publish", "withdraw", "vote", "propose"];
  const flat = tx.witness_set().redeemers()?.to_flat_format();
  let executionMemory = 0n;
  let executionSteps = 0n;
  const redeemers = Array.from({ length: flat?.len() ?? 0 }, (_, i) => {
    const redeemer = flat!.get(i);
    executionMemory += redeemer.ex_units().mem();
    executionSteps += redeemer.ex_units().steps();
    return {
      validator: {
        purpose: purposes[redeemer.tag()],
        index: Number(redeemer.index()),
      },
      redeemer: redeemer.data().to_cbor_hex(),
    };
  });
  const transaction = decodeHistoryChainTransaction({
    id: txHash,
    spends: "inputs",
    inputs: refs(body.inputs()).map(rawRef),
    references: references.map(rawRef),
    collaterals: refs(body.collateral_inputs()).map(rawRef),
    outputs: outputs.map((output) => ({
      address: output.address,
      value: rawValue(output.assets),
      ...(output.datum == null ? {} : { datum: output.datum }),
      ...(output.datumHash == null ? {} : { datumHash: output.datumHash }),
      ...(output.scriptRef == null ? {} : { script: output.scriptRef }),
    })),
    mint,
    withdrawals,
    redeemers,
    validityInterval: {
      ...(body.validity_interval_start() === undefined
        ? {}
        : { invalidBefore: Number(body.validity_interval_start()) }),
      ...(body.ttl() === undefined ? {} : { invalidAfter: Number(body.ttl()) }),
    },
  });
  return {
    transaction,
    signedCbor,
    historical: historical.map(historyOutputObservation),
    measurement: {
      completeSignedBytes: signedCbor.length / 2,
      fee: body.fee(),
      executionMemory,
      executionSteps,
    },
  };
};

export type AcceptedHistoryObservation = Awaited<
  ReturnType<typeof submitHistoryObservation>
>;

/** Observe the pipeline's own submissions and confirmations without advancing
 * its clock or changing signed bytes. Exact references are captured before
 * submission, while the referenced frontier still exists. */
export const captureConfirmedHistoryObservations = (
  lucid: LucidEvolution,
  emulator: Emulator,
  onConfirmed: (
    observations: readonly AcceptedHistoryObservation[],
  ) => Promise<void>,
) => {
  const submit = emulator.submitTx.bind(emulator);
  const awaitTx = emulator.awaitTx.bind(emulator);
  const pending = new Map<string, { signedCbor: string; historical: UTxO[] }>();
  const observed = new Set<string>();
  const flush = async () => {
    const confirmed: AcceptedHistoryObservation[] = [];
    for (const [hash, item] of pending) {
      if ((await lucid.transactionStatus(hash)).status !== "confirmed") break;
      confirmed.push(
        await observeConfirmedHistoryTransaction(
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
    restore: () => {
      emulator.submitTx = submit;
      emulator.awaitTx = awaitTx;
    },
  };
};
