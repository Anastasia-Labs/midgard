import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import { assetsToValue, CML, walletFromSeed } from "@lucid-evolution/lucid";
import { entropyToMnemonic } from "bip39";
import { buildTransferTxWithMinFee } from "midgard-node/commands/submit-l2-transfer";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "midgard-node/tests/midgard-output-helpers";

import type { StackRunExpectation } from "../src/commands/e2e-finalize-summary.js";
import { writeDurableJson } from "../src/full-stack/journal.js";

export const makeRunDirectory = createTrackedTempDirFactory(
  "midgard-finalize-stack-run-",
);

export const INTENT_DIGEST = "a".repeat(64);
export const MANIFEST_ID = "b".repeat(64);
export const RUN_ID = "stack-run-honest";
export const DEPOSIT_EVENT = "de".repeat(32);
export const WITHDRAWAL_EVENT = "ee".repeat(32);
export const HEADER_HASH = "cd".repeat(28);
const DEPOSIT = 20_000_000n;
const TRANSFER = 10_000_000n;
const USER_FUNDS = 50_000_000n;
export const STEP_IDS = [
  "providers",
  "services",
  "cycle-0-deposit",
  "cycle-0-transfer",
  "cycle-0-withdrawal",
] as const;

export const wallets = () => ({
  user: walletFromSeed(entropyToMnemonic("00".repeat(16)), {
    network: "Preprod",
  }),
  recipient: walletFromSeed(entropyToMnemonic("11".repeat(16)), {
    network: "Preprod",
  }),
});

/** A Cardano payout transaction with the given outputs, as payout-body.ts reads it. */
export function payoutTransaction(
  outputs: readonly { address: string; lovelace: bigint }[],
) {
  const list = CML.TransactionOutputList.new();
  for (const { address, lovelace } of outputs)
    list.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(lovelace),
      ),
    );
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    list,
    1n,
  );
  return {
    cbor: CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex(),
    txHash: CML.hash_transaction(body).to_hex(),
  };
}

export type HonestRun = {
  readonly expectation: StackRunExpectation;
  readonly transferTxId: string;
  readonly payout: { readonly cbor: string; readonly txHash: string };
  readonly recipientAddress: string;
  /** Rewrites one JSON record of the run directory. */
  edit(name: string, change: (value: any) => unknown): Promise<void>;
  /** Rewrites one confirmed step's journal data. */
  editStep(id: string, change: (data: any) => unknown): Promise<void>;
};

/**
 * One confirmed wallet-journey cycle written exactly as journey.ts, payout.ts,
 * public-da.ts and controller.ts write it. The transfer is a real signed
 * Midgard transaction and the payout a real Cardano body.
 */
export async function honestRun(): Promise<HonestRun> {
  const runDirectory = await makeRunDirectory();
  const { user, recipient } = wallets();
  const utxoHash = "44".repeat(32);
  const built = await buildTransferTxWithMinFee({
    senderAddress: user.address,
    destinationAddress: recipient.address,
    signer: user.paymentKey,
    availableUtxos: [
      {
        txHash: utxoHash,
        outputIndex: 0,
        outrefCbor: makeOutRefCbor(utxoHash, 0),
        outputCbor: Buffer.from(
          makeMidgardTxOutput(
            CML.Address.from_bech32(user.address),
            assetsToValue({ lovelace: USER_FUNDS }),
          ).to_cbor_bytes(),
        ),
        address: user.address,
        assets: { lovelace: USER_FUNDS },
      },
    ],
    requestedAssets: { lovelace: TRANSFER },
    network: "Preprod",
    networkId: 0n,
    minFeeA: 44n,
    minFeeB: 155381n,
  });
  const txId = built.txIdHex;
  const fee = built.fee;
  const payout = payoutTransaction([
    { address: recipient.address, lovelace: TRANSFER },
  ]);
  const payload = Buffer.from("public da payload envelope");
  const da = {
    headerHash: HEADER_HASH,
    peerId: "committee-peer",
    sha256: createHash("sha256").update(payload).digest("hex"),
    bytes: payload.length,
    deploymentId: MANIFEST_ID,
  };
  const assets = { lovelace: String(TRANSFER) };
  const records: Record<string, unknown> = {
    "cycle-0-deposit-intent.json": { balance: String(USER_FUNDS - DEPOSIT) },
    "cycle-0-deposit.json": {
      metadata: { depositEventId: DEPOSIT_EVENT },
    },
    "cycle-0-deposit-balance.json": {
      before: String(USER_FUNDS - DEPOSIT),
      after: String(USER_FUNDS),
      credited: String(DEPOSIT),
    },
    "cycle-0-transfer.json": {
      txId,
      signedTxCbor: built.txHex,
      fee: String(fee),
      recipientBalanceBefore: "0",
      senderBalanceBefore: String(USER_FUNDS),
    },
    "cycle-0-transfer-balance.json": {
      senderBalance: String(USER_FUNDS - TRANSFER - fee),
      recipientBalance: String(TRANSFER),
      fee: String(fee),
      received: String(TRANSFER),
    },
    [`da-${HEADER_HASH}.json`]: da,
    "cycle-0-withdrawal-intent.json": {
      l2OutRef: `${txId}#0`,
      address: recipient.address,
      recipientBalanceBefore: String(TRANSFER),
      assets,
    },
    "cycle-0-withdrawal.json": { withdrawalEventId: WITHDRAWAL_EVENT },
    "cycle-0-withdrawal-balance.json": {
      before: String(TRANSFER),
      after: "0",
      debited: String(TRANSFER),
    },
  };
  const data: Record<string, unknown> = {
    providers: { kupo: "http://127.0.0.1:1442" },
    services: { started: true },
    "cycle-0-deposit": {
      eventId: DEPOSIT_EVENT,
      observed: { jobs: [{ phase: "complete" }], attempts: null },
    },
    "cycle-0-transfer": {
      merged: {
        txId,
        status: "committed",
        headerHash: HEADER_HASH,
        confirmedLedgerFinalized: true,
      },
      da,
    },
    "cycle-0-withdrawal": {
      eventId: WITHDRAWAL_EVENT,
      txHash: payout.txHash,
      outputIndex: 0,
      address: recipient.address,
      assets,
      observation: {
        transactionHash: payout.txHash,
        signedTransactionCborHex: payout.cbor,
        status: "included",
      },
    },
  };
  for (const [name, value] of Object.entries(records))
    await writeDurableJson(join(runDirectory, name), value);
  await writeFile(join(runDirectory, `da-${HEADER_HASH}.cbor`), payload);
  const journal = {
    schemaVersion: "midgard-full-stack-v1",
    runId: RUN_ID,
    intentDigest: INTENT_DIGEST,
    steps: Object.fromEntries(
      STEP_IDS.map((id) => [
        id,
        { status: "complete", attempts: 1, data: data[id] },
      ]),
    ),
  };
  await writeDurableJson(join(runDirectory, "stack-journal.json"), journal);
  await writeDurableJson(join(runDirectory, "journey-summary.json"), {
    schemaVersion: "midgard-full-stack-summary-v1",
    runId: RUN_ID,
    result: "wallet-journey-complete",
    network: "Preprod",
    confirmedSteps: [...STEP_IDS],
    journal: join(runDirectory, "stack-journal.json"),
    services: "docker-compose",
    cycles: 1,
  });
  const edit = async (name: string, change: (value: any) => unknown) => {
    const path = join(runDirectory, name);
    const value = JSON.parse(await readFile(path, "utf8"));
    await writeDurableJson(path, change(value) ?? value);
  };
  return {
    expectation: {
      runDirectory,
      intentDigest: INTENT_DIGEST,
      manifestId: MANIFEST_ID,
      stepIds: STEP_IDS,
      cycles: 1,
      depositLovelace: String(DEPOSIT),
      transferLovelace: String(TRANSFER),
    },
    transferTxId: txId,
    payout,
    recipientAddress: recipient.address,
    edit,
    editStep: (id, change) =>
      edit("stack-journal.json", (value) => {
        const step = value.steps[id];
        step.data = change(step.data) ?? step.data;
      }),
  };
}
