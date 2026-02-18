import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  Emulator,
  Lucid,
  generateEmulatorAccount,
  utxoToTransactionInput,
  utxoToTransactionOutput,
} from "@lucid-evolution/lucid";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const fixturesDir = path.resolve(__dirname, "../fixtures");
const initialLedgerPath = path.join(fixturesDir, "initial-ledger-state.json");
const txSequencePath = path.join(fixturesDir, "tx-sequence.json");

const txCount = Number.parseInt(process.env.BENCH_FIXTURE_TX_COUNT ?? "5000", 10);
const accountCount = Number.parseInt(
  process.env.BENCH_FIXTURE_ACCOUNT_COUNT ?? "3",
  10,
);
const initialLovelace = BigInt(
  process.env.BENCH_FIXTURE_INITIAL_LOVELACE ?? "2000000000000",
);
const transferLovelace = BigInt(
  process.env.BENCH_FIXTURE_TRANSFER_LOVELACE ?? "2000000",
);

if (Number.isNaN(txCount) || txCount < 4000) {
  throw new Error("BENCH_FIXTURE_TX_COUNT must be >= 4000");
}
if (Number.isNaN(accountCount) || accountCount < 2) {
  throw new Error("BENCH_FIXTURE_ACCOUNT_COUNT must be >= 2");
}

console.log(
  `Generating validation fixtures: txCount=${txCount}, accountCount=${accountCount}`,
);

const accounts = Array.from({ length: accountCount }, () =>
  generateEmulatorAccount({ lovelace: initialLovelace }),
);

const emulator = new Emulator(accounts);
const lucid = await Lucid(emulator, "Custom");

const walletContexts = [];
for (const account of accounts) {
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const address = await lucid.wallet().address();
  walletContexts.push({
    seedPhrase: account.seedPhrase,
    address,
    assets: Object.fromEntries(
      Object.entries(account.assets).map(([unit, amount]) => [
        unit,
        amount.toString(),
      ]),
    ),
  });
}

const ledgerEntries = [];
for (const wallet of walletContexts) {
  const utxos = await emulator.getUtxos(wallet.address);
  for (const utxo of utxos) {
    const outRef = utxoToTransactionInput(utxo);
    const output = utxoToTransactionOutput(utxo);
    ledgerEntries.push({
      txIdHex: utxo.txHash,
      outRefCborHex: Buffer.from(outRef.to_cbor_bytes()).toString("hex"),
      outputCborHex: Buffer.from(output.to_cbor_bytes()).toString("hex"),
      address: utxo.address,
    });
  }
}

const transactions = [];
for (let i = 0; i < txCount; i++) {
  const sender = walletContexts[i % walletContexts.length];
  const recipient = walletContexts[(i + 1) % walletContexts.length];

  lucid.selectWallet.fromSeed(sender.seedPhrase);
  const tx = await lucid
    .newTx()
    .pay.ToAddress(recipient.address, { lovelace: transferLovelace })
    .complete();
  const signed = await tx.sign.withWallet().complete();
  const txId = await signed.submit();
  await lucid.awaitTx(txId);

  transactions.push({
    index: i + 1,
    txId,
    cborHex: signed.toCBOR(),
    fromAddress: sender.address,
    toAddress: recipient.address,
  });

  if ((i + 1) % 500 === 0) {
    console.log(`Generated ${i + 1}/${txCount} transactions`);
  }
}

fs.mkdirSync(fixturesDir, { recursive: true });

const generatedAtIso = new Date().toISOString();
const initialLedgerFixture = {
  network: "Custom",
  generatedAtIso,
  ledgerEntries,
  accounts: walletContexts.map((wallet) => ({
    address: wallet.address,
    seedPhrase: wallet.seedPhrase,
    assets: wallet.assets,
  })),
};
const txSequenceFixture = {
  network: "Custom",
  generatedAtIso,
  txCount: transactions.length,
  transactions,
};

fs.writeFileSync(initialLedgerPath, `${JSON.stringify(initialLedgerFixture, null, 2)}\n`);
fs.writeFileSync(txSequencePath, `${JSON.stringify(txSequenceFixture, null, 2)}\n`);

console.log(`Wrote ${initialLedgerPath}`);
console.log(`Wrote ${txSequencePath}`);
