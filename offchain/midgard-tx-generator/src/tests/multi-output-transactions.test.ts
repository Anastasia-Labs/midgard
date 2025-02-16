import fs from 'node:fs';
import path from 'node:path';

import { Network } from '@lucid-evolution/lucid';
import chalk from 'chalk';
import { describe, expect, it } from 'vitest';

import { SerializedMidgardTransaction } from '../core/types.js';
import { generateMultiOutputTransactions } from '../generators/multi-output-transactions.js';
import { generateTestWallet } from './utils.js';

describe('Multi-Output Transaction Generation', () => {
  it('Generate valid multi-output transactions for midgard-node testing', async () => {
    const { privateKey, testUTxO, seedPhrase } = await generateTestWallet();

    console.log('\n' + chalk.bold.green('🔑 Multi-Output Test Wallet'));
    console.log(chalk.cyan('━━━━━━━━━━━━━━━━━━━━━━━━━'));
    console.log(chalk.dim('Seed Phrase:'), seedPhrase || 'N/A');
    console.log(chalk.dim('Private Key:'), chalk.yellow(privateKey));
    console.log(chalk.dim('Address:'), chalk.yellow(testUTxO.address));
    console.log(
      chalk.dim('Initial Balance:'),
      chalk.green(`${testUTxO.assets.lovelace.toString()} lovelace`) +
        chalk.gray(` (${Number(testUTxO.assets.lovelace) / 1_000_000} ADA)`)
    );

    const result = await generateMultiOutputTransactions({
      network: 'Preview' as Network,
      initialUTxO: testUTxO,
      utxosCount: 100,
      walletSeedOrPrivateKey: privateKey,
    });

    const parsed = JSON.parse(result!);
    const outputDir = path.join(process.cwd(), 'test-output');
    if (!fs.existsSync(outputDir)) {
      fs.mkdirSync(outputDir);
    }

    const outputPath = path.join(outputDir, 'multi-output-transactions.json');
    fs.writeFileSync(outputPath, JSON.stringify(parsed, null, 2));

    // display transaction details
    console.log(
      '\n' + chalk.bold.green('📝 Generated Multi-Output Transactions')
    );
    console.log(chalk.green('━━━━━━━━━━━━━━━━━━━━━━━━━━'));
    console.log(chalk.dim('Output File:'), chalk.underline.blue(outputPath));
    console.log(
      chalk.dim('Transaction Count:'),
      chalk.yellow(parsed.clientDatasets[0].txSequence.length)
    );

    // transaction structure
    expect(parsed).toHaveProperty('clientDatasets');
    expect(parsed.clientDatasets).toHaveLength(1);
    expect(parsed.clientDatasets[0]).toHaveProperty('txSequence');

    const txs = parsed.clientDatasets[0].txSequence;
    expect(txs.length).toBeGreaterThan(0);

    txs.forEach((tx: SerializedMidgardTransaction) => {
      expect(tx).toHaveProperty('type', 'Midgard L2 User Transaction');
      // transactions should be either a distribution or collection
      expect(
        tx.description === 'Multi-Output Distribution (1-to-20 Split)' ||
          tx.description === 'Multi-Output Collection (20-to-1 Merge)'
      ).toBeTruthy();
      expect(tx).toHaveProperty('cborHex', expect.any(String));
      expect(tx).toHaveProperty('txId', expect.any(String));
    });

    const distributionTxs = txs.filter(
      (tx: SerializedMidgardTransaction) =>
        tx.description === 'Multi-Output Distribution (1-to-20 Split)'
    );
    const collectionTxs = txs.filter(
      (tx: SerializedMidgardTransaction) =>
        tx.description === 'Multi-Output Collection (20-to-1 Merge)'
    );

    expect(distributionTxs.length).toBeGreaterThan(0);
    expect(collectionTxs.length).toBeGreaterThan(0);
  });
});
