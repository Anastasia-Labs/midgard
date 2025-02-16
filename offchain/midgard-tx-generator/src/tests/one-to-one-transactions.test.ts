import fs from 'node:fs';
import path from 'node:path';

import { Network } from '@lucid-evolution/lucid';
import chalk from 'chalk';
import { describe, expect, it } from 'vitest';

import { SerializedMidgardTransaction } from '../core/types.js';
import { generateOneToOneTransactions } from '../generators/one-to-one-transactions.js';
import { generateTestWallet } from './utils.js';

describe('One-to-One Transaction Generation', () => {
  it('Generate valid one-to-one transactions for midgard-node testing', async () => {
    const { privateKey, testUTxO, seedPhrase } = await generateTestWallet();

    console.log('\n' + chalk.bold.green('🔑 One-to-One Test Wallet'));
    console.log(chalk.cyan('━━━━━━━━━━━━━━━━━━━━━━━'));
    console.log(chalk.dim('Seed Phrase:'), seedPhrase || 'N/A');
    console.log(chalk.dim('Private Key:'), chalk.yellow(privateKey));
    console.log(chalk.dim('Address:'), chalk.yellow(testUTxO.address));
    console.log(
      chalk.dim('Initial Balance:'),
      chalk.green(`${testUTxO.assets.lovelace.toString()} lovelace`) +
        chalk.gray(` (${Number(testUTxO.assets.lovelace) / 1_000_000} ADA)`)
    );

    const result = await generateOneToOneTransactions({
      network: 'Preview' as Network,
      initialUTxO: testUTxO,
      txsCount: 100,
      walletSeedOrPrivateKey: privateKey,
    });

    const parsed = JSON.parse(result!);
    const outputDir = path.join(process.cwd(), 'test-output');
    if (!fs.existsSync(outputDir)) {
      fs.mkdirSync(outputDir);
    }

    const outputPath = path.join(outputDir, 'one-to-one-transactions.json');
    fs.writeFileSync(outputPath, JSON.stringify(parsed, null, 2));

    console.log(
      '\n' + chalk.bold.green('📝 Generated One-to-One Transactions')
    );
    console.log(chalk.green('━━━━━━━━━━━━━━━━━━━━━━━━━━'));
    console.log(chalk.dim('Output File:'), chalk.underline.blue(outputPath));
    console.log(
      chalk.dim('Transaction Count:'),
      chalk.yellow(parsed.clientDatasets[0].txSequence.length)
    );

    expect(parsed).toHaveProperty('clientDatasets');
    expect(parsed.clientDatasets).toHaveLength(1);
    expect(parsed.clientDatasets[0]).toHaveProperty('txSequence');

    const txs = parsed.clientDatasets[0].txSequence;
    expect(txs).toHaveLength(100);

    // verify each transaction matches Midgard format
    txs.forEach((tx: SerializedMidgardTransaction) => {
      expect(tx).toHaveProperty('type', 'Midgard L2 User Transaction');
      expect(tx).toHaveProperty(
        'description',
        'One-to-One Self Transfer (Single Input/Output)'
      );
      expect(tx).toHaveProperty('cborHex', expect.any(String));
      expect(tx).toHaveProperty('txId', expect.any(String));
    });
  });
});
