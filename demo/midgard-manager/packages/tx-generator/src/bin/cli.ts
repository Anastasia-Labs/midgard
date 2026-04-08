import { mkdir, writeFile } from 'node:fs/promises';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { Network } from '@lucid-evolution/lucid';

import {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from '../lib/generators/index.js';
import { generateTestWallet } from '../utils/test-utils.js';

/**
 * ESM-compatible location helpers used for writing output fixtures.
 */
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Default configuration for the standalone tx-generator demo CLI.
 */
const DEFAULT_CONFIG = {
  network: 'Preview' as Network,
  txCount: 100,
  outputDir: 'test-output',
};

/**
 * Generates sample one-to-one and multi-output transaction fixtures.
 */
async function main() {
  // Create test wallet
  const { privateKey, testUTxO } = await generateTestWallet();

  // output directory exists
  const projectRoot = join(__dirname, '../../..');
  const outputDir = join(projectRoot, DEFAULT_CONFIG.outputDir);
  await mkdir(outputDir, { recursive: true });

  console.log('Starting transaction generation...');
  console.log('Network:', DEFAULT_CONFIG.network);
  console.log('Transaction Count:', DEFAULT_CONFIG.txCount);
  console.log('Output Directory:', DEFAULT_CONFIG.outputDir);
  console.log('\nTest Wallet:');
  console.log('Private Key:', privateKey);
  console.log('Address:', testUTxO.address);
  console.log('Balance:', testUTxO.assets.lovelace.toString(), 'lovelace');

  // Generate one-to-one transactions
  console.log('\nGenerating one-to-one transactions...');
  const oneToOneResult = await generateOneToOneTransactions({
    network: DEFAULT_CONFIG.network,
    initialUTxO: testUTxO,
    txsCount: DEFAULT_CONFIG.txCount,
    walletSeedOrPrivateKey: privateKey,
  });

  // Generate multi-output transactions
  console.log('Generating multi-output transactions...');
  const multiOutputResult = await generateMultiOutputTransactions({
    network: DEFAULT_CONFIG.network,
    initialUTxO: testUTxO,
    utxosCount: DEFAULT_CONFIG.txCount * 20,
    finalUtxosCount: 1,
    walletSeedOrPrivateKey: privateKey,
  });

  // Write generated results to file if node is not running
  const oneToOnePath = join(outputDir, 'one-to-one-transactions.json');
  const multiOutputPath = join(outputDir, 'multi-output-transactions.json');

  await writeFile(oneToOnePath, JSON.stringify(oneToOneResult, null, 2));
  await writeFile(multiOutputPath, JSON.stringify(multiOutputResult, null, 2));

  console.log('\nDone! Transactions have been written to:');
  console.log(`- ${oneToOnePath}`);
  console.log(`- ${multiOutputPath}`);
}

/**
 * Top-level CLI error handler for the standalone tx-generator script.
 */
main().catch((error) => {
  console.error('Error generating transactions:', error);
  process.exit(1);
});
