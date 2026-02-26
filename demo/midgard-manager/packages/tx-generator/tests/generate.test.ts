import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { CML } from '@lucid-evolution/lucid';

import { MidgardNodeClient } from '../src/lib/client/node-client';
import { getGeneratorStatus, startGenerator, stopGenerator } from '../src/lib/scheduler/scheduler';
import { TransactionGeneratorConfig } from '../src/lib/types';
import { txTest } from './setup';

// Mock the node client
vi.mock('../src/lib/client/node-client', () => {
  const mockUtxos = [
    {
      txHash: '11'.repeat(32),
      outputIndex: 0,
      assets: { lovelace: 2_000_000n },
      address:
        'addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs',
      datum: null,
      datumHash: null,
      scriptRef: null,
    },
    {
      txHash: '22'.repeat(32),
      outputIndex: 1,
      assets: { lovelace: 2_500_000n },
      address:
        'addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs',
      datum: null,
      datumHash: null,
      scriptRef: null,
    },
  ];
  return {
    MidgardNodeClient: vi.fn().mockImplementation(() => {
      return {
        submitTransaction: vi.fn().mockResolvedValue({ txId: 'mock_tx_id' }),
        isAvailable: vi.fn().mockResolvedValue(true),
        getSpendableUtxos: vi.fn().mockResolvedValue(mockUtxos),
      };
    }),
  };
});

// Mock the transaction generation functions
vi.mock('../src/lib/generators/index.js', () => {
  return {
    generateOneToOneTransactions: vi.fn().mockResolvedValue([
      { txId: 'test_tx_1', cborHex: 'mock_cbor_1', type: 'one-to-one' },
      { txId: 'test_tx_2', cborHex: 'mock_cbor_2', type: 'one-to-one' },
    ]),
    generateMultiOutputTransactions: vi.fn().mockResolvedValue([
      { txId: 'test_tx_3', cborHex: 'mock_cbor_3', type: 'multi-output' },
      { txId: 'test_tx_4', cborHex: 'mock_cbor_4', type: 'multi-output' },
    ]),
  };
});

describe('Transaction Generator', () => {
  const walletKey = CML.PrivateKey.generate_ed25519().to_bech32();
  const initialUTxO = {
    txHash: '33'.repeat(32),
    outputIndex: 0,
    assets: { lovelace: 5_000_000n },
    address:
      'addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs',
    datum: null,
    datumHash: null,
    scriptRef: null,
  } as const;

  // Reset mocks before each test
  beforeEach(() => {
    vi.clearAllMocks();
  });

  // Clean up after each test
  afterEach(async () => {
    await stopGenerator();
  });

  it('should start and stop the generator correctly', async () => {
    // Default minimal config for testing
    const config: Partial<TransactionGeneratorConfig> = {
      walletSeedOrPrivateKey: walletKey,
      nodeEndpoint: 'http://localhost:3000',
      initialUTxO,
      transactionType: 'one-to-one',
      batchSize: 2,
      interval: 0.1,
      concurrency: 1,
    };

    // Start the generator
    await startGenerator(config);

    // Check if the generator is running
    const status = getGeneratorStatus();
    expect(status.running).toBe(true);

    // Stop the generator
    await stopGenerator();

    // Check that the generator has stopped
    const finalStatus = getGeneratorStatus();
    expect(finalStatus.running).toBe(false);
  });

  it('should handle different transaction types', async () => {
    // Test with one-to-one type
    const oneToOneConfig: Partial<TransactionGeneratorConfig> = {
      walletSeedOrPrivateKey: walletKey,
      nodeEndpoint: 'http://localhost:3000',
      initialUTxO,
      transactionType: 'one-to-one',
      batchSize: 2,
      interval: 0.1,
    };

    await startGenerator(oneToOneConfig);

    // Give it a moment to generate transactions
    await new Promise((resolve) => setTimeout(resolve, 200));

    // Check status and stats
    const oneToOneStatus = getGeneratorStatus();
    expect(oneToOneStatus.transactionsGenerated).toBeGreaterThan(0);

    await stopGenerator();

    // Test with multi-output type
    const multiOutputConfig: Partial<TransactionGeneratorConfig> = {
      walletSeedOrPrivateKey: walletKey,
      nodeEndpoint: 'http://localhost:3000',
      initialUTxO,
      transactionType: 'multi-output',
      batchSize: 2,
      interval: 0.1,
    };

    await startGenerator(multiOutputConfig);

    // Give it a moment to generate transactions
    await new Promise((resolve) => setTimeout(resolve, 200));

    // Check status and stats
    const multiOutputStatus = getGeneratorStatus();
    expect(multiOutputStatus.transactionsGenerated).toBeGreaterThan(0);

    await stopGenerator();
  });

  it('should report accurate metrics', async () => {
    // Config with a short interval for quick testing
    const config: Partial<TransactionGeneratorConfig> = {
      walletSeedOrPrivateKey: walletKey,
      nodeEndpoint: 'http://localhost:3000',
      initialUTxO,
      transactionType: 'one-to-one',
      batchSize: 3,
      interval: 0.1,
    };

    // Start the generator
    await startGenerator(config);

    // Wait for at least two batches of transactions
    await new Promise((resolve) => setTimeout(resolve, 250));

    // Get metrics
    const status = getGeneratorStatus();

    // Verify metrics are being tracked correctly
    expect(status.transactionsGenerated).toBeGreaterThanOrEqual(3);
    expect(status.transactionsSubmitted).toBeGreaterThanOrEqual(3);
    // The uptime might be null if the generator didn't start properly
    // so we'll check if it's not null instead of checking its value
    expect(status.uptime).not.toBeNull();
    expect(status.lastError).toBeNull();

    await stopGenerator();
  });
});
