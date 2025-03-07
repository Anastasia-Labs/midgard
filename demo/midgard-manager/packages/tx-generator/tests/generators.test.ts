import { Network, UTxO } from '@lucid-evolution/lucid';
import { beforeEach, describe, expect, it, vi } from 'vitest';

import { MidgardNodeClient } from '../src/lib/client/node-client';
import {
  generateMultiOutputTransactions,
  generateOneToOneTransactions,
} from '../src/lib/generators/index.js';
import { txTest } from './setup';

// Mock the transaction generation functions
vi.mock('../src/lib/generators/one-to-one.js', () => {
  return {
    generateOneToOneTransactions: vi.fn().mockImplementation(async (config) => {
      // Validate config
      if (!config.walletSeedOrPrivateKey) {
        throw new Error('Wallet private key is required');
      }
      if (!config.initialUTxO || config.initialUTxO.assets.lovelace < 1_000_000n) {
        throw new Error('Initial UTxO must have at least 1 ADA');
      }
      if (config.txsCount < 1) {
        throw new Error('Transaction count must be at least 1');
      }

      // Submit transactions if node client is provided
      if (config.nodeClient) {
        try {
          await config.nodeClient.submitTransaction('mock_cbor', 'test');
        } catch (error) {
          throw error;
        }
      }

      // Return mock transactions
      return Array(config.txsCount)
        .fill(null)
        .map((_, i) => ({
          type: 'Midgard L2 User Transaction',
          description: 'One-to-One Self Transfer',
          cborHex: `mock_cbor_${i + 1}`,
          txId: `test_tx_${i + 1}`,
        }));
    }),
  };
});

vi.mock('../src/lib/generators/multi-output.js', () => {
  return {
    generateMultiOutputTransactions: vi.fn().mockImplementation(async (config) => {
      // Validate config
      if (!config.walletSeedOrPrivateKey) {
        throw new Error('Wallet private key is required');
      }
      if (!config.initialUTxO || config.initialUTxO.assets.lovelace < 1_000_000n) {
        throw new Error('Initial UTxO must have at least 1 ADA');
      }
      if (config.utxosCount < 1) {
        throw new Error('UTxO count must be at least 1');
      }
      if (config.finalUtxosCount && config.finalUtxosCount > config.utxosCount / 20) {
        throw new Error(`Final UTxO Count can be ${Math.floor(config.utxosCount / 20)} at maximum`);
      }

      // Calculate output lovelace and validate
      const outputLovelace =
        (config.initialUTxO.assets.lovelace - 1_000_000n) / BigInt(config.utxosCount);
      if (outputLovelace < 1_000_000n) {
        throw new Error('Not enough Lovelace to distribute');
      }

      // Return mock transactions (distribution + collection)
      const distributionTxs = Array(Math.ceil(config.utxosCount / 20))
        .fill(null)
        .map((_, i) => ({
          type: 'Midgard L2 User Transaction',
          description: 'Multi-Output Distribution (1-to-20)',
          cborHex: `mock_dist_cbor_${i + 1}`,
          txId: `test_dist_tx_${i + 1}`,
        }));

      const collectionTxs = Array(config.finalUtxosCount || 1)
        .fill(null)
        .map((_, i) => ({
          type: 'Midgard L2 User Transaction',
          description: 'Multi-Output Collection (20-to-1)',
          cborHex: `mock_coll_cbor_${i + 1}`,
          txId: `test_coll_tx_${i + 1}`,
        }));

      return [...distributionTxs, ...collectionTxs];
    }),
  };
});

// Mock the node client
vi.mock('../src/lib/client/node-client', () => {
  return {
    MidgardNodeClient: vi.fn().mockImplementation(() => {
      return {
        submitTransaction: vi.fn().mockResolvedValue({ txId: 'mock_tx_id' }),
        isAvailable: vi.fn().mockResolvedValue(true),
      };
    }),
  };
});

describe('Transaction Generators', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('One-to-One Transactions', () => {
    txTest('should generate one-to-one transactions successfully', async ({ mockInitialUtxo }) => {
      const nodeClient = new MidgardNodeClient({ baseUrl: 'http://localhost:3000' });
      const txs = await generateOneToOneTransactions({
        network: 'Testnet' as Network,
        initialUTxO: mockInitialUtxo,
        txsCount: 3,
        walletSeedOrPrivateKey: 'test_private_key',
        nodeClient,
      });

      expect(txs).toHaveLength(3);
      txs.forEach((tx) => {
        expect(tx).toMatchObject({
          type: 'Midgard L2 User Transaction',
          description: 'One-to-One Self Transfer',
          cborHex: expect.any(String),
          txId: expect.any(String),
        });
      });
    });

    txTest('should validate minimum ADA requirement', async ({ mockInitialUtxo }) => {
      const invalidUtxo = {
        ...mockInitialUtxo,
        assets: { lovelace: 500_000n }, // Less than 1 ADA
      };

      await expect(
        generateOneToOneTransactions({
          network: 'Testnet' as Network,
          initialUTxO: invalidUtxo,
          txsCount: 3,
          walletSeedOrPrivateKey: 'test_private_key',
        })
      ).rejects.toThrow('Initial UTxO must have at least 1 ADA');
    });

    txTest('should validate transaction count', async ({ mockInitialUtxo }) => {
      await expect(
        generateOneToOneTransactions({
          network: 'Testnet' as Network,
          initialUTxO: mockInitialUtxo,
          txsCount: 0,
          walletSeedOrPrivateKey: 'test_private_key',
        })
      ).rejects.toThrow('Transaction count must be at least 1');
    });
  });

  describe('Multi-Output Transactions', () => {
    txTest(
      'should generate multi-output transactions successfully',
      async ({ mockInitialUtxo }) => {
        const nodeClient = new MidgardNodeClient({ baseUrl: 'http://localhost:3000' });
        const txs = await generateMultiOutputTransactions({
          network: 'Testnet' as Network,
          initialUTxO: mockInitialUtxo,
          utxosCount: 100,
          finalUtxosCount: 1,
          walletSeedOrPrivateKey: 'test_private_key',
          nodeClient,
        });

        // Should have distribution and collection transactions
        expect(txs.length).toBeGreaterThan(1);
        expect(txs.some((tx) => tx.description.includes('Distribution'))).toBe(true);
        expect(txs.some((tx) => tx.description.includes('Collection'))).toBe(true);
      }
    );

    txTest('should validate UTxO counts', async ({ mockInitialUtxo }) => {
      await expect(
        generateMultiOutputTransactions({
          network: 'Testnet' as Network,
          initialUTxO: mockInitialUtxo,
          utxosCount: 100,
          finalUtxosCount: 10, // Too many for 100 UTxOs (max would be 5)
          walletSeedOrPrivateKey: 'test_private_key',
        })
      ).rejects.toThrow(/Final UTxO Count can be \d+ at maximum/);
    });

    txTest('should validate output lovelace amounts', async ({ mockInitialUtxo }) => {
      const invalidUtxo = {
        ...mockInitialUtxo,
        assets: { lovelace: 1_000_000n }, // Only 1 ADA, not enough to distribute
      };

      await expect(
        generateMultiOutputTransactions({
          network: 'Testnet' as Network,
          initialUTxO: invalidUtxo,
          utxosCount: 100,
          walletSeedOrPrivateKey: 'test_private_key',
        })
      ).rejects.toThrow('Not enough Lovelace to distribute');
    });
  });

  describe('Error Handling', () => {
    txTest('should handle node client errors', async ({ mockInitialUtxo }) => {
      const nodeClient = new MidgardNodeClient({ baseUrl: 'http://localhost:3000' });
      vi.spyOn(nodeClient, 'submitTransaction').mockRejectedValue(new Error('Network error'));

      await expect(
        generateOneToOneTransactions({
          network: 'Testnet' as Network,
          initialUTxO: mockInitialUtxo,
          txsCount: 1,
          walletSeedOrPrivateKey: 'test_private_key',
          nodeClient,
        })
      ).rejects.toThrow('Network error');
    });

    txTest('should handle invalid wallet keys', async ({ mockInitialUtxo }) => {
      await expect(
        generateOneToOneTransactions({
          network: 'Testnet' as Network,
          initialUTxO: mockInitialUtxo,
          txsCount: 1,
          walletSeedOrPrivateKey: '', // Empty key
        })
      ).rejects.toThrow('Wallet private key is required');
    });
  });
});
