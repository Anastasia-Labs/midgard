import { generateEmulatorAccountFromPrivateKey } from '@lucid-evolution/lucid';

/**
 * Test utilities for Midgard transaction generation
 */

/**
 * Generates a test wallet for transaction generation
 * TODO: Midgard provider will need to be implemented
 */
export const generateTestWallet = async () => {
  // Generate emulator account
  const account = await generateEmulatorAccountFromPrivateKey({});

  // Create test UTxO with some test ADA
  const testUTxO = {
    txHash: Buffer.from(Array(32).fill(0)).toString('hex'), // 32-byte zero hash
    outputIndex: 0,
    address: account.address,
    assets: {
      lovelace: 10_000_000_000n, // 10,000 ADA for testing
    },
  };

  return {
    seedPhrase: account.seedPhrase,
    privateKey: account.privateKey,
    testUTxO,
  };
};
