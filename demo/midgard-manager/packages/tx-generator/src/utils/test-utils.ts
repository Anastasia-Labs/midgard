import { generateEmulatorAccountFromPrivateKey, UTxO } from '@lucid-evolution/lucid';

/**
 * Test-only wallet bundle returned by {@link generateTestWallet}.
 */
export interface TestWallet {
  privateKey: string;
  seedPhrase: string;
  testUTxO: UTxO;
}

/**
 * Generates a disposable emulator wallet and bootstrap UTxO for transaction
 * generation tests.
 *
 * The zero-hash outref intentionally behaves like a predictable local fixture
 * rather than a real chain-derived output reference.
 */
export async function generateTestWallet(): Promise<TestWallet> {
  const account = await generateEmulatorAccountFromPrivateKey({});

  // Create test UTxO with some test ADA
  const testUTxO: UTxO = {
    txHash: Buffer.from(Array(32).fill(0)).toString('hex'), // 32-byte zero hash
    outputIndex: 0,
    address: account.address,
    assets: {
      lovelace: 10_000_000_000n, // 10,000 ADA for testing
    },
    datumHash: null,
  };

  return {
    seedPhrase: account.seedPhrase,
    privateKey: account.privateKey,
    testUTxO,
  };
}
