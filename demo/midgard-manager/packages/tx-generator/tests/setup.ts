import { Lucid, Network, UTxO } from '@lucid-evolution/lucid';
import { CML } from '@lucid-evolution/lucid';
import { test } from 'vitest';

/**
 * Creates a test setup for transaction generator tests
 * Provides mock Lucid and node client instances
 */
const setup = async () => {
  // Generate a test private key
  const privateKey = CML.PrivateKey.generate_ed25519().to_bech32();

  // Create a mock address
  const address = CML.EnterpriseAddress.new(
    0, // Testnet
    CML.Credential.new_pub_key(CML.PrivateKey.from_bech32(privateKey).to_public().hash())
  )
    .to_address()
    .to_bech32();

  // Create a mock UTxO for testing
  const mockInitialUtxo: UTxO = {
    txHash: '0000000000000000000000000000000000000000000000000000000000000000',
    outputIndex: 0,
    assets: {
      lovelace: 1_000_000_000_000n,
    },
    address,
    datum: null,
    datumHash: null,
    scriptRef: null,
  };

  // Create a mock node client that just logs actions but doesn't actually submit transactions
  const mockNodeClient = {
    submitTransaction: async (cborHex: string) => {
      console.log(`Mock submitting transaction: ${cborHex.substring(0, 32)}...`);
      return { txId: `mock_${Date.now()}` };
    },
    isAvailable: async () => true,
  };

  // Create a mock Lucid instance
  const lucid = {
    selectWalletFromPrivateKey: (key: string) => lucid,
    wallet: {
      address: () => address,
      getUtxos: () => Promise.resolve([mockInitialUtxo]),
    },
    utxos: () => Promise.resolve([mockInitialUtxo]),
    provider: {
      network: () => Promise.resolve('Testnet' as Network),
      getUtxosByOutRef: () => Promise.resolve([mockInitialUtxo]),
    },
    newTx: () => ({
      payToAddress: () => ({
        complete: () => ({
          sign: () => ({ submit: () => Promise.resolve('mock_tx_id') }),
        }),
      }),
    }),
  } as unknown as Lucid;

  return {
    privateKey,
    lucid,
    mockInitialUtxo,
    mockNodeClient,
    network: 'Testnet' as Network,
  };
};

// Create a custom test function with our setup context
const txTest = test.extend(await setup());

export { txTest };
