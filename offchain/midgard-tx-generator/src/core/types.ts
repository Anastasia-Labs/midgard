/**
 * Core transaction types for Midgard
 */

interface MidgardTransaction {
  // Based on codec.cddl transaction format
  // This is the target format we want to generate once Midgard Provider is implemented
  transaction_body: {
    inputs: Array<{ txHash: string; index: number }>; // transaction_input
    outputs: Array<{
      address: string;
      value: {
        coin: bigint;
        assets?: Record<string, Record<string, bigint>>; // multiasset format
      };
      datum?: string;
      scriptRef?: string;
    }>;
    fee: bigint;
    ttl?: number;
    validityStart?: number;
    networkId?: number; // network identifier (10 for Mainnet, 11 for Testnet)
  };
  witness_set: {
    vkeys?: Array<{
      key: string; // verification key
      signature: string; // Ed25519 signature
    }>;
    nativeScripts?: Array<string>;
    plutusV3Scripts?: Array<string>;
  };
  isValid: boolean; // is always true in Midgard?
  auxiliaryData: null; // must be null in Midgard?
}

interface SerializedMidgardTransaction {
  type: 'Midgard L2 User Transaction';
  description: string;
  cborHex: string;
  txId: string;
}

export type { MidgardTransaction, SerializedMidgardTransaction };
