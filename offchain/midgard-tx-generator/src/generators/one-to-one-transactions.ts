import { Writable } from 'node:stream';

import {
  Emulator,
  EmulatorAccount,
  Lucid,
  Network,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
} from '@lucid-evolution/lucid';

import { SerializedMidgardTransaction } from '../core/types.js';
import {
  getPrivateKeyCborHex,
  getPublicKeyHashFromPrivateKey,
  parseUnknownKeytoBech32PrivateKey,
  serializeAssets,
} from '../core/utils.js';

/**
 * Configuration for generating one-to-one transactions
 * These transactions simulate simple transfers between addresses
 */
interface OneToOneTransactionConfig {
  network: Network;
  initialUTxO: UTxO;
  txsCount: number;
  walletSeedOrPrivateKey: string;
  writable?: Writable;
}

/**
 * Generates a sequence of one-to-one transactions for testing Midgard node functionality
 *
 * These transactions simulate simple transfers on the Midgard network:
 * - Each transaction is a single-input, single-output transfer
 * - Transactions are serialized to CBOR format
 * - Output can be written to a stream or returned as JSON
 *
 * Note: In production, these transactions would be submitted by real users
 * and the Midgard node would be responsible for deserializing and validating them.
 */
const generateOneToOneTransactions = async (
  config: OneToOneTransactionConfig
) => {
  const { network, initialUTxO, txsCount, walletSeedOrPrivateKey, writable } =
    config;

  // get payment key to spend initial utxo
  const privateKey = parseUnknownKeytoBech32PrivateKey(walletSeedOrPrivateKey);

  // check if privateKey is valid one to spend initial UTxO
  const publicKeyHash = getPublicKeyHashFromPrivateKey(privateKey);
  const initialUTxOAddressPubKeyHash = paymentCredentialOf(
    initialUTxO.address
  ).hash;
  if (publicKeyHash != initialUTxOAddressPubKeyHash)
    throw new Error('Payment Key is not valid to spend Initial UTxO');

  const refinedInitialUTxO = {
    [`${initialUTxO.txHash}#${initialUTxO.outputIndex}`]: {
      ...initialUTxO,
      assets: serializeAssets(initialUTxO.assets),
    },
  };
  const refinedpaymentKey = {
    cborHex: getPrivateKeyCborHex(privateKey),
    description: 'paymentKey',
    type: 'PaymentSigningKeyShelley_ed25519',
  };

  // write initial utxo and payment key if streaming
  if (writable)
    writable.write(`{"clientDatasets":[{
        "initialUTxO": ${JSON.stringify(refinedInitialUTxO)},
        "paymentKey": ${JSON.stringify(refinedpaymentKey)},
        "txSequence": [`);

  // setup mock environment with initial utxo
  const account: EmulatorAccount = {
    seedPhrase: '',
    address: initialUTxO.address,
    assets: initialUTxO.assets,
    privateKey,
  };
  const emulator = new Emulator([account]);
  emulator.ledger = {
    [`${initialUTxO.txHash}${initialUTxO.outputIndex}`]: {
      utxo: initialUTxO,
      spent: false,
    },
  };

  // initialize lucid with zero fees for testing
  const lucid = await Lucid(emulator, network, {
    presetProtocolParameters: {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      minFeeA: 0,
      minFeeB: 0,
      priceMem: 0,
      priceStep: 0,
      coinsPerUtxoByte: 0n,
    },
  });

  lucid.selectWallet.fromAddress(initialUTxO.address, [initialUTxO]);

  // generate the mock transactions
  const mockTransactions: SerializedMidgardTransaction[] = [];
  for (let i = 0; i < txsCount; i++) {
    const txBuilder = lucid.newTx();
    const [newWalletUTxOs, , txSignBuilder] = await txBuilder.pay
      .ToAddress(initialUTxO.address, initialUTxO.assets)
      .chain();
    const txSigned = await txSignBuilder.sign
      .withPrivateKey(privateKey)
      .complete();

    // create serialized transaction in Midgard format
    const txHash = txSigned.toHash();
    const tx: SerializedMidgardTransaction = {
      cborHex: txSigned.toCBOR(),
      description: 'One-to-One Self Transfer (Single Input/Output)',
      txId: txHash,
      type: 'Midgard L2 User Transaction',
    };

    // handle output (stream or array)
    if (writable) {
      writable.write(`${i > 0 ? ',\n' : ''}${JSON.stringify(tx)}`);
    } else {
      mockTransactions.push(tx);
    }

    // prepare for next transaction
    lucid.overrideUTxOs(newWalletUTxOs);

    // cleanup memory
    txBuilder.rawConfig().txBuilder.free();
    txSignBuilder.toTransaction().free();
    txSigned.toTransaction().free();

    // prevent memory pressure on large transaction counts
    if (i % 1000 == 0)
      await new Promise<void>((resolve) => setTimeout(() => resolve(), 100));
  }

  // finalize output
  if (writable) {
    writable.write(`]}]}`);
    return;
  }

  return JSON.stringify({
    clientDatasets: [
      {
        initialUTxO: refinedInitialUTxO,
        paymentKey: refinedpaymentKey,
        txSequence: mockTransactions,
      },
    ],
  });
};

export { generateOneToOneTransactions };
export type { OneToOneTransactionConfig };
