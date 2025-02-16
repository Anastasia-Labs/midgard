import { Writable } from 'node:stream';

import {
  Emulator,
  EmulatorAccount,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  Network,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
} from '@lucid-evolution/lucid';
import pLimit from 'p-limit';

import { SerializedMidgardTransaction } from '../core/types.js';
import {
  getPrivateKeyCborHex,
  getPublicKeyHashFromPrivateKey,
  parseUnknownKeytoBech32PrivateKey,
  serializeAssets,
} from '../core/utils.js';

const TOTAL_ACCOUNT_COUNT = 100;
const OUTPUT_UTXOS_CHUNK = 20;

interface MultiOutputTransactionConfig {
  network: Network;
  initialUTxO: UTxO;
  utxosCount: number;
  walletSeedOrPrivateKey: string;
  writable?: Writable;
}

const generateMultiOutputTransactions = async (
  config: MultiOutputTransactionConfig
) => {
  const { network, initialUTxO, utxosCount, walletSeedOrPrivateKey, writable } =
    config;

  // validate private key against initial UTxO
  const privateKey = parseUnknownKeytoBech32PrivateKey(walletSeedOrPrivateKey);
  const publicKeyHash = getPublicKeyHashFromPrivateKey(privateKey);
  const initialUTxOAddressPubKeyHash = paymentCredentialOf(
    initialUTxO.address
  ).hash;
  if (publicKeyHash != initialUTxOAddressPubKeyHash) {
    throw new Error('Payment Key is not valid to spend Initial UTxO');
  }

  // prepare initial data for json output
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

  // write initial data if streaming
  if (writable) {
    writable.write(`{"clientDatasets":[{
      "initialUTxO": ${JSON.stringify(refinedInitialUTxO)},
      "paymentKey": ${JSON.stringify(refinedpaymentKey)},
      "txSequence": [`);
  }

  const mainAccount: EmulatorAccount = {
    seedPhrase: '',
    address: initialUTxO.address,
    assets: initialUTxO.assets,
    privateKey,
  };
  const emulator = new Emulator([mainAccount]);
  emulator.ledger = {
    [`${initialUTxO.txHash}${initialUTxO.outputIndex}`]: {
      utxo: initialUTxO,
      spent: false,
    },
  };

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

  // generate multiple accounts for distribution
  const limit = pLimit(10);
  const accounts = await Promise.all(
    Array.from({ length: TOTAL_ACCOUNT_COUNT }, () =>
      limit(() => generateEmulatorAccountFromPrivateKey({}))
    )
  );

  // state variables
  let currentUtxosCount = 1;
  let currentTxsCount = 0;
  const rollBackers: { utxos: UTxO[]; privateKey: string }[] = [];
  const mainAccountLovelace = initialUTxO.assets.lovelace;
  const outputLovelace =
    (mainAccountLovelace - 1_000_000n) / BigInt(utxosCount);

  lucid.selectWallet.fromAddress(initialUTxO.address, [initialUTxO]);

  // transactions with multiple outputs
  const transactions: SerializedMidgardTransaction[] = [];
  while (currentUtxosCount < utxosCount) {
    // select random account for distribution
    const randomAccount =
      accounts[Math.floor(Math.random() * TOTAL_ACCOUNT_COUNT)];
    const txBuilder = lucid.newTx();

    // create multiple outputs in single transaction
    Array.from({ length: OUTPUT_UTXOS_CHUNK }).forEach(() =>
      txBuilder.pay.ToAddress(randomAccount.address, {
        lovelace: outputLovelace,
      })
    );

    const [newWalletUTxOs, derivedOutputs, txSignBuilder] =
      await txBuilder.chain();
    const txSigned = await txSignBuilder.sign
      .withPrivateKey(privateKey)
      .complete();

    // serialized transaction in Midgard format
    const txHash = txSigned.toHash();
    const tx: SerializedMidgardTransaction = {
      cborHex: txSigned.toCBOR(),
      description: 'Multi-Output Distribution (1-to-20 Split)',
      txId: txHash,
      type: 'Midgard L2 User Transaction',
    };

    // handle output (stream or array)
    if (writable) {
      writable.write(
        `${currentTxsCount > 0 ? ',\n' : ''}${JSON.stringify(tx)}`
      );
    } else {
      transactions.push(tx);
    }

    currentUtxosCount += OUTPUT_UTXOS_CHUNK;
    currentTxsCount += 1;

    // update UTxO state
    lucid.overrideUTxOs(newWalletUTxOs);

    // store outputs for collection phase
    derivedOutputs.pop(); // Remove own output
    rollBackers.unshift({
      utxos: derivedOutputs,
      privateKey: randomAccount.privateKey,
    });

    // memory cleanup
    txBuilder.rawConfig().txBuilder.free();
    txSignBuilder.toTransaction().free();
    txSigned.toTransaction().free();

    // periodic GC pause
    if (currentTxsCount % 250 === 0) {
      await new Promise<void>((resolve) => setTimeout(() => resolve(), 100));
    }
  }

  // collection phase - gather distributed UTxOs back
  let currentMainAccountLovelace = (await lucid.wallet().getUtxos()).reduce(
    (acc, cur) => acc + cur.assets.lovelace,
    0n
  );

  while (rollBackers.length > 0) {
    const rollBacker = rollBackers.shift();
    if (!rollBacker) continue;

    const txBuilder = lucid.newTx();
    currentMainAccountLovelace += rollBacker.utxos.reduce(
      (acc, cur) => acc + cur.assets.lovelace,
      0n
    );

    const [newWalletUTxOs, , txSignBuilder] = await txBuilder
      .collectFrom(rollBacker.utxos)
      .pay.ToAddress(mainAccount.address, {
        lovelace: currentMainAccountLovelace,
      })
      .chain();

    const txSigned = await txSignBuilder.sign
      .withPrivateKey(privateKey)
      .sign.withPrivateKey(rollBacker.privateKey)
      .complete();

    // serialized transaction in Midgard format
    const txHash = txSigned.toHash();
    const tx: SerializedMidgardTransaction = {
      cborHex: txSigned.toCBOR(),
      description: 'Multi-Output Collection (20-to-1 Merge)',
      txId: txHash,
      type: 'Midgard L2 User Transaction',
    };

    if (writable) {
      writable.write(
        `${currentTxsCount > 0 ? ',\n' : ''}${JSON.stringify(tx)}`
      );
    } else {
      transactions.push(tx);
    }

    currentUtxosCount -= OUTPUT_UTXOS_CHUNK;
    currentTxsCount += 1;

    // update UTxO state
    lucid.overrideUTxOs(newWalletUTxOs);

    // memory cleanup
    txBuilder.rawConfig().txBuilder.free();
    txSignBuilder.toTransaction().free();
    txSigned.toTransaction().free();
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
        txSequence: transactions,
      },
    ],
  });
};

export { generateMultiOutputTransactions };
export type { MultiOutputTransactionConfig };
