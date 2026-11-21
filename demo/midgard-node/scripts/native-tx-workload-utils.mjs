import fs from "node:fs";

import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import dotenv from "dotenv";

const MIDGARD_NETWORK_ID_PREPROD = 0n;

export const parseEnv = (filename) => {
  const raw = fs.readFileSync(filename, "utf8");
  return dotenv.parse(raw);
};

export const makeWalletsFromEnv = (env) => {
  const keys = [
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_B",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_C",
  ];

  return keys
    .map((key) => {
      const seed = env[key];
      if (!seed || seed.trim().length === 0) {
        return null;
      }
      const wallet = walletFromSeed(seed.trim(), { network: "Preprod" });
      return {
        key,
        seed: seed.trim(),
        address: wallet.address,
        signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
      };
    })
    .filter((wallet) => wallet !== null);
};

const encodeByteListPreimage = (items) =>
  encodeCbor(items.map((item) => Buffer.from(item)));

// These bytes become a §5.3 field-0/1 spend-input item, so they must be the
// fixed 38-byte `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` form — never CML's
// minimal-index `TransactionInput` CBOR.
export const toOutRefCbor = (txId, outputIndex) =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txId),
    outputIndex: Number(outputIndex),
  });

export const decodeCoin = (outputHex) => {
  const output = CML.TransactionOutput.from_cbor_bytes(
    Buffer.from(outputHex, "hex"),
  );
  return output.amount().coin();
};

export const outputHasMultiAssets = (outputCbor) =>
  CML.TransactionOutput.from_cbor_bytes(outputCbor).amount().has_multiassets();

export const withOutputCoin = (outputCbor, coin) => {
  const output = CML.TransactionOutput.from_cbor_bytes(outputCbor);
  const currentAmount = output.amount();
  const nextAmount = currentAmount.has_multiassets()
    ? CML.Value.new(coin, currentAmount.multi_asset())
    : CML.Value.from_coin(coin);
  output.set_amount(nextAmount);
  return Buffer.from(output.to_cbor_bytes());
};

export const lovelaceOnlyOutputForTemplate = (templateOutputCbor, coin) => {
  const template = CML.TransactionOutput.from_cbor_bytes(templateOutputCbor);
  return Buffer.from(
    CML.TransactionOutput.new(
      template.address(),
      CML.Value.from_coin(coin),
    ).to_cbor_bytes(),
  );
};

const buildNativeSignedOutputs = ({
  spendOutRefCbor,
  outputCbors,
  signer,
  fee,
}) => {
  const spendInputsPreimageCbor = encodeByteListPreimage([spendOutRefCbor]);
  const referenceInputsPreimageCbor = EMPTY_CBOR_LIST;
  const outputsPreimageCbor = encodeByteListPreimage(outputCbors);
  const requiredObserversPreimageCbor = EMPTY_CBOR_LIST;
  const requiredSignersPreimageCbor = encodeByteListPreimage([
    Buffer.from(signer.to_public().hash().to_raw_bytes()),
  ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const body = {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor,
    requiredSignersPreimageCbor,
    mintPreimageCbor,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NETWORK_ID_PREPROD,
  };
  const emptyWitnessSet = {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  };
  const transactionId = computeMidgardNativeTxId(
    deriveMidgardNativeTxCompact(body, emptyWitnessSet, "TxIsValid"),
  );
  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(transactionId),
    signer,
  );

  const witnessSet = {
    addrTxWitsPreimageCbor: encodeByteListPreimage([
      Buffer.from(witness.to_cbor_bytes()),
    ]),
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  };
  const txCbor = encodeMidgardNativeTxCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    body,
    witnessSet,
    validity: "TxIsValid",
  });

  return {
    txId: transactionId,
    txHex: txCbor.toString("hex"),
  };
};

export const buildNativeSignedOneToOneWithFee = ({
  spendOutRefCbor,
  outputCbor,
  signer,
  fee,
}) => {
  const tx = buildNativeSignedOutputs({
    spendOutRefCbor,
    outputCbors: [outputCbor],
    signer,
    fee,
  });
  return {
    txId: tx.txId,
    txHex: tx.txHex,
    nextOutRef: toOutRefCbor(tx.txId, 0),
    outputCbor,
    fee,
  };
};

export const buildNativeSignedOneToOne = ({
  spendOutRefCbor,
  outputCbor,
  signer,
}) =>
  buildNativeSignedOneToOneWithFee({
    spendOutRefCbor,
    outputCbor,
    signer,
    fee: 0n,
  });

export const buildNativeSignedSplitWithFee = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  outputCount,
  fee,
}) => {
  const input = CML.TransactionOutput.from_cbor_bytes(inputOutputCbor);
  if (input.amount().has_multiassets()) {
    throw new Error("fanout setup only supports lovelace-only source UTxOs");
  }
  const inputCoin = input.amount().coin();
  if (inputCoin <= fee) {
    throw new Error(
      `fanout source coin ${inputCoin.toString()} cannot cover fee ${fee.toString()}`,
    );
  }
  const available = inputCoin - fee;
  const outputCountBig = BigInt(outputCount);
  const baseCoin = available / outputCountBig;
  const remainder = available % outputCountBig;
  if (baseCoin <= 0n) {
    throw new Error(
      `fanout source coin ${inputCoin.toString()} too small for ${outputCount} outputs`,
    );
  }

  const outputs = Array.from({ length: outputCount }, (_, index) =>
    lovelaceOnlyOutputForTemplate(
      inputOutputCbor,
      baseCoin + (BigInt(index) < remainder ? 1n : 0n),
    ),
  );
  const tx = buildNativeSignedOutputs({
    spendOutRefCbor,
    outputCbors: outputs,
    signer,
    fee,
  });
  return {
    txId: tx.txId,
    txHex: tx.txHex,
    outputs: outputs.map((outputCbor, outputIndex) => {
      const spendOutRefCbor = toOutRefCbor(tx.txId, outputIndex);
      return {
        outputCbor,
        spendOutRefCbor,
        outRefHex: spendOutRefCbor.toString("hex"),
        outputIndex,
      };
    }),
    fee,
  };
};

export const buildNativeSignedSplit = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  outputCount,
  minFeeA,
  minFeeB,
}) => {
  let fee = minFeeB;
  for (let iteration = 0; iteration < 12; iteration += 1) {
    const tx = buildNativeSignedSplitWithFee({
      spendOutRefCbor,
      inputOutputCbor,
      signer,
      outputCount,
      fee,
    });
    const requiredFee =
      minFeeA * BigInt(Buffer.from(tx.txHex, "hex").length) + minFeeB;
    if (requiredFee === fee) {
      return tx;
    }
    fee = requiredFee;
  }
  throw new Error("failed to converge native fanout transaction min fee");
};

export const buildNativeSignedOneToOneWithMinFee = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  minFeeA,
  minFeeB,
}) => {
  let fee = minFeeB;
  for (let iteration = 0; iteration < 12; iteration += 1) {
    const inputCoin = CML.TransactionOutput.from_cbor_bytes(inputOutputCbor)
      .amount()
      .coin();
    if (inputCoin <= fee) {
      throw new Error(
        `input coin ${inputCoin.toString()} cannot cover fee ${fee.toString()}`,
      );
    }
    const outputCbor = withOutputCoin(inputOutputCbor, inputCoin - fee);
    const tx = buildNativeSignedOneToOneWithFee({
      spendOutRefCbor,
      outputCbor,
      signer,
      fee,
    });
    const requiredFee =
      minFeeA * BigInt(Buffer.from(tx.txHex, "hex").length) + minFeeB;
    if (requiredFee === fee) {
      return tx;
    }
    fee = requiredFee;
  }
  throw new Error("failed to converge native stress transaction min fee");
};
