// Signed Midgard-native transactions for submit-ingress measurements: the
// cost bench (`tests/benchmarks/submit-check-cost.bench.ts`) and the load
// harness corpus (`submit-ingress-load.corpus.mjs`) build every valid tx here.
//
// A tx spends N key-locked outputs and produces M lovelace-only outputs. Each
// input belongs to its own key, so the tx carries one vkey witness per input.
// Field items follow the canonical order Phase A checks: spend inputs by their
// 38-byte item, address witnesses by signer key hash.
import { createHash } from "node:crypto";

import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

/** Body network id of every tx built here (testnet). */
export const LOAD_TX_NETWORK_ID = 0n;

/** Lovelace each generated genesis output holds. */
export const LOAD_GENESIS_OUTPUT_LOVELACE = 100_000_000n;

const EMPTY_WITNESS_SET = {
  addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
};

/**
 * A deterministic key: the same `(seed, index)` always gives the same key, so
 * a corpus and the genesis it spends can be rebuilt independently.
 */
export const makeLoadKey = (seed, index) => {
  const privateKey = CML.PrivateKey.from_normal_bytes(
    createHash("sha256")
      .update(`midgard-submit-ingress-load:${seed}:${index.toString()}`)
      .digest(),
  );
  const keyHash = privateKey.to_public().hash();
  return {
    privateKey,
    keyHash: Buffer.from(keyHash.to_raw_bytes()),
    addressBytes: Buffer.from(
      CML.EnterpriseAddress.new(
        Number(LOAD_TX_NETWORK_ID),
        CML.Credential.new_pub_key(keyHash),
      )
        .to_address()
        .to_raw_bytes(),
    ),
  };
};

/** A lovelace-only Midgard output to `addressBytes`. */
export const makeLoadOutput = (addressBytes, lovelace) =>
  encodeMidgardTxOutput({
    address: Buffer.from(addressBytes),
    value: { lovelace, assets: new Map() },
  });

/** The §5.3 38-byte outref item, which is also the mempool ledger key. */
export const loadOutRef = (txId, outputIndex) =>
  encodeMidgardSpendInputItem({ txId: Buffer.from(txId), outputIndex });

const encodeByteList = (items) => encodeCbor([...items]);

const encodeSignedTx = ({
  inputs,
  outputs,
  fee,
  signedTxId,
  networkId = LOAD_TX_NETWORK_ID,
  validityIntervalEnd = MIDGARD_POSIX_TIME_NONE,
}) => {
  const spendItems = inputs
    .map((input) => input.outRef)
    .sort((left, right) => Buffer.compare(left, right));
  const body = {
    spendInputsPreimageCbor: encodeByteList(spendItems),
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: encodeByteList(outputs),
    fee,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd,
    requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: EMPTY_CBOR_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId,
  };
  const txId = computeMidgardNativeTxId(
    deriveMidgardNativeTxCompact(body, EMPTY_WITNESS_SET, "TxIsValid"),
  );
  const signers = new Map();
  for (const input of inputs)
    signers.set(input.key.keyHash.toString("hex"), input.key);
  const witnesses = [...signers.values()]
    .sort((left, right) => Buffer.compare(left.keyHash, right.keyHash))
    .map((key) =>
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(signedTxId ?? txId),
          key.privateKey,
        ).to_cbor_bytes(),
      ),
    );
  const txCbor = encodeMidgardNativeTxCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body,
    witnessSet: {
      ...EMPTY_WITNESS_SET,
      addrTxWitsPreimageCbor: encodeByteList(witnesses),
    },
  });
  return { txId, txCbor };
};

/**
 * Builds a signed tx spending `inputs` (each `{outRef, lovelace, key}`) into
 * one output per entry of `outputAddresses`, paying at least
 * `minFeeA * size + minFeeB`. The inputs' value less the fee is split evenly;
 * the first output takes the remainder. `feeOverride` pins the fee instead
 * (junk corpora use it for a fee below the minimum), and `signedTxId` is an
 * id the witnesses sign in place of the tx's own (for a bad signature).
 * `networkId` replaces the body network id and `validityIntervalEnd` (an L1
 * slot) bounds the tx's validity; by default it has none.
 */
export const buildLoadTx = ({
  inputs,
  outputAddresses,
  minFeeA,
  minFeeB,
  feeOverride,
  signedTxId,
  networkId,
  validityIntervalEnd,
}) => {
  if (inputs.length === 0) throw new Error("a load tx needs an input");
  if (outputAddresses.length === 0)
    throw new Error("a load tx needs an output");
  const inputLovelace = inputs.reduce((sum, input) => sum + input.lovelace, 0n);
  const outputCount = BigInt(outputAddresses.length);
  const build = (fee) => {
    if (inputLovelace <= fee)
      throw new Error(
        `inputs ${inputLovelace.toString()} cannot cover fee ${fee.toString()}`,
      );
    const share = (inputLovelace - fee) / outputCount;
    const remainder = inputLovelace - fee - share * outputCount;
    const outputs = outputAddresses.map((address, index) =>
      makeLoadOutput(address, index === 0 ? share + remainder : share),
    );
    return {
      ...encodeSignedTx({
        inputs,
        outputs,
        fee,
        signedTxId,
        networkId,
        validityIntervalEnd,
      }),
      fee,
      outputs,
    };
  };
  if (feeOverride !== undefined) return build(feeOverride);
  let fee = minFeeB;
  for (let iteration = 0; iteration < 12; iteration += 1) {
    const tx = build(fee);
    const required = minFeeA * BigInt(tx.txCbor.length) + minFeeB;
    if (required <= fee) return tx;
    fee = required;
  }
  throw new Error("load tx fee did not converge");
};

/**
 * A tx of `inputCount` inputs and `outputCount` outputs over synthetic
 * genesis outputs: the shapes the submit-check cost is measured on. `nonce`
 * makes each tx distinct; any other option is passed to `buildLoadTx`.
 */
export const buildLoadTxShape = ({
  inputCount,
  outputCount,
  nonce = 0,
  minFeeA = 1n,
  minFeeB = 2_000n,
  ...txOptions
}) => {
  const inputs = Array.from({ length: inputCount }, (_, index) => {
    const key = makeLoadKey(`shape-${nonce.toString()}`, index);
    return {
      key,
      lovelace: LOAD_GENESIS_OUTPUT_LOVELACE,
      outRef: loadOutRef(
        createHash("sha256")
          .update(`shape-genesis:${nonce.toString()}:${index.toString()}`)
          .digest(),
        0,
      ),
    };
  });
  const outputAddresses = Array.from(
    { length: outputCount },
    (_, index) =>
      makeLoadKey(`shape-out-${nonce.toString()}`, index).addressBytes,
  );
  return {
    ...buildLoadTx({
      inputs,
      outputAddresses,
      minFeeA,
      minFeeB,
      ...txOptions,
    }),
    inputs,
  };
};
