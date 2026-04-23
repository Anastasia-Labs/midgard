#!/usr/bin/env node

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { encode as cborEncode } from 'cborg';
import dotenv from 'dotenv';
import { blake2b } from '@noble/hashes/blake2.js';
import { CML, walletFromSeed } from '@lucid-evolution/lucid';

const MIDGARD_NATIVE_TX_VERSION = 1n;
const MIDGARD_POSIX_TIME_NONE = -1n;
const MIDGARD_NETWORK_ID_PREPROD = 0n;
const TX_IS_VALID_CODE = 0n;
const HASH32_LEN = 32;

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const pkgRoot = path.resolve(__dirname, '..');

const envPath = process.env.STRESS_ENV_FILE ?? path.join(pkgRoot, '.env');
const submitEndpoint = process.env.STRESS_SUBMIT_ENDPOINT ?? 'http://127.0.0.1:3000';
const metricsEndpoint = process.env.STRESS_METRICS_ENDPOINT ?? 'http://127.0.0.1:9464/metrics';
const chainLength = Number.parseInt(process.env.STRESS_CHAIN_LENGTH ?? '500', 10);
const maxChains = Number.parseInt(process.env.STRESS_MAX_CHAINS ?? '8', 10);
const utxosPerWallet = Number.parseInt(process.env.STRESS_UTXOS_PER_WALLET ?? '3', 10);
const minLovelace = BigInt(process.env.STRESS_MIN_LOVELACE ?? '0');
const fanoutEnabled =
  (process.env.STRESS_FANOUT_ENABLED ?? 'true').trim().toLowerCase() !== 'false';
const fanoutMaxOutputsPerTx = Number.parseInt(
  process.env.STRESS_FANOUT_MAX_OUTPUTS_PER_TX ?? '256',
  10,
);
const fanoutOutputLovelace =
  process.env.STRESS_FANOUT_OUTPUT_LOVELACE === undefined
    ? null
    : BigInt(process.env.STRESS_FANOUT_OUTPUT_LOVELACE);
const fanoutStatusTimeoutMs = Number.parseInt(
  process.env.STRESS_FANOUT_STATUS_TIMEOUT_MS ?? '30000',
  10,
);
const retry503 = Number.parseInt(process.env.STRESS_RETRY_503 ?? '3', 10);
const retryDelayMs = Number.parseInt(process.env.STRESS_RETRY_DELAY_MS ?? '25', 10);
const metricsPollMs = Number.parseInt(process.env.STRESS_METRICS_POLL_MS ?? '1000', 10);
const observeAfterSubmitSec = Number.parseInt(
  process.env.STRESS_OBSERVE_AFTER_SUBMIT_SEC ?? '15',
  10,
);
const targetAcceptedTps = Number.parseFloat(process.env.STRESS_TARGET_ACCEPTED_TPS ?? '600');
const requireFreshChains =
  (process.env.STRESS_REQUIRE_FRESH_CHAINS ?? 'true').trim().toLowerCase() !==
  'false';
const txStatusRetries = Number.parseInt(process.env.STRESS_TX_STATUS_RETRIES ?? '5', 10);
const txStatusRetryDelayMs = Number.parseInt(
  process.env.STRESS_TX_STATUS_RETRY_DELAY_MS ?? '50',
  10,
);

if (!Number.isFinite(chainLength) || chainLength <= 0) {
  throw new Error('STRESS_CHAIN_LENGTH must be a positive integer');
}
if (!Number.isFinite(maxChains) || maxChains <= 0) {
  throw new Error('STRESS_MAX_CHAINS must be a positive integer');
}
if (!Number.isFinite(utxosPerWallet) || utxosPerWallet <= 0) {
  throw new Error('STRESS_UTXOS_PER_WALLET must be a positive integer');
}
if (!Number.isFinite(fanoutMaxOutputsPerTx) || fanoutMaxOutputsPerTx <= 1) {
  throw new Error('STRESS_FANOUT_MAX_OUTPUTS_PER_TX must be an integer greater than 1');
}
if (!Number.isFinite(fanoutStatusTimeoutMs) || fanoutStatusTimeoutMs <= 0) {
  throw new Error('STRESS_FANOUT_STATUS_TIMEOUT_MS must be a positive integer');
}
if (!Number.isFinite(metricsPollMs) || metricsPollMs <= 0) {
  throw new Error('STRESS_METRICS_POLL_MS must be a positive integer');
}

/** @typedef {{ outref: string; value: string }} NodeUtxo */
/** @typedef {{ txHex: string; txIdHex: string }} PrebuiltTx */

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

/**
 * Encodes a value into hexadecimal CBOR text.
 */
const encodeCbor = (value) => Buffer.from(cborEncode(value));
/**
 * Computes a 32-byte Blake2b hash.
 */
const hash32 = (value) => Buffer.from(blake2b(value, { dkLen: HASH32_LEN }));

/**
 * Encodes a byte-list preimage used by the native transaction fixtures.
 */
const encodeByteListPreimage = (items) =>
  encodeCbor(items.map((item) => Buffer.from(item)));

/**
 * Encodes a transaction output reference into CBOR.
 */
const toOutRefCbor = (txId, outputIndex) =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(txId),
      BigInt(outputIndex),
    ).to_cbor_bytes(),
  );

/**
 * Parses environment configuration for the valid-stress workload.
 */
const parseEnv = (filename) => {
  const raw = fs.readFileSync(filename, 'utf8');
  return dotenv.parse(raw);
};

/**
 * Builds wallet descriptors from environment configuration.
 */
const makeWalletsFromEnv = (env) => {
  const keys = [
    'TESTNET_GENESIS_WALLET_SEED_PHRASE_A',
    'TESTNET_GENESIS_WALLET_SEED_PHRASE_B',
    'TESTNET_GENESIS_WALLET_SEED_PHRASE_C',
  ];

  return keys
    .map((key) => {
      const seed = env[key];
      if (!seed || seed.trim().length === 0) {
        return null;
      }
      const wallet = walletFromSeed(seed.trim(), { network: 'Preprod' });
      return {
        key,
        seed: seed.trim(),
        address: wallet.address,
        signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
      };
    })
    .filter((wallet) => wallet !== null);
};

/**
 * Fetches raw Prometheus metrics text from the node.
 */
const fetchMetricsText = async () => {
  const resp = await fetch(metricsEndpoint);
  if (!resp.ok) {
    throw new Error(`metrics endpoint returned ${resp.status}`);
  }
  return resp.text();
};

/**
 * Escapes a string for literal use in a regular expression.
 */
const escapeRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

/**
 * Extracts a Prometheus counter value from metrics text.
 */
const extractCounter = (text, names) => {
  for (const name of names) {
    const pattern = `^${escapeRegex(name)}(?:\\{[^}]*\\})?\\s+([0-9]+(?:\\.[0-9]+)?)$`;
    const m = text.match(new RegExp(pattern, 'm'));
    if (m !== null) {
      return Number(m[1]);
    }
  }
  return 0;
};

/**
 * Fetches and parses the counters used by the workload monitor.
 */
const readCounters = async () => {
  const text = await fetchMetricsText();
  return {
    submit: extractCounter(text, ['tx_count_total', 'tx_count']),
    accept: extractCounter(text, [
      'validation_accept_count_total',
      'validation_accept_count',
    ]),
    reject: extractCounter(text, [
      'validation_reject_count_total',
      'validation_reject_count',
    ]),
  };
};

/**
 * Fetches spendable UTxOs for a wallet address.
 */
const fetchUtxos = async (address) => {
  const resp = await fetch(
    `${submitEndpoint}/utxos?address=${encodeURIComponent(address)}`,
  );
  if (!resp.ok) {
    throw new Error(`utxos endpoint returned ${resp.status} for ${address}`);
  }
  const body = await resp.json();
  if (!Array.isArray(body.utxos)) {
    return [];
  }
  return /** @type {NodeUtxo[]} */ (body.utxos);
};

/**
 * Fetches transaction status information from the node.
 */
const fetchTxStatus = async (txIdHex) => {
  let attempt = 0;
  while (attempt <= txStatusRetries) {
    try {
      const resp = await fetch(
        `${submitEndpoint}/tx-status?tx_hash=${encodeURIComponent(txIdHex)}`,
      );
      if (!resp.ok && resp.status !== 404) {
        if ((resp.status === 429 || resp.status === 503) && attempt < txStatusRetries) {
          attempt += 1;
          await sleep(txStatusRetryDelayMs);
          continue;
        }
        throw new Error(`tx-status endpoint returned ${resp.status} for ${txIdHex}`);
      }
      let body = {};
      try {
        body = await resp.json();
      } catch {
        // Keep fallback behavior below.
      }
      if (typeof body?.status === 'string') {
        return body.status;
      }
      return resp.status === 404 ? 'not_found' : 'unknown';
    } catch (error) {
      if (attempt >= txStatusRetries) {
        throw error;
      }
      attempt += 1;
      await sleep(txStatusRetryDelayMs);
    }
  }
  return 'unknown';
};

/**
 * Decodes a lovelace quantity from a UTxO payload.
 */
const decodeCoin = (outputHex) => {
  const output = CML.TransactionOutput.from_cbor_bytes(Buffer.from(outputHex, 'hex'));
  return output.amount().coin();
};

/**
 * Returns true when a transaction output carries non-ADA assets.
 */
const outputHasMultiAssets = (outputCbor) =>
  CML.TransactionOutput.from_cbor_bytes(outputCbor).amount().has_multiassets();

/**
 * Returns a transaction output equivalent to `outputCbor` with a reduced coin.
 */
const withOutputCoin = (outputCbor, coin) => {
  const output = CML.TransactionOutput.from_cbor_bytes(outputCbor);
  const currentAmount = output.amount();
  const nextAmount = currentAmount.has_multiassets()
    ? CML.Value.new(coin, currentAmount.multi_asset())
    : CML.Value.from_coin(coin);
  output.set_amount(nextAmount);
  return Buffer.from(output.to_cbor_bytes());
};

/**
 * Returns a transaction output with the same address as `templateOutputCbor`
 * and a lovelace-only value.
 */
const lovelaceOnlyOutputForTemplate = (templateOutputCbor, coin) => {
  const template = CML.TransactionOutput.from_cbor_bytes(templateOutputCbor);
  return Buffer.from(
    CML.TransactionOutput.new(
      template.address(),
      CML.Value.from_coin(coin),
    ).to_cbor_bytes(),
  );
};

/**
 * Splits a lovelace-only native UTxO into many same-address outputs for
 * high-concurrency benchmark setup.
 */
const buildNativeSignedSplitWithFee = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  outputCount,
  fee,
}) => {
  const input = CML.TransactionOutput.from_cbor_bytes(inputOutputCbor);
  if (input.amount().has_multiassets()) {
    throw new Error('fanout setup only supports lovelace-only source UTxOs');
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

  const spendInputsPreimageCbor = encodeByteListPreimage([spendOutRefCbor]);
  const referenceInputsPreimageCbor = EMPTY_CBOR_LIST;
  const outputsPreimageCbor = encodeByteListPreimage(outputs);
  const requiredObserversPreimageCbor = EMPTY_CBOR_LIST;
  const requiredSignersPreimageCbor = encodeByteListPreimage([
    Buffer.from(signer.to_public().hash().to_raw_bytes()),
  ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const scriptIntegrityHash = hash32(EMPTY_CBOR_NULL);
  const auxiliaryDataHash = hash32(EMPTY_CBOR_NULL);

  const bodyCompact = [
    hash32(spendInputsPreimageCbor),
    hash32(referenceInputsPreimageCbor),
    hash32(outputsPreimageCbor),
    fee,
    MIDGARD_POSIX_TIME_NONE,
    MIDGARD_POSIX_TIME_NONE,
    hash32(requiredObserversPreimageCbor),
    hash32(requiredSignersPreimageCbor),
    hash32(mintPreimageCbor),
    scriptIntegrityHash,
    auxiliaryDataHash,
    MIDGARD_NETWORK_ID_PREPROD,
  ];

  const bodyHash = hash32(encodeCbor(bodyCompact));
  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(bodyHash),
    signer,
  );

  const addrTxWitsPreimageCbor = encodeByteListPreimage([
    Buffer.from(witness.to_cbor_bytes()),
  ]);
  const scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const redeemerTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const datumTxWitsPreimageCbor = EMPTY_CBOR_LIST;

  const witnessCompact = [
    hash32(addrTxWitsPreimageCbor),
    hash32(scriptTxWitsPreimageCbor),
    hash32(redeemerTxWitsPreimageCbor),
    hash32(datumTxWitsPreimageCbor),
  ];

  const compact = [
    MIDGARD_NATIVE_TX_VERSION,
    bodyHash,
    hash32(encodeCbor(witnessCompact)),
    TX_IS_VALID_CODE,
  ];

  const bodyFull = [
    bodyCompact[0],
    spendInputsPreimageCbor,
    bodyCompact[1],
    referenceInputsPreimageCbor,
    bodyCompact[2],
    outputsPreimageCbor,
    bodyCompact[3],
    bodyCompact[4],
    bodyCompact[5],
    bodyCompact[6],
    requiredObserversPreimageCbor,
    bodyCompact[7],
    requiredSignersPreimageCbor,
    bodyCompact[8],
    mintPreimageCbor,
    bodyCompact[9],
    bodyCompact[10],
    bodyCompact[11],
  ];

  const witnessFull = [
    witnessCompact[0],
    addrTxWitsPreimageCbor,
    witnessCompact[1],
    scriptTxWitsPreimageCbor,
    witnessCompact[2],
    redeemerTxWitsPreimageCbor,
    witnessCompact[3],
    datumTxWitsPreimageCbor,
  ];

  const txCbor = encodeCbor([
    MIDGARD_NATIVE_TX_VERSION,
    compact,
    bodyFull,
    witnessFull,
  ]);

  return {
    txId: bodyHash,
    txHex: txCbor.toString('hex'),
    outputs: outputs.map((outputCbor, outputIndex) => ({
      outputCbor,
      spendOutRefCbor: toOutRefCbor(bodyHash, outputIndex),
      outRefHex: toOutRefCbor(bodyHash, outputIndex).toString('hex'),
      outputIndex,
    })),
    fee,
  };
};

/**
 * Builds a native split transaction with a converged linear min fee.
 */
const buildNativeSignedSplit = ({
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
    const requiredFee = minFeeA * BigInt(Buffer.from(tx.txHex, 'hex').length) + minFeeB;
    if (requiredFee === fee) {
      return tx;
    }
    fee = requiredFee;
  }
  throw new Error('failed to converge native fanout transaction min fee');
};

/**
 * Builds and signs a native one-to-one transfer transaction for a fixed fee.
 */
const buildNativeSignedOneToOneWithFee = ({
  spendOutRefCbor,
  outputCbor,
  signer,
  fee,
}) => {
  const spendInputsPreimageCbor = encodeByteListPreimage([spendOutRefCbor]);
  const referenceInputsPreimageCbor = EMPTY_CBOR_LIST;
  const outputsPreimageCbor = encodeByteListPreimage([outputCbor]);
  const requiredObserversPreimageCbor = EMPTY_CBOR_LIST;
  const requiredSignersPreimageCbor = encodeByteListPreimage([
    Buffer.from(signer.to_public().hash().to_raw_bytes()),
  ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const scriptIntegrityHash = hash32(EMPTY_CBOR_NULL);
  const auxiliaryDataHash = hash32(EMPTY_CBOR_NULL);

  const bodyCompact = [
    hash32(spendInputsPreimageCbor),
    hash32(referenceInputsPreimageCbor),
    hash32(outputsPreimageCbor),
    fee,
    MIDGARD_POSIX_TIME_NONE,
    MIDGARD_POSIX_TIME_NONE,
    hash32(requiredObserversPreimageCbor),
    hash32(requiredSignersPreimageCbor),
    hash32(mintPreimageCbor),
    scriptIntegrityHash,
    auxiliaryDataHash,
    MIDGARD_NETWORK_ID_PREPROD,
  ];

  const bodyHash = hash32(encodeCbor(bodyCompact));
  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(bodyHash),
    signer,
  );

  const addrTxWitsPreimageCbor = encodeByteListPreimage([
    Buffer.from(witness.to_cbor_bytes()),
  ]);
  const scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const redeemerTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const datumTxWitsPreimageCbor = EMPTY_CBOR_LIST;

  const witnessCompact = [
    hash32(addrTxWitsPreimageCbor),
    hash32(scriptTxWitsPreimageCbor),
    hash32(redeemerTxWitsPreimageCbor),
    hash32(datumTxWitsPreimageCbor),
  ];

  const compact = [
    MIDGARD_NATIVE_TX_VERSION,
    bodyHash,
    hash32(encodeCbor(witnessCompact)),
    TX_IS_VALID_CODE,
  ];

  const bodyFull = [
    bodyCompact[0],
    spendInputsPreimageCbor,
    bodyCompact[1],
    referenceInputsPreimageCbor,
    bodyCompact[2],
    outputsPreimageCbor,
    bodyCompact[3],
    bodyCompact[4],
    bodyCompact[5],
    bodyCompact[6],
    requiredObserversPreimageCbor,
    bodyCompact[7],
    requiredSignersPreimageCbor,
    bodyCompact[8],
    mintPreimageCbor,
    bodyCompact[9],
    bodyCompact[10],
    bodyCompact[11],
  ];

  const witnessFull = [
    witnessCompact[0],
    addrTxWitsPreimageCbor,
    witnessCompact[1],
    scriptTxWitsPreimageCbor,
    witnessCompact[2],
    redeemerTxWitsPreimageCbor,
    witnessCompact[3],
    datumTxWitsPreimageCbor,
  ];

  const txCbor = encodeCbor([
    MIDGARD_NATIVE_TX_VERSION,
    compact,
    bodyFull,
    witnessFull,
  ]);

  const txId = bodyHash;

  return {
    txId,
    txHex: txCbor.toString('hex'),
    nextOutRef: toOutRefCbor(txId, 0),
    outputCbor,
    fee,
  };
};

/**
 * Builds and signs a native one-to-one transfer transaction with a converged
 * linear min fee.
 */
const buildNativeSignedOneToOne = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  minFeeA,
  minFeeB,
}) => {
  let fee = minFeeB;
  for (let iteration = 0; iteration < 12; iteration += 1) {
    const inputCoin = CML.TransactionOutput.from_cbor_bytes(
      inputOutputCbor,
    ).amount().coin();
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
    const requiredFee = minFeeA * BigInt(Buffer.from(tx.txHex, 'hex').length) + minFeeB;
    if (requiredFee === fee) {
      return tx;
    }
    fee = requiredFee;
  }
  throw new Error('failed to converge native stress transaction min fee');
};

/**
 * Prebuilds a dependent transaction chain for the stress workload.
 */
const prebuildChain = (chain, length, feeConfig) => {
  /** @type {PrebuiltTx[]} */
  const txs = [];
  let currentOutRef = chain.spendOutRefCbor;
  let currentOutputCbor = chain.outputCbor;
  for (let i = 0; i < length; i++) {
    const tx = buildNativeSignedOneToOne({
      spendOutRefCbor: currentOutRef,
      signer: chain.signer,
      inputOutputCbor: currentOutputCbor,
      minFeeA: feeConfig.minFeeA,
      minFeeB: feeConfig.minFeeB,
    });
    txs.push({
      txHex: tx.txHex,
      txIdHex: tx.txId.toString('hex'),
    });
    currentOutRef = tx.nextOutRef;
    currentOutputCbor = tx.outputCbor;
  }
  return txs;
};

/**
 * Submits a CBOR transaction hex payload to the node.
 */
const submitTxHex = async (txHex) => {
  let attempt = 0;
  while (attempt <= retry503) {
    const resp = await fetch(`${submitEndpoint}/submit`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ tx_cbor: txHex }),
    });

    if (resp.ok) {
      return { ok: true, status: resp.status };
    }

    const body = await resp.text();
    if ((resp.status === 503 || resp.status === 429) && attempt < retry503) {
      attempt += 1;
      await sleep(retryDelayMs);
      continue;
    }

    return {
      ok: false,
      status: resp.status,
      body,
    };
  }

  return { ok: false, status: 0, body: 'retry loop exhausted' };
};

/**
 * Waits until a transaction has passed validation or failed.
 */
const waitForAcceptedTx = async (txIdHex, label) => {
  const startedAt = Date.now();
  while (Date.now() - startedAt <= fanoutStatusTimeoutMs) {
    const status = await fetchTxStatus(txIdHex);
    if (
      status === 'accepted' ||
      status === 'pending_commit' ||
      status === 'awaiting_local_recovery' ||
      status === 'committed'
    ) {
      return status;
    }
    if (status === 'rejected') {
      throw new Error(`${label} ${txIdHex} was rejected`);
    }
    await sleep(txStatusRetryDelayMs);
  }
  throw new Error(
    `${label} ${txIdHex} did not reach accepted status within ${fanoutStatusTimeoutMs}ms`,
  );
};

/**
 * Estimates a safe leaf value for generated fanout UTxOs.
 */
const defaultFanoutOutputLovelace = (minFeeA, minFeeB) => {
  const estimatedTxBytes = 900n;
  const estimatedFee = minFeeA * estimatedTxBytes + minFeeB;
  return estimatedFee * BigInt(chainLength + 2) + 1_000_000n;
};

/**
 * Expands available source UTxOs into enough independent benchmark chains.
 */
const ensureFanoutCandidates = async ({
  candidates,
  minFeeA,
  minFeeB,
}) => {
  if (!fanoutEnabled || candidates.length >= maxChains) {
    return {
      candidates,
      fanoutTxCount: 0,
      fanoutOutputCount: 0,
    };
  }

  const leafLovelace =
    fanoutOutputLovelace ?? defaultFanoutOutputLovelace(minFeeA, minFeeB);
  const result = [...candidates];
  let fanoutTxCount = 0;
  let fanoutOutputCount = 0;

  result.sort((a, b) =>
    a.lovelace === b.lovelace ? 0 : a.lovelace > b.lovelace ? -1 : 1,
  );

  for (let sourceIndex = 0; result.length < maxChains && sourceIndex < result.length; sourceIndex += 1) {
    const source = result[sourceIndex];
    if (outputHasMultiAssets(source.outputCbor)) {
      continue;
    }

    const remainingAfterSpendingSource = result.length - 1;
    const neededOutputs = maxChains - remainingAfterSpendingSource;
    const affordableOutputs = Number(source.lovelace / leafLovelace);
    const outputCount = Math.min(
      fanoutMaxOutputsPerTx,
      neededOutputs,
      affordableOutputs,
    );
    if (outputCount <= 1) {
      continue;
    }

    const split = buildNativeSignedSplit({
      spendOutRefCbor: source.spendOutRefCbor,
      inputOutputCbor: source.outputCbor,
      signer: source.signer,
      outputCount,
      minFeeA,
      minFeeB,
    });
    const txIdHex = split.txId.toString('hex');
    const submittedResult = await submitTxHex(split.txHex);
    if (!submittedResult.ok) {
      throw new Error(
        `fanout submit failed for ${source.outRefHex}: status=${submittedResult.status} body=${submittedResult.body ?? ''}`,
      );
    }
    const status = await waitForAcceptedTx(txIdHex, 'fanout tx');
    console.log(
      `fanout tx accepted: tx=${txIdHex} status=${status} outputs=${split.outputs.length} source=${source.outRefHex}`,
    );

    result.splice(
      sourceIndex,
      1,
      ...split.outputs.map((output) => ({
        walletKey: source.walletKey,
        address: source.address,
        signer: source.signer,
        spendOutRefCbor: output.spendOutRefCbor,
        outputCbor: output.outputCbor,
        lovelace: CML.TransactionOutput.from_cbor_bytes(output.outputCbor)
          .amount()
          .coin(),
        outRefHex: output.outRefHex,
      })),
    );
    fanoutTxCount += 1;
    fanoutOutputCount += split.outputs.length;
    sourceIndex += split.outputs.length - 1;
  }

  return {
    candidates: result,
    fanoutTxCount,
    fanoutOutputCount,
  };
};

/**
 * Runs the valid-stress throughput workload.
 */
const main = async () => {
  const env = parseEnv(envPath);
  const wallets = makeWalletsFromEnv(env);
  const minFeeA = BigInt(process.env.STRESS_MIN_FEE_A ?? env.MIN_FEE_A ?? '0');
  const minFeeB = BigInt(process.env.STRESS_MIN_FEE_B ?? env.MIN_FEE_B ?? '0');

  if (wallets.length === 0) {
    throw new Error(`No genesis wallet seeds found in ${envPath}`);
  }

  console.log('Starting valid throughput stress test with config:');
  console.log(
    JSON.stringify(
      {
        envPath,
        submitEndpoint,
        metricsEndpoint,
        chainLength,
        maxChains,
        utxosPerWallet,
        minLovelace: minLovelace.toString(),
        fanoutEnabled,
        fanoutMaxOutputsPerTx,
        fanoutOutputLovelace:
          fanoutOutputLovelace === null ? null : fanoutOutputLovelace.toString(),
        fanoutStatusTimeoutMs,
        retry503,
        retryDelayMs,
        metricsPollMs,
        observeAfterSubmitSec,
        targetAcceptedTps,
        requireFreshChains,
        minFeeA: minFeeA.toString(),
        minFeeB: minFeeB.toString(),
      },
      null,
      2,
    ),
  );

  const candidates = [];
  const seenOutRefs = new Set();
  for (const wallet of wallets) {
    const utxos = await fetchUtxos(wallet.address);
    let selected = 0;
    for (const utxo of utxos) {
      if (selected >= utxosPerWallet) {
        break;
      }
      const outRefHex = utxo.outref.toLowerCase();
      if (seenOutRefs.has(outRefHex)) {
        continue;
      }
      try {
        const coin = decodeCoin(utxo.value);
        if (coin < minLovelace) {
          continue;
        }
        candidates.push({
          walletKey: wallet.key,
          address: wallet.address,
          signer: wallet.signer,
          spendOutRefCbor: Buffer.from(utxo.outref, 'hex'),
          outputCbor: Buffer.from(utxo.value, 'hex'),
          lovelace: coin,
          outRefHex,
        });
        seenOutRefs.add(outRefHex);
        selected += 1;
      } catch {
        // Skip malformed or non-decodable UTxOs.
      }
    }
    console.log(
      `wallet ${wallet.key} address=${wallet.address} selected_utxos=${selected}`,
    );
  }

  if (candidates.length === 0) {
    throw new Error('No spendable UTxOs found for configured wallets');
  }

  const fanout = await ensureFanoutCandidates({
    candidates,
    minFeeA,
    minFeeB,
  });
  const effectiveCandidates = fanout.candidates;
  console.log(
    `Available benchmark source UTxOs after fanout: ${effectiveCandidates.length} (fanout_txs=${fanout.fanoutTxCount}, fanout_outputs=${fanout.fanoutOutputCount})`,
  );

  /** @type {{ outRefHex: string; txs: PrebuiltTx[]; signer: any; outputCbor: Buffer; spendOutRefCbor: Buffer; walletKey: string; address: string; lovelace: bigint; }[]} */
  const selectedChains = [];
  const generatedTxIds = new Set();
  let replaySkipped = 0;
  let duplicateSkipped = 0;
  let chainBuildSkipped = 0;

  for (const candidate of effectiveCandidates) {
    if (selectedChains.length >= maxChains) {
      break;
    }
    let txs;
    try {
      txs = prebuildChain(candidate, chainLength, { minFeeA, minFeeB });
    } catch (error) {
      chainBuildSkipped += 1;
      console.log(
        `Skipping source UTxO that cannot fund requested chain: outref=${candidate.outRefHex} lovelace=${candidate.lovelace.toString()} reason=${error instanceof Error ? error.message : String(error)}`,
      );
      continue;
    }
    if (txs.length === 0) {
      continue;
    }
    if (requireFreshChains) {
      const firstStatus = await fetchTxStatus(txs[0].txIdHex);
      if (firstStatus !== 'not_found') {
        replaySkipped += 1;
        continue;
      }
    }
    let hasDuplicate = false;
    for (const tx of txs) {
      if (generatedTxIds.has(tx.txIdHex)) {
        hasDuplicate = true;
        break;
      }
    }
    if (hasDuplicate) {
      duplicateSkipped += 1;
      continue;
    }
    for (const tx of txs) {
      generatedTxIds.add(tx.txIdHex);
    }
    selectedChains.push({
      ...candidate,
      txs,
    });
  }

  if (selectedChains.length === 0) {
    throw new Error(
      'No eligible fresh chains found. Increase STRESS_UTXOS_PER_WALLET or disable STRESS_REQUIRE_FRESH_CHAINS=false for diagnostics.',
    );
  }

  console.log(
    `Using ${selectedChains.length} independent prebuilt chains (replay_skipped=${replaySkipped}, duplicate_skipped=${duplicateSkipped}, chain_build_skipped=${chainBuildSkipped})`,
  );

  const startCounters = await readCounters();
  const startedAt = Date.now();

  let submitted = 0;
  let submitErrors = 0;
  /** @type {Record<string, number>} */
  const submitStatusCounts = {};
  /** @type {string[]} */
  const firstErrors = [];

  let maxAcceptRate1s = 0;
  let maxRejectRate1s = 0;
  let maxSubmitRate1s = 0;
  let targetReachedAt = null;

  let submitFinishedAt = null;

  /**
   * Monitors workload progress and emits periodic metrics.
   */
  const monitorPromise = (async () => {
    let prev = await readCounters();
    let prevTs = Date.now();

    while (true) {
      await sleep(metricsPollMs);
      const now = await readCounters();
      const nowTs = Date.now();
      const dt = (nowTs - prevTs) / 1000;

      const submitRate = (now.submit - prev.submit) / dt;
      const acceptRate = (now.accept - prev.accept) / dt;
      const rejectRate = (now.reject - prev.reject) / dt;

      if (submitRate > maxSubmitRate1s) maxSubmitRate1s = submitRate;
      if (acceptRate > maxAcceptRate1s) maxAcceptRate1s = acceptRate;
      if (rejectRate > maxRejectRate1s) maxRejectRate1s = rejectRate;

      if (
        targetReachedAt === null &&
        Number.isFinite(targetAcceptedTps) &&
        targetAcceptedTps > 0 &&
        acceptRate >= targetAcceptedTps
      ) {
        targetReachedAt = new Date(nowTs).toISOString();
      }

      console.log(
        `rate_submit=${submitRate.toFixed(2)} rate_accept=${acceptRate.toFixed(2)} rate_reject=${rejectRate.toFixed(2)} totals={submit:${now.submit},accept:${now.accept},reject:${now.reject}}`,
      );

      prev = now;
      prevTs = nowTs;

      const settledDelta =
        now.accept - startCounters.accept + (now.reject - startCounters.reject);

      if (
        submitFinishedAt !== null &&
        settledDelta >= submitted
      ) {
        break;
      }

      if (
        submitFinishedAt !== null &&
        nowTs - submitFinishedAt >= observeAfterSubmitSec * 1000
      ) {
        break;
      }
    }
  })();

  await Promise.all(
    selectedChains.map(async (chain, chainIndex) => {
      for (let i = 0; i < chain.txs.length; i++) {
        const tx = chain.txs[i];
        const submittedResult = await submitTxHex(tx.txHex);
        const key = String(submittedResult.status);
        submitStatusCounts[key] = (submitStatusCounts[key] ?? 0) + 1;

        if (!submittedResult.ok) {
          submitErrors += 1;
          if (firstErrors.length < 10) {
            firstErrors.push(
              `chain=${chainIndex} index=${i} status=${submittedResult.status} body=${submittedResult.body ?? ''}`,
            );
          }
          // Chain depends on this tx output; stop this chain on first failed submit.
          break;
        }

        submitted += 1;
      }
    }),
  );

  submitFinishedAt = Date.now();
  await monitorPromise;

  const endCounters = await readCounters();
  const elapsedSec = (Date.now() - startedAt) / 1000;

  const summary = {
    chainCount: selectedChains.length,
    chainLength,
    attempted: selectedChains.reduce((acc, chain) => acc + chain.txs.length, 0),
    replaySkipped,
    duplicateSkipped,
    chainBuildSkipped,
    fanoutTxCount: fanout.fanoutTxCount,
    fanoutOutputCount: fanout.fanoutOutputCount,
    uniquePrebuiltTxIds: generatedTxIds.size,
    submitted,
    submitErrors,
    submitStatusCounts,
    acceptDelta: endCounters.accept - startCounters.accept,
    rejectDelta: endCounters.reject - startCounters.reject,
    submitDelta: endCounters.submit - startCounters.submit,
    avgAcceptedTps: (endCounters.accept - startCounters.accept) / elapsedSec,
    avgSubmittedTps: (endCounters.submit - startCounters.submit) / elapsedSec,
    maxAcceptRate1s,
    maxSubmitRate1s,
    maxRejectRate1s,
    targetAcceptedTps,
    targetReachedAt,
    elapsedSec,
    firstErrors,
  };

  console.log('Stress test summary:');
  console.log(JSON.stringify(summary, null, 2));

  if ((summary.acceptDelta <= 0 || summary.submitErrors > 0) && firstErrors.length > 0) {
    process.exitCode = 1;
  }
};

main().catch((error) => {
  console.error('throughput-valid-stress failed:', error);
  process.exitCode = 1;
});
