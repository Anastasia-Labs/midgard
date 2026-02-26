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
if (!Number.isFinite(metricsPollMs) || metricsPollMs <= 0) {
  throw new Error('STRESS_METRICS_POLL_MS must be a positive integer');
}

/** @typedef {{ outref: string; value: string }} NodeUtxo */
/** @typedef {{ txHex: string; txIdHex: string }} PrebuiltTx */

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const encodeCbor = (value) => Buffer.from(cborEncode(value));
const hash32 = (value) => Buffer.from(blake2b(value, { dkLen: HASH32_LEN }));

const encodeByteListPreimage = (items) =>
  encodeCbor(items.map((item) => Buffer.from(item)));

const toOutRefCbor = (txId, outputIndex) =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(txId),
      BigInt(outputIndex),
    ).to_cbor_bytes(),
  );

const parseEnv = (filename) => {
  const raw = fs.readFileSync(filename, 'utf8');
  return dotenv.parse(raw);
};

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

const fetchMetricsText = async () => {
  const resp = await fetch(metricsEndpoint);
  if (!resp.ok) {
    throw new Error(`metrics endpoint returned ${resp.status}`);
  }
  return resp.text();
};

const escapeRegex = (value) => value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

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

const decodeCoin = (outputHex) => {
  const output = CML.TransactionOutput.from_cbor_bytes(Buffer.from(outputHex, 'hex'));
  return output.amount().coin();
};

const buildNativeSignedOneToOne = ({ spendOutRefCbor, outputCbor, signer }) => {
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
    0n,
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

  const witnessCompact = [
    hash32(addrTxWitsPreimageCbor),
    hash32(scriptTxWitsPreimageCbor),
    hash32(redeemerTxWitsPreimageCbor),
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
  ];

  const txCbor = encodeCbor([
    MIDGARD_NATIVE_TX_VERSION,
    compact,
    bodyFull,
    witnessFull,
  ]);

  const txId = hash32(encodeCbor(compact));

  return {
    txId,
    txHex: txCbor.toString('hex'),
    nextOutRef: toOutRefCbor(txId, 0),
  };
};

const prebuildChain = (chain, length) => {
  /** @type {PrebuiltTx[]} */
  const txs = [];
  let currentOutRef = chain.spendOutRefCbor;
  for (let i = 0; i < length; i++) {
    const tx = buildNativeSignedOneToOne({
      spendOutRefCbor: currentOutRef,
      outputCbor: chain.outputCbor,
      signer: chain.signer,
    });
    txs.push({
      txHex: tx.txHex,
      txIdHex: tx.txId.toString('hex'),
    });
    currentOutRef = tx.nextOutRef;
  }
  return txs;
};

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

const main = async () => {
  const env = parseEnv(envPath);
  const wallets = makeWalletsFromEnv(env);

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
        retry503,
        retryDelayMs,
        metricsPollMs,
        observeAfterSubmitSec,
        targetAcceptedTps,
        requireFreshChains,
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

  /** @type {{ outRefHex: string; txs: PrebuiltTx[]; signer: any; outputCbor: Buffer; spendOutRefCbor: Buffer; walletKey: string; address: string; lovelace: bigint; }[]} */
  const selectedChains = [];
  const generatedTxIds = new Set();
  let replaySkipped = 0;
  let duplicateSkipped = 0;

  for (const candidate of candidates) {
    if (selectedChains.length >= maxChains) {
      break;
    }
    const txs = prebuildChain(candidate, chainLength);
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
    `Using ${selectedChains.length} independent prebuilt chains (replay_skipped=${replaySkipped}, duplicate_skipped=${duplicateSkipped})`,
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
