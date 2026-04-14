/**
 * Native user-transfer command support for Midgard L2.
 * This module derives a wallet from a seed phrase, queries the live Midgard
 * ledger view for spendable UTxOs, constructs a balanced Midgard-native
 * transaction with explicit change, and submits it through the node's public
 * `/submit` endpoint.
 */
import { encode as cborEncode } from "cborg";
import { Effect } from "effect";
import {
  type Assets,
  CML,
  assetsToValue,
  getAddressDetails,
  valueToAssets,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import {
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  deriveMidgardNativeTxCompact,
  encodeMidgardNativeTxFull,
  type MidgardNativeTxBodyFull,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetFull,
} from "@/midgard-tx-codec/index.js";
import * as MempoolDB from "@/database/mempool.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import * as TxRejectionsDB from "@/database/txRejections.js";
import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "@/asset-specs.js";
import {
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
  type QueuedTx,
} from "@/validation/index.js";
import {
  Database as DatabaseService,
  Lucid,
  NodeConfig as NodeConfigService,
} from "@/services/index.js";
import { compareOutRefs, outRefLabel, type OutRefLike } from "@/tx-context.js";

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);
const DEFAULT_MIDGARD_NODE_PORT = "3000";
export const DEFAULT_WALLET_SEED_ENV = "USER_WALLET";
export const LEGACY_DEFAULT_WALLET_SEED_ENV = "USER_SEED_PHRASE";
export type SubmissionMode = "api" | "local";

export type SubmitL2TransferConfig = {
  readonly l2Address: string;
  readonly lovelace: bigint;
  readonly additionalAssets: Readonly<Assets>;
  readonly nodeEndpoint: string;
  readonly networkId: bigint;
  readonly submissionMode: SubmissionMode;
};

export type ResolvedWalletSeedPhrase = {
  readonly seedPhrase: string;
  readonly resolvedFrom:
    | "direct-argument"
    | typeof DEFAULT_WALLET_SEED_ENV
    | typeof LEGACY_DEFAULT_WALLET_SEED_ENV
    | string;
};

export type RawNodeUtxo = {
  readonly outref: string;
  readonly value: string;
};

export type NodeUtxo = OutRefLike & {
  readonly outrefCbor: Buffer;
  readonly outputCbor: Buffer;
  readonly address: string;
  readonly assets: Readonly<Assets>;
};

export type BuiltTransferTx = {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCbor: Buffer;
  readonly txHex: string;
  readonly fee: bigint;
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly selectedInputs: readonly NodeUtxo[];
  readonly requestedAssets: Readonly<Assets>;
  readonly changeAssets: Readonly<Assets>;
};

export type SubmitL2TransferResult = {
  readonly txId: string;
  readonly status: string;
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly selectedInputs: readonly string[];
  readonly requestedAssets: Readonly<Assets>;
  readonly changeAssets: Readonly<Assets>;
  readonly walletSeedSource: string;
  readonly nodeEndpoint: string;
};

/**
 * JSON replacer that preserves bigint values as decimal strings.
 */
const jsonReplacer = (_key: string, value: unknown): unknown =>
  typeof value === "bigint" ? value.toString(10) : value;

/**
 * Stable JSON serializer used for CLI and error output.
 */
const serializeJson = (value: unknown): string =>
  JSON.stringify(value, jsonReplacer, 2);

/**
 * CBOR-encodes a list of byte arrays in the format expected by Midgard-native
 * transaction hashing.
 */
const encodeByteListPreimage = (items: readonly Uint8Array[]): Buffer =>
  Buffer.from(cborEncode(items.map((item) => Buffer.from(item))));

/**
 * Normalizes and validates the configured Midgard node endpoint.
 */
const parseNodeEndpoint = (value: string): string => {
  const normalized = value.trim();
  if (normalized.length === 0) {
    throw new Error("Midgard node endpoint must not be empty.");
  }
  let url: URL;
  try {
    url = new URL(normalized);
  } catch (cause) {
    throw new Error(
      `Invalid Midgard node endpoint "${value}": ${String(cause)}`,
    );
  }
  url.pathname = url.pathname.replace(/\/+$/, "");
  url.search = "";
  url.hash = "";
  return url.toString().replace(/\/+$/, "");
};

/**
 * Converts an unknown failure into an `Error` with a stable prefix.
 */
const toError = (cause: unknown, prefix: string): Error =>
  cause instanceof Error
    ? new Error(`${prefix}: ${cause.message}`)
    : new Error(`${prefix}: ${String(cause)}`);

/**
 * Maps a network id into the Lucid wallet-network discriminator.
 */
const walletNetworkFromId = (
  networkId: bigint,
): "Mainnet" | "Preprod" => (networkId === 1n ? "Mainnet" : "Preprod");

/**
 * Parses the submission mode from CLI or environment input.
 */
const parseSubmissionMode = (value: string | undefined): SubmissionMode => {
  const normalized = value?.trim().toLowerCase() ?? "api";
  if (normalized === "api" || normalized === "local") {
    return normalized;
  }
  throw new Error(
    `Invalid submission mode "${value}". Expected "api" or "local".`,
  );
};

/**
 * Resolves the default Midgard node endpoint from the supported environment
 * variables.
 */
export const defaultMidgardNodeEndpoint = (
  env: NodeJS.ProcessEnv = process.env,
): string =>
  parseNodeEndpoint(
    env.MIDGARD_NODE_URL?.trim() ??
      env.ACTIVITY_SUBMIT_ENDPOINT?.trim() ??
      env.STRESS_SUBMIT_ENDPOINT?.trim() ??
      `http://127.0.0.1:${env.PORT?.trim() || DEFAULT_MIDGARD_NODE_PORT}`,
  );

/**
 * Removes zero-quantity units and coerces asset values to bigint.
 */
const normalizeAssets = (assets: Readonly<Assets>): Assets => {
  const normalized: Assets = {};
  for (const [unit, amount] of Object.entries(assets)) {
    const quantity = BigInt(amount);
    if (quantity !== 0n) {
      normalized[unit] = quantity;
    }
  }
  return normalized;
};

/**
 * Adds two asset maps together, dropping units whose net quantity is zero.
 */
const addAssetMaps = (
  lhs: Readonly<Assets>,
  rhs: Readonly<Assets>,
): Readonly<Assets> => {
  const total: Assets = { ...normalizeAssets(lhs) };
  for (const [unit, amount] of Object.entries(rhs)) {
    const quantity = BigInt(amount);
    if (quantity === 0n) {
      continue;
    }
    total[unit] = (total[unit] ?? 0n) + quantity;
    if (total[unit] === 0n) {
      delete total[unit];
    }
  }
  return total;
};

/**
 * Subtracts one asset map from another, failing if the subtraction would go
 * negative.
 */
const subtractAssetMaps = (
  lhs: Readonly<Assets>,
  rhs: Readonly<Assets>,
): Readonly<Assets> => {
  const remaining: Assets = { ...normalizeAssets(lhs) };
  for (const [unit, amount] of Object.entries(rhs)) {
    const quantity = BigInt(amount);
    if (quantity === 0n) {
      continue;
    }
    const available = remaining[unit] ?? 0n;
    if (available < quantity) {
      throw new Error(
        `Insufficient ${unit} while calculating transfer change (${available} < ${quantity}).`,
      );
    }
    const next = available - quantity;
    if (next === 0n) {
      delete remaining[unit];
    } else {
      remaining[unit] = next;
    }
  }
  return remaining;
};

/**
 * Reduces a required-asset set by the contribution made from one candidate
 * input.
 */
const reduceRequiredAssets = (
  remaining: Readonly<Assets>,
  contribution: Readonly<Assets>,
): Readonly<Assets> => {
  const reduced: Assets = {};
  for (const [unit, amount] of Object.entries(remaining)) {
    const missing = BigInt(amount);
    if (missing <= 0n) {
      continue;
    }
    const available = BigInt(contribution[unit] ?? 0n);
    if (available < missing) {
      reduced[unit] = missing - available;
    }
  }
  return reduced;
};

/**
 * Returns whether every required asset has been fully covered.
 */
const assetsSatisfied = (required: Readonly<Assets>): boolean =>
  Object.keys(required).length === 0;

/**
 * Orders candidate inputs by how well they cover the currently missing assets.
 */
const compareAssetsByCoverage = (
  lhs: Readonly<Assets>,
  rhs: Readonly<Assets>,
  required: Readonly<Assets>,
): number => {
  /**
   * Scores candidate UTxOs for transfer-input selection.
   */
  const score = (assets: Readonly<Assets>) => {
    let requiredTokenKinds = 0;
    let requiredTokenQuantity = 0n;
    for (const [unit, amount] of Object.entries(required)) {
      if (unit === "lovelace") {
        continue;
      }
      const present = BigInt(assets[unit] ?? 0n);
      if (present > 0n) {
        requiredTokenKinds += 1;
        requiredTokenQuantity += present < amount ? present : amount;
      }
    }
    return {
      requiredTokenKinds,
      requiredTokenQuantity,
      lovelace: BigInt(assets.lovelace ?? 0n),
    };
  };

  const left = score(lhs);
  const right = score(rhs);

  if (left.requiredTokenKinds !== right.requiredTokenKinds) {
    return right.requiredTokenKinds - left.requiredTokenKinds;
  }
  if (left.requiredTokenQuantity !== right.requiredTokenQuantity) {
    return left.requiredTokenQuantity > right.requiredTokenQuantity ? -1 : 1;
  }
  if (left.lovelace !== right.lovelace) {
    return left.lovelace > right.lovelace ? -1 : 1;
  }
  return 0;
};

/**
 * Encodes one selected input into the CBOR byte form used by the Midgard-
 * native transaction preimage.
 */
const toTransactionInputBytes = (utxo: NodeUtxo): Buffer =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(utxo.txHash),
      BigInt(utxo.outputIndex),
    ).to_cbor_bytes(),
  );

/**
 * Decodes one raw UTxO payload returned by the node's `/utxos` endpoint.
 */
const decodeNodeUtxo = (raw: RawNodeUtxo): NodeUtxo => {
  const outrefHex = raw.outref.trim().toLowerCase();
  const valueHex = raw.value.trim().toLowerCase();
  const outrefBytes = Buffer.from(outrefHex, "hex");
  const outputBytes = Buffer.from(valueHex, "hex");
  const input = CML.TransactionInput.from_cbor_bytes(outrefBytes);
  const output = CML.TransactionOutput.from_cbor_bytes(outputBytes);

  return {
    txHash: input.transaction_id().to_hex(),
    outputIndex: Number(input.index()),
    outrefCbor: outrefBytes,
    outputCbor: outputBytes,
    address: output.address().to_bech32(),
    assets: valueToAssets(output.amount()),
  };
};

/**
 * Validates and extracts the raw UTxO array from the node's JSON response.
 */
const parseNodeUtxoResponse = (payload: unknown): readonly RawNodeUtxo[] => {
  if (typeof payload !== "object" || payload === null) {
    throw new Error("Midgard node returned a non-object UTxO payload.");
  }
  const utxos = (payload as { readonly utxos?: unknown }).utxos;
  if (!Array.isArray(utxos)) {
    throw new Error("Midgard node UTxO response is missing an `utxos` array.");
  }
  return utxos.map((entry, index) => {
    if (typeof entry !== "object" || entry === null) {
      throw new Error(`UTxO entry ${index} is not an object.`);
    }
    const { outref, value } = entry as {
      readonly outref?: unknown;
      readonly value?: unknown;
    };
    if (typeof outref !== "string" || typeof value !== "string") {
      throw new Error(
        `UTxO entry ${index} must contain string outref/value fields.`,
      );
    }
    return { outref, value };
  });
};

/**
 * Parses CLI-style transfer arguments into a normalized transfer config.
 */
export const parseSubmitL2TransferConfig = ({
  l2Address,
  lovelace,
  assetSpecs,
  nodeEndpoint,
  submissionMode,
}: {
  readonly l2Address: string;
  readonly lovelace: string;
  readonly assetSpecs: readonly string[];
  readonly nodeEndpoint?: string;
  readonly submissionMode?: string;
}): SubmitL2TransferConfig => {
  const normalizedL2Address = l2Address.trim();
  if (normalizedL2Address.length === 0) {
    throw new Error("L2 address must not be empty.");
  }

  let addressDetails: ReturnType<typeof getAddressDetails>;
  try {
    addressDetails = getAddressDetails(normalizedL2Address);
  } catch (cause) {
    throw new Error(
      `Invalid L2 address "${normalizedL2Address}": ${String(cause)}`,
    );
  }
  if (!addressDetails.paymentCredential) {
    throw new Error("L2 address must include a payment credential.");
  }

  return {
    l2Address: addressDetails.address.bech32,
    lovelace: parseLovelaceAmount(
      lovelace,
      "Transfer lovelace amount must be greater than zero.",
    ),
    additionalAssets: parseAdditionalAssetSpecs(assetSpecs),
    nodeEndpoint: parseNodeEndpoint(nodeEndpoint ?? defaultMidgardNodeEndpoint()),
    networkId: BigInt(addressDetails.networkId),
    submissionMode: parseSubmissionMode(submissionMode),
  };
};

/**
 * Resolves the wallet seed phrase from either a direct argument or a
 * configured environment-variable fallback chain.
 */
export const resolveWalletSeedPhrase = ({
  walletSeedPhrase,
  walletSeedPhraseEnv,
  env = process.env,
}: {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv: string;
  readonly env?: NodeJS.ProcessEnv;
}): ResolvedWalletSeedPhrase => {
  const direct = walletSeedPhrase?.trim() ?? "";
  if (direct.length > 0) {
    return {
      seedPhrase: direct,
      resolvedFrom: "direct-argument",
    };
  }

  const normalizedEnvVar = walletSeedPhraseEnv.trim();
  if (normalizedEnvVar.length === 0) {
    throw new Error("Wallet seed phrase env var name must not be empty.");
  }

  const configuredSeed = env[normalizedEnvVar]?.trim() ?? "";
  if (configuredSeed.length > 0) {
    return {
      seedPhrase: configuredSeed,
      resolvedFrom: normalizedEnvVar,
    };
  }

  if (normalizedEnvVar === DEFAULT_WALLET_SEED_ENV) {
    const legacySeed = env[LEGACY_DEFAULT_WALLET_SEED_ENV]?.trim() ?? "";
    if (legacySeed.length > 0) {
      return {
        seedPhrase: legacySeed,
        resolvedFrom: LEGACY_DEFAULT_WALLET_SEED_ENV,
      };
    }
  }

  throw new Error(
    `Environment variable "${normalizedEnvVar}" does not contain a wallet seed phrase.`,
  );
};

/**
 * Builds the requested transfer asset map from the normalized command config.
 */
const buildRequestedAssets = (
  config: SubmitL2TransferConfig,
): Readonly<Assets> => ({
  lovelace: config.lovelace,
  ...config.additionalAssets,
});

/**
 * Selects a deterministic set of wallet inputs that covers the requested asset
 * set.
 */
export const selectTransferInputs = (
  utxos: readonly NodeUtxo[],
  requestedAssets: Readonly<Assets>,
): readonly NodeUtxo[] => {
  const ranked = [...utxos].sort((lhs, rhs) => {
    const coverageComparison = compareAssetsByCoverage(
      lhs.assets,
      rhs.assets,
      requestedAssets,
    );
    if (coverageComparison !== 0) {
      return coverageComparison;
    }
    return compareOutRefs(lhs, rhs);
  });

  const selected: NodeUtxo[] = [];
  let remaining: Readonly<Assets> = normalizeAssets(requestedAssets);

  for (const utxo of ranked) {
    const nextRemaining = reduceRequiredAssets(remaining, utxo.assets);
    if (Object.keys(nextRemaining).length === Object.keys(remaining).length) {
      continue;
    }
    selected.push(utxo);
    remaining = nextRemaining;
    if (assetsSatisfied(remaining)) {
      break;
    }
  }

  if (!assetsSatisfied(remaining)) {
    throw new Error(
      `Insufficient Midgard L2 funds for requested transfer. Missing ${serializeJson(remaining)}.`,
    );
  }

  return selected.sort(compareOutRefs);
};

/**
 * Constructs one transaction output from a bech32 address and normalized asset
 * map.
 */
const makeOutput = (
  address: string,
  assets: Readonly<Assets>,
): InstanceType<typeof CML.TransactionOutput> =>
  CML.TransactionOutput.new(
    CML.Address.from_bech32(address),
    assetsToValue(normalizeAssets(assets)),
  );

/**
 * Builds a fully signed Midgard-native transfer transaction with explicit
 * change handling.
 */
export const buildTransferTx = ({
  senderAddress,
  destinationAddress,
  signer,
  selectedInputs,
  requestedAssets,
  networkId,
  fee = 0n,
}: {
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly signer: ReturnType<typeof CML.PrivateKey.from_bech32>;
  readonly selectedInputs: readonly NodeUtxo[];
  readonly requestedAssets: Readonly<Assets>;
  readonly networkId: bigint;
  readonly fee?: bigint;
}): BuiltTransferTx => {
  if (selectedInputs.length === 0) {
    throw new Error("Cannot build a transfer without selected inputs.");
  }
  const orderedInputs = [...selectedInputs].sort(compareOutRefs);

  const selectedTotal = orderedInputs.reduce<Readonly<Assets>>(
    (acc, utxo) => addAssetMaps(acc, utxo.assets),
    {},
  );
  const changeAssets = subtractAssetMaps(
    selectedTotal,
    addAssetMaps(requestedAssets, fee > 0n ? { lovelace: fee } : {}),
  );

  const spendInputs = orderedInputs.map(toTransactionInputBytes);
  const outputs = [
    Buffer.from(
      makeOutput(destinationAddress, requestedAssets).to_cbor_bytes(),
    ),
  ];
  if (Object.keys(changeAssets).length > 0) {
    outputs.push(
      Buffer.from(makeOutput(senderAddress, changeAssets).to_cbor_bytes()),
    );
  }

  const spendInputsPreimageCbor = encodeByteListPreimage(spendInputs);
  const referenceInputsPreimageCbor = EMPTY_CBOR_LIST;
  const outputsPreimageCbor = encodeByteListPreimage(outputs);
  const requiredObserversPreimageCbor = EMPTY_CBOR_LIST;
  const requiredSignersPreimageCbor = encodeByteListPreimage([
    Buffer.from(signer.to_public().hash().to_raw_bytes()),
  ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const body: MidgardNativeTxBodyFull = {
    spendInputsRoot: computeHash32(spendInputsPreimageCbor),
    spendInputsPreimageCbor,
    referenceInputsRoot: computeHash32(referenceInputsPreimageCbor),
    referenceInputsPreimageCbor,
    outputsRoot: computeHash32(outputsPreimageCbor),
    outputsPreimageCbor,
    fee,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversRoot: computeHash32(requiredObserversPreimageCbor),
    requiredObserversPreimageCbor,
    requiredSignersRoot: computeHash32(requiredSignersPreimageCbor),
    requiredSignersPreimageCbor,
    mintRoot: computeHash32(mintPreimageCbor),
    mintPreimageCbor,
    scriptIntegrityHash: computeHash32(EMPTY_CBOR_NULL),
    auxiliaryDataHash: computeHash32(EMPTY_CBOR_NULL),
    networkId,
  };

  const bodyHash = deriveMidgardNativeTxCompact(
    body,
    {
      addrTxWitsRoot: computeHash32(EMPTY_CBOR_LIST),
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsRoot: computeHash32(EMPTY_CBOR_LIST),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsRoot: computeHash32(EMPTY_CBOR_LIST),
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      datumTxWitsRoot: computeHash32(EMPTY_CBOR_LIST),
      datumTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
    "TxIsValid",
  ).transactionBodyHash;

  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(bodyHash),
    signer,
  );
  const addrTxWitsPreimageCbor = encodeByteListPreimage([
    Buffer.from(witness.to_cbor_bytes()),
  ]);
  const witnessSet: MidgardNativeTxWitnessSetFull = {
    addrTxWitsRoot: computeHash32(addrTxWitsPreimageCbor),
    addrTxWitsPreimageCbor,
    scriptTxWitsRoot: computeHash32(EMPTY_CBOR_LIST),
    scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    redeemerTxWitsRoot: computeHash32(EMPTY_CBOR_LIST),
    redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    datumTxWitsRoot: computeHash32(EMPTY_CBOR_LIST),
    datumTxWitsPreimageCbor: EMPTY_CBOR_LIST,
  };

  const tx: MidgardNativeTxFull = {
    version: MIDGARD_NATIVE_TX_VERSION,
    compact: deriveMidgardNativeTxCompact(body, witnessSet, "TxIsValid"),
    body,
    witnessSet,
  };
  const txCbor = encodeMidgardNativeTxFull(tx);
  const txId = computeMidgardNativeTxIdFromFull(tx);

  return {
    txId,
    txIdHex: txId.toString("hex"),
    txCbor,
    txHex: txCbor.toString("hex"),
    fee,
    senderAddress,
    destinationAddress,
    selectedInputs: orderedInputs,
    requestedAssets,
    changeAssets,
  };
};

/**
 * Computes the linear minimum fee implied by the configured fee policy.
 */
const computeRequiredMinFee = (
  txByteLength: number,
  minFeeA: bigint,
  minFeeB: bigint,
): bigint => minFeeA * BigInt(txByteLength) + minFeeB;

/**
 * Rebuilds the transfer transaction until the selected fee converges on the
 * minimum fee implied by the final transaction size.
 */
export const buildTransferTxWithMinFee = ({
  senderAddress,
  destinationAddress,
  signer,
  availableUtxos,
  requestedAssets,
  networkId,
  minFeeA,
  minFeeB,
}: {
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly signer: ReturnType<typeof CML.PrivateKey.from_bech32>;
  readonly availableUtxos: readonly NodeUtxo[];
  readonly requestedAssets: Readonly<Assets>;
  readonly networkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
}): BuiltTransferTx => {
  let fee = minFeeB;

  for (let iteration = 0; iteration < 12; iteration += 1) {
    const selectedInputs = selectTransferInputs(
      availableUtxos,
      addAssetMaps(requestedAssets, fee > 0n ? { lovelace: fee } : {}),
    );
    const built = buildTransferTx({
      senderAddress,
      destinationAddress,
      signer,
      selectedInputs,
      requestedAssets,
      networkId,
      fee,
    });
    const requiredFee = computeRequiredMinFee(
      built.txCbor.length,
      minFeeA,
      minFeeB,
    );
    if (requiredFee === fee) {
      return built;
    }
    fee = requiredFee;
  }

  throw new Error(
    "Failed to converge on a stable Midgard-native minimum fee for the transfer.",
  );
};

/**
 * Queries the node's public `/utxos` endpoint and decodes the returned wallet
 * UTxOs.
 */
export const fetchNodeUtxos = (
  nodeEndpoint: string,
  address: string,
): Effect.Effect<readonly NodeUtxo[], Error> =>
  Effect.tryPromise({
    try: async () => {
      const response = await fetch(
        `${nodeEndpoint}/utxos?address=${encodeURIComponent(address)}`,
      );
      const responseText = await response.text();
      if (!response.ok) {
        throw new Error(
          `Midgard node UTxO query failed (${response.status}): ${responseText}`,
        );
      }
      const payload = JSON.parse(responseText) as unknown;
      return parseNodeUtxoResponse(payload).map(decodeNodeUtxo);
    },
    catch: (cause) =>
      new Error(`Failed to fetch Midgard UTxOs: ${String(cause)}`),
  });

/**
 * Reads the sender's spendable UTxOs from the local mempool-ledger view rather
 * than the public HTTP API.
 */
export const fetchLocalUtxos = (
  address: string,
): Effect.Effect<readonly NodeUtxo[], Error, DatabaseService> =>
  MempoolLedgerDB.retrieveByAddress(address).pipe(
    Effect.map((entries) =>
      entries.map((entry) =>
        decodeNodeUtxo({
          outref: entry.outref.toString("hex"),
          value: entry.output.toString("hex"),
        }),
      ),
    ),
    Effect.mapError((cause) =>
      toError(cause, "Failed to fetch local Midgard UTxOs"),
    ),
  );

/**
 * Submits a Midgard-native transfer through the node's public `/submit`
 * endpoint and verifies the returned tx id.
 */
export const submitNativeTransferTx = (
  nodeEndpoint: string,
  txHex: string,
  expectedTxIdHex: string,
): Effect.Effect<{ readonly txId: string; readonly status: string }, Error> =>
  Effect.tryPromise({
    try: async () => {
      const response = await fetch(`${nodeEndpoint}/submit`, {
        method: "POST",
        headers: {
          "content-type": "application/json",
        },
        body: JSON.stringify({ tx_cbor: txHex }),
      });
      const responseText = await response.text();
      if (!response.ok) {
        throw new Error(
          `Midgard node transfer submit failed (${response.status}): ${responseText}`,
        );
      }
      const parsed = JSON.parse(responseText) as {
        readonly txId?: unknown;
        readonly status?: unknown;
      };
      if (typeof parsed.txId !== "string" || typeof parsed.status !== "string") {
        throw new Error(
          "Midgard node submit response must contain string txId/status fields.",
        );
      }
      if (parsed.txId.toLowerCase() !== expectedTxIdHex.toLowerCase()) {
        throw new Error(
          `Midgard node returned mismatched txId ${parsed.txId} (expected ${expectedTxIdHex}).`,
        );
      }
      return {
        txId: parsed.txId,
        status: parsed.status,
      };
    },
    catch: (cause) =>
      new Error(`Failed to submit Midgard-native transfer: ${String(cause)}`),
  });

/**
 * Converts a built transfer into the queue-entry shape expected by validation.
 */
const toQueuedTx = (built: BuiltTransferTx): QueuedTx => ({
  txId: built.txId,
  txCbor: built.txCbor,
  arrivalSeq: 0n,
  createdAt: new Date(),
});

/**
 * Runs the transfer through local validation and inserts it directly into the
 * mempool tables when accepted.
 */
export const submitNativeTransferLocally = (
  built: BuiltTransferTx,
): Effect.Effect<
  { readonly txId: string; readonly status: string },
  Error,
  DatabaseService | NodeConfigService | Lucid
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfigService;
    const { api: lucid } = yield* Lucid;
    const phaseA = yield* runPhaseAValidation([toQueuedTx(built)], {
      expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
      minFeeA: nodeConfig.MIN_FEE_A,
      minFeeB: nodeConfig.MIN_FEE_B,
      concurrency: 1,
      strictnessProfile: nodeConfig.VALIDATION_STRICTNESS_PROFILE,
    });

    const preStateEntries = yield* MempoolLedgerDB.retrieve;
    const preState = new Map<string, Buffer>();
    for (const entry of preStateEntries) {
      preState.set(entry.outref.toString("hex"), entry.output);
    }

    const phaseB = yield* runPhaseBValidationWithPatch(phaseA.accepted, preState, {
      nowCardanoSlotNo: BigInt(lucid.currentSlot()),
      bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
    });
    const rejected = [...phaseA.rejected, ...phaseB.rejected];
    if (rejected.length > 0) {
      yield* TxRejectionsDB.insertMany(
        rejected.map((entry) => ({
          tx_id: entry.txId,
          reject_code: entry.code,
          reject_detail: entry.detail,
        })),
      );
      const first = rejected[0]!;
      return yield* Effect.fail(
        new Error(
          `Local Midgard transfer validation rejected ${built.txIdHex}: ${first.code} (${first.detail})`,
        ),
      );
    }

    if (phaseB.accepted.length !== 1) {
      return yield* Effect.fail(
        new Error(
          `Expected exactly one accepted transfer, got ${phaseB.accepted.length}.`,
        ),
      );
    }

    yield* MempoolDB.insertMultiple(
      phaseB.accepted.map((accepted) => accepted.processedTx),
    );
    return {
      txId: built.txIdHex,
      status: "accepted-local",
    };
  });

/**
 * End-to-end L2 transfer submission program.
 *
 * The flow derives the sender wallet, gathers available inputs, builds a
 * fee-balanced Midgard-native transfer, and submits it through either the HTTP
 * API or the local validation path.
 */
export const submitL2TransferProgram = ({
  config,
  resolvedWalletSeedPhrase,
  assertWalletAddress,
}: {
  readonly config: SubmitL2TransferConfig;
  readonly resolvedWalletSeedPhrase: ResolvedWalletSeedPhrase;
  readonly assertWalletAddress?: (walletAddress: string) => void;
}): Effect.Effect<
  SubmitL2TransferResult,
  Error,
  DatabaseService | NodeConfigService | Lucid
> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfigService;
    const wallet = yield* Effect.try({
      try: () =>
        walletFromSeed(resolvedWalletSeedPhrase.seedPhrase, {
          network: walletNetworkFromId(config.networkId),
        }),
      catch: (cause) => toError(cause, "Failed to derive wallet from seed phrase"),
    });
    const senderAddress = wallet.address;
    yield* Effect.sync(() => assertWalletAddress?.(senderAddress));
    const senderDetails = getAddressDetails(senderAddress);
    if (!senderDetails.paymentCredential) {
      return yield* Effect.fail(
        new Error("Derived sender address must include a payment credential."),
      );
    }
    if (BigInt(senderDetails.networkId) !== config.networkId) {
      return yield* Effect.fail(
        new Error(
          `Sender wallet network id ${senderDetails.networkId} does not match destination network id ${config.networkId.toString()}.`,
        ),
      );
    }

    const requestedAssets = buildRequestedAssets(config);
    const availableUtxos =
      config.submissionMode === "local"
        ? yield* fetchLocalUtxos(senderAddress)
        : yield* fetchNodeUtxos(config.nodeEndpoint, senderAddress);
    if (availableUtxos.length === 0) {
      return yield* Effect.fail(
        new Error(`No Midgard L2 UTxOs found for sender address ${senderAddress}.`),
      );
    }

    const signer = CML.PrivateKey.from_bech32(wallet.paymentKey);
    const built = yield* Effect.try({
      try: () =>
        buildTransferTxWithMinFee({
          senderAddress,
          destinationAddress: config.l2Address,
          signer,
          availableUtxos,
          requestedAssets,
          networkId: config.networkId,
          minFeeA: nodeConfig.MIN_FEE_A,
          minFeeB: nodeConfig.MIN_FEE_B,
        }),
      catch: (cause) => toError(cause, "Failed to build Midgard-native transfer"),
    });
    const submitResult =
      config.submissionMode === "local"
        ? yield* submitNativeTransferLocally(built)
        : yield* submitNativeTransferTx(
            config.nodeEndpoint,
            built.txHex,
            built.txIdHex,
          );

    return {
      txId: submitResult.txId,
      status: submitResult.status,
      senderAddress,
      destinationAddress: config.l2Address,
      selectedInputs: built.selectedInputs.map(outRefLabel),
      requestedAssets: built.requestedAssets,
      changeAssets: built.changeAssets,
      walletSeedSource: resolvedWalletSeedPhrase.resolvedFrom,
      nodeEndpoint: config.nodeEndpoint,
    };
  });

/**
 * Formats the transfer result as stable JSON for CLI output.
 */
export const formatSubmitL2TransferResult = (
  result: SubmitL2TransferResult,
): string => serializeJson(result);
