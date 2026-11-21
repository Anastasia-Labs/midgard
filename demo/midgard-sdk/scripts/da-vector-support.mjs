/**
 * Shared support for the three DA vector generators:
 *
 *   * `generate-da-attestation-capacity-v1-fixture.mjs`, the signed commitment
 *     block of `onchain/aiken/validators/da_attestation_capacity.test.ak`;
 *   * `generate-da-commitment-v1-goldens.mjs`, the `CommitmentV1` /
 *     `ChallengeRecordV1` cross-language goldens; and
 *   * `generate-da-bond-pool-v1-goldens.mjs`, the pooled DA bond datum and
 *     redeemers, `StateQueueStatusV1` and `ParametersV1` goldens.
 *
 * All three need the bytes Aiken's `builtin.serialise_data` produces for the DA
 * types, so all three use the one encoder below. It is deliberately small and
 * refuses every shape it has not been proved on against the Aiken builtin
 * (integers outside the signed 64-bit range, byte strings longer than 64 bytes,
 * constructor indices above 6, maps). A vector that needs one of those has to
 * extend the encoder and prove the extension first.
 *
 * It does not use the SDK codec on purpose: the generators must run without a
 * built `dist/`, and they pin the Aiken types directly rather than the SDK's
 * reading of them.
 */

import { createHash } from "node:crypto";

import { blake2b } from "@noble/hashes/blake2.js";

// ---------------------------------------------------------------------------
// Plutus Data values
// ---------------------------------------------------------------------------

/**
 * A Plutus Data value is one of: a `bigint` (Int), a `Uint8Array` (Bytes), an
 * array (List) or `{ constructor, fields }` (Constr).
 */
export const constr = (constructor, fields) => ({ constructor, fields });

const INT64_MIN = -(1n << 63n);
const INT64_LIMIT = 1n << 63n;
const MAX_BYTES_LENGTH = 64;

/** A CBOR head: major type plus its argument in the shortest form. */
const head = (majorType, argument) => {
  const major = majorType << 5;
  if (argument < 24n) {
    return [major | Number(argument)];
  }
  const widths = [
    [1n << 8n, 24, 1],
    [1n << 16n, 25, 2],
    [1n << 32n, 26, 4],
    [1n << 64n, 27, 8],
  ];
  for (const [limit, additional, width] of widths) {
    if (argument < limit) {
      const out = [major | additional];
      for (let shift = BigInt((width - 1) * 8); shift >= 0n; shift -= 8n) {
        out.push(Number((argument >> shift) & 0xffn));
      }
      return out;
    }
  }
  throw new Error(`CBOR argument ${argument} does not fit in 64 bits`);
};

const encodeList = (items, out) => {
  if (items.length === 0) {
    out.push(0x80);
    return;
  }
  out.push(0x9f);
  for (const item of items) {
    encodeData(item, out);
  }
  out.push(0xff);
};

const encodeData = (value, out) => {
  if (typeof value === "bigint") {
    if (value < INT64_MIN || value >= INT64_LIMIT) {
      throw new Error(
        `integer ${value} is outside the proved signed 64-bit range`,
      );
    }
    out.push(...(value >= 0n ? head(0, value) : head(1, -1n - value)));
    return;
  }
  if (value instanceof Uint8Array) {
    if (value.length > MAX_BYTES_LENGTH) {
      throw new Error(
        `byte string of ${value.length} bytes exceeds the proved ${MAX_BYTES_LENGTH}-byte form`,
      );
    }
    out.push(...head(2, BigInt(value.length)), ...value);
    return;
  }
  if (Array.isArray(value)) {
    encodeList(value, out);
    return;
  }
  if (
    value !== null &&
    typeof value === "object" &&
    Number.isInteger(value.constructor) &&
    Array.isArray(value.fields)
  ) {
    if (value.constructor < 0 || value.constructor > 6) {
      throw new Error(
        `constructor ${value.constructor} is outside the proved compact tags 121..127`,
      );
    }
    out.push(...head(6, BigInt(121 + value.constructor)));
    encodeList(value.fields, out);
    return;
  }
  throw new Error(`unsupported Plutus Data value: ${String(value)}`);
};

/** `builtin.serialise_data`: indefinite non-empty lists, `80` for empty ones. */
export const serialiseData = (value) => {
  const out = [];
  encodeData(value, out);
  return Uint8Array.from(out);
};

// ---------------------------------------------------------------------------
// Bytes and hashes
// ---------------------------------------------------------------------------

export const toHex = (value) => Buffer.from(value).toString("hex");

export const fromHex = (value) => {
  if (!/^(?:[0-9a-f]{2})*$/u.test(value)) {
    throw new Error(`not lowercase even-length hex: ${value}`);
  }
  return Uint8Array.from(Buffer.from(value, "hex"));
};

export const concatBytes = (...parts) =>
  Uint8Array.from(Buffer.concat(parts.map((part) => Buffer.from(part))));

export const utf8 = (text) => Uint8Array.from(Buffer.from(text, "utf8"));

export const blake2b256 = (value) => blake2b(value, { dkLen: 32 });

export const blake2b224 = (value) => blake2b(value, { dkLen: 28 });

export const sha256 = (value) =>
  Uint8Array.from(createHash("sha256").update(value).digest());

// ---------------------------------------------------------------------------
// The DA types (`lib/midgard/availability-challenge.ak`)
// ---------------------------------------------------------------------------

export const ATTESTATION_MESSAGE_DOMAIN_V1 =
  "MidgardDaAvailabilityAttestationV1";

export const CHALLENGE_ASSET_NAME_PREFIX_V1 = "DACH";

const requireBytes = (label, value, length) => {
  if (!(value instanceof Uint8Array) || value.length !== length) {
    throw new Error(`${label} must be ${length} bytes`);
  }
  return value;
};

const requireInt = (label, value) => {
  if (typeof value !== "bigint") {
    throw new Error(`${label} must be a bigint`);
  }
  return value;
};

/** `ResponseGeometryV1`. */
export const responseGeometryV1Data = (geometry) =>
  constr(0, [
    requireInt("chunkByteLength", geometry.chunkByteLength),
    requireInt("trancheByteLength", geometry.trancheByteLength),
    requireInt("maxTrancheCount", geometry.maxTrancheCount),
  ]);

/** `TrancheDescriptorV1`. */
export const trancheDescriptorV1Data = (descriptor) =>
  constr(0, [
    requireInt("trancheIndex", descriptor.trancheIndex),
    requireInt("startOffset", descriptor.startOffset),
    requireInt("byteLength", descriptor.byteLength),
    requireInt("chunkCount", descriptor.chunkCount),
    requireBytes("chunkCommitment", descriptor.chunkCommitment, 32),
    requireBytes("terminalAccumulator", descriptor.terminalAccumulator, 32),
  ]);

/** `CommitmentV1` (post-#688: no bond-owner credential). */
export const commitmentV1Data = (commitment) =>
  constr(0, [
    requireInt("version", commitment.version),
    requireBytes("deploymentIdentity", commitment.deploymentIdentity, 28),
    requireBytes("headerHash", commitment.headerHash, 28),
    requireInt("payloadByteLength", commitment.payloadByteLength),
    responseGeometryV1Data(commitment.responseGeometry),
    commitment.trancheDescriptors.map(trancheDescriptorV1Data),
  ]);

/** `ChallengeRecordV1`. */
export const challengeRecordV1Data = (record) =>
  constr(0, [
    commitmentV1Data(record.commitment),
    requireBytes("challengeAssetName", record.challengeAssetName, 32),
    requireBytes("challenger", record.challenger, 28),
    requireInt("openedAt", record.openedAt),
    requireInt("responseDeadline", record.responseDeadline),
  ]);

/** `StateQueueStatusV1`, keyed by `kind` in constructor order. */
export const stateQueueStatusV1Data = (status) => {
  switch (status.kind) {
    case "Unattested":
      return constr(0, []);
    case "Attested":
      return constr(1, [
        requireBytes("commitmentHash", status.commitmentHash, 32),
      ]);
    case "Challenged":
      return constr(2, [
        requireBytes("commitmentHash", status.commitmentHash, 32),
        requireBytes("challengeAssetName", status.challengeAssetName, 32),
      ]);
    case "Published":
      return constr(3, [
        requireBytes("terminalCommitment", status.terminalCommitment, 32),
      ]);
    default:
      throw new Error(`unknown StateQueueStatusV1 arm ${String(status.kind)}`);
  }
};

/** `ParametersV1`, in its constructor field order. */
export const parametersV1Data = (parameters) =>
  constr(0, [
    responseGeometryV1Data(parameters.responseGeometry),
    ...[
      "daBondLovelace",
      "challengerBondLovelace",
      "maxOpenFeeLovelace",
      "maxPublicationFeeLovelace",
      "maxSettlementFeeLovelace",
      "maxCloseFeeLovelace",
      "maxTimeoutFeeLovelace",
      "daSlashPenaltyLovelace",
      "daBondMinTopUpLovelace",
      "daBondPoolFloorLovelace",
      "challengeRecordLovelace",
    ].map((key) => requireInt(key, parameters[key])),
  ]);

// ---------------------------------------------------------------------------
// The pooled DA bond types (`lib/midgard/da-bond-pool.ak`)
// ---------------------------------------------------------------------------

/** `DaBondPoolDatum`: `Bonded` or `Withdrawing { unlock_at }`. */
export const daBondPoolDatumData = (datum) => {
  switch (datum.kind) {
    case "Bonded":
      return constr(0, []);
    case "Withdrawing":
      return constr(1, [requireInt("unlockAt", datum.unlockAt)]);
    default:
      throw new Error(`unknown DaBondPoolDatum arm ${String(datum.kind)}`);
  }
};

/** The pool `MintRedeemer`: its one constructor `InitPool { output_index }`. */
export const daBondPoolMintRedeemerData = (redeemer) =>
  constr(0, [requireInt("outputIndex", redeemer.outputIndex)]);

/** Field names of each pool `SpendRedeemer` arm, in constructor order. */
export const DA_BOND_POOL_SPEND_REDEEMER_ARMS = Object.freeze([
  ["TopUp", ["outputIndex"]],
  [
    "Slash",
    [
      "hubOracleRefInputIndex",
      "stateQueueMintRedeemerIndex",
      "correctionLockInputIndex",
      "outputIndex",
    ],
  ],
  ["BeginWithdraw", ["daParamsRefInputIndex", "outputIndex"]],
  ["CancelWithdraw", ["daParamsRefInputIndex", "outputIndex"]],
  ["CompleteWithdraw", ["amount", "daParamsRefInputIndex", "outputIndex"]],
]);

/** The pool `SpendRedeemer`, keyed by `kind`. */
export const daBondPoolSpendRedeemerData = (redeemer) => {
  const index = DA_BOND_POOL_SPEND_REDEEMER_ARMS.findIndex(
    ([kind]) => kind === redeemer.kind,
  );
  if (index < 0) {
    throw new Error(`unknown pool SpendRedeemer arm ${String(redeemer.kind)}`);
  }
  const [, fields] = DA_BOND_POOL_SPEND_REDEEMER_ARMS[index];
  return constr(
    index,
    fields.map((field) => requireInt(field, redeemer[field])),
  );
};

/** `cardano/transaction.OutputReference`. */
export const outputReferenceData = (outputReference) =>
  constr(0, [
    requireBytes("transactionId", outputReference.transactionId, 32),
    requireInt("outputIndex", outputReference.outputIndex),
  ]);

/** `commitment_hash_v1`: blake2b_256(serialise_data(commitment)), untagged. */
export const commitmentHashV1 = (commitment) =>
  blake2b256(serialiseData(commitmentV1Data(commitment)));

/** `attestation_message_v1` over any commitment Data value. */
export const attestationMessageForData = (commitmentData) =>
  blake2b256(
    concatBytes(
      utf8(ATTESTATION_MESSAGE_DOMAIN_V1),
      serialiseData(commitmentData),
    ),
  );

export const attestationMessageV1 = (commitment) =>
  attestationMessageForData(commitmentV1Data(commitment));

/** `challenge_asset_name_v1`: "DACH" ++ blake2b_224(serialise_data(outref)). */
export const challengeAssetNameV1 = (outputReference) =>
  concatBytes(
    utf8(CHALLENGE_ASSET_NAME_PREFIX_V1),
    blake2b224(serialiseData(outputReferenceData(outputReference))),
  );

const ceilDiv = (numerator, denominator) =>
  (numerator + denominator - 1n) / denominator;

/**
 * The canonical descriptors of a payload (`descriptors_are_canonical_v1`):
 * contiguous tranches of `trancheByteLength` bytes, the last one short, each
 * with `ceil(length / chunkByteLength)` chunks. `hashesFor(index)` supplies the
 * two 32-byte commitments of each tranche.
 */
export const canonicalTrancheDescriptors = ({
  payloadByteLength,
  trancheByteLength,
  chunkByteLength,
  hashesFor,
}) => {
  const descriptors = [];
  for (
    let index = 0n, offset = 0n;
    offset < payloadByteLength;
    index += 1n, offset += trancheByteLength
  ) {
    const remaining = payloadByteLength - offset;
    const byteLength =
      remaining < trancheByteLength ? remaining : trancheByteLength;
    const { chunkCommitment, terminalAccumulator } = hashesFor(index);
    descriptors.push({
      trancheIndex: index,
      startOffset: offset,
      byteLength,
      chunkCount: ceilDiv(byteLength, chunkByteLength),
      chunkCommitment,
      terminalAccumulator,
    });
  }
  return descriptors;
};

/**
 * The parameter-independent half of `commitment_is_canonical_v1`: field
 * widths, a known payload size class and canonical descriptors. Throws on the
 * first violation so a generator can never emit a vector the on-chain side
 * would refuse as non-canonical.
 */
export const assertCommitmentCanonical = (commitment) => {
  const smallMax = 64n * 1024n;
  const fullMax = 64n * 1024n * 1024n;
  const { payloadByteLength, responseGeometry } = commitment;
  if (commitment.version !== 1n) {
    throw new Error("commitment version must be 1");
  }
  requireBytes("deploymentIdentity", commitment.deploymentIdentity, 28);
  requireBytes("headerHash", commitment.headerHash, 28);
  if (payloadByteLength <= 0n || payloadByteLength > fullMax) {
    throw new Error("payload byte length has no response window");
  }
  if (
    responseGeometry.chunkByteLength <= 0n ||
    responseGeometry.chunkByteLength > 15_148n ||
    responseGeometry.trancheByteLength < smallMax ||
    responseGeometry.trancheByteLength > fullMax ||
    responseGeometry.maxTrancheCount <= 0n ||
    responseGeometry.maxTrancheCount > 64n ||
    ceilDiv(fullMax, responseGeometry.trancheByteLength) >
      responseGeometry.maxTrancheCount
  ) {
    throw new Error("response geometry is not canonical");
  }
  const expected = canonicalTrancheDescriptors({
    payloadByteLength,
    trancheByteLength: responseGeometry.trancheByteLength,
    chunkByteLength: responseGeometry.chunkByteLength,
    hashesFor: (index) => commitment.trancheDescriptors[Number(index)] ?? {},
  });
  if (
    BigInt(commitment.trancheDescriptors.length) >
      responseGeometry.maxTrancheCount ||
    commitment.trancheDescriptors.length !== expected.length
  ) {
    throw new Error("tranche descriptor count is not canonical");
  }
  commitment.trancheDescriptors.forEach((descriptor, position) => {
    const want = expected[position];
    for (const key of [
      "trancheIndex",
      "startOffset",
      "byteLength",
      "chunkCount",
    ]) {
      if (descriptor[key] !== want[key]) {
        throw new Error(`descriptor ${position} ${key} is not canonical`);
      }
    }
    requireBytes("chunkCommitment", descriptor.chunkCommitment, 32);
    requireBytes("terminalAccumulator", descriptor.terminalAccumulator, 32);
  });
};

// ---------------------------------------------------------------------------
// Aiken rendering
// ---------------------------------------------------------------------------

export const aikenBytesLiteral = (value) => `#"${toHex(value)}"`;

/** An Aiken integer literal with `_` thousands separators, as the tree writes them. */
export const aikenIntLiteral = (value) => {
  const digits = (value < 0n ? -value : value).toString();
  const grouped = digits.replace(/\B(?=(?:\d{3})+$)/gu, "_");
  return value < 0n ? `-${grouped}` : grouped;
};

// ---------------------------------------------------------------------------
// Generator plumbing
// ---------------------------------------------------------------------------

/**
 * Parses `[--check] [--print] [--out <path>]` style arguments. `options` maps
 * each accepted flag to whether it takes a value. Unknown flags are a usage
 * error: a generator that ignored `--chek` in CI would silently rewrite.
 */
export const parseGeneratorArguments = (usage, options) => {
  const parsed = {};
  const commandArguments = process.argv.slice(2);
  for (let index = 0; index < commandArguments.length; index += 1) {
    const flag = commandArguments[index];
    if (!(flag in options) || flag in parsed) {
      console.error(usage);
      process.exit(2);
    }
    if (options[flag]) {
      const value = commandArguments[index + 1];
      if (value === undefined || value.startsWith("--")) {
        console.error(usage);
        process.exit(2);
      }
      parsed[flag] = value;
      index += 1;
    } else {
      parsed[flag] = true;
    }
  }
  return parsed;
};
