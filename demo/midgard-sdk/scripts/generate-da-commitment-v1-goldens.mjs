#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the DA availability
 * commitment types of `onchain/aiken/lib/midgard/availability-challenge.ak`:
 * the `CommitmentV1` bytes, `commitment_hash_v1`, `attestation_message_v1`, the
 * `ChallengeRecordV1` bytes and `challenge_asset_name_v1`.
 *
 * **Why this channel exists.** The commitment hash is what a state-queue node's
 * `Attested` / `Challenged` status carries, and Open accepts a challenge record
 * only when the hash of its commitment equals that status. The TypeScript
 * builders compute the same hash off-chain. An encoding difference between the
 * two sides (a field order, an integer width, a definite- versus
 * indefinite-length list) would let the node attest a commitment that no
 * challenger can open against, and nothing but a pinned byte vector on both
 * sides would notice.
 *
 * **The vectors.** One per tranche-count class:
 *
 *   * `tranches_1`: a small payload, one short tranche, deployed geometry;
 *   * `tranches_16`: a 64 MiB payload in the deployed 4 MiB tranches;
 *   * `tranches_64`: a 64 MiB payload in 1 MiB tranches with 16-byte chunks,
 *     the widest encoding a canonical commitment admits (every chunk count and
 *     every offset past the first takes a 5-byte CBOR integer, both times a
 *     9-byte one). Its record is the shape `challenge_record_lovelace` is
 *     measured on.
 *
 * Every descriptor list is canonical (contiguous, exact lengths, 32-byte
 * hashes); the generator refuses to emit one that is not.
 *
 * Two artifacts:
 *
 *   * `demo/midgard-sdk/tests/fixtures/da-commitment-v1.generated.json`, the
 *     one place an SDK test is to read the vectors from. The SDK side is
 *     `demo/midgard-sdk/tests/da-golden-vectors.test.ts` (run by the SDK test
 *     step), which reproduces every hex with the SDK codecs; `--check` compares
 *     only this fixture with this generator's encoder and the Aiken module;
 *     and
 *   * `onchain/aiken/lib/midgard/availability-challenge-commitment-v1-golden.test.ak`,
 *     which rebuilds each value as an Aiken literal and asserts every byte
 *     string, one named test per field and vector.
 *
 * Vectors are regenerated, never hand-edited. The generator needs no built
 * `dist/`: it encodes with `da-vector-support.mjs`, not the SDK codec.
 *
 * usage: node scripts/generate-da-commitment-v1-goldens.mjs
 *          [--check] [--aiken-out <path>] [--json-out <path>]
 *
 *   --check              compare both artifacts; exit 1 on any difference
 *   --aiken-out <path>   use <path> instead of the checked-in Aiken module
 *   --json-out <path>    use <path> instead of the checked-in JSON fixture
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  formatAikenSource,
  goldenChannelEmitter,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

import {
  aikenBytesLiteral,
  aikenIntLiteral,
  assertCommitmentCanonical,
  ATTESTATION_MESSAGE_DOMAIN_V1,
  attestationMessageV1,
  blake2b224,
  blake2b256,
  canonicalTrancheDescriptors,
  CHALLENGE_ASSET_NAME_PREFIX_V1,
  challengeAssetNameV1,
  challengeRecordV1Data,
  commitmentHashV1,
  commitmentV1Data,
  parseGeneratorArguments,
  serialiseData,
  toHex,
  utf8,
} from "./da-vector-support.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");

export const GENERATOR_PATH =
  "demo/midgard-sdk/scripts/generate-da-commitment-v1-goldens.mjs";

export const JSON_PATH = join(
  packageRoot,
  "tests/fixtures/da-commitment-v1.generated.json",
);

export const AIKEN_PATH = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/availability-challenge-commitment-v1-golden.test.ak",
);

const MiB = 1024n * 1024n;

/** A labelled, deterministic stand-in for a hash the vectors need. */
const seeded = (label, suffix, length) =>
  (length === 28 ? blake2b224 : blake2b256)(
    utf8(`midgard-da-commitment-v1-golden/${label}/${suffix}`),
  );

/** The deployed geometry (`DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE`). */
const DEPLOYED_GEOMETRY = Object.freeze({
  chunkByteLength: 14_020n,
  trancheByteLength: 4n * MiB,
  maxTrancheCount: 16n,
});

const VECTOR_SHAPES = [
  {
    label: "tranches_1",
    note: "small payload: one short tranche in the deployed geometry",
    payloadByteLength: 40_000n,
    responseGeometry: DEPLOYED_GEOMETRY,
    outputIndex: 0n,
    openedAt: 1_790_000_000_000n,
    responseWindow: 720_000n,
  },
  {
    label: "tranches_16",
    note: "64 MiB payload in the deployed 4 MiB tranches",
    payloadByteLength: 64n * MiB,
    responseGeometry: DEPLOYED_GEOMETRY,
    outputIndex: 24n,
    openedAt: 1_790_000_600_000n,
    responseWindow: 840_000n,
  },
  {
    label: "tranches_64",
    note: "worst-case widths: 64 MiB payload, 1 MiB tranches, 16-byte chunks; the challenge_record_lovelace measurement shape",
    payloadByteLength: 64n * MiB,
    responseGeometry: {
      chunkByteLength: 16n,
      trancheByteLength: MiB,
      maxTrancheCount: 64n,
    },
    outputIndex: 300n,
    openedAt: 1_790_001_200_000n,
    responseWindow: 259_200_000n,
  },
];

const buildVector = (shape) => {
  const { label } = shape;
  const commitment = {
    version: 1n,
    deploymentIdentity: seeded("shared", "deployment-identity", 28),
    headerHash: seeded(label, "header-hash", 28),
    payloadByteLength: shape.payloadByteLength,
    responseGeometry: shape.responseGeometry,
    trancheDescriptors: canonicalTrancheDescriptors({
      payloadByteLength: shape.payloadByteLength,
      trancheByteLength: shape.responseGeometry.trancheByteLength,
      chunkByteLength: shape.responseGeometry.chunkByteLength,
      hashesFor: (index) => ({
        chunkCommitment: seeded(label, `chunk-commitment/${index}`, 32),
        terminalAccumulator: seeded(label, `terminal-accumulator/${index}`, 32),
      }),
    }),
  };
  assertCommitmentCanonical(commitment);
  const outputReference = {
    transactionId: seeded(label, "challenger-input", 32),
    outputIndex: shape.outputIndex,
  };
  const challengeAssetName = challengeAssetNameV1(outputReference);
  const record = {
    commitment,
    challengeAssetName,
    challenger: seeded(label, "challenger", 28),
    openedAt: shape.openedAt,
    responseDeadline: shape.openedAt + shape.responseWindow,
  };
  const commitmentCbor = serialiseData(commitmentV1Data(commitment));
  const recordCbor = serialiseData(challengeRecordV1Data(record));
  return {
    shape,
    commitment,
    outputReference,
    record,
    commitmentCbor,
    commitmentHash: commitmentHashV1(commitment),
    attestationMessage: attestationMessageV1(commitment),
    recordCbor,
    challengeAssetName,
  };
};

// ---------------------------------------------------------------------------
// JSON
// ---------------------------------------------------------------------------

const jsonInt = (value) => {
  const number = Number(value);
  if (!Number.isSafeInteger(number)) {
    throw new Error(`${value} is not a safe JSON integer`);
  }
  return number;
};

const commitmentJson = (commitment) => ({
  version: jsonInt(commitment.version),
  deploymentIdentity: toHex(commitment.deploymentIdentity),
  headerHash: toHex(commitment.headerHash),
  payloadByteLength: jsonInt(commitment.payloadByteLength),
  responseGeometry: {
    chunkByteLength: jsonInt(commitment.responseGeometry.chunkByteLength),
    trancheByteLength: jsonInt(commitment.responseGeometry.trancheByteLength),
    maxTrancheCount: jsonInt(commitment.responseGeometry.maxTrancheCount),
  },
  trancheDescriptors: commitment.trancheDescriptors.map((descriptor) => ({
    trancheIndex: jsonInt(descriptor.trancheIndex),
    startOffset: jsonInt(descriptor.startOffset),
    byteLength: jsonInt(descriptor.byteLength),
    chunkCount: jsonInt(descriptor.chunkCount),
    chunkCommitment: toHex(descriptor.chunkCommitment),
    terminalAccumulator: toHex(descriptor.terminalAccumulator),
  })),
});

const buildJson = (vectors) => ({
  schema: "midgard-da-commitment-v1-goldens",
  generator: GENERATOR_PATH,
  aikenModule:
    "onchain/aiken/lib/midgard/availability-challenge-commitment-v1-golden.test.ak",
  attestationMessageDomain: ATTESTATION_MESSAGE_DOMAIN_V1,
  challengeAssetNamePrefixHex: toHex(utf8(CHALLENGE_ASSET_NAME_PREFIX_V1)),
  vectors: vectors.map((vector) => ({
    label: vector.shape.label,
    note: vector.shape.note,
    trancheCount: vector.commitment.trancheDescriptors.length,
    commitment: commitmentJson(vector.commitment),
    commitmentCborHex: toHex(vector.commitmentCbor),
    commitmentCborByteLength: vector.commitmentCbor.length,
    commitmentHashHex: toHex(vector.commitmentHash),
    attestationMessageHex: toHex(vector.attestationMessage),
    challengeRecord: {
      challengeAssetName: toHex(vector.record.challengeAssetName),
      challenger: toHex(vector.record.challenger),
      openedAt: jsonInt(vector.record.openedAt),
      responseDeadline: jsonInt(vector.record.responseDeadline),
    },
    challengeRecordCborHex: toHex(vector.recordCbor),
    challengeRecordCborByteLength: vector.recordCbor.length,
    outputReference: {
      transactionId: toHex(vector.outputReference.transactionId),
      outputIndex: jsonInt(vector.outputReference.outputIndex),
    },
    challengeAssetNameHex: toHex(vector.challengeAssetName),
  })),
});

// ---------------------------------------------------------------------------
// Aiken
// ---------------------------------------------------------------------------

const renderCommitment = (commitment) => {
  const geometry = commitment.responseGeometry;
  return [
    "availability.CommitmentV1 {",
    `  version: ${aikenIntLiteral(commitment.version)},`,
    `  deployment_identity: ${aikenBytesLiteral(commitment.deploymentIdentity)},`,
    `  header_hash: ${aikenBytesLiteral(commitment.headerHash)},`,
    `  payload_byte_length: ${aikenIntLiteral(commitment.payloadByteLength)},`,
    "  response_geometry: availability.ResponseGeometryV1 {",
    `    chunk_byte_length: ${aikenIntLiteral(geometry.chunkByteLength)},`,
    `    tranche_byte_length: ${aikenIntLiteral(geometry.trancheByteLength)},`,
    `    max_tranche_count: ${aikenIntLiteral(geometry.maxTrancheCount)},`,
    "  },",
    "  tranche_descriptors: [",
    ...commitment.trancheDescriptors.flatMap((descriptor) => [
      "    availability.TrancheDescriptorV1 {",
      `      tranche_index: ${aikenIntLiteral(descriptor.trancheIndex)},`,
      `      start_offset: ${aikenIntLiteral(descriptor.startOffset)},`,
      `      byte_length: ${aikenIntLiteral(descriptor.byteLength)},`,
      `      chunk_count: ${aikenIntLiteral(descriptor.chunkCount)},`,
      `      chunk_commitment: ${aikenBytesLiteral(descriptor.chunkCommitment)},`,
      `      terminal_accumulator: ${aikenBytesLiteral(descriptor.terminalAccumulator)},`,
      "    },",
    ]),
    "  ],",
    "}",
  ];
};

const indent = (lines, by) => lines.map((line) => `${" ".repeat(by)}${line}`);

const renderVector = (vector) => {
  const { label, note } = vector.shape;
  const prefix = `da_commitment_v1_golden_${label}`;
  const { record, outputReference } = vector;
  return [
    `// ${label}: ${note}.`,
    `fn ${label}_commitment() -> availability.CommitmentV1 {`,
    ...indent(renderCommitment(vector.commitment), 2),
    "}",
    "",
    `fn ${label}_record() -> availability.ChallengeRecordV1 {`,
    "  availability.ChallengeRecordV1 {",
    `    commitment: ${label}_commitment(),`,
    `    challenge_asset_name: ${aikenBytesLiteral(record.challengeAssetName)},`,
    `    challenger: ${aikenBytesLiteral(record.challenger)},`,
    `    opened_at: ${aikenIntLiteral(record.openedAt)},`,
    `    response_deadline: ${aikenIntLiteral(record.responseDeadline)},`,
    "  }",
    "}",
    "",
    `const ${label}_output_reference: OutputReference =`,
    "  OutputReference {",
    `    transaction_id: ${aikenBytesLiteral(outputReference.transactionId)},`,
    `    output_index: ${aikenIntLiteral(outputReference.outputIndex)},`,
    "  }",
    "",
    `test ${prefix}_commitment_cbor() {`,
    `  let commitment: Data = ${label}_commitment()`,
    `  builtin.serialise_data(commitment) == ${aikenBytesLiteral(vector.commitmentCbor)}`,
    "}",
    "",
    `test ${prefix}_commitment_hash() {`,
    `  availability.commitment_hash_v1(${label}_commitment()) == ${aikenBytesLiteral(vector.commitmentHash)}`,
    "}",
    "",
    `test ${prefix}_attestation_message() {`,
    `  availability.attestation_message_v1(${label}_commitment()) == ${aikenBytesLiteral(vector.attestationMessage)}`,
    "}",
    "",
    `test ${prefix}_record_cbor() {`,
    `  let record: Data = ${label}_record()`,
    `  builtin.serialise_data(record) == ${aikenBytesLiteral(vector.recordCbor)}`,
    "}",
    "",
    `test ${prefix}_challenge_asset_name() {`,
    `  availability.challenge_asset_name_v1(${label}_output_reference) == ${aikenBytesLiteral(vector.challengeAssetName)}`,
    "}",
    "",
  ];
};

const renderAiken = (vectors) =>
  [
    "//// Generated by",
    `//// ${GENERATOR_PATH}.`,
    "//// Do not edit; regenerate with",
    "//// `pnpm --dir demo/midgard-sdk fixtures:da-commitment-v1:sync`.",
    "////",
    "//// Cross-language vectors for the DA commitment types. The TypeScript twin",
    "//// reads the same values from",
    "//// demo/midgard-sdk/tests/fixtures/da-commitment-v1.generated.json. Each",
    "//// vector rebuilds a `CommitmentV1`, its `ChallengeRecordV1` and the",
    "//// challenger input's `OutputReference` as literals and pins, in its own",
    "//// named test, the commitment bytes, `commitment_hash_v1`,",
    "//// `attestation_message_v1`, the record bytes and `challenge_asset_name_v1`.",
    "",
    "use aiken/builtin",
    "use cardano/transaction.{OutputReference}",
    "use midgard/availability_challenge as availability",
    "",
    ...vectors.flatMap(renderVector),
  ].join("\n");

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

export const buildVectors = () => VECTOR_SHAPES.map(buildVector);

export const renderGoldenJson = (vectors) =>
  `${JSON.stringify(buildJson(vectors), null, 2)}\n`;

export const renderGoldenAiken = (vectors) =>
  formatAikenSource({
    source: renderAiken(vectors),
    fileName: "availability-challenge-commitment-v1-golden.test.ak",
    repositoryRoot,
    tmpPrefix: "midgard-da-commitment-v1-aiken-format-",
  });

const main = () => {
  const options = parseGeneratorArguments(
    "usage: node scripts/generate-da-commitment-v1-goldens.mjs [--check] [--aiken-out <path>] [--json-out <path>]",
    { "--check": false, "--aiken-out": true, "--json-out": true },
  );
  const writeOrCheck = goldenChannelEmitter({
    repositoryRoot,
    checkOnly: options["--check"] === true,
  });
  const vectors = buildVectors();
  writeOrCheck(
    options["--json-out"] ? resolve(options["--json-out"]) : JSON_PATH,
    renderGoldenJson(vectors),
  );
  writeOrCheck(
    options["--aiken-out"] ? resolve(options["--aiken-out"]) : AIKEN_PATH,
    renderGoldenAiken(vectors),
  );
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  main();
}
