#!/usr/bin/env node

/**
 * Regenerates the signed commitment block of
 * `onchain/aiken/validators/da_attestation_capacity.test.ak`.
 *
 * That module exercises `validate_add_signatures` with up to 256 committee
 * signatures over `availability.attestation_message_v1` of one fixed
 * commitment. The signatures are 256 × 65 bytes of hex that nobody can check by
 * eye, and every change to `CommitmentV1` (a field added or removed, a field
 * width) changes the signed message and so every signature. They are therefore
 * never hand-edited: this generator owns the whole block between the
 * `BEGIN GENERATED` and `END GENERATED` markers — the commitment fixture, the
 * constants it reads, the packed verification keys, the signature witnesses and
 * a test pinning the message — so a hand edit to the fixture cannot drift from
 * its signatures without `--check` saying so.
 *
 * The keys are deterministic: seed `i` is
 * `sha256("midgard-da-capacity-regression-v1:" ++ u32be(i))`, the verification
 * keys are packed in ascending byte order, and witness `j` is the byte `j`
 * followed by the Ed25519 signature of the `j`-th packed key.
 *
 * usage: node scripts/generate-da-attestation-capacity-v1-fixture.mjs
 *          [--check] [--print] [--file <path>]
 *
 *   (none)         splice the block into the module and write it
 *   --check        exit 1 when the module's block differs from what is generated
 *   --print        write the block to stdout; touch nothing
 *   --file <path>  operate on <path> instead of the checked-in module
 */

import {
  createPrivateKey,
  createPublicKey,
  sign as signMessage,
  verify as verifyMessage,
} from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, isAbsolute, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  aikenBytesLiteral,
  aikenIntLiteral,
  attestationMessageForData,
  commitmentV1Data,
  concatBytes,
  parseGeneratorArguments,
  sha256,
  toHex,
  utf8,
} from "./da-vector-support.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../../..");

export const CAPACITY_MODULE_PATH = join(
  repositoryRoot,
  "onchain/aiken/validators/da_attestation_capacity.test.ak",
);

export const GENERATOR_PATH =
  "demo/midgard-sdk/scripts/generate-da-attestation-capacity-v1-fixture.mjs";

export const BEGIN_MARKER =
  "// BEGIN GENERATED: da-attestation-capacity-v1 (do not edit by hand)";

export const END_MARKER = "// END GENERATED: da-attestation-capacity-v1";

export const CAPACITY_SEED_DOMAIN = "midgard-da-capacity-regression-v1:";

export const CAPACITY_SIGNER_COUNT = 256;

/** Verification keys per `capacity_remaining_signers_<n>` constant. */
export const SIGNERS_PER_CONSTANT = 16;

/** Signature witnesses per `capacity_signatures_<n>` constant. */
export const WITNESSES_PER_CONSTANT = 8;

// ---------------------------------------------------------------------------
// The commitment the committee signs
// ---------------------------------------------------------------------------

export const CAPACITY_DA_POLICY = Uint8Array.from(Buffer.alloc(28, 0xaa));

export const CAPACITY_HEADER_HASH = Uint8Array.from(Buffer.alloc(28, 0x11));

/**
 * The fixture commitment. Every numeric field carries its value and the Aiken
 * expression the module writes for it, so the rendered function and the signed
 * bytes come from the same entry; the generated message test catches a pair
 * that disagrees.
 */
export const CAPACITY_COMMITMENT = Object.freeze({
  version: { value: 1n, aiken: "availability.commitment_version_v1" },
  payloadByteLength: { value: 1n, aiken: "1" },
  responseGeometry: {
    chunkByteLength: { value: 4_096n, aiken: "4_096" },
    trancheByteLength: {
      value: 4n * 1024n * 1024n,
      aiken: "4 * 1024 * 1024",
    },
    maxTrancheCount: { value: 16n, aiken: "16" },
  },
  trancheDescriptor: {
    trancheIndex: { value: 0n, aiken: "0" },
    startOffset: { value: 0n, aiken: "0" },
    byteLength: { value: 1n, aiken: "1" },
    chunkCount: { value: 1n, aiken: "1" },
    chunkCommitment: Uint8Array.from(Buffer.alloc(32, 0xac)),
    terminalAccumulator: Uint8Array.from(Buffer.alloc(32, 0xab)),
  },
});

/** The fixture as the plain values `da-vector-support.mjs` encodes. */
export const capacityCommitmentValues = () => {
  const { responseGeometry: geometry, trancheDescriptor: descriptor } =
    CAPACITY_COMMITMENT;
  return {
    version: CAPACITY_COMMITMENT.version.value,
    deploymentIdentity: CAPACITY_DA_POLICY,
    headerHash: CAPACITY_HEADER_HASH,
    payloadByteLength: CAPACITY_COMMITMENT.payloadByteLength.value,
    responseGeometry: {
      chunkByteLength: geometry.chunkByteLength.value,
      trancheByteLength: geometry.trancheByteLength.value,
      maxTrancheCount: geometry.maxTrancheCount.value,
    },
    trancheDescriptors: [
      {
        trancheIndex: descriptor.trancheIndex.value,
        startOffset: descriptor.startOffset.value,
        byteLength: descriptor.byteLength.value,
        chunkCount: descriptor.chunkCount.value,
        chunkCommitment: descriptor.chunkCommitment,
        terminalAccumulator: descriptor.terminalAccumulator,
      },
    ],
  };
};

export const capacityAttestationMessage = () =>
  attestationMessageForData(commitmentV1Data(capacityCommitmentValues()));

// ---------------------------------------------------------------------------
// Keys and signatures
// ---------------------------------------------------------------------------

const PKCS8_ED25519_PREFIX = Buffer.from(
  "302e020100300506032b657004220420",
  "hex",
);

export const capacitySeed = (index) => {
  const counter = Buffer.alloc(4);
  counter.writeUInt32BE(index);
  return sha256(concatBytes(utf8(CAPACITY_SEED_DOMAIN), counter));
};

/**
 * The committee in packed order: `{ publicKey, privateKey }` for each seed,
 * sorted by verification key bytes.
 */
export const capacitySigners = () => {
  const signers = [];
  for (let index = 0; index < CAPACITY_SIGNER_COUNT; index += 1) {
    const privateKey = createPrivateKey({
      key: Buffer.concat([PKCS8_ED25519_PREFIX, capacitySeed(index)]),
      format: "der",
      type: "pkcs8",
    });
    const spki = createPublicKey(privateKey).export({
      format: "der",
      type: "spki",
    });
    signers.push({
      publicKey: Uint8Array.from(spki.subarray(spki.length - 32)),
      privateKey,
    });
  }
  return signers.sort((left, right) =>
    Buffer.compare(Buffer.from(left.publicKey), Buffer.from(right.publicKey)),
  );
};

/** Witness `j`: the byte `j`, then packed signer `j`'s signature. */
export const capacityWitnesses = (signers, message) =>
  concatBytes(
    ...signers.map(({ privateKey }, index) =>
      concatBytes(
        Uint8Array.of(index),
        signMessage(null, Buffer.from(message), privateKey),
      ),
    ),
  );

/**
 * Verifies every witness against its packed key, exactly as
 * `verify_indexed_signatures` walks them. Throws on the first failure.
 */
export const verifyCapacityWitnesses = (signers, message, witnesses) => {
  const witnessByteCount = 65;
  if (witnesses.length !== signers.length * witnessByteCount) {
    throw new Error("witness block has the wrong length");
  }
  signers.forEach(({ publicKey }, index) => {
    const witness = witnesses.subarray(
      index * witnessByteCount,
      (index + 1) * witnessByteCount,
    );
    if (witness[0] !== index) {
      throw new Error(`witness ${index} names signer ${witness[0]}`);
    }
    const key = createPublicKey({
      key: {
        kty: "OKP",
        crv: "Ed25519",
        x: Buffer.from(publicKey).toString("base64url"),
      },
      format: "jwk",
    });
    if (!verifyMessage(null, Buffer.from(message), key, witness.subarray(1))) {
      throw new Error(`witness ${index} does not verify`);
    }
  });
  return signers.length;
};

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

const renderByteConstant = (name, bytes) =>
  `const ${name}: ByteArray =\n  ${aikenBytesLiteral(bytes)}\n`;

/**
 * `<prefix>_0 … <prefix>_<n>` holding `bytes` in chunks of `chunkByteCount`,
 * then `<prefix>` concatenating them, in the layout `aiken fmt` keeps.
 */
export const renderChunkedByteConstants = (prefix, bytes, chunkByteCount) => {
  if (bytes.length % chunkByteCount !== 0) {
    throw new Error(`${prefix} is not a whole number of chunks`);
  }
  const names = [];
  const parts = [];
  for (let offset = 0; offset < bytes.length; offset += chunkByteCount) {
    const name = `${prefix}_${names.length}`;
    names.push(name);
    parts.push(
      renderByteConstant(name, bytes.subarray(offset, offset + chunkByteCount)),
    );
  }
  const [first, ...rest] = names;
  parts.push(
    [
      `const ${prefix}: ByteArray =`,
      `  ${first}`,
      ...rest.map((name) => `    |> bytearray.concat(${name})`),
    ].join("\n") + "\n",
  );
  return parts.join("\n");
};

/** The packed keys and the witnesses: the constants the proof reproduces. */
export const renderSignerAndSignatureConstants = (signers, witnesses) =>
  [
    renderChunkedByteConstants(
      "capacity_remaining_signers",
      concatBytes(...signers.map(({ publicKey }) => publicKey)),
      SIGNERS_PER_CONSTANT * 32,
    ),
    renderChunkedByteConstants(
      "capacity_signatures",
      witnesses,
      WITNESSES_PER_CONSTANT * 65,
    ),
  ].join("\n");

export const renderCapacityCommitmentFunction = () => {
  const {
    version,
    payloadByteLength,
    responseGeometry: geometry,
    trancheDescriptor: descriptor,
  } = CAPACITY_COMMITMENT;
  return [
    "fn capacity_availability_commitment() -> availability.CommitmentV1 {",
    "  availability.CommitmentV1 {",
    `    version: ${version.aiken},`,
    "    deployment_identity: capacity_da_policy,",
    "    header_hash: capacity_header_hash,",
    `    payload_byte_length: ${payloadByteLength.aiken},`,
    "    response_geometry: availability.ResponseGeometryV1 {",
    `      chunk_byte_length: ${geometry.chunkByteLength.aiken},`,
    `      tranche_byte_length: ${geometry.trancheByteLength.aiken},`,
    `      max_tranche_count: ${geometry.maxTrancheCount.aiken},`,
    "    },",
    "    tranche_descriptors: [",
    "      availability.TrancheDescriptorV1 {",
    `        tranche_index: ${descriptor.trancheIndex.aiken},`,
    `        start_offset: ${descriptor.startOffset.aiken},`,
    `        byte_length: ${descriptor.byteLength.aiken},`,
    `        chunk_count: ${descriptor.chunkCount.aiken},`,
    `        chunk_commitment: ${aikenBytesLiteral(descriptor.chunkCommitment)},`,
    `        terminal_accumulator: ${aikenBytesLiteral(descriptor.terminalAccumulator)},`,
    "      },",
    "    ],",
    "  }",
    "}",
    "",
  ].join("\n");
};

const splitHex = (hexValue, width) => {
  const lines = [];
  for (let offset = 0; offset < hexValue.length; offset += width) {
    lines.push(hexValue.slice(offset, offset + width));
  }
  return lines;
};

/** The whole generated block, markers included, ending in a newline. */
export const renderCapacityBlock = () => {
  const signers = capacitySigners();
  const message = capacityAttestationMessage();
  const witnesses = capacityWitnesses(signers, message);
  verifyCapacityWitnesses(signers, message, witnesses);
  const messageHex = toHex(message);
  return [
    BEGIN_MARKER,
    "// Generated by",
    `// ${GENERATOR_PATH}.`,
    "// Regenerate with",
    "// `pnpm --dir demo/midgard-sdk fixtures:da-attestation-capacity-v1:sync`.",
    "//",
    "// Deterministic Ed25519 fixture for DA signature capacity regression.",
    '// Seeds are sha256("midgard-da-capacity-regression-v1:" <> u32be(index)).',
    "// Signer verification keys are packed in ascending byte order; witness j is",
    "// the byte j followed by packed signer j's signature over",
    "// availability.attestation_message_v1(capacity_availability_commitment()):",
    ...splitHex(messageHex, 32).map((line) => `//   ${line}`),
    `const capacity_fixture_signature_count: Int = ${aikenIntLiteral(BigInt(CAPACITY_SIGNER_COUNT))}`,
    "",
    renderByteConstant("capacity_da_policy", CAPACITY_DA_POLICY).replace(
      ": ByteArray",
      ": PolicyId",
    ),
    renderByteConstant("capacity_header_hash", CAPACITY_HEADER_HASH).replace(
      ": ByteArray",
      ": HeaderHash",
    ),
    renderCapacityCommitmentFunction(),
    "/// The signed message, pinned so a changed `CommitmentV1` encoding fails",
    "/// here by name rather than as 256 failing signatures.",
    "test capacity_attestation_message_is_pinned() {",
    '  availability.attestation_message_v1(capacity_availability_commitment()) == #"' +
      messageHex +
      '"',
    "}",
    "",
    renderSignerAndSignatureConstants(signers, witnesses),
    END_MARKER,
    "",
  ].join("\n");
};

/**
 * Replaces the marked block of `source` with `block`. Exactly one BEGIN and
 * one END marker must be present, in order, each on a line of its own.
 */
export const spliceCapacityBlock = (source, block) => {
  const lines = source.split("\n");
  const begins = lines.flatMap((line, index) =>
    line === BEGIN_MARKER ? [index] : [],
  );
  const ends = lines.flatMap((line, index) =>
    line === END_MARKER ? [index] : [],
  );
  if (begins.length !== 1 || ends.length !== 1 || ends[0] < begins[0]) {
    throw new Error(
      `expected exactly one "${BEGIN_MARKER}" line followed by one "${END_MARKER}" line`,
    );
  }
  const before = lines.slice(0, begins[0]).join("\n");
  const after = lines.slice(ends[0] + 1).join("\n");
  return `${before}${before === "" ? "" : "\n"}${block}${after}`;
};

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

const main = () => {
  const usage =
    "usage: node scripts/generate-da-attestation-capacity-v1-fixture.mjs [--check] [--print] [--file <path>]";
  const options = parseGeneratorArguments(usage, {
    "--check": false,
    "--print": false,
    "--file": true,
  });
  if (options["--check"] && options["--print"]) {
    console.error(usage);
    process.exit(2);
  }
  const block = renderCapacityBlock();
  if (options["--print"]) {
    process.stdout.write(block);
    return;
  }
  const target = options["--file"]
    ? resolve(options["--file"])
    : CAPACITY_MODULE_PATH;
  const inRepository = relative(repositoryRoot, target);
  const shown =
    inRepository.startsWith("..") || isAbsolute(inRepository)
      ? target
      : inRepository;
  const source = readFileSync(target, "utf8");
  const expected = spliceCapacityBlock(source, block);
  if (options["--check"]) {
    if (source !== expected) {
      console.error(`stale generated block: ${shown}`);
      process.exit(1);
    }
    console.log(`checked ${shown}`);
    return;
  }
  writeFileSync(target, expected, "utf8");
  console.log(`wrote ${shown}`);
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  main();
}
