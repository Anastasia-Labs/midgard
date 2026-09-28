#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the pooled DA committee bond
 * and the DA types around it: the `DaBondPoolDatum`, the pool's mint and spend
 * redeemers (`onchain/aiken/lib/midgard/da-bond-pool.ak`), every arm of
 * `StateQueueStatusV1` and `ParametersV1` at max_tranche_count 1, 16 and 64
 * (`onchain/aiken/lib/midgard/availability-challenge.ak`).
 *
 * **Why this channel exists.** The pool datum decides whether an Apply may
 * attest (`Bonded`) and when a withdrawal may complete (`unlock_at`); the spend
 * redeemer's constructor index picks the pool transition the validator runs;
 * the node status carries the commitment hash a challenger opens against; and
 * `ParametersV1` is applied to the attestation, pool and availability scripts,
 * so its bytes are part of their hashes. An off-chain encoder that swapped two
 * fields or two constructors (BeginWithdraw and CancelWithdraw take the same
 * fields) would build transactions the validators refuse or, worse, read, and
 * only a byte vector pinned on both sides would notice.
 *
 * **The vectors.**
 *
 *   * pool datums: `Bonded`, and `Withdrawing` at 0, a small value, a
 *     realistic unlock time and the largest signed 64-bit integer;
 *   * the pool mint redeemer `InitPool` and all five spend redeemers, each with
 *     distinct field values so a transposed field changes the bytes;
 *   * `StateQueueStatusV1` in all four arms;
 *   * `ParametersV1` at max_tranche_count 1, 16 and 64 with the preprod-testing
 *     profile's `da_bond` amounts (read from
 *     `config/deployments/preprod-testing.yaml`) and the deployed 14,020-byte
 *     response chunk.
 *
 * Two artifacts:
 *
 *   * `demo/midgard-sdk/tests/fixtures/da-bond-pool-v1.generated.json`, which
 *     `tests/da-golden-vectors.test.ts` reproduces with the SDK codecs; and
 *   * `onchain/aiken/lib/midgard/da-bond-pool-v1-golden.test.ak`, which
 *     rebuilds each value as an Aiken literal and asserts, in two named tests
 *     per vector, that `builtin.serialise_data` of the literal is the pinned
 *     hex and that the pinned hex decodes back to the literal.
 *
 * Vectors are regenerated, never hand-edited. The generator needs no built
 * `dist/`: it encodes with `da-vector-support.mjs`, not the SDK codec, so the
 * TypeScript suite is an independent cross-check.
 *
 * usage: node scripts/generate-da-bond-pool-v1-goldens.mjs
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
  DA_RESPONSE_CHUNK_BYTES,
  readProfiles,
} from "../../scripts/deployment-profiles.mjs";
import {
  aikenBytesLiteral,
  aikenIntLiteral,
  blake2b224,
  blake2b256,
  CHALLENGE_ASSET_NAME_PREFIX_V1,
  concatBytes,
  DA_BOND_POOL_SPEND_REDEEMER_ARMS,
  daBondPoolDatumData,
  daBondPoolMintRedeemerData,
  daBondPoolSpendRedeemerData,
  parametersV1Data,
  parseGeneratorArguments,
  serialiseData,
  stateQueueStatusV1Data,
  toHex,
  utf8,
} from "./da-vector-support.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");

export const GENERATOR_PATH =
  "demo/midgard-sdk/scripts/generate-da-bond-pool-v1-goldens.mjs";

const AIKEN_MODULE = "onchain/aiken/lib/midgard/da-bond-pool-v1-golden.test.ak";

export const JSON_PATH = join(
  packageRoot,
  "tests/fixtures/da-bond-pool-v1.generated.json",
);

export const AIKEN_PATH = join(repositoryRoot, AIKEN_MODULE);

/** The profile whose `da_bond` amounts the `ParametersV1` vectors carry. */
const PARAMETERS_PROFILE = "preprod-testing";

const MiB = 1024n * 1024n;
const FULL_PAYLOAD_MAX_BYTES = 64n * MiB;
const INT64_MAX = (1n << 63n) - 1n;

/** A labelled, deterministic stand-in for a hash the vectors need. */
const seeded = (label, length) =>
  (length === 28 ? blake2b224 : blake2b256)(
    utf8(`midgard-da-bond-pool-v1-golden/${label}`),
  );

// ---------------------------------------------------------------------------
// Vector shapes
// ---------------------------------------------------------------------------

const POOL_DATUMS = [
  { label: "pool_datum_bonded", note: "Bonded", datum: { kind: "Bonded" } },
  {
    label: "pool_datum_withdrawing_zero",
    note: "Withdrawing at unlock_at 0",
    datum: { kind: "Withdrawing", unlockAt: 0n },
  },
  {
    label: "pool_datum_withdrawing_small",
    note: "Withdrawing at a small unlock_at (a 3-byte CBOR integer)",
    datum: { kind: "Withdrawing", unlockAt: 1_000n },
  },
  {
    label: "pool_datum_withdrawing_posix_ms",
    note: "Withdrawing at a realistic POSIX-millisecond unlock_at (a 9-byte CBOR integer)",
    datum: { kind: "Withdrawing", unlockAt: 1_790_002_340_000n },
  },
  {
    label: "pool_datum_withdrawing_large",
    note: "Withdrawing at the largest signed 64-bit unlock_at",
    datum: { kind: "Withdrawing", unlockAt: INT64_MAX },
  },
];

const POOL_MINT_REDEEMERS = [
  {
    label: "pool_mint_init_pool",
    note: "InitPool",
    redeemer: { kind: "InitPool", outputIndex: 2n },
  },
];

// Distinct values per field, and BeginWithdraw / CancelWithdraw on identical
// fields, so a transposed field or a swapped constructor changes the bytes.
const POOL_SPEND_REDEEMERS = [
  {
    label: "pool_spend_top_up",
    redeemer: { kind: "TopUp", outputIndex: 1n },
  },
  {
    label: "pool_spend_slash",
    redeemer: {
      kind: "Slash",
      hubOracleRefInputIndex: 2n,
      stateQueueMintRedeemerIndex: 5n,
      correctionLockInputIndex: 7n,
      outputIndex: 11n,
    },
  },
  {
    label: "pool_spend_begin_withdraw",
    redeemer: {
      kind: "BeginWithdraw",
      daParamsRefInputIndex: 1n,
      outputIndex: 4n,
    },
  },
  {
    label: "pool_spend_cancel_withdraw",
    redeemer: {
      kind: "CancelWithdraw",
      daParamsRefInputIndex: 1n,
      outputIndex: 4n,
    },
  },
  {
    label: "pool_spend_complete_withdraw",
    redeemer: {
      kind: "CompleteWithdraw",
      amount: 45_000_000_000_000_000n,
      daParamsRefInputIndex: 3n,
      outputIndex: 6n,
    },
  },
];

const STATUSES = [
  { label: "status_unattested", status: { kind: "Unattested" } },
  {
    label: "status_attested",
    status: {
      kind: "Attested",
      commitmentHash: seeded("attested/commitment-hash", 32),
    },
  },
  {
    label: "status_challenged",
    status: {
      kind: "Challenged",
      commitmentHash: seeded("challenged/commitment-hash", 32),
      challengeAssetName: concatBytes(
        utf8(CHALLENGE_ASSET_NAME_PREFIX_V1),
        seeded("challenged/challenge-asset-name", 28),
      ),
    },
  },
  {
    label: "status_published",
    status: {
      kind: "Published",
      terminalCommitment: seeded("published/terminal-commitment", 32),
    },
  },
];

/**
 * Deployment data that is not in a profile: the challenger bond measurement
 * candidate and the fee ceilings the SDK suites use. Each tranche class must
 * still satisfy the fee-reserve relation the SDK enforces, which
 * `assertFeeReserveCovered` checks below.
 */
const DEPLOYMENT_AMOUNTS = Object.freeze({
  challengerBondLovelace: 10_000_000_000n,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});

const PARAMETER_TRANCHE_COUNTS = [1n, 16n, 64n];

const profileAmounts = () => {
  const profile = readProfiles()[PARAMETERS_PROFILE];
  const bond = profile.da_bond;
  return {
    daBondLovelace: BigInt(bond.da_bond_lovelace),
    daSlashPenaltyLovelace: BigInt(bond.da_slash_penalty_lovelace),
    daBondMinTopUpLovelace: BigInt(bond.da_bond_min_top_up_lovelace),
    daBondPoolFloorLovelace: BigInt(bond.da_bond_pool_floor_lovelace),
    challengeRecordLovelace: BigInt(bond.challenge_record_lovelace),
  };
};

const ceilDiv = (numerator, denominator) =>
  (numerator + denominator - 1n) / denominator;

/**
 * Every maximum-size publication plus every settlement plus the larger
 * terminal fee must stay below the challenger bond.
 */
const assertFeeReserveCovered = (parameters) => {
  const { chunkByteLength, trancheByteLength, maxTrancheCount } =
    parameters.responseGeometry;
  let publications = 0n;
  for (
    let offset = 0n;
    offset < FULL_PAYLOAD_MAX_BYTES;
    offset += trancheByteLength
  ) {
    const remaining = FULL_PAYLOAD_MAX_BYTES - offset;
    publications += ceilDiv(
      remaining < trancheByteLength ? remaining : trancheByteLength,
      chunkByteLength,
    );
  }
  const terminal =
    parameters.maxCloseFeeLovelace > parameters.maxTimeoutFeeLovelace
      ? parameters.maxCloseFeeLovelace
      : parameters.maxTimeoutFeeLovelace;
  if (
    publications * parameters.maxPublicationFeeLovelace +
      maxTrancheCount * parameters.maxSettlementFeeLovelace +
      terminal >=
    parameters.challengerBondLovelace
  ) {
    throw new Error(
      `ParametersV1 at max_tranche_count ${maxTrancheCount} does not cover its fee reserve`,
    );
  }
};

const parameterShapes = () => {
  const amounts = profileAmounts();
  return PARAMETER_TRANCHE_COUNTS.map((maxTrancheCount) => {
    const parameters = {
      responseGeometry: {
        chunkByteLength: BigInt(DA_RESPONSE_CHUNK_BYTES),
        trancheByteLength: FULL_PAYLOAD_MAX_BYTES / maxTrancheCount,
        maxTrancheCount,
      },
      daBondLovelace: amounts.daBondLovelace,
      challengerBondLovelace: DEPLOYMENT_AMOUNTS.challengerBondLovelace,
      maxOpenFeeLovelace: DEPLOYMENT_AMOUNTS.maxOpenFeeLovelace,
      maxPublicationFeeLovelace: DEPLOYMENT_AMOUNTS.maxPublicationFeeLovelace,
      maxSettlementFeeLovelace: DEPLOYMENT_AMOUNTS.maxSettlementFeeLovelace,
      maxCloseFeeLovelace: DEPLOYMENT_AMOUNTS.maxCloseFeeLovelace,
      maxTimeoutFeeLovelace: DEPLOYMENT_AMOUNTS.maxTimeoutFeeLovelace,
      daSlashPenaltyLovelace: amounts.daSlashPenaltyLovelace,
      daBondMinTopUpLovelace: amounts.daBondMinTopUpLovelace,
      daBondPoolFloorLovelace: amounts.daBondPoolFloorLovelace,
      challengeRecordLovelace: amounts.challengeRecordLovelace,
    };
    assertFeeReserveCovered(parameters);
    return {
      label: `parameters_tranches_${maxTrancheCount}`,
      note: `${PARAMETERS_PROFILE} amounts, ${FULL_PAYLOAD_MAX_BYTES / maxTrancheCount / MiB} MiB tranches, max_tranche_count ${maxTrancheCount}`,
      parameters,
    };
  });
};

// ---------------------------------------------------------------------------
// Vectors
// ---------------------------------------------------------------------------

const withCbor = (shapes, key, toData) =>
  shapes.map((shape) => ({
    ...shape,
    cbor: serialiseData(toData(shape[key])),
  }));

export const buildVectors = () => ({
  poolDatums: withCbor(POOL_DATUMS, "datum", daBondPoolDatumData),
  poolMintRedeemers: withCbor(
    POOL_MINT_REDEEMERS,
    "redeemer",
    daBondPoolMintRedeemerData,
  ),
  poolSpendRedeemers: withCbor(
    POOL_SPEND_REDEEMERS,
    "redeemer",
    daBondPoolSpendRedeemerData,
  ),
  statuses: withCbor(STATUSES, "status", stateQueueStatusV1Data),
  parameters: withCbor(parameterShapes(), "parameters", parametersV1Data),
});

// ---------------------------------------------------------------------------
// JSON: integers as decimal strings (unlock_at reaches 2^63 - 1), bytes as hex
// ---------------------------------------------------------------------------

const jsonValue = (value) => {
  if (typeof value === "bigint") return value.toString();
  if (value instanceof Uint8Array) return toHex(value);
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value).map(([key, inner]) => [key, jsonValue(inner)]),
    );
  }
  return value;
};

const jsonVectors = (vectors, key) =>
  vectors.map((vector) => ({
    label: vector.label,
    ...(vector.note === undefined ? {} : { note: vector.note }),
    [key]: jsonValue(vector[key]),
    cborHex: toHex(vector.cbor),
  }));

const buildJson = (vectors) => ({
  schema: "midgard-da-bond-pool-v1-goldens",
  generator: GENERATOR_PATH,
  aikenModule: AIKEN_MODULE,
  parametersProfile: PARAMETERS_PROFILE,
  integerEncoding: "decimal strings",
  poolDatums: jsonVectors(vectors.poolDatums, "datum"),
  poolMintRedeemers: jsonVectors(vectors.poolMintRedeemers, "redeemer"),
  poolSpendRedeemers: jsonVectors(vectors.poolSpendRedeemers, "redeemer"),
  statuses: jsonVectors(vectors.statuses, "status"),
  parameters: jsonVectors(vectors.parameters, "parameters"),
});

// ---------------------------------------------------------------------------
// Aiken
// ---------------------------------------------------------------------------

const snake = (name) =>
  name.replace(/[A-Z]/gu, (letter) => `_${letter.toLowerCase()}`);

/** `prefix.Kind { field: value, ... }`, or `prefix.Kind` without fields. */
const renderConstructor = (qualified, fields) =>
  fields.length === 0
    ? [qualified]
    : [
        `${qualified} {`,
        ...fields.map(([name, literal]) => `  ${snake(name)}: ${literal},`),
        "}",
      ];

const literalOf = (value) =>
  value instanceof Uint8Array
    ? aikenBytesLiteral(value)
    : aikenIntLiteral(value);

const fieldsOf = (value) =>
  Object.entries(value)
    .filter(([key]) => key !== "kind")
    .map(([key, inner]) => [key, literalOf(inner)]);

const renderPoolDatum = (datum) =>
  renderConstructor(`pool.${datum.kind}`, fieldsOf(datum));

const renderPoolMintRedeemer = (redeemer) =>
  renderConstructor(`pool.${redeemer.kind}`, fieldsOf(redeemer));

const renderPoolSpendRedeemer = (redeemer) => {
  const arm = DA_BOND_POOL_SPEND_REDEEMER_ARMS.find(
    ([kind]) => kind === redeemer.kind,
  );
  // Fields in the constructor's declared order, not the object's.
  return renderConstructor(
    `pool.${redeemer.kind}`,
    arm[1].map((field) => [field, literalOf(redeemer[field])]),
  );
};

const renderStatus = (status) =>
  renderConstructor(`availability.${status.kind}`, fieldsOf(status));

const renderParameters = (parameters) => {
  const geometry = parameters.responseGeometry;
  return [
    "availability.ParametersV1 {",
    "  response_geometry: availability.ResponseGeometryV1 {",
    `    chunk_byte_length: ${aikenIntLiteral(geometry.chunkByteLength)},`,
    `    tranche_byte_length: ${aikenIntLiteral(geometry.trancheByteLength)},`,
    `    max_tranche_count: ${aikenIntLiteral(geometry.maxTrancheCount)},`,
    "  },",
    ...Object.entries(parameters)
      .filter(([key]) => key !== "responseGeometry")
      .map(([key, value]) => `  ${snake(key)}: ${aikenIntLiteral(value)},`),
    "}",
  ];
};

const indent = (lines, by) => lines.map((line) => `${" ".repeat(by)}${line}`);

const renderVector = ({ vector, key, type, render }) => {
  const { label } = vector;
  const prefix = `da_bond_pool_v1_golden_${label}`;
  return [
    ...(vector.note === undefined ? [] : [`// ${label}: ${vector.note}.`]),
    `fn ${label}() -> ${type} {`,
    ...indent(render(vector[key]), 2),
    "}",
    "",
    `const ${label}_cbor: ByteArray = ${aikenBytesLiteral(vector.cbor)}`,
    "",
    `test ${prefix}_cbor() {`,
    `  let value: Data = ${label}()`,
    `  builtin.serialise_data(value) == ${label}_cbor`,
    "}",
    "",
    `test ${prefix}_decodes() {`,
    `  expect Some(data) = cbor.deserialise(${label}_cbor)`,
    `  expect decoded: ${type} = data`,
    `  decoded == ${label}()`,
    "}",
    "",
  ];
};

const SECTIONS = [
  {
    title: "DaBondPoolDatum",
    group: "poolDatums",
    key: "datum",
    type: "pool.DaBondPoolDatum",
    render: renderPoolDatum,
  },
  {
    title: "pool MintRedeemer",
    group: "poolMintRedeemers",
    key: "redeemer",
    type: "pool.MintRedeemer",
    render: renderPoolMintRedeemer,
  },
  {
    title: "pool SpendRedeemer",
    group: "poolSpendRedeemers",
    key: "redeemer",
    type: "pool.SpendRedeemer",
    render: renderPoolSpendRedeemer,
  },
  {
    title: "StateQueueStatusV1",
    group: "statuses",
    key: "status",
    type: "availability.StateQueueStatusV1",
    render: renderStatus,
  },
  {
    title: "ParametersV1",
    group: "parameters",
    key: "parameters",
    type: "availability.ParametersV1",
    render: renderParameters,
  },
];

const renderAiken = (vectors) =>
  [
    "//// Generated by",
    `//// ${GENERATOR_PATH}.`,
    "//// Do not edit; regenerate with",
    "//// `pnpm --dir demo/midgard-sdk fixtures:da-bond-pool-v1:sync`.",
    "////",
    "//// Cross-language vectors for the pooled DA bond datum and redeemers,",
    "//// `StateQueueStatusV1` and `ParametersV1`. The TypeScript twin reads the",
    "//// same values from",
    "//// demo/midgard-sdk/tests/fixtures/da-bond-pool-v1.generated.json. Each",
    "//// vector rebuilds its value as a literal and pins, in two named tests,",
    "//// that `builtin.serialise_data` of the literal is the pinned hex and that",
    "//// the pinned hex decodes back to the literal.",
    "",
    "use aiken/builtin",
    "use aiken/cbor",
    "use midgard/availability_challenge as availability",
    "use midgard/da_bond_pool as pool",
    "",
    ...SECTIONS.flatMap((section) => [
      `// ${section.title}`,
      "",
      ...vectors[section.group].flatMap((vector) =>
        renderVector({ vector, ...section }),
      ),
    ]),
  ].join("\n");

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

export const renderGoldenJson = (vectors) =>
  `${JSON.stringify(buildJson(vectors), null, 2)}\n`;

export const renderGoldenAiken = (vectors) =>
  formatAikenSource({
    source: renderAiken(vectors),
    fileName: "da-bond-pool-v1-golden.test.ak",
    repositoryRoot,
    tmpPrefix: "midgard-da-bond-pool-v1-aiken-format-",
  });

const main = () => {
  const options = parseGeneratorArguments(
    "usage: node scripts/generate-da-bond-pool-v1-goldens.mjs [--check] [--aiken-out <path>] [--json-out <path>]",
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
