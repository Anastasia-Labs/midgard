import {
  aikenBytesLiteral,
  aikenIntLiteral,
  blake2b224,
  blake2b256,
  DA_BOND_POOL_SPEND_REDEEMER_ARMS,
  serialiseData,
  toHex,
  utf8,
} from "./da-vector-support.mjs";

/** A labelled, deterministic stand-in for a hash the vectors need. */
export const seeded = (label, length) =>
  (length === 28 ? blake2b224 : blake2b256)(
    utf8(`midgard-da-bond-pool-v1-golden/${label}`),
  );

export const ceilDiv = (numerator, denominator) =>
  (numerator + denominator - 1n) / denominator;

// ---------------------------------------------------------------------------
// Vectors
// ---------------------------------------------------------------------------

export const withCbor = (shapes, key, toData) =>
  shapes.map((shape) => ({
    ...shape,
    cbor: serialiseData(toData(shape[key])),
  }));

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

export const jsonVectors = (vectors, key) =>
  vectors.map((vector) => ({
    label: vector.label,
    ...(vector.note === undefined ? {} : { note: vector.note }),
    [key]: jsonValue(vector[key]),
    cborHex: toHex(vector.cbor),
  }));

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

export const renderPoolDatum = (datum) =>
  renderConstructor(`pool.${datum.kind}`, fieldsOf(datum));

export const renderPoolMintRedeemer = (redeemer) =>
  renderConstructor(`pool.${redeemer.kind}`, fieldsOf(redeemer));

export const renderPoolSpendRedeemer = (redeemer) => {
  const arm = DA_BOND_POOL_SPEND_REDEEMER_ARMS.find(
    ([kind]) => kind === redeemer.kind,
  );
  // Fields in the constructor's declared order, not the object's.
  return renderConstructor(
    `pool.${redeemer.kind}`,
    arm[1].map((field) => [field, literalOf(redeemer[field])]),
  );
};

export const renderStatus = (status) =>
  renderConstructor(`availability.${status.kind}`, fieldsOf(status));

export const renderParameters = (parameters) => {
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

export const renderVector = ({ vector, key, type, render }) => {
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
