#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import {
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeMidgardNativeTxCanonicalV1,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const canonicalPath = join(
  packageRoot,
  "tests/fixtures/transaction-root-v1.canonical.json",
);
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/transaction-root-v1.generated.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/transaction-root-v1-golden.test.ak",
);
const buildEntrypointPath = join(packageRoot, "dist/transaction-root-v1.js");
const checkOnly = process.argv.includes("--check");

if (
  process.argv.slice(2).some((argument) => argument !== "--check") ||
  process.argv.slice(2).filter((argument) => argument === "--check").length > 1
) {
  throw new Error(
    "usage: node scripts/generate-transaction-root-v1-fixture.mjs [--check]",
  );
}

const fail = (message) => {
  throw new Error(`transaction-root-v1 fixture generation failed: ${message}`);
};

const isRecord = (value) =>
  value !== null && typeof value === "object" && !Array.isArray(value);

const exactKeys = (value, keys, label) => {
  if (!isRecord(value)) fail(`${label} must be an object`);
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    fail(`${label} must contain exactly ${keys.join(", ")}`);
  }
  return value;
};

const nonEmptyString = (value, label) => {
  if (typeof value !== "string" || value.length === 0) {
    fail(`${label} must be a non-empty string`);
  }
  return value;
};

const hex = (value, label, byteLength) => {
  nonEmptyString(value, label);
  if (!/^[0-9a-f]+$/u.test(value) || value.length % 2 !== 0) {
    fail(`${label} must be lowercase even-length hexadecimal`);
  }
  if (byteLength !== undefined && value.length !== byteLength * 2) {
    fail(`${label} must be exactly ${byteLength.toString()} bytes`);
  }
  return Buffer.from(value, "hex");
};

const decimal = (value, label) => {
  if (typeof value !== "string" || !/^(?:0|-?[1-9][0-9]*)$/u.test(value)) {
    fail(`${label} must be a canonical decimal integer string`);
  }
  return BigInt(value);
};

const enumValue = (value, allowed, label) => {
  if (typeof value !== "string" || !allowed.includes(value)) {
    fail(`${label} must be one of ${allowed.join(", ")}`);
  }
  return value;
};

// The canonical-input verdict vocabulary. Each name selects one
// `OperatorVerdictV1` value (#640): `ForcedTxValid`, or `ForcedTxInvalid` with
// the named `RejectionReasonV1` arm at subject ordinal 0. The names are the
// arm spellings of `midgard/rejection_reason_v1` — the canonical input names
// the reason, not a coarse validity bucket.
const VERDICTS = {
  ForcedTxValid: {
    sdk: "ForcedTxValid",
    aiken: "rejection_reason_v1.ForcedTxValid",
  },
  InputNotFound: {
    sdk: {
      ForcedTxInvalid: {
        reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
      },
    },
    aiken:
      "rejection_reason_v1.ForcedTxInvalid {\n      reason: rejection_reason_v1.InputNotFound { source_kind: 0, input_index: 0 },\n    }",
  },
  AddressWitnessSignatureInvalid: {
    sdk: {
      ForcedTxInvalid: {
        reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
      },
    },
    aiken:
      "rejection_reason_v1.ForcedTxInvalid {\n      reason: rejection_reason_v1.AddressWitnessSignatureInvalid {\n        witness_index: 0,\n      },\n    }",
  },
  PlutusExecutionFailed: {
    sdk: {
      ForcedTxInvalid: {
        reason: { PlutusExecutionFailed: { execution_index: 0n } },
      },
    },
    aiken:
      "rejection_reason_v1.ForcedTxInvalid {\n      reason: rejection_reason_v1.PlutusExecutionFailed { execution_index: 0 },\n    }",
  },
  FeeBelowMinimum: {
    sdk: { ForcedTxInvalid: { reason: "FeeBelowMinimum" } },
    aiken:
      "rejection_reason_v1.ForcedTxInvalid {\n      reason: rejection_reason_v1.FeeBelowMinimum,\n    }",
  },
  ValueNotPreserved: {
    sdk: { ForcedTxInvalid: { reason: "ValueNotPreserved" } },
    aiken:
      "rejection_reason_v1.ForcedTxInvalid {\n      reason: rejection_reason_v1.ValueNotPreserved,\n    }",
  },
};
const VERDICT_NAMES = Object.keys(VERDICTS);

const parseCanonicalTransaction = (value, label) => {
  exactKeys(
    value,
    ["name", "version", "validity", "body", "witnessSet"],
    label,
  );
  const name = nonEmptyString(value.name, `${label}.name`);
  exactKeys(
    value.body,
    [
      "spendInputsPreimageCbor",
      "referenceInputsPreimageCbor",
      "outputsPreimageCbor",
      "fee",
      "validityIntervalStart",
      "validityIntervalEnd",
      "requiredObserversPreimageCbor",
      "requiredSignersPreimageCbor",
      "mintPreimageCbor",
      "scriptIntegrityHash",
      "auxiliaryDataHash",
      "networkId",
    ],
    `${label}.body`,
  );
  exactKeys(
    value.witnessSet,
    [
      "addrTxWitsPreimageCbor",
      "scriptTxWitsPreimageCbor",
      "redeemerTxWitsPreimageCbor",
    ],
    `${label}.witnessSet`,
  );
  const body = value.body;
  const witnessSet = value.witnessSet;
  return {
    name,
    transaction: {
      version: decimal(value.version, `${label}.version`),
      validity: enumValue(value.validity, ["TxIsValid"], `${label}.validity`),
      body: {
        spendInputsPreimageCbor: hex(
          body.spendInputsPreimageCbor,
          `${label}.body.spendInputsPreimageCbor`,
        ),
        referenceInputsPreimageCbor: hex(
          body.referenceInputsPreimageCbor,
          `${label}.body.referenceInputsPreimageCbor`,
        ),
        outputsPreimageCbor: hex(
          body.outputsPreimageCbor,
          `${label}.body.outputsPreimageCbor`,
        ),
        fee: decimal(body.fee, `${label}.body.fee`),
        validityIntervalStart: decimal(
          body.validityIntervalStart,
          `${label}.body.validityIntervalStart`,
        ),
        validityIntervalEnd: decimal(
          body.validityIntervalEnd,
          `${label}.body.validityIntervalEnd`,
        ),
        requiredObserversPreimageCbor: hex(
          body.requiredObserversPreimageCbor,
          `${label}.body.requiredObserversPreimageCbor`,
        ),
        requiredSignersPreimageCbor: hex(
          body.requiredSignersPreimageCbor,
          `${label}.body.requiredSignersPreimageCbor`,
        ),
        mintPreimageCbor: hex(
          body.mintPreimageCbor,
          `${label}.body.mintPreimageCbor`,
        ),
        scriptIntegrityHash: hex(
          body.scriptIntegrityHash,
          `${label}.body.scriptIntegrityHash`,
          32,
        ),
        auxiliaryDataHash: hex(
          body.auxiliaryDataHash,
          `${label}.body.auxiliaryDataHash`,
          32,
        ),
        networkId: decimal(body.networkId, `${label}.body.networkId`),
      },
      witnessSet: {
        addrTxWitsPreimageCbor: hex(
          witnessSet.addrTxWitsPreimageCbor,
          `${label}.witnessSet.addrTxWitsPreimageCbor`,
        ),
        scriptTxWitsPreimageCbor: hex(
          witnessSet.scriptTxWitsPreimageCbor,
          `${label}.witnessSet.scriptTxWitsPreimageCbor`,
        ),
        redeemerTxWitsPreimageCbor: hex(
          witnessSet.redeemerTxWitsPreimageCbor,
          `${label}.witnessSet.redeemerTxWitsPreimageCbor`,
        ),
      },
    },
  };
};

const toJsonTransaction = (parsed) => ({
  version: parsed.version.toString(),
  validity: parsed.validity,
  body: Object.fromEntries(
    Object.entries(parsed.body).map(([key, value]) => [
      key,
      value instanceof Buffer ? value.toString("hex") : value.toString(),
    ]),
  ),
  witnessSet: Object.fromEntries(
    Object.entries(parsed.witnessSet).map(([key, value]) => [
      key,
      value.toString("hex"),
    ]),
  ),
});

const aikenBytes = (value) => `#"${value}"`;
const aikenInt = (value) => value.toString();

const aikenVerdict = (name) => {
  const verdict = VERDICTS[name];
  if (verdict === undefined) fail(`unsupported Aiken verdict ${name}`);
  return verdict.aiken;
};

const makeAiken = ({ transactions, forcedOrders, roots }) => {
  const lines = [
    "// Generated by demo/midgard-node/scripts/generate-transaction-root-v1-fixture.mjs.",
    "// Do not edit; update transaction-root-v1.canonical.json and regenerate.",
    "",
    "use aiken/cbor",
    "use aiken/merkle_patricia_forestry as mpf",
    "use aiken/primitive/bytearray",
    "use cardano/transaction.{OutputReference}",
    "use midgard/ledger_state",
    "use midgard/rejection_reason_v1",
    "use midgard/transition_trace",
    "",
  ];

  for (const entry of transactions) {
    lines.push(
      `const ${entry.constantPrefix}_key = ${aikenBytes(entry.keyHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_value_cbor = ${aikenBytes(entry.valueCborHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_compact_cbor = ${aikenBytes(entry.compactCborHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_witness_set_compact_cbor = ${aikenBytes(entry.witnessSetCompactCborHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_field_preimage_lengths_cbor = ${aikenBytes(entry.fieldPreimageLengthsCborHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_proof_cbor = ${aikenBytes(entry.proofCborHex)}`,
    );
    lines.push("");
  }
  for (const entry of forcedOrders) {
    lines.push(
      `const ${entry.constantPrefix}_key = ${aikenBytes(entry.keyHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_value_cbor = ${aikenBytes(entry.valueCborHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_order_transaction_id = ${aikenBytes(entry.orderId.transactionIdHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_compact_cbor = ${aikenBytes(entry.compactCborHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_witness_set_compact_cbor = ${aikenBytes(entry.witnessSetCompactCborHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_field_preimage_lengths_cbor = ${aikenBytes(entry.fieldPreimageLengthsCborHex)}`,
    );
    lines.push(
      `const ${entry.constantPrefix}_proof_cbor = ${aikenBytes(entry.proofCborHex)}`,
    );
    lines.push("");
  }

  lines.push(
    `const transactions_phas_root = ${aikenBytes(roots.transactions.phasRootHex)}`,
    `const transactions_root = ${aikenBytes(roots.transactions.rootHex)}`,
    `const forced_transactions_phas_root = ${aikenBytes(roots.forcedTransactions.phasRootHex)}`,
    `const forced_transactions_root = ${aikenBytes(roots.forcedTransactions.rootHex)}`,
    "",
  );

  const orderedTransactions = [...transactions].sort((left, right) =>
    Buffer.compare(
      Buffer.from(left.keyHex, "hex"),
      Buffer.from(right.keyHex, "hex"),
    ),
  );
  const orderedForcedOrders = [...forcedOrders].sort((left, right) =>
    Buffer.compare(
      Buffer.from(left.keyHex, "hex"),
      Buffer.from(right.keyHex, "hex"),
    ),
  );
  const addMembershipFunction = (entry, rootConstant) => {
    lines.push(
      `fn ${entry.constantPrefix}_membership_is_exact() -> Bool {`,
      `  expect Some(data) = cbor.deserialise(${entry.constantPrefix}_proof_cbor)`,
      "  expect proof: mpf.Proof = data",
      `  mpf.has(mpf.from_root(${rootConstant}), ${entry.constantPrefix}_key, ${entry.constantPrefix}_value_cbor, proof)`,
      "}",
      "",
    );
  };
  const addOrderingFunction = (name, entries) => {
    if (entries.length === 2) {
      lines.push(
        `fn ${name}() -> Bool {`,
        `  bytearray.compare(${entries[0].constantPrefix}_key, ${entries[1].constantPrefix}_key) == Less`,
        "}",
        "",
      );
      return;
    }
    lines.push(`fn ${name}() -> Bool {`, "  and {");
    for (let index = 1; index < entries.length; index += 1) {
      lines.push(
        `    bytearray.compare(${entries[index - 1].constantPrefix}_key, ${entries[index].constantPrefix}_key) == Less${index === entries.length - 1 ? "" : ","}`,
      );
    }
    lines.push("  }", "}", "");
  };
  for (const entry of orderedTransactions) {
    addMembershipFunction(entry, "transactions_phas_root");
  }
  for (const entry of orderedForcedOrders) {
    addMembershipFunction(entry, "forced_transactions_phas_root");
  }
  addOrderingFunction("transactions_keys_are_canonical", orderedTransactions);
  addOrderingFunction(
    "forced_transaction_order_keys_are_canonical",
    orderedForcedOrders,
  );

  lines.push(
    "test rf031_transaction_root_uses_exact_domain_and_ordering() {",
    "  and {",
    "    transactions_keys_are_canonical(),",
    ...orderedTransactions.map(
      (entry) => `    ${entry.constantPrefix}_membership_is_exact(),`,
    ),
    "    transition_trace.commit_counted_root(",
    "      transition_trace.TransactionsV1RootDomain,",
    "      transactions_phas_root,",
    `      ${aikenInt(roots.transactions.count)},`,
    "    ) == transactions_root,",
    "  }",
    "}",
    "",
    "test rf031_forced_transaction_root_uses_order_keys_and_domain() {",
    "  and {",
    "    forced_transaction_order_keys_are_canonical(),",
    ...orderedForcedOrders.map(
      (entry) => `    ${entry.constantPrefix}_membership_is_exact(),`,
    ),
    "    transition_trace.commit_counted_root(",
    "      transition_trace.ForcedTransactionsV1RootDomain,",
    "      forced_transactions_phas_root,",
    `      ${aikenInt(roots.forcedTransactions.count)},`,
    "    ) == forced_transactions_root,",
    "    forced_transactions_root != transition_trace.commit_counted_root(",
    "      transition_trace.TransactionsV1RootDomain,",
    "      forced_transactions_phas_root,",
    `      ${aikenInt(roots.forcedTransactions.count)},`,
    "    ),",
    "  }",
    "}",
    "",
  );

  for (const entry of transactions) {
    lines.push(
      `test rf031_${entry.constantPrefix}_transaction_source_is_exact() {`,
      `  expect Some(data) = cbor.deserialise(${entry.constantPrefix}_value_cbor)`,
      `  expect value: ledger_state.L2TransactionSourceV1 = data`,
      "  and {",
      `    cbor.serialise(value) == ${entry.constantPrefix}_value_cbor,`,
      `    value.tx_id == ${entry.constantPrefix}_key,`,
      `    value.source.compact_cbor == ${entry.constantPrefix}_compact_cbor,`,
      `    value.source.witness_set_compact_cbor == ${entry.constantPrefix}_witness_set_compact_cbor,`,
      `    value.source.field_preimage_lengths_cbor == ${entry.constantPrefix}_field_preimage_lengths_cbor,`,
      "  }",
      "}",
      "",
    );
  }

  for (const entry of forcedOrders) {
    lines.push(
      `test rf031_${entry.constantPrefix}_forced_source_and_verdict_are_exact() {`,
      `  expect Some(data) = cbor.deserialise(${entry.constantPrefix}_value_cbor)`,
      `  expect value: ledger_state.ForcedInclusionTxV1 = data`,
      `  expect Some(order_data) = cbor.deserialise(${entry.constantPrefix}_key)`,
      "  expect order: OutputReference = order_data",
      "  and {",
      `    cbor.serialise(value) == ${entry.constantPrefix}_value_cbor,`,
      `    cbor.serialise(order) == ${entry.constantPrefix}_key,`,
      `    order.transaction_id == ${entry.constantPrefix}_order_transaction_id,`,
      `    order.output_index == ${aikenInt(entry.orderId.outputIndex)},`,
      `    value.tx_id == ${aikenBytes(entry.txIdHex)},`,
      `    value.source.compact_cbor == ${entry.constantPrefix}_compact_cbor,`,
      `    value.source.witness_set_compact_cbor == ${entry.constantPrefix}_witness_set_compact_cbor,`,
      `    value.source.field_preimage_lengths_cbor == ${entry.constantPrefix}_field_preimage_lengths_cbor,`,
      `    value.verdict == ${aikenVerdict(entry.verdict)},`,
      "  }",
      "}",
      "",
    );
  }

  return `${lines.join("\n")}\n`;
};

const writeOrCheck = (target, expected) => {
  const label = relative(repositoryRoot, target);
  if (checkOnly) {
    if (!existsSync(target)) fail(`missing generated artifact ${label}`);
    const actual = readFileSync(target, "utf8");
    if (actual !== expected) fail(`generated artifact is stale: ${label}`);
    return;
  }
  writeFileSync(target, expected, "utf8");
};

const formatAiken = (source) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-rf031-aiken-format-"));
  const target = join(directory, "transaction-root-v1-golden.test.ak");
  const aikenBinary = process.env.MIDGARD_AIKEN_BIN ?? "aiken";
  if (aikenBinary.length === 0) {
    fail("MIDGARD_AIKEN_BIN must be a non-empty executable path");
  }
  try {
    writeFileSync(target, source, "utf8");
    const result = spawnSync(aikenBinary, ["fmt", target], {
      cwd: join(repositoryRoot, "onchain/aiken"),
      encoding: "utf8",
    });
    if (result.status !== 0) {
      fail(
        `Aiken formatter (${aikenBinary}) failed: ${result.error?.message ?? result.stderr.trim()}`,
      );
    }
    return readFileSync(target, "utf8");
  } finally {
    rmSync(directory, { force: true, recursive: true });
  }
};

const canonicalBytes = readFileSync(canonicalPath);
let canonical;
try {
  canonical = JSON.parse(canonicalBytes.toString("utf8"));
} catch (error) {
  fail(`canonical input is invalid JSON: ${String(error)}`);
}
exactKeys(
  canonical,
  ["schema", "version", "transactions", "forcedOrders"],
  "canonical input",
);
if (canonical.schema !== "midgard-transaction-root-v1-canonical-input") {
  fail(
    "canonical input schema is not midgard-transaction-root-v1-canonical-input",
  );
}
if (canonical.version !== 1) fail("canonical input version must be 1");
if (
  !Array.isArray(canonical.transactions) ||
  canonical.transactions.length < 2
) {
  fail("canonical input must contain at least two normal transactions");
}
if (
  !Array.isArray(canonical.forcedOrders) ||
  canonical.forcedOrders.length < 2
) {
  fail("canonical input must contain at least two forced orders");
}

if (!existsSync(buildEntrypointPath)) {
  fail(
    `production build entrypoint does not exist: ${relative(repositoryRoot, buildEntrypointPath)}; run pnpm run fixtures:transaction-root-v1`,
  );
}
const production = await import(pathToFileURL(buildEntrypointPath).href);

const parsedTransactions = canonical.transactions.map((value, index) =>
  parseCanonicalTransaction(
    value,
    `canonical input.transactions[${index.toString()}]`,
  ),
);
const transactionNames = new Set();
for (const transaction of parsedTransactions) {
  if (transactionNames.has(transaction.name)) {
    fail(`duplicate normal transaction name ${transaction.name}`);
  }
  transactionNames.add(transaction.name);
}

const normalEntries = parsedTransactions.map((parsed) => {
  const canonicalCbor = encodeMidgardNativeTxCanonicalV1(parsed.transaction);
  const valueCbor = production.encodeTransactionRootValue(
    canonicalCbor,
    MIDGARD_CONSENSUS_PROFILE_V1,
  );
  const decoded = Data.from(
    valueCbor.toString("hex"),
    SDK.L2TransactionSourceV1,
  );
  const source = decoded;
  const full = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor);
  return {
    name: parsed.name,
    constantPrefix: `transaction_${parsed.name.replaceAll(/[^A-Za-z0-9_]/gu, "_")}`,
    keyHex: source.tx_id,
    valueCborHex: valueCbor.toString("hex"),
    canonicalTransactionCborHex: canonicalCbor.toString("hex"),
    txIdHex: source.tx_id,
    compactCborHex: source.source.compact_cbor,
    witnessSetCompactCborHex: source.source.witness_set_compact_cbor,
    fieldPreimageLengthsCborHex: source.source.field_preimage_lengths_cbor,
    decodedVersion: full.version.toString(),
  };
});

const normalByName = new Map(normalEntries.map((entry) => [entry.name, entry]));
const forcedInputNames = new Set();
const forcedEntries = [];
for (const [index, value] of canonical.forcedOrders.entries()) {
  const label = `canonical input.forcedOrders[${index.toString()}]`;
  exactKeys(
    value,
    ["name", "transaction", "orderId", "verdict"],
    label,
  );
  const name = nonEmptyString(value.name, `${label}.name`);
  if (forcedInputNames.has(name)) fail(`duplicate forced order name ${name}`);
  forcedInputNames.add(name);
  const transaction = normalByName.get(
    nonEmptyString(value.transaction, `${label}.transaction`),
  );
  if (transaction === undefined) {
    fail(`${label}.transaction does not name a normal transaction`);
  }
  exactKeys(
    value.orderId,
    ["transactionId", "outputIndex"],
    `${label}.orderId`,
  );
  const orderTransactionId = hex(
    value.orderId.transactionId,
    `${label}.orderId.transactionId`,
    32,
  );
  const outputIndex = decimal(
    value.orderId.outputIndex,
    `${label}.orderId.outputIndex`,
  );
  if (outputIndex < 0n || outputIndex > 0xffff_ffff_ffff_ffffn) {
    fail(`${label}.orderId.outputIndex must fit uint64`);
  }
  const verdictName = enumValue(
    value.verdict,
    VERDICT_NAMES,
    `${label}.verdict`,
  );
  const orderId = {
    transactionId: orderTransactionId.toString("hex"),
    outputIndex,
  };
  const keyCbor = Buffer.from(Data.to(orderId, SDK.OutputReference), "hex");
  const forced = await Effect.runPromise(
    production.encodeForcedInclusionValueV1({
      nativeTxCbor: Buffer.from(transaction.canonicalTransactionCborHex, "hex"),
      verdict: VERDICTS[verdictName].sdk,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    }),
  );
  const decoded = Data.from(
    forced.value.toString("hex"),
    SDK.ForcedInclusionTxV1,
  );
  if (
    JSON.stringify(decoded.verdict, (_key, item) =>
      typeof item === "bigint" ? item.toString() : item,
    ) !==
    JSON.stringify(VERDICTS[verdictName].sdk, (_key, item) =>
      typeof item === "bigint" ? item.toString() : item,
    )
  ) {
    fail(`${label} production encoder changed the requested operator verdict`);
  }
  forcedEntries.push({
    name,
    constantPrefix: `forced_${name.replaceAll(/[^A-Za-z0-9_]/gu, "_")}`,
    keyHex: keyCbor.toString("hex"),
    valueCborHex: forced.value.toString("hex"),
    orderId: {
      transactionIdHex: orderTransactionId.toString("hex"),
      outputIndex,
    },
    txIdHex: decoded.tx_id,
    compactCborHex: decoded.source.compact_cbor,
    witnessSetCompactCborHex: decoded.source.witness_set_compact_cbor,
    fieldPreimageLengthsCborHex: decoded.source.field_preimage_lengths_cbor,
    canonicalTransactionCborHex: transaction.canonicalTransactionCborHex,
    verdict: verdictName,
  });
}

const buildRoot = async (domain, entries, names) => {
  const built = await Effect.runPromise(
    production.buildAuthenticatedRootFromEncodedEntries(domain, entries),
  );
  const byKey = new Map(names.map((entry) => [entry.keyHex, entry]));
  return {
    domain,
    count: Number(built.count),
    phasRootHex: built.phasRoot,
    rootHex: built.root,
    orderedEntries: built.entries.map((entry) => {
      const keyHex = entry.key.toString("hex");
      const named = byKey.get(keyHex);
      if (named === undefined)
        fail(`root builder returned an unknown key ${keyHex}`);
      return {
        name: named.name,
        keyHex,
        valueCborHex: entry.value.toString("hex"),
      };
    }),
  };
};

const transactionsRoot = await buildRoot(
  SDK.ROOT_DOMAINS.transactionsV1,
  normalEntries.map((entry) => ({
    key: Buffer.from(entry.keyHex, "hex"),
    value: Buffer.from(entry.valueCborHex, "hex"),
  })),
  normalEntries,
);
const forcedTransactionsRoot = await buildRoot(
  SDK.ROOT_DOMAINS.forcedTransactionsV1,
  forcedEntries.map((entry) => ({
    key: Buffer.from(entry.keyHex, "hex"),
    value: Buffer.from(entry.valueCborHex, "hex"),
  })),
  forcedEntries,
);

const attachMembershipProofs = async (entries) => {
  const keys = entries.map((entry) => Buffer.from(entry.keyHex, "hex"));
  const values = entries.map((entry) => Buffer.from(entry.valueCborHex, "hex"));
  return Promise.all(
    entries.map(async (entry) => {
      const proof = await Effect.runPromise(
        production.keyValuePhasProof(
          keys,
          values,
          Buffer.from(entry.keyHex, "hex"),
        ),
      );
      return {
        ...entry,
        proofCborHex: Data.to(proof, SDK.Proof),
      };
    }),
  );
};

const normalEntriesWithProofs = await attachMembershipProofs(normalEntries);
const forcedEntriesWithProofs = await attachMembershipProofs(forcedEntries);

const golden = {
  schema: "midgard-transaction-root-v1-golden",
  version: 1,
  canonicalInput:
    "demo/midgard-node/tests/fixtures/transaction-root-v1.canonical.json",
  canonicalInputSha256: createHash("sha256")
    .update(canonicalBytes)
    .digest("hex"),
  transactions: normalEntriesWithProofs.map(({ decodedVersion, ...entry }) => ({
    ...entry,
    transaction: toJsonTransaction(
      parsedTransactions.find((candidate) => candidate.name === entry.name)
        .transaction,
    ),
    decodedVersion,
  })),
  forcedOrders: forcedEntriesWithProofs.map((entry) => ({
    ...entry,
    orderId: {
      transactionId: entry.orderId.transactionIdHex,
      outputIndex: entry.orderId.outputIndex.toString(),
    },
  })),
  roots: {
    transactions: transactionsRoot,
    forcedTransactions: forcedTransactionsRoot,
  },
};
const goldenJson = `${JSON.stringify(golden, null, 2)}\n`;
const aiken = formatAiken(
  makeAiken({
    transactions: normalEntriesWithProofs,
    forcedOrders: forcedEntriesWithProofs,
    roots: golden.roots,
  }),
);

writeOrCheck(generatedJsonPath, goldenJson);
writeOrCheck(generatedAikenPath, aiken);

if (!checkOnly) {
  process.stdout.write(
    `generated transaction-root-v1 fixture: ${normalEntries.length.toString()} transactions, ${forcedEntries.length.toString()} forced orders\n`,
  );
}
