#!/usr/bin/env node

/**
 * The `forced-tx-admission-v1` cross-language golden channel: the rules the
 * tx-order mint applies to the forced transaction an order carries.
 *
 * Each row is a forced transaction, spelled here byte by byte from the §2.5 and
 * §5.1 layouts rather than through the package's encoders, and labelled by
 * hand with the check that must refuse it, or with null when it must be
 * admitted. The generator refuses to emit a row the TypeScript screen
 * disagrees with, and two consumers recompute every row:
 *
 *   * `tests/forced-tx-admission-v1.test.ts` runs
 *     `screenMidgardForcedTxAdmission` on each row and expects its label; and
 *   * `onchain/aiken/lib/midgard/user-events/forced-tx-admission-v1-golden.test.ak`
 *     runs `forced_tx_is_admissible_v1` on the same bytes and expects the same
 *     verdict and the same first failing check.
 *
 * Every row is one the mint's field door accepts: each field is a §5.1
 * envelope whose item count agrees with its bytes, checked below, so the rows
 * exercise the admission rules and nothing before them.
 *
 * usage: node scripts/generate-forced-tx-admission-v1-goldens.mjs [--check]
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { decodeMidgardTxOutput } from "../dist/codec/output.js";
import { encodeMidgardMintPolicyItem } from "../dist/codec/native-tx-field-items.js";
import { midgardValueToCmlValue } from "../dist/codec/value.js";
import {
  MIDGARD_FORCED_TX_ADMISSION_CHECKS,
  screenMidgardForcedTxAdmission,
} from "../dist/consensus-validation.js";
import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";
import {
  ADDRESS_WITNESSES,
  canonicalMintRows,
  head,
  MINT,
  OUTPUTS,
  REDEEMERS,
  REFERENCE_INPUTS,
  REQUIRED_OBSERVERS,
  REQUIRED_SIGNERS,
  rows,
  SCRIPT_WITNESSES,
  SPEND_INPUTS,
} from "./generate-forced-tx-admission-v1-goldens.rows.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/forced-tx-admission-v1.generated.json",
);
const aikenFileName = "forced-tx-admission-v1-golden.test.ak";
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/user-events",
  aikenFileName,
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-forced-tx-admission-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const fail = (message) => {
  throw new Error(`forced-tx-admission-v1: ${message}`);
};

// ## Every row reaches the admission rules

const FIXED_STRIDES = new Map([
  [SPEND_INPUTS, 40],
  [REFERENCE_INPUTS, 40],
  [REQUIRED_OBSERVERS, 30],
  [REQUIRED_SIGNERS, 30],
  [ADDRESS_WITNESSES, 103],
]);
const fieldHeader = (preimage) => {
  const tag = preimage[0];
  if (tag >= 0x80 && tag <= 0x97) return [1, tag - 0x80];
  if (tag === 0x98) return [2, preimage[1]];
  if (tag === 0x99) return [3, preimage.readUInt16BE(1)];
  return fail(`field header ${tag}`);
};
/** The mint's field door: fixed fields by arithmetic, the rest by walking. */
const assertDoorAccepts = (name, index, preimage) => {
  const [headerLength, count] = fieldHeader(preimage);
  const stride = FIXED_STRIDES.get(index);
  if (stride !== undefined) {
    if (headerLength + stride * count !== preimage.length) {
      fail(`${name}: field ${index} length disagrees with its count`);
    }
    return;
  }
  let offset = headerLength;
  for (let item = 0; item < count; item += 1) {
    const tag = preimage[offset];
    const [start, length] =
      tag < 0x58
        ? [offset + 1, tag - 0x40]
        : tag === 0x58
          ? [offset + 2, preimage[offset + 1]]
          : [offset + 3, preimage.readUInt16BE(offset + 1)];
    if (!head(2, length).equals(preimage.subarray(offset, start))) {
      fail(`${name}: field ${index} item ${item} head is not minimal`);
    }
    offset = start + length;
  }
  if (offset !== preimage.length) fail(`${name}: field ${index} walk`);
};

// The value rows' outputs are canonical outputs whose Cardano value size is
// the value's own byte count.
for (const row of rows.filter(({ name }) => name.startsWith("value_of_"))) {
  const [, items] = fieldHeader(row.fields[OUTPUTS]);
  if (items !== 1) fail(`${row.name}: one output expected`);
  const outputBytes = row.fields[OUTPUTS].subarray(4);
  const decoded = decodeMidgardTxOutput(outputBytes);
  const cardanoSize = midgardValueToCmlValue(decoded.value).to_cbor_bytes()
    .length;
  const size = Number(row.name.split("_")[2]);
  if (cardanoSize !== size) {
    fail(`${row.name}: Cardano value is ${cardanoSize} bytes, not ${size}`);
  }
}

// The admitted mint rows' items are the bytes the package's canonical mint
// encoder writes, so the channel's fixtures and the producers agree.
for (const { name, entries } of canonicalMintRows) {
  const row = rows.find((candidate) => candidate.name === name);
  const preimage = row.fields[MINT];
  const [headerLength] = fieldHeader(preimage);
  let offset = headerLength;
  for (const entry of entries) {
    const encoded = encodeMidgardMintPolicyItem(entry);
    const itemHead = head(2, encoded.length);
    const item = preimage.subarray(
      offset,
      offset + itemHead.length + encoded.length,
    );
    if (!item.equals(Buffer.concat([itemHead, encoded]))) {
      fail(`${name}: mint item disagrees with encodeMidgardMintPolicyItem`);
    }
    offset += item.length;
  }
  if (offset !== preimage.length) fail(`${name}: mint items left over`);
}

const admissionInput = (row) => ({
  body: {
    spendInputsPreimageCbor: row.fields[SPEND_INPUTS],
    referenceInputsPreimageCbor: row.fields[REFERENCE_INPUTS],
    outputsPreimageCbor: row.fields[OUTPUTS],
    validityIntervalEnd: row.validityIntervalEnd,
    requiredObserversPreimageCbor: row.fields[REQUIRED_OBSERVERS],
    requiredSignersPreimageCbor: row.fields[REQUIRED_SIGNERS],
    mintPreimageCbor: row.fields[MINT],
    scriptIntegrityHash: row.scriptIntegrityHash,
    auxiliaryDataHash: row.auxiliaryDataHash,
    networkId: row.networkId,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: row.fields[ADDRESS_WITNESSES],
    scriptTxWitsPreimageCbor: row.fields[SCRIPT_WITNESSES],
    redeemerTxWitsPreimageCbor: row.fields[REDEEMERS],
  },
});

const names = new Set();
for (const row of rows) {
  if (names.has(row.name)) fail(`duplicate row ${row.name}`);
  names.add(row.name);
  row.fields.forEach((preimage, index) =>
    assertDoorAccepts(row.name, index, preimage),
  );
  if (
    row.expected !== null &&
    !MIDGARD_FORCED_TX_ADMISSION_CHECKS.includes(row.expected)
  ) {
    fail(`${row.name}: unknown check ${row.expected}`);
  }
  const refusal = screenMidgardForcedTxAdmission(admissionInput(row));
  const actual = refusal === null ? null : refusal.check;
  const halts = refusal !== null && refusal.detail.startsWith("malformed:");
  if (halts !== (row.halts === true)) {
    fail(`${row.name}: the screen ${halts ? "halts" : "does not halt"}`);
  }
  if (actual !== row.expected) {
    fail(
      `${row.name}: the screen says ${actual}, the row says ${row.expected}`,
    );
  }
}

// ## Emission

const golden = {
  checks: MIDGARD_FORCED_TX_ADMISSION_CHECKS,
  rows: rows.map((row) => ({
    name: row.name,
    expected: row.expected,
    halts: row.halts === true,
    fields: row.fields.map(hex),
    validityIntervalEnd: row.validityIntervalEnd.toString(),
    scriptIntegrityHash: hex(row.scriptIntegrityHash),
    auxiliaryDataHash: hex(row.auxiliaryDataHash),
    networkId: row.networkId.toString(),
  })),
};
writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);

const refusalIndex = (expected) =>
  expected === null ? -1 : MIDGARD_FORCED_TX_ADMISSION_CHECKS.indexOf(expected);

const firstRefusalBody = MIDGARD_FORCED_TX_ADMISSION_CHECKS.map(
  (check, index) => `if !${check} {\n    ${index}\n  } else `,
).join("");

const rowViews = (row, fields) => `row_views(
      [${fields.map((preimage) => aikenBytes(hex(preimage))).join(", ")}],
      ${row.validityIntervalEnd.toString()},
      ${aikenBytes(hex(row.scriptIntegrityHash))},
      ${aikenBytes(hex(row.auxiliaryDataHash))},
      ${row.networkId.toString()},
    )`;

// A row whose check halts the mint cannot be compared by value: its test
// expects the mint to fail, and a second test expects every other check to pass
// with the halting field emptied. Only the mint field's check halts, and it is
// the last check.
const lastCheck = MIDGARD_FORCED_TX_ADMISSION_CHECKS.length - 1;
const haltingRowTests = (row) => {
  if (refusalIndex(row.expected) !== lastCheck) {
    fail(`${row.name}: only the last check may halt`);
  }
  const emptied = row.fields.map((preimage, index) =>
    index === MINT ? Buffer.from([0x80]) : preimage,
  );
  return `
test forced_tx_admission_${row.name}() fail {
  forced_tx_is_admissible_v1(${rowViews(row, row.fields)})
}

test forced_tx_admission_${row.name}_passes_the_other_checks() {
  first_refusal(${rowViews(row, emptied)}) == -1
}
`;
};

const rowTests = rows
  .map((row) =>
    row.halts === true
      ? haltingRowTests(row)
      : `
test forced_tx_admission_${row.name}() {
  let views =
    ${rowViews(row, row.fields)}
  and {
    first_refusal(views) == ${refusalIndex(row.expected)},
    forced_tx_is_admissible_v1(views) == ${row.expected === null ? "True" : "False"},
  }
}
`,
  )
  .join("");

const aikenSource = `//// Generated by
//// demo/midgard-core/scripts/generate-forced-tx-admission-v1-goldens.mjs. Do
//// not edit; regenerate.
////
//// The forced-transaction admission rules of the tx-order mint, on the rows the
//// TypeScript screen \`screenMidgardForcedTxAdmission\` is checked against. Each
//// test expects the row's verdict and the index of the first check that
//// refuses it (-1 when none does), in the order the mint runs the checks.

use aiken/collection/list
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1.{
  Whole, decode_field_array_header, field_commitment, field_stride,
}
use midgard/user_events/tx_order_v1.{
  OrderMaterialViewsV1, address_witness_items_are_fixed, forced_tx_is_admissible_v1,
  has_a_spend_input, has_no_auxiliary_data, has_no_redeemers,
  has_no_validity_upper_bound, hash28_items_are_fixed, input_items_are_fixed,
  is_not_observed_on_untagged_network, mint_items_are_valid,
  outputs_are_admissible, script_witnesses_are_native,
}

/// The views the mint hands the rules: the nine fields as the door returns
/// them, \`None\` for an empty one, and the body and witness set committing to
/// them.
fn row_views(
  preimages: List<ByteArray>,
  validity_interval_end: Int,
  script_integrity_hash: ByteArray,
  auxiliary_data_hash: ByteArray,
  network_id: Int,
) -> OrderMaterialViewsV1 {
  expect [p0, p1, p2, p3, p4, p5, p6, p7, p8] = preimages
  OrderMaterialViewsV1 {
    body: NativeTxBodyCompact {
      spend_inputs_hash: field_commitment(p0),
      reference_inputs_hash: field_commitment(p1),
      outputs_hash: field_commitment(p2),
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end,
      required_observers_hash: field_commitment(p3),
      required_signers_hash: field_commitment(p4),
      mint_hash: field_commitment(p5),
      script_integrity_hash,
      auxiliary_data_hash,
      network_id,
    },
    witness_set: NativeTxWitnessSetCompact {
      addr_tx_wits_hash: field_commitment(p7),
      script_tx_wits_hash: field_commitment(p6),
      redeemer_tx_wits_hash: field_commitment(p8),
    },
    fields: list.indexed_map(
      preimages,
      fn(index, preimage) {
        if preimage == #"80" {
          None
        } else {
          let Pair(_, count) = decode_field_array_header(preimage)
          Some(Whole { bytes: preimage, count, stride: field_stride(index) })
        }
      },
    ),
  }
}

/// The index of the first check that refuses, in the mint's order, or -1.
fn first_refusal(views: OrderMaterialViewsV1) -> Int {
  let OrderMaterialViewsV1 { body, witness_set, fields } = views
  expect [
    spend_inputs, reference_inputs, outputs, required_observers,
    required_signers, mint, script_witnesses, address_witnesses, _redeemers,
  ] = fields
  ${firstRefusalBody}{
    -1
  }
}
${rowTests}`;

writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source: aikenSource,
    fileName: aikenFileName,
    repositoryRoot,
    tmpPrefix: "midgard-forced-tx-admission-v1-aiken-format-",
  }),
);
