#!/usr/bin/env node

/**
 * Produces the cross-language golden rows for the **staged builtin arms**
 * (arm 31, semantics groups 7 and 8): every row of
 * `tests/fixtures/cek-staged-arm-v1.vectors.mjs` built by the TypeScript twin
 * and judged by `verifyMidgardCekStagedBuiltin`, then written to two places:
 *
 *   * `tests/fixtures/cek-staged-arm-v1.generated.json` — the hash of each
 *     row's witness and its verdict, recomputed by
 *     `tests/cek-staged-arm-goldens.test.ts`, which drives the same rows
 *     through `src/`; and
 *   * `onchain/aiken/validators/fraud-proofs/validation-trace/cek-core-staged-golden.test.ak`
 *     — where each row's step is rebuilt (its staging outputs and inline
 *     sources from the same byte constructions, its witness values as the
 *     TypeScript twin carries them), its witness is required to hash to the
 *     TypeScript witness, and the staged bytes or integer semantics validator
 *     must accept exactly the rows the TypeScript verifier accepts.
 *
 * A row whose verdict differs from the verdict the row declares stops the
 * generator: the rows are what both verifiers must reach, not a record of
 * what one of them happens to do.
 *
 * usage: node scripts/generate-cek-staged-arm-v1-goldens.mjs [--check]
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

import * as api from "../dist/index.js";
import {
  buildCekStagedArmRow,
  CEK_STAGED_ARM_ROWS,
} from "../tests/fixtures/cek-staged-arm-v1.vectors.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/cek-staged-arm-v1.generated.json",
);
const aikenFileName = "cek-core-staged-golden.test.ak";
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/validators/fraud-proofs/validation-trace",
  aikenFileName,
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-cek-staged-arm-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const names = new Set(CEK_STAGED_ARM_ROWS.map((row) => row.name));
if (names.size !== CEK_STAGED_ARM_ROWS.length) {
  throw new Error("two rows share a name");
}

const built = CEK_STAGED_ARM_ROWS.map((row) => {
  const result = buildCekStagedArmRow(row, api);
  if (result.verdict !== row.accepts) {
    throw new Error(
      `${row.name}: the TypeScript verifier ${result.verdict ? "accepts" : "refuses"} it`,
    );
  }
  return { row, ...result };
});

// ## Aiken rendering

/** A byte construction as Aiken pieces of at most 65,536 bytes each. */
const piecesOf = (spec) => {
  if (spec.pattern) return [`pattern(${spec.pattern[0]}, ${spec.pattern[1]})`];
  if (spec.text !== undefined) return [`text(${spec.text})`];
  if (spec.hex !== undefined) return [aikenBytes(spec.hex)];
  if (spec.repeat) return [`repeat_byte(${spec.repeat[1]}, ${spec.repeat[0]})`];
  if (spec.concat) return spec.concat.flatMap(piecesOf);
  if (spec.flipLast) return [`flip_last_byte(${single(spec.flipLast)})`];
  if (spec.integerCbor) {
    const { negative, argument } = spec.integerCbor;
    return [
      `encode_integer_cbor_bulk_v1(CanonicalIntegerV1 { negative: ${negative ? "True" : "False"}, argument: ${single(argument)} })`,
    ];
  }
  throw new Error(`unknown bytes spec ${JSON.stringify(spec)}`);
};

/** A byte construction of at most 65,536 bytes as one Aiken expression. */
const single = (spec) =>
  piecesOf(spec).reduce(
    (left, right) => `builtin.append_bytearray(${left}, ${right})`,
  );

const piecesList = (spec) => `[${piecesOf(spec).join(", ")}]`;

const valueExpression = (witness) => {
  if (witness.kind === "constant") {
    return `ConstantValue(ConstantWitnessV1 { type_cbor: ${aikenBytes(hex(witness.witness.typeCbor))}, payload_cbor: ${aikenBytes(hex(witness.witness.payloadCbor))} })`;
  }
  if (witness.kind === "semanticConstant") {
    const { typeCbor, payload, memory } = witness.witness;
    return `SemanticConstantValue { type_cbor: ${aikenBytes(hex(typeCbor))}, payload: DataSummaryV1 { root: ${aikenBytes(hex(payload.root))}, cbor_length: ${payload.cborLength}, memory: ${payload.memory} }, memory: ${memory} }`;
  }
  throw new Error(`a staged row carries a ${witness.kind} value`);
};

const sourceExpression = (source, witnessSource) => {
  if (source.inline) return `InlineOperand { bytes: ${single(source.inline)} }`;
  if (source.staged !== undefined) return `staged(${source.staged})`;
  if (source.bytesLeaf) {
    return `BytesLeafOperand { root: flat_root_of_pieces_v1(${piecesList(source.bytesLeaf)}), length: ${witnessSource.length} }`;
  }
  if (source.integerLeaf) {
    return `IntegerLeafOperand { cbor_root: flat_root_v1(${single({ integerCbor: source.integerLeaf })}), cbor_length: ${witnessSource.cborLength}, words: ${witnessSource.words} }`;
  }
  return `SmallIntegerOperand { value: ${source.small} }`;
};

const stagingExpression = (row) =>
  row.staged.length === 0
    ? "no_staging()"
    : `stage_pieces([${row.staged.map(piecesList).join(", ")}])`;

const validatorOf = (row) =>
  api.midgardCekSemanticGroup(BigInt(row.tag)) === 7
    ? "staged_bytes"
    : "staged_integer";

const rowSource = ({
  row,
  witness,
  witnessHash,
}) => `fn ${row.name}_step() -> StagedStepV1 {
  let staging = ${stagingExpression(row)}
  staged_step(
    ${row.tag},
    [${witness.arguments.map(valueExpression).join(", ")}],
    ${valueExpression(witness.result)},
    material(staging, [${row.sources.map((source, index) => sourceExpression(source, witness.material.sources[index])).join(", ")}]),
    staging.reference_inputs,
  )
}

test ${row.name}_has_the_typescript_witness() {
  let step = ${row.name}_step()
  expect Some(datum) = step.datum
  expect Some(bound) = datum.data
  bound.witness_hash == ${aikenBytes(witnessHash)}
}

test ${row.name}() ${row.accepts ? "" : "fail "}{
  spend_${validatorOf(row)}(${row.name}_step())
}`;

const PREAMBLE = `//// Generated by demo/midgard-validation/scripts/generate-cek-staged-arm-v1-goldens.mjs.
//// Do not edit; regenerate with
//// \`pnpm --dir demo/midgard-validation run fixtures:cek-staged-arm-v1:sync\`.
////
//// Staged builtin steps (arm 31) built by the TypeScript twin: each step's
//// witness must hash to the TypeScript witness, and the staged semantics
//// validators must accept exactly the steps the TypeScript verifier accepts.

use aiken/builtin
use fraud_proofs/validation_trace/cek_core_staged_bytes as staged_bytes
use fraud_proofs/validation_trace/cek_core_staged_integer as staged_integer
use midgard/cek_builtin_v1.{ConstantValue, SemanticConstantValue}
use midgard/cek_constant_v1.{ConstantWitnessV1}
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/cek_staged_arm_v1.{
  BytesLeafOperand, InlineOperand, IntegerLeafOperand, SmallIntegerOperand,
}
use midgard/cek_staged_blob_v1.{flat_root_of_pieces_v1, flat_root_v1}
use midgard/cek_staged_integer_cbor_v1.{encode_integer_cbor_bulk_v1}
use midgard/cek_staged_integer_v1.{CanonicalIntegerV1}
use midgard/fraud_proofs/cek_staged_fixture_v1.{
  StagedStepV1, flip_last_byte, material, no_staging, pattern, stage_pieces,
  staged, staged_step, text,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

/// \`count\` copies of \`byte\`, built 8,192 at a time.
fn repeat_byte(count: Int, byte: Int) -> ByteArray {
  if count <= 8192 {
    builtin.replicate_byte(count, byte)
  } else {
    builtin.append_bytearray(
      builtin.replicate_byte(8192, byte),
      repeat_byte(count - 8192, byte),
    )
  }
}

fn spend_staged_bytes(step: StagedStepV1) -> Bool {
  staged_bytes.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

fn spend_staged_integer(step: StagedStepV1) -> Bool {
  staged_integer.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}`;

const source = [PREAMBLE, ...built.map(rowSource)].join("\n\n");

writeOrCheck(
  generatedJsonPath,
  `${JSON.stringify(
    {
      generator:
        "demo/midgard-validation/scripts/generate-cek-staged-arm-v1-goldens.mjs",
      aikenModule: `onchain/aiken/validators/fraud-proofs/validation-trace/${aikenFileName}`,
      rows: built.map(({ row, witnessHash, verdict }) => ({
        name: row.name,
        tag: row.tag,
        accepts: verdict,
        witnessHash,
      })),
    },
    null,
    2,
  )}\n`,
);
writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source: `${source}\n`,
    fileName: aikenFileName,
    repositoryRoot,
    tmpPrefix: "cek-staged-arm-v1-",
  }),
);
