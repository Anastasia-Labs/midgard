#!/usr/bin/env node

/**
 * Produces the cross-language golden rows for the **serialise loop**
 * (serialiseData, tag 51, semantics group 10): the hops the TypeScript
 * planner gives each value of `tests/fixtures/cek-serialise-loop-v1.vectors.mjs`,
 * run through the TypeScript twin, written to two places:
 *
 *   * `tests/fixtures/cek-serialise-loop-v1.generated.json` — each value's
 *     hop count per plan and each rendered hop's end, shape and modelled
 *     ex-units; and
 *   * `onchain/aiken/validators/fraud-proofs/validation-trace/cek-core-semantic-serialise-loop-golden.test.ak`
 *     — where each rendered hop is spent through the loop validator with the
 *     redeemer and the state the TypeScript twin gives it: the validator must
 *     accept the hop, so its own next state must equal the TypeScript state
 *     (and the last hop must end the loop), and each value's arm 31 witness
 *     must be the one the TypeScript executor encodes. Every hop row has a control row
 *     that builds the same transaction without spending it, so a row's units
 *     less its control's are the hop's own.
 *
 * A plan the TypeScript twin rejects stops the generator.
 *
 * usage: node scripts/generate-cek-serialise-loop-v1-goldens.mjs [--check]
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { buildMidgardCekDataTraverseTrace } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import { Data } from "@lucid-evolution/lucid";

import * as api from "../dist/index.js";
import { values } from "../tests/fixtures/cek-serialise-loop-v1.vectors.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/cek-serialise-loop-v1.generated.json",
);
const aikenFileName = "cek-core-semantic-serialise-loop-golden.test.ak";
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/validators/fraud-proofs/validation-trace",
  aikenFileName,
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-cek-serialise-loop-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

/** The staging output of run `index`, as the Aiken fixture names it. */
const refOf = (index) => ({
  transactionId: computeHash32(Buffer.from([10 + index])),
  outputIndex: 10 + index,
});

const loopOf = (bytes) => {
  const summary = buildMidgardCekDataTraverseTrace({
    sourceStart: 0,
    source: bytes,
  }).terminal.result;
  if (summary === null || summary === undefined) {
    throw new Error("a value is not canonical Data");
  }
  const { facts, chunks } = api.midgardCekSerialiseFactsOf(summary, bytes);
  const runs = [];
  for (let index = 0; index < chunks.length; index += 3) {
    runs.push(chunks.slice(index, index + 3));
  }
  return {
    summary,
    loop: { facts, runs, refs: runs.map((_, index) => refOf(index)) },
  };
};

/** Every hop of `loop`, at most `limit` steps each (the budget's when null). */
const planOf = (loop, limit) => {
  const hops = [];
  let point = api.MIDGARD_CEK_SERIALISE_START;
  while (point !== null) {
    const planned = api.planMidgardCekSerialiseHop(
      loop,
      point,
      limit === null ? {} : { limit },
    );
    const advanced = api.advanceMidgardCekSerialiseHop(
      loop.facts,
      planned.hop,
      planned.referenceInputs,
      planned.from,
      planned.end,
    );
    if (advanced.kind !== (planned.next === null ? "done" : "next")) {
      throw new Error(`the twin gives a planned hop ${advanced.kind}`);
    }
    hops.push(planned);
    point = planned.point;
  }
  return hops;
};

/** The hops rendered as rows: the first two, a middle one, the last two. */
const rendered = (count) =>
  [...new Set([0, 1, Math.floor(count / 2), count - 2, count - 1])].filter(
    (index) => index >= 0 && index < count,
  );

const PLANS = [
  ["limit_1", 1],
  ["limit_4", 4],
  ["limit_32", 32],
  ["limit_256", 256],
  ["budget", null],
];

const cbor = (data) => Data.to(data);

const values_ = values.map((value) => {
  const bytes = Buffer.from(value.hex, "hex");
  const { summary, loop } = loopOf(bytes);
  const plans = (value.maximum ? PLANS.slice(-1) : PLANS).map(
    ([plan, limit]) => {
      const hops = planOf(loop, limit);
      return {
        plan,
        count: hops.length,
        rows: rendered(hops.length).map((index) => ({
          index,
          planned: hops[index],
        })),
      };
    },
  );
  return { value, summary, loop, plans };
});

// ## Aiken rendering

const PIECE_BYTES = 16_380;

/** A value's parts as Aiken pieces of at most about 16 KB each. */
const piecesOf = (parts) =>
  parts.flatMap((part) => {
    if (typeof part === "string") {
      if (part.length === 0) return [];
      const pieces = [];
      for (let offset = 0; offset < part.length; offset += 2 * PIECE_BYTES) {
        pieces.push(aikenBytes(part.slice(offset, offset + 2 * PIECE_BYTES)));
      }
      return pieces;
    }
    const [unit, count] = part;
    const per = Math.max(1, Math.floor(PIECE_BYTES / (unit.length / 2)));
    const pieces = [];
    for (let done = 0; done < count; done += per) {
      pieces.push(
        `repeat_hex(${aikenBytes(unit)}, ${Math.min(per, count - done)})`,
      );
    }
    return pieces;
  });

/** The arm 31 witness the TypeScript executor encodes for the value's step. */
const witnessCbor = (loop) =>
  cbor(
    api.midgardCekCoreStepWitnessData({
      kind: "executeBuiltinSerialise",
      arguments: loop.facts.arguments,
      result: loop.facts.result,
      serialised: Buffer.alloc(0),
    }),
  );

/**
 * The loop's last hop checks the summary its reader reaches against this one,
 * and the witness the budget hop binds must be the one the TypeScript
 * executor encodes.
 */
const valueSource = ({
  value,
  summary,
  loop,
}) => `fn ${value.name}_case() -> SerialiseCaseV1 {
  let pieces = [${piecesOf(value.parts).join(", ")}]
  case_of(
    data_value(DataSummaryV1 { root: ${aikenBytes(hex(summary.root))}, cbor_length: ${summary.cborLength}, memory: ${summary.memory} }),
    bytes_result(pieces),
    pieces,
  )
}

test ${value.name}_witness_is_the_executors() {
  cbor.serialise(${value.name}_case().witness) == ${aikenBytes(witnessCbor(loop))}
}`;

const hopExpression = (value, planned) => {
  const from =
    planned.from === null
      ? "None"
      : `Some(Pair(${planned.from.cursor}, ${aikenBytes(cbor(api.midgardCekSerialiseStateData(planned.from.state)))}))`;
  const next =
    planned.next === null
      ? "None"
      : `Some(${aikenBytes(cbor(api.midgardCekSerialiseStateData(planned.next)))})`;
  const runs = planned.referenceInputs.map(
    (input) => input.outRef.outputIndex - 10,
  );
  return `hop_row(${value.name}_case(), ${from}, ${planned.end}, ${aikenBytes(cbor(api.midgardCekSerialiseHopData(planned.hop)))}, ${next}, [${runs.join(", ")}])`;
};

const rowName = (value, plan, index) => `${value.name}_${plan}_hop_${index}`;

const rowSource = (value, plan, { index, planned }) => {
  const name = rowName(value, plan, index);
  return `test ${name}() {
  spend(${hopExpression(value, planned)})
}

test ${name}_control() {
  built(${hopExpression(value, planned)})
}`;
};

const PREAMBLE = `//// Generated by demo/midgard-validation/scripts/generate-cek-serialise-loop-v1-goldens.mjs.
//// Do not edit; regenerate with
//// \`pnpm --dir demo/midgard-validation run fixtures:cek-serialise-loop-v1:sync\`.
////
//// Serialise loop hops (serialiseData, arm 31, semantics group 10) planned
//// by the TypeScript twin: the loop validator must accept each hop with the
//// redeemer and the next state the twin gives it. A row's units less its
//// control row's are the hop's own. Each value's witness row checks that the
//// arm 31 witness is the one the TypeScript executor encodes.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use cardano/transaction.{Input}
use fraud_proofs/validation_trace/cek_core_semantic_serialise_loop as serialise_loop
use midgard/cek_data_v1.{DataSummaryV1}
use midgard/cek_serialise_loop_v1.{SerialiseHopV1}
use midgard/fraud_proofs/cek_serialise_fixture_v1.{
  PlannedHopV1, SerialiseCaseV1, SerialiseStepV1, bytes_result, case_of,
  data_value, hop_step_over,
}
use midgard/fraud_proofs/cek_staged_fixture_v1.{other_input}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

/// \`count\` copies of \`piece\`.
fn repeat_hex(piece: ByteArray, count: Int) -> ByteArray {
  if count == 0 {
    #""
  } else if count % 2 == 0 {
    let half = repeat_hex(piece, count / 2)
    builtin.append_bytearray(half, half)
  } else {
    builtin.append_bytearray(piece, repeat_hex(piece, count - 1))
  }
}

fn decoded(bytes: ByteArray) -> Data {
  expect Some(data) = cbor.deserialise(bytes)
  data
}

/// The hop of \`case\` the TypeScript twin planned, over the staging outputs
/// of \`runs\` (in ledger order).
fn hop_row(
  case: SerialiseCaseV1,
  from: Option<Pair<Int, ByteArray>>,
  end: Int,
  hop: ByteArray,
  next: Option<ByteArray>,
  runs: List<Int>,
) -> SerialiseStepV1 {
  expect hop: SerialiseHopV1 = decoded(hop)
  let from =
    when from is {
      None -> None
      Some(Pair(cursor, state)) -> Some(Pair(cursor, decoded(state)))
    }
  let next =
    when next is {
      None -> None
      Some(state) -> Some(decoded(state))
    }
  let inputs: List<Input> =
    list.map(
      runs,
      fn(run) {
        expect Some(input) = list.at(case.reference_inputs, run + 1)
        input
      },
    )
  hop_step_over(
    case,
    PlannedHopV1 { from, end, hop, next },
    [other_input(1), ..inputs],
  )
}

fn spend(step: SerialiseStepV1) -> Bool {
  serialise_loop.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// The step is built (its control row): every part the spend reads.
fn built(step: SerialiseStepV1) -> Bool {
  expect Some(_) = step.datum
  expect [_, ..] = step.transaction.reference_inputs
  expect [_, ..] = step.transaction.outputs
  True
}`;

const source = [
  PREAMBLE,
  ...values_.flatMap(({ value, summary, loop, plans }) => [
    valueSource({ value, summary, loop }),
    ...plans.flatMap(({ plan, rows }) =>
      rows.map((row) => rowSource(value, plan, row)),
    ),
  ]),
].join("\n\n");

const shapeJson = (shape) => ({
  ...shape,
  steps: { ...shape.steps },
});

writeOrCheck(
  generatedJsonPath,
  `${JSON.stringify(
    {
      generator:
        "demo/midgard-validation/scripts/generate-cek-serialise-loop-v1-goldens.mjs",
      aikenModule: `onchain/aiken/validators/fraud-proofs/validation-trace/${aikenFileName}`,
      values: values_.map(({ value, plans }) => ({
        name: value.name,
        bytes: value.hex.length / 2,
        plans: plans.map(({ plan, count, rows }) => ({
          plan,
          hops: count,
          rows: rows.map(({ index, planned }) => ({
            name: rowName(value, plan, index),
            from: planned.from?.cursor ?? 0,
            end: planned.end,
            shape: shapeJson(planned.shape),
            model: {
              memory: Number(planned.cost.memory),
              cpu: Number(planned.cost.cpu),
            },
          })),
        })),
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
    tmpPrefix: "cek-serialise-loop-v1-",
  }),
);
