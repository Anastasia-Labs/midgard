#!/usr/bin/env node

/**
 * Produces the cross-language golden rows for the **writeBits loop**
 * (writeBits, tag 80, semantics index 12): the hops the TypeScript planner
 * gives each step of `tests/fixtures/cek-write-bits-loop-v1.vectors.mjs`,
 * run through the TypeScript twin, written to two places:
 *
 *   * `tests/fixtures/cek-write-bits-loop-v1.generated.json` — each step's
 *     hop count per plan and each rendered hop's end, shape and modelled
 *     ex-units; and
 *   * `onchain/aiken/validators/fraud-proofs/validation-trace/cek-core-staged-write-bits-loop-golden.test.ak`
 *     — where each rendered hop is spent through the loop validator with the
 *     redeemer and the state the TypeScript twin gives it: the validator must
 *     accept the hop, so its own next state must equal the TypeScript state
 *     (and the last hop must end the loop), and each step's witness, taken
 *     from a proof-mode run of the TypeScript executor, must be the one the
 *     Aiken fixture builds from its parts. Every hop row has a control row
 *     that builds the same transaction without spending it, so a row's units
 *     less its control's are the hop's own.
 *
 * A plan the TypeScript twin rejects stops the generator.
 *
 * usage: node scripts/generate-cek-write-bits-loop-v1-goldens.mjs [--check]
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { encodeCbor } from "@al-ft/midgard-core";
import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";
import { DataB } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  constT,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
} from "@harmoniclabs/uplc";
import { Data } from "@lucid-evolution/lucid";

import * as api from "../dist/index.js";
import { steps } from "../tests/fixtures/cek-write-bits-loop-v1.vectors.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/cek-write-bits-loop-v1.generated.json",
);
const aikenFileName = "cek-core-staged-write-bits-loop-golden.test.ak";
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/validators/fraud-proofs/validation-trace",
  aikenFileName,
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-cek-write-bits-loop-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

const LOOP_KINDS = new Set([
  "executeBuiltinWriteBits",
  "executeBuiltinWriteBitsFailure",
]);

/** The writeBits loop witness of a proof-mode run of writeBits over the
 * step's constants. */
const witnessOf = (step) => {
  const apply = (term, argument) => new Application(term, argument);
  const body = [
    UPLCConst.byteString(new DataB(step.input).bytes),
    UPLCConst.listOf(constT.int)(
      step.indices.map((index) => UPLCConst.int(index).value),
    ),
    UPLCConst.bool(step.set),
  ].reduce(apply, Builtin.writeBits);
  // `(\_. ()) body` under the script's context lambda.
  const program = new UPLCProgram(
    [1, 1, 0],
    new Lambda(apply(new Lambda(UPLCConst.unit), body)),
  );
  const outcome = api.evaluateMidgardCekScript(
    {
      scriptBytes: encodeCbor(Buffer.from(UPLCEncoder.compile(program))),
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.from("d87980", "hex"),
      executionIndex: 0n,
      maxSteps: 10_000,
      executionBudget: { cpu: 10_000_000_000n, memory: 16_500_000n },
    },
    "proof",
  );
  const found = (outcome.execution?.steps ?? []).filter(({ witness }) =>
    LOOP_KINDS.has(witness.kind),
  );
  if (found.length !== 1) {
    throw new Error(`${step.name} does not take the writeBits loop once`);
  }
  return found[0].witness;
};

/** Every hop of `loop`, at most `limit` indices each (the budget's when
 * null), each checked against the twin. */
const planOf = (loop, limit) =>
  api.planMidgardCekWriteBitsHops(loop, limit === null ? {} : { limit });

/** The hops rendered as rows: the first two, a middle one, the last two. */
const rendered = (count) =>
  [...new Set([0, 1, Math.floor(count / 2), count - 2, count - 1])].filter(
    (index) => index >= 0 && index < count,
  );

const PLANS = [
  ["limit_1", 1],
  ["limit_8", 8],
  ["limit_32", 32],
  ["budget", null],
];

const cbor = (data) => Data.to(data);

const steps_ = steps.map((step) => {
  const witness = witnessOf(step);
  const loop = api.midgardCekWriteBitsLoopOfWitness(witness);
  const plans = PLANS.map(([plan, limit]) => {
    const hops = planOf(loop, limit);
    return {
      plan,
      count: hops.length,
      charge: hops.at(-1).charge,
      rows: rendered(hops.length).map((index) => ({
        index,
        planned: hops[index],
      })),
    };
  });
  return { step, witness, loop, plans };
});

// ## Aiken rendering

const stateCbor = (failure, state) =>
  cbor(
    failure
      ? api.midgardCekWriteBitsWalkData(state)
      : api.midgardCekWriteBitsStateData(state),
  );

const witnessCbor = (witness) =>
  cbor(api.midgardCekCoreStepWitnessData(witness));

/**
 * The step's case, built by the Aiken fixture from the parts of the witness
 * the TypeScript executor encodes, charged as the twin's last hop charges it.
 */
const stepSource = ({ step, witness, plans }) => {
  const charge = plans[0].charge ?? { cpu: 0n, memory: 0n };
  return `fn ${step.name}_case() -> WriteBitsCaseV1 {
  case_of(
    ${aikenBytes(witnessCbor(witness))},
    ${aikenBytes(hex(step.input))},
    [${step.indices.join(", ")}],
    ${charge.cpu},
    ${charge.memory},
  )
}

test ${step.name}_witness_is_the_executors() {
  cbor.serialise(${step.name}_case().witness) == ${aikenBytes(witnessCbor(witness))}
}`;
};

const hopExpression = (step, failure, planned) => {
  const from =
    planned.from === null
      ? "None"
      : `Some(Pair(${planned.from.cursor}, ${aikenBytes(stateCbor(failure, planned.from.state))}))`;
  const next =
    planned.next === null
      ? "None"
      : `Some(${aikenBytes(stateCbor(failure, planned.next))})`;
  return `hop_row(${step.name}_case(), ${from}, ${planned.end}, ${aikenBytes(cbor(api.midgardCekWriteBitsHopData(planned.hop)))}, ${next})`;
};

const rowName = (step, plan, index) => `${step.name}_${plan}_hop_${index}`;

const rowSource = (step, failure, plan, { index, planned }) => {
  const name = rowName(step, plan, index);
  return `test ${name}() {
  spend(${hopExpression(step, failure, planned)})
}

test ${name}_control() {
  built(${hopExpression(step, failure, planned)})
}`;
};

const PREAMBLE = `//// Generated by demo/midgard-validation/scripts/generate-cek-write-bits-loop-v1-goldens.mjs.
//// Do not edit; regenerate with
//// \`pnpm --dir demo/midgard-validation run fixtures:cek-write-bits-loop-v1:sync\`.
////
//// writeBits loop hops (writeBits, arm 31 and arm 36, semantics index 12)
//// planned by the TypeScript twin: the loop validator must accept each hop
//// with the redeemer and the next state the twin gives it. A row's units
//// less its control row's are the hop's own. Each step's witness row checks
//// that the witness the TypeScript executor encodes is the one the fixture
//// builds from its parts.

use aiken/builtin
use aiken/cbor
use fraud_proofs/validation_trace/cek_core_staged_write_bits_loop as write_bits_loop
use midgard/cek_builtin_v1.{ValueWitnessV1}
use midgard/cek_write_bits_loop_v1.{WriteBitsHopV1}
use midgard/fraud_proofs/cek_write_bits_fixture_v1.{
  PlannedHopV1, WriteBitsCaseV1, WriteBitsStepV1, failure_case_charging,
  hop_step, success_case_claiming,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

fn decoded(bytes: ByteArray) -> Data {
  expect Some(data) = cbor.deserialise(bytes)
  data
}

/// The case of the step whose witness the TypeScript executor encodes as
/// \`witness\`, a success charging \`cpu\` and \`memory\`.
fn case_of(
  witness: ByteArray,
  input: ByteArray,
  indices: List<Int>,
  cpu: Int,
  memory: Int,
) -> WriteBitsCaseV1 {
  expect Pair(arm, fields) = builtin.un_constr_data(decoded(witness))
  if arm == 31 {
    expect [_, arguments_data, result_data, _] = fields
    expect arguments: List<ValueWitnessV1> = arguments_data
    expect result: ValueWitnessV1 = result_data
    success_case_claiming(input, indices, arguments, result, cpu, memory)
  } else {
    expect [_, arguments_data, _] = fields
    expect arguments: List<ValueWitnessV1> = arguments_data
    failure_case_charging(input, indices, arguments, 0)
  }
}

/// The hop of \`case\` the TypeScript twin planned.
fn hop_row(
  case: WriteBitsCaseV1,
  from: Option<Pair<Int, ByteArray>>,
  end: Int,
  hop: ByteArray,
  next: Option<ByteArray>,
) -> WriteBitsStepV1 {
  expect hop: WriteBitsHopV1 = decoded(hop)
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
  hop_step(case, PlannedHopV1 { from, end, hop, next })
}

fn spend(step: WriteBitsStepV1) -> Bool {
  write_bits_loop.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    step.datum,
    step.redeemer,
    fixture.own_out_ref(),
    step.transaction,
  )
}

/// The step is built without being spent (its control row).
fn built(step: WriteBitsStepV1) -> Bool {
  when step.datum is {
    Some(_) -> True
    None -> False
  }
}`;

const source = [
  PREAMBLE,
  ...steps_.flatMap(({ step, witness, loop, plans }) => [
    stepSource({ step, witness, plans }),
    ...plans.flatMap(({ plan, rows }) =>
      rows.map((row) => rowSource(step, loop.failure, plan, row)),
    ),
  ]),
].join("\n\n");

writeOrCheck(
  generatedJsonPath,
  `${JSON.stringify(
    {
      generator:
        "demo/midgard-validation/scripts/generate-cek-write-bits-loop-v1-goldens.mjs",
      aikenModule: `onchain/aiken/validators/fraud-proofs/validation-trace/${aikenFileName}`,
      steps: steps_.map(({ step, loop, plans }) => ({
        name: step.name,
        inputBytes: step.length,
        indices: step.indices.length,
        failure: loop.failure,
        plans: plans.map(({ plan, count, rows }) => ({
          plan,
          hops: count,
          rows: rows.map(({ index, planned }) => ({
            name: rowName(step, plan, index),
            from: planned.from?.cursor ?? 0,
            end: planned.end,
            shape: planned.shape,
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
    tmpPrefix: "cek-write-bits-loop-v1-",
  }),
);
