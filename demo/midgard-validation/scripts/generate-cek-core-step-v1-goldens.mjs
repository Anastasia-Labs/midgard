#!/usr/bin/env node

/**
 * Produces the cross-language golden vectors for the V1 CEK **core step**:
 * the transition `verify_core_step_v1(pre, post, witness)` that the Aiken
 * verifier (`onchain/aiken/lib/midgard/cek-machine-v1.ak`) adjudicates and
 * the TypeScript trace builder (`src/cek-executor.ts`) emits.
 *
 * Before this channel the two machines were pinned against each other by one
 * Aiken test of seventeen integer tags and one hand-pasted `ComputeApplication`
 * CBOR vector. Nothing proved that a step the TypeScript builder emits is a
 * step the Aiken verifier accepts. This generator closes that gap: it compiles
 * the curated programs of `tests/fixtures/cek-core-step-v1.vectors.mjs`,
 * runs each through `buildMidgardCekExecutionGraph` and
 * `executeMidgardCekStructuralProgram`, and for every emitted step writes the
 * exact `CoreStepEvidenceV1` CBOR from `encodeMidgardCekCoreStepDataCbor`
 * together with the `hashMidgardCekMachineState` of its pre- and post-states.
 * Every value is written to two places:
 *
 *   * `tests/fixtures/cek-core-step-v1.generated.json` — recomputed by
 *     `tests/cek-core-step-goldens.test.ts`, which re-runs the builder on the
 *     same program definitions and asserts byte-equality, so a drifting
 *     builder or encoder fails on the TypeScript side; and
 *   * `onchain/aiken/lib/midgard/cek-core-step-v1-golden.test.ak` — where
 *     each pinned CBOR is decoded into `cek_machine_v1.CoreStepEvidenceV1`
 *     and re-verified by the Aiken machine under the fork runner, so a
 *     divergence between the two machines fails on the Aiken side.
 *
 * Each program also carries **negative twists**: a genuine step whose
 * post-state has exactly one field perturbed (`cpu - 1`, or the mode flipped),
 * pinned as well-formed evidence CBOR both verifiers must refuse. Without
 * them a verifier that accepted everything would pass the positive half.
 *
 * The plumbing — the `--check` contract, the `aiken fmt` trip, the emission —
 * is the shared `@al-ft/midgard-core/scripts/golden-channel.mjs`; only what is
 * computed lives here. `--check` asserts the checked-in artifacts are exactly
 * what the twins produce today and never repairs them.
 *
 * usage: node scripts/generate-cek-core-step-v1-goldens.mjs [--check]
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import {
  aikenBytes,
  formatAikenSource,
  goldenChannelEmitter,
  hex,
  parseGoldenChannelArguments,
} from "@al-ft/midgard-core/scripts/golden-channel.mjs";

import {
  buildMidgardCanonicalCekProgram,
  buildMidgardCekExecutionGraph,
  encodeMidgardCekCoreStepDataCbor,
  encodeMidgardCekPlutusData,
  executeMidgardCekStructuralProgram,
  verifyMidgardCekCoreStep,
} from "../dist/index.js";
// The program definitions live beside the fixture they produce, not here: the
// vitest suite drives the *same* definitions through `src/` while this script
// drives them through `dist/`, so a builder that drifts is caught on both
// sides rather than only by `--check`.
import {
  CEK_CORE_STEP_PROGRAMS,
  CEK_CORE_STEP_TWISTS,
  CEK_CORE_STEP_WITNESS_KINDS,
  CEK_CORE_STEP_WITNESS_KINDS_UNCOVERED,
  compileMidgardCekGoldenProgram,
  INITIAL_MODE,
} from "../tests/fixtures/cek-core-step-v1.vectors.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(scriptDirectory, "..");
const repositoryRoot = resolve(packageRoot, "../..");
const generatedJsonPath = join(
  packageRoot,
  "tests/fixtures/cek-core-step-v1.generated.json",
);
const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/cek-core-step-v1-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-cek-core-step-v1-goldens.mjs [--check]",
);
const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

/**
 * How many steps one generated Aiken test verifies. Each step costs one
 * pure-Aiken `cbor.deserialise`, one `expect` into `CoreStepEvidenceV1`, one
 * re-serialisation, two state hashes and the verifier itself, so a whole
 * trace in one test would exceed a mainnet-shaped execution budget for the
 * longer programs. The fork runner does not cap a test's budget, so the cap
 * is chosen rather than imposed: four keeps the heaviest chunks (the
 * `stepBuiltinListToMap` and BLS final-verify steps, ~2.7M mem each) under
 * the 14M-mem / 10G-cpu mainnet limits; measured with the fork compiler.
 */
const AIKEN_STEPS_PER_TEST = 4;

const stateJson = (state) => ({
  mode: state.mode,
  executionIndex: state.executionIndex.toString(10),
  focusRoot: hex(state.focusRoot),
  environmentRoot: hex(state.environmentRoot),
  continuationRoot: hex(state.continuationRoot),
  auxiliary: state.auxiliary.toString(10),
  cpu: state.cpu.toString(10),
  memory: state.memory.toString(10),
});

const buildProgramVector = (definition) => {
  const program = buildMidgardCanonicalCekProgram(
    compileMidgardCekGoldenProgram(definition.term()),
  );
  const contextCbor = encodeMidgardCekPlutusData(definition.context());
  const graph = buildMidgardCekExecutionGraph(
    program.envelope,
    program.material.values(),
    contextCbor,
  );
  const execution = executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: definition.maxSteps,
    executionBudget: definition.executionBudget,
  });
  if (execution.initialState.mode !== INITIAL_MODE) {
    throw new Error(
      `${definition.label}: initial mode ${execution.initialState.mode} is not ${INITIAL_MODE}`,
    );
  }
  if (execution.steps.length === 0) {
    throw new Error(`${definition.label}: the trace emitted no steps`);
  }

  let expectedPre = hashMidgardCekMachineState(execution.initialState);
  const steps = execution.steps.map((step, index) => {
    const preHash = hashMidgardCekMachineState(step.pre);
    const postHash = hashMidgardCekMachineState(step.post);
    if (!preHash.equals(expectedPre)) {
      throw new Error(
        `${definition.label}: step ${index} does not continue the trace`,
      );
    }
    if (!verifyMidgardCekCoreStep(step.pre, step.post, step.witness)) {
      throw new Error(
        `${definition.label}: step ${index} (${step.witness.kind}) fails the TypeScript verifier`,
      );
    }
    expectedPre = postHash;
    return {
      index,
      kind: step.witness.kind,
      preHash: hex(preHash),
      postHash: hex(postHash),
      evidenceCbor: hex(encodeMidgardCekCoreStepDataCbor(step)),
    };
  });
  const terminalStateHash = hashMidgardCekMachineState(execution.terminalState);
  if (!terminalStateHash.equals(expectedPre)) {
    throw new Error(`${definition.label}: terminal state is not the last post`);
  }

  const negatives = CEK_CORE_STEP_TWISTS.map((twist) => {
    const stepIndex = twist.select(execution.steps);
    const twisted = twist.apply(execution.steps[stepIndex]);
    if (verifyMidgardCekCoreStep(twisted.pre, twisted.post, twisted.witness)) {
      throw new Error(
        `${definition.label}: twist ${twist.label} of step ${stepIndex} still verifies`,
      );
    }
    return {
      twist: twist.label,
      stepIndex,
      kind: twisted.witness.kind,
      evidenceCbor: hex(encodeMidgardCekCoreStepDataCbor(twisted)),
    };
  });

  return {
    label: definition.label,
    note: definition.note,
    maxSteps: definition.maxSteps,
    ...(definition.executionBudget === undefined
      ? {}
      : {
          executionBudget: {
            cpu: definition.executionBudget.cpu.toString(10),
            memory: definition.executionBudget.memory.toString(10),
          },
        }),
    scriptFlat: hex(compileMidgardCekGoldenProgram(definition.term())),
    contextCbor: hex(contextCbor),
    envelope: {
      uplcVersion: program.envelope.uplcVersion.map((part) =>
        part.toString(10),
      ),
      termRoot: hex(program.envelope.termRoot),
      nodeCount: program.envelope.nodeCount.toString(10),
      materialByteLength: program.envelope.materialByteLength.toString(10),
    },
    envelopeHash: hex(program.envelopeHash),
    root: hex(graph.root),
    contextValueRoot: hex(graph.contextValueRoot),
    initialState: stateJson(execution.initialState),
    initialStateHash: hex(hashMidgardCekMachineState(execution.initialState)),
    steps,
    terminalState: stateJson(execution.terminalState),
    terminalStateHash: hex(terminalStateHash),
    stopReason: execution.stopReason,
    negatives,
  };
};

const buildGolden = () => {
  const programs = CEK_CORE_STEP_PROGRAMS.map(buildProgramVector);
  const covered = new Set(
    programs.flatMap((program) => program.steps.map((step) => step.kind)),
  );
  const expectedUncovered = new Set(CEK_CORE_STEP_WITNESS_KINDS_UNCOVERED);
  for (const kind of CEK_CORE_STEP_WITNESS_KINDS) {
    if (covered.has(kind) === expectedUncovered.has(kind)) {
      throw new Error(
        covered.has(kind)
          ? `witness arm ${kind} is covered but listed as uncovered`
          : `witness arm ${kind} is not reached by any curated program`,
      );
    }
  }
  for (const kind of covered) {
    if (!CEK_CORE_STEP_WITNESS_KINDS.includes(kind)) {
      throw new Error(`witness arm ${kind} is not in the pinned ABI list`);
    }
  }
  return {
    generator:
      "demo/midgard-validation/scripts/generate-cek-core-step-v1-goldens.mjs",
    aikenModule: "onchain/aiken/lib/midgard/cek-core-step-v1-golden.test.ak",
    aikenStepsPerTest: AIKEN_STEPS_PER_TEST,
    witnessKindsCovered: CEK_CORE_STEP_WITNESS_KINDS.filter((kind) =>
      covered.has(kind),
    ),
    witnessKindsUncovered: [...CEK_CORE_STEP_WITNESS_KINDS_UNCOVERED],
    programs,
  };
};

// ---------------------------------------------------------------------------
// Aiken rendering
// ---------------------------------------------------------------------------

const section = (title) => [
  "// ---------------------------------------------------------------------------",
  `// ${title}`,
  "// ---------------------------------------------------------------------------",
  "",
];

const pad2 = (value) => String(value).padStart(2, "0");

const stepName = (program, step) =>
  `golden_${program.label}_s${pad2(step.index)}`;

const negativeName = (program, negative) =>
  `golden_${program.label}_${negative.twist}`;

const renderAiken = (golden) => {
  const lines = [
    `// Generated by ${golden.generator}.`,
    "// Do not edit; regenerate from the TypeScript twins.",
    "//",
    "// Cross-language golden vectors for the V1 CEK core step. Every",
    "// `CoreStepEvidenceV1` below was emitted by the TypeScript trace builder",
    "// (`demo/midgard-validation/src/cek-executor.ts`, encoded by",
    "// `src/cek-data.ts`) from a curated UPLC program, and is decoded and",
    "// re-verified here by `cek_machine_v1.verify_core_step_v1`, so a step the",
    "// builder emits is proven to be a step this verifier accepts. The pinned",
    "// state hashes are `hashMidgardCekMachineState` on the TypeScript side and",
    "// `hash_state_v1` here.",
    "//",
    "// Each program's negative twists are genuine steps with exactly one",
    "// post-state field perturbed; the verifier must refuse them, and each",
    "// refusal is pinned as its own boolean in a non-`fail` test.",
    "",
    "use aiken/cbor",
    "use midgard/cek_machine_v1.{CoreStepEvidenceV1}",
    "",
    "fn decode_evidence(evidence_cbor: ByteArray) -> CoreStepEvidenceV1 {",
    "  expect Some(data) = cbor.deserialise(evidence_cbor)",
    "  expect evidence: CoreStepEvidenceV1 = data",
    "  evidence",
    "}",
    "",
    "fn step_verifies(",
    "  evidence_cbor: ByteArray,",
    "  pre_hash: ByteArray,",
    "  post_hash: ByteArray,",
    ") -> Bool {",
    "  let evidence = decode_evidence(evidence_cbor)",
    "  and {",
    "    cbor.serialise(evidence) == evidence_cbor,",
    "    cek_machine_v1.hash_state_v1(evidence.pre) == pre_hash,",
    "    cek_machine_v1.hash_state_v1(evidence.post) == post_hash,",
    "    cek_machine_v1.verify_core_step_v1(",
    "      evidence.pre,",
    "      evidence.post,",
    "      evidence.witness,",
    "    ),",
    "  }",
    "}",
    "",
    "fn step_is_rejected(evidence_cbor: ByteArray) -> Bool {",
    "  let evidence = decode_evidence(evidence_cbor)",
    "  and {",
    "    cbor.serialise(evidence) == evidence_cbor,",
    "    !cek_machine_v1.verify_core_step_v1(",
    "      evidence.pre,",
    "      evidence.post,",
    "      evidence.witness,",
    "    ),",
    "  }",
    "}",
    "",
  ];

  for (const program of golden.programs) {
    lines.push(
      ...section(
        `${program.label}: ${program.note} (${program.steps.length} steps, ${program.stopReason})`,
      ),
    );
    for (const step of program.steps) {
      const name = stepName(program, step);
      lines.push(`// step ${step.index}: ${step.kind}`);
      lines.push(`const ${name}_cbor = ${aikenBytes(step.evidenceCbor)}`);
      lines.push("");
      lines.push(`const ${name}_pre = ${aikenBytes(step.preHash)}`);
      lines.push("");
      lines.push(`const ${name}_post = ${aikenBytes(step.postHash)}`);
      lines.push("");
    }
    for (const negative of program.negatives) {
      lines.push(
        `// ${negative.twist} applied to step ${negative.stepIndex} (${negative.kind})`,
      );
      lines.push(
        `const ${negativeName(program, negative)}_cbor = ${aikenBytes(negative.evidenceCbor)}`,
      );
      lines.push("");
    }
    for (
      let from = 0;
      from < program.steps.length;
      from += golden.aikenStepsPerTest
    ) {
      const chunk = program.steps.slice(from, from + golden.aikenStepsPerTest);
      const to = from + chunk.length - 1;
      lines.push(
        `test golden_${program.label}_steps_${pad2(from)}_${pad2(to)}_verify() {`,
      );
      const conjuncts = chunk.map((step) => {
        const name = stepName(program, step);
        return `step_verifies(${name}_cbor, ${name}_pre, ${name}_post)`;
      });
      // Aiken rejects an `and { … }` with fewer than two operands, so a
      // trailing one-step chunk is pinned as the bare expression.
      if (conjuncts.length === 1) {
        lines.push(`  ${conjuncts[0]}`);
      } else {
        lines.push("  and {");
        for (const conjunct of conjuncts) {
          lines.push(`    ${conjunct},`);
        }
        lines.push("  }");
      }
      lines.push("}");
      lines.push("");
    }
    lines.push(`test golden_${program.label}_twists_are_rejected() {`);
    lines.push("  and {");
    for (const negative of program.negatives) {
      lines.push(
        `    step_is_rejected(${negativeName(program, negative)}_cbor),`,
      );
    }
    lines.push("  }");
    lines.push("}");
    lines.push("");
  }
  return `${lines.join("\n").trimEnd()}\n`;
};

/**
 * The fork's `aiken fmt` leaves trailing spaces after monadic `let`/`expect`
 * lines; CI normalises them away over every tracked `.ak` file, so the
 * checked-in module must not carry them or `--check` and the format gate would
 * disagree about the same bytes.
 */
const stripTrailingWhitespace = (source) => source.replace(/[ \t]+$/gmu, "");

// ---------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------

const golden = buildGolden();
writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);
writeOrCheck(
  generatedAikenPath,
  stripTrailingWhitespace(
    formatAikenSource({
      source: renderAiken(golden),
      fileName: "cek-core-step-v1-golden.test.ak",
      repositoryRoot,
      tmpPrefix: "midgard-cek-core-step-aiken-format-",
    }),
  ),
);
