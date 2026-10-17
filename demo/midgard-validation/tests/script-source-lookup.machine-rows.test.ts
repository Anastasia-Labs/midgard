import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  decodeSingleCbor,
  encodeCbor,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import {
  buildValidationOneStepArgument,
  validationMachineStateData,
} from "../src/validation-machine-data.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

/**
 * One table, three verdicts per row: Phase B, the trace builder, and the
 * on-chain one-step verifier. Phase B looks a purpose's script up among the
 * reference scripts of every resolved output in canonical out-ref order, then
 * among the inline witnesses; an inline witness repeating a resolved script is
 * unused. The machine authenticates inline sources first and resolved-output
 * sources after them, and its lookup visits the resolved-output sources first.
 *
 * Every script-source step of stages 3, 9 and 11 of each row's trace is
 * exported to `script-source-lookup-rows.test.ak`, where the Aiken machine
 * checks it through the one-step dispatcher and the deployed split verifiers.
 * Set `MIDGARD_WRITE_SCRIPT_SOURCE_LOOKUP_ROWS=1` to rewrite that file;
 * otherwise this suite fails when the checked-in vectors are stale.
 */

// A fixed key: the shared fixtures' key is fresh on every run, and these rows
// are exported as checked-in vectors.
const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x5a));
const ADDRESS = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(KEY.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);

const script = nativeScriptWitness({ type: "all", scripts: [] });
const policy = Buffer.from(hashScriptWitness(script), "hex");
const name = Buffer.from("42", "hex");
const carrying = encodeMidgardTxOutput({
  address: ADDRESS,
  value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  script_ref: script,
});
const funded = makeOutput(FUNDED_OUTPUT_LOVELACE, ADDRESS);
const minted = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  ADDRESS,
  new Map([[policy.toString("hex"), new Map([[name.toString("hex"), 5n]])]]),
);
const mint = makeMintPreimageCbor(new Map([[policy, new Map([[name, 5n]])]]));

/** The block an event's trace is built in. */
const context = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x41)]),
  sourceKind: "normal" as const,
  blockTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

const [spentA, refA, refB] = [0x71, 0x73, 0x74].map((byte) =>
  outRefFromByte(byte),
) as [Buffer, Buffer, Buffer];

type Visit = {
  readonly sourceIndex: number;
  readonly originKind: "inline" | "reference";
  readonly sourceKey: Buffer;
};

type Row = {
  readonly name: string;
  readonly spent: readonly Buffer[];
  readonly referenceInputs: readonly Buffer[];
  readonly state: readonly (readonly [Buffer, Buffer])[];
  readonly inline: boolean;
  /** Phase B's verdict; null is acceptance. */
  readonly rejection: null | {
    readonly code: (typeof RejectCodes)[keyof typeof RejectCodes];
    readonly subject: unknown;
    readonly reason: unknown;
  };
  /** The stage-nine lookup visits, in order, ending at the match if any. */
  readonly lookup: readonly Visit[];
};

const reference = (key: Buffer, sourceIndex: number): Visit => ({
  sourceIndex,
  originKind: "reference",
  sourceKey: key,
});
const inline: Visit = {
  sourceIndex: 0,
  originKind: "inline",
  sourceKey: Buffer.from("00", "hex"),
};
const unused = {
  code: RejectCodes.InvalidFieldType,
  subject: { arm: "UnusedScriptWitness", index: 0n },
  reason: { UnusedScriptWitness: { script_index: 0n } },
};

const ROWS: readonly Row[] = [
  {
    name: "spent_only_source",
    spent: [spentA],
    referenceInputs: [],
    state: [[spentA, carrying]],
    inline: false,
    rejection: null,
    lookup: [reference(spentA, 0)],
  },
  {
    name: "inline_duplicate_of_reference",
    spent: [spentA],
    referenceInputs: [refA],
    state: [
      [spentA, funded],
      [refA, carrying],
    ],
    inline: true,
    rejection: unused,
    lookup: [reference(refA, 1)],
  },
  {
    name: "inline_duplicate_of_spent",
    spent: [spentA],
    referenceInputs: [],
    state: [[spentA, carrying]],
    inline: true,
    rejection: unused,
    lookup: [reference(spentA, 1)],
  },
  {
    name: "first_of_two_references",
    spent: [spentA],
    referenceInputs: [refA, refB],
    state: [
      [spentA, funded],
      [refA, carrying],
      [refB, carrying],
    ],
    inline: false,
    rejection: null,
    lookup: [reference(refA, 0)],
  },
  {
    name: "missing_source",
    spent: [spentA],
    referenceInputs: [refA],
    state: [
      [spentA, funded],
      [refA, funded],
    ],
    inline: false,
    rejection: {
      code: RejectCodes.MissingRequiredWitness,
      subject: {
        arm: "ScriptSourceMissing",
        purposeKind: 1n,
        purposeIndex: 0n,
      },
      reason: { ScriptSourceMissing: { purpose_kind: 1n, purpose_index: 0n } },
    },
    lookup: [],
  },
  {
    name: "inline_only_source",
    spent: [spentA],
    referenceInputs: [],
    state: [[spentA, funded]],
    inline: true,
    rejection: null,
    lookup: [inline],
  },
  {
    name: "reference_only_source",
    spent: [spentA],
    referenceInputs: [refA],
    state: [
      [spentA, funded],
      [refA, carrying],
    ],
    inline: false,
    rejection: null,
    lookup: [reference(refA, 0)],
  },
];

const txOptions = (row: Row) => ({
  privateKey: KEY,
  outputs: [minted],
  mintPreimageCbor: mint,
  referenceInputs: [...row.referenceInputs],
  ...(row.inline ? { scriptWitnesses: [script] } : {}),
});

const phaseB = async (row: Row) => {
  const result = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [makePhaseBCandidate({ spent: [...row.spent], ...txOptions(row) })],
      new Map(row.state.map(([key, output]) => [key.toString("hex"), output])),
      {
        nowCardanoSlotNo: 100n,
        bucketConcurrency: 1,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      },
    ),
  );
  return result.rejected.length === 0
    ? { accepted: result.accepted.length, rejected: null }
    : { accepted: result.accepted.length, rejected: result.rejected[0]! };
};

const machineTrace = async (row: Row) => {
  const transaction = makeNativeTx({
    spendInputs: [...row.spent],
    ...txOptions(row),
  });
  const entries = row.state.map(([outRef, output]) => ({ outRef, output }));
  const accepted = row.rejection === null;
  const expectedLedgerOps = accepted
    ? [
        ...row.spent.map((key) => ({ type: "delete" as const, key })),
        buildValidationMachineLedgerInsertOp({
          key: outRefFromTxId(transaction.txId),
          outputCbor: minted,
        }),
      ]
    : [];
  const ledgerMutationSteps = accepted
    ? await buildValidationMachineLedgerMutationSteps({
        initialEntries: entries,
        operations: expectedLedgerOps,
      })
    : [];
  const unchangedRoot = Buffer.alloc(32, 0x2e).toString("hex");
  return await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      ...context,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: accepted
        ? ledgerMutationSteps[0]!.preRoot.toString("hex")
        : unchangedRoot,
      postUtxosRoot: accepted
        ? ledgerMutationSteps.at(-1)!.postRoot.toString("hex")
        : unchangedRoot,
      ledgerWitnessEntries: entries,
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: accepted ? "accepted" : "rejected",
      expectedRejectionCode: row.rejection?.code ?? null,
    }),
  );
};

/** The script-sources control stage a witness carries (control item 9). */
const scriptSourcesStage = (cbor: Buffer): number =>
  Number((decodeSingleCbor(cbor) as readonly bigint[])[9]);

const stageNineVisits = (trace: DeterministicValidationMachineTrace) =>
  trace.witnesses.flatMap((witness) =>
    witness.phase === "scriptSources" &&
    scriptSourcesStage(witness.cbor) === 9 &&
    witness.auxiliary?.kind === "scriptSourceScan"
      ? [
          {
            sourceIndex: witness.auxiliary.sourceIndex,
            originKind: witness.auxiliary.originKind,
            sourceKey: witness.auxiliary.sourceKey,
          },
        ]
      : [],
  );

const ONE_STEP_STAGES = new Set([3, 9, 11]);

/** Every stage-3, stage-9 and stage-11 step of the trace, as one-step input. */
const oneStepVectors = (trace: DeterministicValidationMachineTrace) =>
  trace.witnesses.flatMap((witness, stateIndex) => {
    if (witness.phase !== "scriptSources") return [];
    const stage = scriptSourcesStage(witness.cbor);
    if (!ONE_STEP_STAGES.has(stage)) return [];
    const argument = buildValidationOneStepArgument({ trace, stateIndex });
    return [
      {
        stage,
        pre: Data.to(
          validationMachineStateData(trace.states[stateIndex]!) as never,
        ),
        transition: argument.transitionCbor.toString("hex"),
        auxiliary: argument.auxiliaryCbor.toString("hex"),
      },
    ];
  });

const AIKEN_ROWS_PATH = fileURLToPath(
  new URL(
    "../../../onchain/aiken/lib/midgard/validation-machine-tests/script-source-lookup-rows.test.ak",
    import.meta.url,
  ),
);

const renderAikenRows = (
  rows: readonly {
    readonly name: string;
    readonly rejection: Row["rejection"];
    readonly vectors: ReturnType<typeof oneStepVectors>;
  }[],
) => {
  const lines = [
    "// Generated by demo/midgard-validation/tests/script-source-lookup.machine-rows.test.ts",
    "// (MIDGARD_WRITE_SCRIPT_SOURCE_LOOKUP_ROWS=1). Do not edit by hand.",
    "",
    "use midgard/rejection_reason_v1",
    "use midgard/validation_machine_tests/script_source_lookup_rows_support.{",
    "  OneStepVector, rejects_with, verifies_every_step,",
    "}",
    "",
  ];
  for (const row of rows) {
    lines.push(`const ${row.name}_steps: List<OneStepVector> =`);
    lines.push("  [");
    for (const vector of row.vectors)
      lines.push(
        "    OneStepVector {",
        `      stage: ${vector.stage},`,
        `      pre: #"${vector.pre}",`,
        `      transition: #"${vector.transition}",`,
        `      auxiliary: #"${vector.auxiliary}",`,
        "    },",
      );
    lines.push("  ]", "");
    lines.push(`test ${row.name}_steps_verify_on_chain() {`);
    const reason = row.rejection?.reason as
      | { UnusedScriptWitness: { script_index: bigint } }
      | { ScriptSourceMissing: { purpose_kind: bigint; purpose_index: bigint } }
      | undefined;
    if (reason === undefined) {
      lines.push(`  verifies_every_step(${row.name}_steps)`);
    } else {
      // Laid out as `aiken fmt` lays it out.
      const aikenReason =
        "UnusedScriptWitness" in reason
          ? [
              `      rejection_reason_v1.UnusedScriptWitness { script_index: ${reason.UnusedScriptWitness.script_index} },`,
            ]
          : [
              "      rejection_reason_v1.ScriptSourceMissing {",
              `        purpose_kind: ${reason.ScriptSourceMissing.purpose_kind},`,
              `        purpose_index: ${reason.ScriptSourceMissing.purpose_index},`,
              "      },",
            ];
      lines.push(
        "  and {",
        `    verifies_every_step(${row.name}_steps),`,
        "    rejects_with(",
        `      ${row.name}_steps,`,
        ...aikenReason,
        "    ),",
        "  }",
      );
    }
    lines.push("}", "");
  }
  return lines.join("\n");
};

describe("script-source lookup: Phase B, trace builder and on-chain one-step agree", () => {
  const exported: Parameters<typeof renderAikenRows>[0][number][] = [];

  it.each(ROWS)(
    "$name",
    async (row) => {
      const verdict = await phaseB(row);
      if (row.rejection === null) {
        expect(verdict).toStrictEqual({ accepted: 1, rejected: null });
      } else {
        expect(verdict.accepted).toBe(0);
        expect(verdict.rejected?.code).toBe(row.rejection.code);
        expect(verdict.rejected?.subject).toStrictEqual(row.rejection.subject);
      }

      const trace = await machineTrace(row);
      expect(trace.verdict).toBe(
        row.rejection === null ? "accepted" : "rejected",
      );
      expect(trace.rejectionCode).toBe(row.rejection?.code ?? null);
      expect(trace.rejectionReason).toStrictEqual(
        row.rejection?.reason ?? null,
      );
      expect(stageNineVisits(trace)).toStrictEqual(row.lookup);

      const vectors = oneStepVectors(trace);
      expect(vectors.some(({ stage }) => stage === 9)).toBe(true);
      exported.push({ name: row.name, rejection: row.rejection, vectors });
    },
    120_000,
  );

  it("the on-chain one-step vectors are the ones this table produces", () => {
    expect(exported.map(({ name }) => name)).toStrictEqual(
      ROWS.map(({ name }) => name),
    );
    const rendered = renderAikenRows(exported);
    if (process.env.MIDGARD_WRITE_SCRIPT_SOURCE_LOOKUP_ROWS === "1")
      writeFileSync(AIKEN_ROWS_PATH, rendered);
    expect(readFileSync(AIKEN_ROWS_PATH, "utf8")).toBe(rendered);
  });
});
