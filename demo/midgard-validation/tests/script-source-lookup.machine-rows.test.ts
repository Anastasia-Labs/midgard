import { readFileSync, writeFileSync } from "node:fs";

import {
  decodeSingleCbor,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type DeterministicValidationMachineTrace,
  RejectCodes,
} from "../src/index.js";
import {
  AIKEN_ROWS_PATH,
  executionScanVectors,
  KEY,
  machineTrace,
  oneStepVector,
  phaseB,
  renderAikenRows,
  type TraceCase,
} from "./script-source-lookup.machine-rows.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeMintPreimageCbor,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
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
 * The native rows export every execution scan step of a native spend, of a
 * native mint, and of a native mint whose script evaluates false, which the
 * Aiken machine checks through the one-step dispatcher: such a step binds its
 * NativeScripts continuation by that continuation's hash.
 * Set `MIDGARD_WRITE_SCRIPT_SOURCE_LOOKUP_ROWS=1` to rewrite that file;
 * otherwise this suite fails when the checked-in vectors are stale.
 */

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

const lookupCase = (row: Row): TraceCase => ({
  spent: row.spent,
  state: row.state,
  options: {
    outputs: [minted],
    mintPreimageCbor: mint,
    referenceInputs: row.referenceInputs,
    ...(row.inline ? { scriptWitnesses: [script] } : {}),
  },
  rejectionCode: row.rejection?.code ?? null,
});

const scriptLocked = makeOutput(
  FUNDED_OUTPUT_LOVELACE,
  Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_raw_bytes(policy)),
    )
      .to_address()
      .to_raw_bytes(),
  ),
);
const spentLocked = outRefFromByte(0x75);

/** A native script no signer set satisfies, sourced from a reference input. */
const unsatisfied = nativeScriptWitness({ type: "any", scripts: [] });
const unsatisfiedPolicy = Buffer.from(hashScriptWitness(unsatisfied), "hex");
const carryingUnsatisfied = encodeMidgardTxOutput({
  address: ADDRESS,
  value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  script_ref: unsatisfied,
});

/**
 * A native script runs for one purpose, so its execution scan steps carry
 * that purpose. `purposeKind` is the purpose's kind (0 spend, 1 mint); a row
 * whose script evaluates false ends in the machine's rejection for that
 * purpose.
 */
type NativeRow = {
  readonly name: string;
  readonly purposeKind: bigint;
  readonly traceCase: TraceCase;
  readonly falseAtIndex: bigint | null;
};

const NATIVE_ROWS: readonly NativeRow[] = [
  {
    name: "native_spend_execution_scan",
    purposeKind: 0n,
    traceCase: {
      spent: [spentLocked],
      state: [[spentLocked, scriptLocked]],
      options: { outputs: [funded], scriptWitnesses: [script] },
      rejectionCode: null,
    },
    falseAtIndex: null,
  },
  {
    name: "native_mint_execution_scan",
    purposeKind: 1n,
    traceCase: {
      spent: [spentA],
      state: [[spentA, funded]],
      options: {
        outputs: [minted],
        mintPreimageCbor: mint,
        scriptWitnesses: [script],
      },
      rejectionCode: null,
    },
    falseAtIndex: null,
  },
  {
    name: "native_mint_false_execution_scan",
    purposeKind: 1n,
    traceCase: {
      spent: [spentA],
      state: [
        [spentA, funded],
        [refA, carryingUnsatisfied],
      ],
      options: {
        outputs: [
          makeOutput(
            FUNDED_OUTPUT_LOVELACE,
            ADDRESS,
            new Map([
              [
                unsatisfiedPolicy.toString("hex"),
                new Map([[name.toString("hex"), 5n]]),
              ],
            ]),
          ),
        ],
        mintPreimageCbor: makeMintPreimageCbor(
          new Map([[unsatisfiedPolicy, new Map([[name, 5n]])]]),
        ),
        referenceInputs: [refA],
      },
      rejectionCode: RejectCodes.NativeScriptInvalid,
    },
    falseAtIndex: 0n,
  },
];

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
    return ONE_STEP_STAGES.has(stage)
      ? [oneStepVector(trace, stateIndex, stage)]
      : [];
  });

describe("script-source lookup: Phase B, trace builder and on-chain one-step agree", () => {
  const exported: Parameters<typeof renderAikenRows>[0][number][] = [];
  const exportedNative: Parameters<typeof renderAikenRows>[1][number][] = [];

  it.each(ROWS)(
    "$name",
    async (row) => {
      const verdict = await phaseB(lookupCase(row));
      if (row.rejection === null) {
        expect(verdict).toStrictEqual({ accepted: 1, rejected: null });
      } else {
        expect(verdict.accepted).toBe(0);
        expect(verdict.rejected?.code).toBe(row.rejection.code);
        expect(verdict.rejected?.subject).toStrictEqual(row.rejection.subject);
      }

      const trace = await machineTrace(lookupCase(row));
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

  it.each(NATIVE_ROWS)(
    "$name: every execution scan step is exported",
    async (row) => {
      const verdict = await phaseB(row.traceCase);
      if (row.falseAtIndex === null) {
        expect(verdict).toStrictEqual({ accepted: 1, rejected: null });
      } else {
        expect(verdict.accepted).toBe(0);
        expect(verdict.rejected?.code).toBe(RejectCodes.NativeScriptInvalid);
        expect(verdict.rejected?.subject).toStrictEqual({
          arm: "ExecutionNativeScriptFalse",
          purposeKind: row.purposeKind,
          purposeIndex: row.falseAtIndex,
        });
      }
      const trace = await machineTrace(row.traceCase);
      expect(trace.verdict).toBe(
        row.falseAtIndex === null ? "accepted" : "rejected",
      );
      const vectors = executionScanVectors(trace);
      expect(vectors.length).toBeGreaterThan(0);
      expect(
        new Set(vectors.map(({ purposeKind }) => purposeKind)),
      ).toStrictEqual(new Set([row.purposeKind]));
      exportedNative.push({
        name: row.name,
        purposeKind: row.purposeKind,
        falseAtIndex: row.falseAtIndex,
        vectors,
      });
    },
    120_000,
  );

  it("the on-chain one-step vectors are the ones this table produces", () => {
    expect(exported.map(({ name }) => name)).toStrictEqual(
      ROWS.map(({ name }) => name),
    );
    expect(exportedNative.map(({ name }) => name)).toStrictEqual(
      NATIVE_ROWS.map(({ name }) => name),
    );
    const rendered = renderAikenRows(exported, exportedNative);
    if (process.env.MIDGARD_WRITE_SCRIPT_SOURCE_LOOKUP_ROWS === "1")
      writeFileSync(AIKEN_ROWS_PATH, rendered);
    expect(readFileSync(AIKEN_ROWS_PATH, "utf8")).toBe(rendered);
  });
});
