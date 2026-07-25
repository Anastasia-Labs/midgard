import {
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekTermNodeV1,
  hashMidgardCekTermNodeV1,
} from "@al-ft/midgard-core/cek-proof";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  Lambda,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { Constr } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { buildMidgardCanonicalCekProgramV1 } from "../src/cek-program.js";
import {
  applyUTxOStatePatch,
  buildConflictComponents,
  LedgerColumns,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import {
  encodeScriptContextCbor,
  evaluateScriptWithHarmonic,
  evaluateUplcWithContextCbor,
} from "../src/local-script-eval.js";
import { MidgardRedeemerTag } from "../src/midgard-redeemers.js";
import type { PhaseBResultWithPatch } from "../src/phase-b.js";
import type { PhaseBConfig, RejectCode, RejectedTx } from "../src/types.js";
import {
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

const phaseBConfig: PhaseBConfig = {
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
};

const runPhaseB = (
  candidates: Parameters<typeof runPhaseBValidationWithPatch>[0],
  preState: Parameters<typeof runPhaseBValidationWithPatch>[1],
  config = phaseBConfig,
) =>
  Effect.runPromise(runPhaseBValidationWithPatch(candidates, preState, config));

const txIds = (txs: PhaseBResultWithPatch["accepted"]) =>
  txs.map((tx) => tx.ledgerTx.txId.toString("hex"));

const preState = (
  entries: readonly (readonly [outRef: Buffer, output: Buffer])[],
) =>
  new Map(entries.map(([outRef, output]) => [outRef.toString("hex"), output]));

const expectSinglePhaseBRejection = (
  result: PhaseBResultWithPatch,
  expectedCode: RejectCode,
): RejectedTx => {
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0].code).toBe(expectedCode);
  return result.rejected[0];
};

describe("phase B validation", () => {
  it("matches the pairwise conflict oracle on randomized ready waves", () => {
    let seed = 0x5eed1234;
    const random = () => {
      seed = (seed * 1_664_525 + 1_013_904_223) >>> 0;
      return seed / 0x1_0000_0000;
    };
    const conflicts = (
      left: { spentOutRefs: Set<string>; referenceOutRefs: Set<string> },
      right: { spentOutRefs: Set<string>; referenceOutRefs: Set<string> },
    ) =>
      [...left.spentOutRefs].some(
        (outRef) =>
          right.spentOutRefs.has(outRef) || right.referenceOutRefs.has(outRef),
      ) ||
      [...right.spentOutRefs].some((outRef) =>
        left.referenceOutRefs.has(outRef),
      );

    for (let sample = 0; sample < 100; sample += 1) {
      const nodes = Array.from(
        { length: 2 + Math.floor(random() * 30) },
        (_, id) => ({
          id,
          spentOutRefs: new Set(
            Array.from({ length: Math.floor(random() * 4) }, () =>
              Math.floor(random() * 20).toString(),
            ),
          ),
          referenceOutRefs: new Set(
            Array.from({ length: Math.floor(random() * 4) }, () =>
              Math.floor(random() * 20).toString(),
            ),
          ),
        }),
      );
      for (const node of nodes) {
        for (const spent of node.spentOutRefs)
          node.referenceOutRefs.delete(spent);
      }

      const expected: number[][] = [];
      const unvisited = new Set(nodes.map((node) => node.id));
      while (unvisited.size > 0) {
        const first = unvisited.values().next().value as number;
        const queue = [first];
        const component: number[] = [];
        unvisited.delete(first);
        while (queue.length > 0) {
          const current = queue.shift()!;
          component.push(current);
          for (const candidate of [...unvisited]) {
            if (conflicts(nodes[current], nodes[candidate])) {
              unvisited.delete(candidate);
              queue.push(candidate);
            }
          }
        }
        expected.push(component.sort((left, right) => left - right));
      }

      const actual = buildConflictComponents(nodes)
        .map((component) =>
          component.map((node) => node.id).sort((a, b) => a - b),
        )
        .sort((left, right) => left[0] - right[0]);
      expected.sort((left, right) => left[0] - right[0]);
      expect(actual).toStrictEqual(expected);
    }
  });
  it("accepts a balanced candidate and returns a deterministic UTxO patch", async () => {
    const spent = outRefFromByte(0x21);
    const inputOutput = makeOutput(10n);
    const candidate = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [spent],
      outputLovelace: 10n,
    });
    const state = preState([[spent, inputOutput]]);

    const result = await runPhaseB([candidate], state);

    expect(result.rejected).toHaveLength(0);
    expect(txIds(result.accepted)).toEqual([
      candidate.ledgerTx.txId.toString("hex"),
    ]);
    expect(result.statePatch.deletedOutRefs).toEqual([spent.toString("hex")]);
    expect(result.statePatch.upsertedOutRefs).toHaveLength(1);

    applyUTxOStatePatch(state, result.statePatch);
    const producedOutRef =
      candidate.graph.produced[0][LedgerColumns.OUTREF].toString("hex");
    expect(state.has(spent.toString("hex"))).toBe(false);
    expect(state.get(producedOutRef)?.equals(makeOutput(10n))).toBe(true);
  });

  it("rejects missing spend inputs before value accounting", async () => {
    const candidate = makePhaseBCandidate({
      spent: [outRefFromByte(0x22)],
      outputLovelace: 10n,
    });

    const result = await runPhaseB([candidate], new Map());

    expectSinglePhaseBRejection(result, RejectCodes.InputNotFound);
  });

  it("rejects missing pubkey witnesses for resolved spend inputs", async () => {
    const spent = outRefFromByte(0x23);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      omitVkeyWitness: true,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.MissingRequiredWitness);
  });

  it("rejects malformed pre-state outputs that cannot be decoded", async () => {
    const spent = outRefFromByte(0x24);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, Buffer.from("ff", "hex")]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.InvalidOutput);
  });

  it("rejects transactions that do not preserve value", async () => {
    const spent = outRefFromByte(0x25);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 9n,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.ValueNotPreserved);
  });

  it("rejects candidates outside the current Cardano slot interval", async () => {
    const spent = outRefFromByte(0x26);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      validityIntervalStart: 101n,
      validityIntervalEnd: 110n,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    const rejection = expectSinglePhaseBRejection(
      result,
      RejectCodes.ValidityIntervalMismatch,
    );
    expect(rejection.detail).toBe("100 < 101");
  });

  it("rejects later candidates that double-spend an accepted input", async () => {
    const spent = outRefFromByte(0x27);
    const reference = outRefFromByte(0x28);
    const first = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [spent],
      outputLovelace: 10n,
    });
    const second = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [spent],
      referenceInputs: [reference],
      outputLovelace: 10n,
    });

    const result = await runPhaseB(
      [first, second],
      preState([
        [spent, makeOutput(10n)],
        [reference, makeOutput(1n)],
      ]),
    );

    expect(txIds(result.accepted)).toEqual([
      first.ledgerTx.txId.toString("hex"),
    ]);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].txId.equals(second.ledgerTx.txId)).toBe(true);
    expect(result.rejected[0].code).toBe(RejectCodes.DoubleSpend);
  });

  it("requires exact material for a historical reference program", async () => {
    const spent = outRefFromByte(0x7a);
    const reference = outRefFromByte(0x7b);
    const termPreimage = encodeMidgardCekTermNodeV1({ kind: "error" });
    const termRoot = hashMidgardCekTermNodeV1({ kind: "error" });
    const envelope = encodeMidgardCekProgramEnvelopeV1({
      uplcVersion: [1n, 1n, 0n],
      termRoot,
      nodeCount: 1n,
      materialByteLength: BigInt(termPreimage.length),
    });
    const referenceOutput = encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace: 1n, assets: new Map() },
      script_ref: {
        language: "PlutusV3",
        scriptBytes: envelope,
      },
    });
    const material = {
      kind: "term" as const,
      root: termRoot,
      preimage: termPreimage,
    };

    const missing = await runPhaseB(
      [
        makePhaseBCandidate({
          spent: [spent],
          referenceInputs: [reference],
          outputLovelace: 10n,
          programMaterialSidecarCbor:
            encodeMidgardCekProgramMaterialSidecarV1([]),
        }),
      ],
      preState([
        [spent, makeOutput(10n)],
        [reference, referenceOutput],
      ]),
    );
    expectSinglePhaseBRejection(
      missing,
      RejectCodes.CekProgramMaterial,
    );

    const covered = await runPhaseB(
      [
        makePhaseBCandidate({
          spent: [spent],
          referenceInputs: [reference],
          outputLovelace: 10n,
          programMaterialSidecarCbor:
            encodeMidgardCekProgramMaterialSidecarV1([material]),
        }),
      ],
      preState([
        [spent, makeOutput(10n)],
        [reference, referenceOutput],
      ]),
    );
    expect(covered.rejected).toHaveLength(0);
    expect(covered.accepted).toHaveLength(1);
  });

  it("rejects a later reference input after an earlier component member spends it", async () => {
    const spent = outRefFromByte(0x2f);
    const other = outRefFromByte(0x30);
    const spender = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [spent],
      outputLovelace: 10n,
    });
    const referencer = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [other],
      referenceInputs: [spent],
      outputLovelace: 10n,
    });
    const result = await runPhaseB(
      [spender, referencer],
      preState([
        [spent, makeOutput(10n)],
        [other, makeOutput(10n)],
      ]),
    );
    expect(txIds(result.accepted)).toStrictEqual([
      spender.ledgerTx.txId.toString("hex"),
    ]);
    expect(result.rejected).toHaveLength(1);
    expect(result.rejected[0].code).toBe(RejectCodes.InputNotFound);
  });

  it("cascade-rejects descendants when an ancestor fails validation", async () => {
    const parentInput = outRefFromByte(0x29);
    const parent = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [parentInput],
      outputLovelace: 9n,
    });
    const parentOutputRef = parent.graph.produced[0][LedgerColumns.OUTREF];
    const child = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [parentOutputRef],
      outputLovelace: 9n,
    });

    const result = await runPhaseB(
      [parent, child],
      preState([[parentInput, makeOutput(10n)]]),
    );

    expect(result.rejected.map((rejection) => rejection.code)).toEqual([
      RejectCodes.ValueNotPreserved,
      RejectCodes.DependsOnRejectedTx,
    ]);
    expect(result.accepted).toHaveLength(0);
    expect(result.rejected[1].detail).toContain(
      parent.ledgerTx.txId.toString("hex"),
    );
  });

  it("rejects defensive dependency cycles in the Phase A candidate graph", async () => {
    const first = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [outRefFromByte(0x2c)],
      outputLovelace: 10n,
    });
    const second = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [first.graph.produced[0][LedgerColumns.OUTREF]],
      outputLovelace: 10n,
    });
    const firstDependsOnSecond = {
      ...first,
      graph: {
        ...first.graph,
        spentOutRefHexes: [
          second.graph.produced[0][LedgerColumns.OUTREF].toString("hex"),
        ],
      },
    };

    const result = await runPhaseB([firstDependsOnSecond, second], new Map());

    expect(result.rejected.map((rejection) => rejection.code)).toEqual([
      RejectCodes.DependencyCycle,
      RejectCodes.DependencyCycle,
    ]);
    expect(result.accepted).toHaveLength(0);
  });

  it("rejects extraneous redeemers with no matching script purpose", async () => {
    const spent = outRefFromByte(0x2a);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      scriptLanguages: ["PlutusV3"],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Mint, index: 0n },
      ]),
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    const rejection = expectSinglePhaseBRejection(
      result,
      RejectCodes.InvalidFieldType,
    );
    expect(rejection.detail).toContain("extraneous redeemer");
  });

  it("rejects PlutusV3 receiving scripts because receive requires MidgardV1 context", async () => {
    const spent = outRefFromByte(0x2b);
    const script = plutusV3ScriptWitness(Buffer.from("010203", "hex"));
    const scriptHash = hashScriptWitness(script);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputs: [makeProtectedScriptOutput(scriptHash, 10n)],
      outputLovelace: 10n,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Receiving, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(10n)]]),
    );

    const rejection = expectSinglePhaseBRejection(
      result,
      RejectCodes.PlutusScriptInvalid,
    );
    expect(rejection.detail).toContain(
      "ReceivingScript requires MidgardV1 context",
    );
  });

  it("injects worker UPLC evaluation without changing coordinator-side context encoding", async () => {
    const spent = outRefFromByte(0x2d);
    const script = plutusV3ScriptWitness(Buffer.from("010203", "hex"));
    const scriptHash = hashScriptWitness(script);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    let evaluatorCalls = 0;
    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeProtectedScriptOutput(scriptHash, 10n)]]),
      {
        ...phaseBConfig,
        evaluateScript: (_scriptBytes, contextCbor) =>
          Effect.sync(() => {
            evaluatorCalls += 1;
            expect(contextCbor.byteLength).toBeGreaterThan(0);
            return { kind: "accepted", budget: { cpu: 1n, memory: 1n } };
          }),
      },
    );
    expect(evaluatorCalls).toBe(1);
    expect(result.accepted).toHaveLength(1);
    expect(result.rejected).toHaveLength(0);
  });

  it("executes V1 envelopes through the authenticated CEK graph", async () => {
    const spent = outRefFromByte(0x6d);
    const program = buildMidgardCanonicalCekProgramV1(
      Buffer.from(
        UPLCEncoder.compile(
          new UPLCProgram([1, 1, 0], new Lambda(new UPLCVar(0))),
        ).toBuffer().buffer,
      ),
    );
    const script = plutusV3ScriptWitness(program.envelopeCbor);
    const scriptHash = hashScriptWitness(script);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
      programMaterialSidecarCbor:
        encodeMidgardCekProgramMaterialSidecarV1(
          [...program.material.values()],
        ),
    });
    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeProtectedScriptOutput(scriptHash, 10n)]]),
    );
    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
  });

  it("propagates worker infrastructure failures instead of rejecting the tx", async () => {
    const spent = outRefFromByte(0x2e);
    const script = plutusV3ScriptWitness(Buffer.from("010203", "hex"));
    const scriptHash = hashScriptWitness(script);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: 10n,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    await expect(
      runPhaseB(
        [candidate],
        preState([[spent, makeProtectedScriptOutput(scriptHash, 10n)]]),
        {
          ...phaseBConfig,
          evaluateScript: () => Effect.fail(new Error("worker crashed")),
        },
      ),
    ).rejects.toThrow("worker crashed");
  });

  it("keeps the split evaluator bit-identical to the composed inline seam", () => {
    const script = Buffer.from("010203", "hex");
    const context = new Constr(0, []);
    expect(
      evaluateUplcWithContextCbor(script, encodeScriptContextCbor(context)),
    ).toStrictEqual(evaluateScriptWithHarmonic(script, context));
  });
});
