import {
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  Application,
  Lambda,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { Constr } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { buildMidgardCanonicalCekProgram } from "../src/cek-program.js";
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
  outputCborMeetsMinAda,
  outputCborMinAdaLovelace,
} from "../src/value-accounting.js";
import {
  FUNDED_OUTPUT_LOVELACE,
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
  // #618 ruling 1 / R8 of decision 0005. The fixtures in this file produce
  // outputs funded at `FUNDED_OUTPUT_LOVELACE` so that the minimum-Ada floor
  // is never what they measure. That is a claim about a number, so it is
  // measured here rather than assumed: if a rate, intercept or codec change
  // ever lifts the floor above the fixture funding, this one test fails with a
  // clear message instead of scattering `E_MIN_ADA` across every unrelated
  // assertion in the file. The adjacent boundary itself is pinned in
  // min-ada-twin-cross-check.test.ts and in the Aiken wiring vectors.
  it("funds every produced fixture output above the minimum-Ada floor", () => {
    const shapes: readonly (readonly [string, Buffer])[] = [
      ["plain enterprise output", makeOutput(FUNDED_OUTPUT_LOVELACE)],
      [
        "protected script output",
        makeProtectedScriptOutput(
          Buffer.alloc(28, 0x5c).toString("hex"),
          FUNDED_OUTPUT_LOVELACE,
        ),
      ],
    ];
    for (const [label, outputCbor] of shapes) {
      const floor = outputCborMinAdaLovelace(outputCbor);
      expect(
        outputCborMeetsMinAda(outputCbor, FUNDED_OUTPUT_LOVELACE),
        `${label}: ${FUNDED_OUTPUT_LOVELACE.toString()} lovelace no longer clears the ${floor.toString()}-lovelace floor for ${outputCbor.length.toString()} serialized bytes`,
      ).toBe(true);
      // Headroom, not a coincidence: the fixture funding is meant to sit well
      // clear of the floor, so a shape that only just clears it is also a
      // failure of this file's premise.
      expect(FUNDED_OUTPUT_LOVELACE).toBeGreaterThan(floor * 2n);
    }
    // The value-preservation fixtures spend one lovelace of the funded amount;
    // that leg must still clear the floor, or those tests would measure
    // min-Ada instead of value preservation.
    expect(
      outputCborMeetsMinAda(
        makeOutput(FUNDED_OUTPUT_LOVELACE - 1n),
        FUNDED_OUTPUT_LOVELACE - 1n,
      ),
    ).toBe(true);
  });

  it("accepts a balanced candidate and returns a deterministic UTxO patch", async () => {
    const spent = outRefFromByte(0x21);
    const inputOutput = makeOutput(FUNDED_OUTPUT_LOVELACE);
    const candidate = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
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
    expect(
      state.get(producedOutRef)?.equals(makeOutput(FUNDED_OUTPUT_LOVELACE)),
    ).toBe(true);
  });

  it("rejects missing spend inputs before value accounting", async () => {
    const candidate = makePhaseBCandidate({
      spent: [outRefFromByte(0x22)],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });

    const result = await runPhaseB([candidate], new Map());

    expectSinglePhaseBRejection(result, RejectCodes.InputNotFound);
  });

  it("rejects missing pubkey witnesses for resolved spend inputs", async () => {
    const spent = outRefFromByte(0x23);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
      omitVkeyWitness: true,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.MissingRequiredWitness);
  });

  it("rejects malformed pre-state outputs that cannot be decoded", async () => {
    const spent = outRefFromByte(0x24);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, Buffer.from("ff", "hex")]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.InvalidOutput);
  });

  it.each([
    { label: "testnet output on mainnet", expected: 1n, rawNibble: 0 },
    { label: "mainnet output on testnet", expected: 0n, rawNibble: 1 },
    {
      label: "foreign unprotected network nibble 2",
      expected: 0n,
      rawNibble: 2,
    },
    {
      label: "foreign protected network nibble 15",
      expected: 0n,
      rawNibble: 15,
    },
  ])(
    "rejects $label at the script-sources output scan",
    async ({ expected, rawNibble }) => {
      const spent = outRefFromByte(0x7d);
      const address = Buffer.from(TEST_ADDRESS_BYTES);
      address[0] = (address[0]! & 0xf0) | rawNibble;
      const candidate = makePhaseBCandidate({
        spent: [spent],
        outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE, address)],
      });
      const rebound = {
        ...candidate,
        derived: { ...candidate.derived, expectedNetworkId: expected },
      };
      const result = await runPhaseB(
        [rebound],
        preState([[spent, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
      );
      const rejection = expectSinglePhaseBRejection(
        result,
        RejectCodes.NetworkIdMismatch,
      );
      expect(rejection.consensusPhase).toBe("scriptSources");
    },
  );

  it("rejects transactions that do not preserve value", async () => {
    const spent = outRefFromByte(0x25);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    expectSinglePhaseBRejection(result, RejectCodes.ValueNotPreserved);
  });

  it("burns the exact L2 fee in production value accounting and rejects fee redirection", async () => {
    const spent = outRefFromByte(0x7c);
    const state = preState([[spent, makeOutput(FUNDED_OUTPUT_LOVELACE)]]);
    const exactBurn = makePhaseBCandidate({
      spent: [spent],
      fee: 1n,
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
    });

    const accepted = await runPhaseB([exactBurn], state);
    expect(accepted.rejected).toHaveLength(0);
    expect(accepted.accepted).toHaveLength(1);

    const redirected = makePhaseBCandidate({
      spent: [spent],
      fee: 1n,
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    const rejected = await runPhaseB([redirected], state);
    const rejection = expectSinglePhaseBRejection(
      rejected,
      RejectCodes.ValueNotPreserved,
    );
    expect(rejection.consensusPhase).toBe("valueAndMint");
    expect(rejection.detail).toBe(
      "equation mismatch: inputs - fee + mint - outputs = lovelace=-1 assets=none",
    );
  });

  it("rejects candidates outside the current Cardano slot interval", async () => {
    const spent = outRefFromByte(0x26);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
      validityIntervalStart: 101n,
      validityIntervalEnd: 110n,
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    const second = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [spent],
      referenceInputs: [reference],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });

    const result = await runPhaseB(
      [first, second],
      preState([
        [spent, makeOutput(FUNDED_OUTPUT_LOVELACE)],
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
    const termPreimage = encodeMidgardCekTermNode({ kind: "error" });
    const termRoot = hashMidgardCekTermNode({ kind: "error" });
    const envelope = encodeMidgardCekProgramEnvelope({
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
          outputLovelace: FUNDED_OUTPUT_LOVELACE,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
            [],
          ),
        }),
      ],
      preState([
        [spent, makeOutput(FUNDED_OUTPUT_LOVELACE)],
        [reference, referenceOutput],
      ]),
    );
    expectSinglePhaseBRejection(missing, RejectCodes.CekProgramMaterial);

    const covered = await runPhaseB(
      [
        makePhaseBCandidate({
          spent: [spent],
          referenceInputs: [reference],
          outputLovelace: FUNDED_OUTPUT_LOVELACE,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
            material,
          ]),
        }),
      ],
      preState([
        [spent, makeOutput(FUNDED_OUTPUT_LOVELACE)],
        [reference, referenceOutput],
      ]),
    );
    expect(covered.rejected).toHaveLength(0);
    expect(covered.accepted).toHaveLength(1);

    const extraNode = { kind: "variable" as const, index: 0n };
    const extra = await runPhaseB(
      [
        makePhaseBCandidate({
          spent: [spent],
          referenceInputs: [reference],
          outputLovelace: FUNDED_OUTPUT_LOVELACE,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
            material,
            {
              kind: "term",
              root: hashMidgardCekTermNode(extraNode),
              preimage: encodeMidgardCekTermNode(extraNode),
            },
          ]),
        }),
      ],
      preState([
        [spent, makeOutput(FUNDED_OUTPUT_LOVELACE)],
        [reference, referenceOutput],
      ]),
    );
    expectSinglePhaseBRejection(extra, RejectCodes.CekProgramMaterial);
  });

  it("rejects a later reference input after an earlier component member spends it", async () => {
    const spent = outRefFromByte(0x2f);
    const other = outRefFromByte(0x30);
    const spender = makePhaseBCandidate({
      arrivalSeq: 0n,
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    const referencer = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [other],
      referenceInputs: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    const result = await runPhaseB(
      [spender, referencer],
      preState([
        [spent, makeOutput(FUNDED_OUTPUT_LOVELACE)],
        [other, makeOutput(FUNDED_OUTPUT_LOVELACE)],
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
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
    });
    const parentOutputRef = parent.graph.produced[0][LedgerColumns.OUTREF];
    const child = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [parentOutputRef],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
    });

    const result = await runPhaseB(
      [parent, child],
      preState([[parentInput, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
    });
    const second = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [first.graph.produced[0][LedgerColumns.OUTREF]],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
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
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
      scriptLanguages: ["PlutusV3"],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Mint, index: 0n },
      ]),
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
      outputs: [makeProtectedScriptOutput(scriptHash, FUNDED_OUTPUT_LOVELACE)],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Receiving, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });

    const result = await runPhaseB(
      [candidate],
      preState([[spent, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    let evaluatorCalls = 0;
    const result = await runPhaseB(
      [candidate],
      preState([
        [spent, makeProtectedScriptOutput(scriptHash, FUNDED_OUTPUT_LOVELACE)],
      ]),
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
    const program = buildMidgardCanonicalCekProgram(
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
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
        ...program.material.values(),
      ]),
    });
    const result = await runPhaseB(
      [candidate],
      preState([
        [spent, makeProtectedScriptOutput(scriptHash, FUNDED_OUTPUT_LOVELACE)],
      ]),
    );
    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
  });

  it("bounds a nonterminating V1 envelope by its declared execution units", async () => {
    const spent = outRefFromByte(0x6e);
    const selfApplication = new Lambda(
      new Application(new UPLCVar(0), new UPLCVar(0)),
    );
    const program = buildMidgardCanonicalCekProgram(
      Buffer.from(
        UPLCEncoder.compile(
          new UPLCProgram(
            [1, 1, 0],
            new Application(selfApplication, selfApplication),
          ),
        ).toBuffer().buffer,
      ),
    );
    const script = plutusV3ScriptWitness(program.envelopeCbor);
    const scriptHash = hashScriptWitness(script);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          exUnits: [0n, 0n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
      programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
        ...program.material.values(),
      ]),
    });

    const result = await runPhaseB(
      [candidate],
      preState([
        [spent, makeProtectedScriptOutput(scriptHash, FUNDED_OUTPUT_LOVELACE)],
      ]),
    );

    const rejection = expectSinglePhaseBRejection(
      result,
      RejectCodes.PlutusScriptInvalid,
    );
    expect(rejection.detail).toContain("budget exceeded");
    expect(rejection.detail).toContain("declared mem=0 cpu=0");
  });

  it("propagates worker infrastructure failures instead of rejecting the tx", async () => {
    const spent = outRefFromByte(0x2e);
    const script = plutusV3ScriptWitness(Buffer.from("010203", "hex"));
    const scriptHash = hashScriptWitness(script);
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputLovelace: FUNDED_OUTPUT_LOVELACE,
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    });
    await expect(
      runPhaseB(
        [candidate],
        preState([
          [
            spent,
            makeProtectedScriptOutput(scriptHash, FUNDED_OUTPUT_LOVELACE),
          ],
        ]),
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
