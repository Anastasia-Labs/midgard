import { DataConstr } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  constT,
  Lambda,
  UPLCConst,
  UPLCProgram,
  type UPLCBuiltinTag,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  buildMidgardCekExecutionGraph,
  executeMidgardCekStructuralProgram,
} from "../src/cek-executor.js";
import {
  buildMidgardCanonicalCekProgram,
  encodeMidgardCekCardanoFlatProgram,
} from "../src/cek-program.js";
import {
  PV11_CASES,
  type Pv11Case,
} from "./fixtures/cek-builtin-pv11.cases.js";
import { call, pv11CaseProgram } from "./fixtures/cek-builtin-pv11.programs.js";

const runtimeTypeFailures: Pv11Case[] = Array.from(
  { length: 14 },
  (_, index) => {
    const tag = BigInt(87 + index);
    const original = PV11_CASES.find((entry) => entry.tag === tag)!;
    return {
      ...original,
      name: `tag ${tag} runtime type failure`,
      arguments: [
        { type: { kind: "unit" }, payload: new DataConstr(0, []) },
        ...original.arguments.slice(1),
      ],
      expected: "failure",
    };
  },
);
runtimeTypeFailures.push({
  name: "dropList wrong constant shape",
  tag: 88n,
  arguments: [
    PV11_CASES.find((entry) => entry.tag === 88n)!.arguments[0]!,
    { type: { kind: "unit" }, payload: new DataConstr(0, []) },
  ],
  expected: "failure",
});

const execute = (flat: Buffer) => {
  const program = buildMidgardCanonicalCekProgram(flat);
  const graph = buildMidgardCekExecutionGraph(
    program.envelope,
    program.material.values(),
    Buffer.from("d87980", "hex"),
  );
  return executeMidgardCekStructuralProgram({
    root: graph.root,
    material: graph.material.values(),
    constantWitnesses: graph.constantWitnesses,
    maxSteps: 100_000,
    executionBudget: { cpu: 10_000_000_000n, memory: 16_500_000n },
    mode: "verdict",
  });
};

const malformedListToArray = (argument: UPLCTerm) =>
  execute(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [1, 1, 0],
        new Lambda(
          new Application(new Lambda(UPLCConst.unit), call(90, 1, argument)),
        ),
      ),
    ),
  );

describe("PV 11 structural execution", () => {
  it.each([
    ["a", 169_938n, 1_008n],
    ["a🦆β", 219_614n, 1_010n],
  ] as const)(
    "charges listToArray text by character count: %s",
    (text, cpu, memory) => {
      const result = malformedListToArray(UPLCConst.str(text));
      expect(result.stopReason).toBe("halted");
      expect(result.terminalState).toMatchObject({
        mode: "haltError",
        cpu,
        memory,
      });
    },
  );

  it("exhausts the budget for listToArray on a pair", () => {
    const result = malformedListToArray(
      UPLCConst.pairOf(constT.int, constT.int)(0n, 0n),
    );
    // Plutus SomeConstant assigns maxBound memory to every pair before charging.
    expect(result.stopReason).toBe("budgetExceeded");
    expect(result.terminalState.cpu).toBeGreaterThanOrEqual((1n << 63n) - 1n);
    expect(result.terminalState.memory).toBeGreaterThanOrEqual(
      (1n << 63n) - 1n,
    );
  });

  it.each([...PV11_CASES, ...runtimeTypeFailures])("$name", (entry) => {
    const result = execute(pv11CaseProgram(entry));
    expect(result.stopReason).toBe("halted");
    expect(result.terminalState.mode).toBe(
      entry.expected === "failure" ? "haltError" : "haltSuccess",
    );
  });

  it("passes PV 11 builtin values through existing polymorphic control builtins", () => {
    const body = call(
      26,
      1,
      UPLCConst.bool(true),
      new Builtin(87 as UPLCBuiltinTag),
      new Builtin(100 as UPLCBuiltinTag),
    );
    const result = execute(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram(
          [1, 1, 0],
          new Lambda(new Application(new Lambda(UPLCConst.unit), body)),
        ),
      ),
    );
    expect(result.stopReason).toBe("halted");
    expect(result.terminalState.mode).toBe("haltSuccess");
  });

  it("continues to refuse unsupported PV 11 proof steps", () => {
    const program = buildMidgardCanonicalCekProgram(
      pv11CaseProgram(PV11_CASES[0]!),
    );
    const graph = buildMidgardCekExecutionGraph(
      program.envelope,
      program.material.values(),
      Buffer.from("d87980", "hex"),
    );
    expect(() =>
      executeMidgardCekStructuralProgram({
        root: graph.root,
        material: graph.material.values(),
        constantWitnesses: graph.constantWitnesses,
        maxSteps: 100_000,
        mode: "proof",
      }),
    ).toThrow(/computeBuiltin transition does not pass/u);
  });
});
