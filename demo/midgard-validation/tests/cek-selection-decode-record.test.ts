import { encodeCbor } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import { evaluateMidgardCekScript } from "../src/phase-b.evaluate-cek-script.js";
import { MidgardUplcCapabilityError } from "../src/plutus-script-evaluator.capability-error.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { Lambda, UPLCProgram, UPLCVar } from "../src/uplc-term.js";

const run = (scriptBytes: Buffer) => ({
  scriptBytes,
  scriptLanguage: "PlutusV3" as const,
  contextCbor: Buffer.from("d87980", "hex"),
  executionIndex: 0n,
  maxSteps: 100,
});

describe("CEK selection phase-two boundary", () => {
  it.each([5, 11] as const)(
    "class %s stops before any graph derivation",
    (phase2Class) => {
      const bytes = encodeCbor(
        Buffer.from(
          encodeMidgardCekCardanoFlatProgram(
            new UPLCProgram(
              phase2Class === 5 ? [2, 0, 0] : [1, 1, 0],
              new UPLCVar(0),
            ),
          ),
        ),
      );
      expect(evaluateMidgardCekScript(run(bytes), "proof")).toEqual({
        result: {
          kind: "script_invalid",
          detail: `UPLC PV11 phase-two class ${phase2Class}`,
        },
        phase2Class,
        graph: null,
        execution: null,
      });
    },
  );

  it("class zero continues into a real local structural execution", () => {
    const script = encodeCbor(
      Buffer.from(
        encodeMidgardCekCardanoFlatProgram(
          new UPLCProgram([1, 1, 0], new Lambda(UPLCConst.unit)),
        ),
      ),
    );
    const outcome = evaluateMidgardCekScript(run(script), "proof");
    expect(outcome.phase2Class).toBe(0);
    expect(outcome.graph).not.toBeNull();
    expect(outcome.execution?.terminalState.mode).toBe("haltSuccess");
    expect(outcome.result.kind).toBe("accepted");
  });

  it("phase-one refusal reaching proof execution is no verdict", () => {
    const outcome = evaluateMidgardCekScript(run(Buffer.from([0])), "proof");
    expect(outcome.result.kind).toBe("indeterminate");
    expect(
      outcome.result.kind === "indeterminate" ? outcome.result.cause : null,
    ).toBeInstanceOf(MidgardUplcCapabilityError);
  });
});
