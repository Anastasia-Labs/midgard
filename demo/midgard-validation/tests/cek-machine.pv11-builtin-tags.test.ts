import {
  decodeMidgardCekProgramTermPreimage,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  MidgardCekErrorCodes,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";

const state = (
  mode: MidgardCekMachineState["mode"],
  focusRoot: Uint8Array,
  auxiliary: bigint,
  cpu: bigint,
  memory: bigint,
): MidgardCekMachineState => ({
  mode,
  executionIndex: 0n,
  focusRoot,
  environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  auxiliary,
  cpu,
  memory,
});

const computeBuiltin = (tag: bigint): MidgardCekMachineState =>
  state(
    "compute",
    hashMidgardCekTermNode({ kind: "builtin", tag }),
    0n,
    0n,
    0n,
  );

const builtinValue = (tag: bigint, forcesRemaining: bigint): Uint8Array =>
  hashMidgardCekValueNode({
    kind: "builtin",
    tag,
    forcesRemaining,
    argumentsCount: 0n,
    argumentsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  });

describe("PV 11 builtin tags", () => {
  it("admits Cardano's builtin indices 0-100 in program material", () => {
    for (const tag of [0n, 86n, 87n, 91n, 100n]) {
      const preimage = encodeMidgardCekTermNode({ kind: "builtin", tag });
      expect(decodeMidgardCekProgramTermPreimage(preimage)).toEqual({
        kind: "builtin",
        tag,
      });
    }
    for (const tag of [101n, 127n, -1n]) {
      expect(() => encodeMidgardCekTermNode({ kind: "builtin", tag })).toThrow(
        /between 0 and 100/u,
      );
    }
  });

  it("steps tags 87-100 to their builtin value, forced as Cardano requires", () => {
    const forces = (tag: bigint): bigint =>
      tag >= 88n && tag <= 91n ? 1n : 0n;
    for (let tag = 87n; tag <= 100n; tag += 1n) {
      const pre = computeBuiltin(tag);
      const step = { kind: "computeBuiltin", tag } as const;
      expect(
        verifyMidgardCekCoreStep(
          pre,
          state("return", builtinValue(tag, forces(tag)), 0n, 16_000n, 100n),
          step,
        ),
      ).toBe(true);
      for (const claimed of [
        state("return", builtinValue(tag, 1n - forces(tag)), 0n, 16_000n, 100n),
        state("return", builtinValue(86n, 0n), 0n, 16_000n, 100n),
        state(
          "haltError",
          hashMidgardCekTermNode({ kind: "error" }),
          MidgardCekErrorCodes.Explicit,
          0n,
          0n,
        ),
      ]) {
        expect(verifyMidgardCekCoreStep(pre, claimed, step)).toBe(false);
      }
    }
    expect(() => builtinValue(101n, 0n)).toThrow(/between 0 and 100/u);
  });
});
