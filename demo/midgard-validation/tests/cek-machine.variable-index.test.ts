import {
  encodeMidgardCekTermNode,
  hashMidgardCekEnvironmentNode,
  hashMidgardCekTermNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  MidgardCekErrorCodes,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";

const hash = (fill: number): Buffer => Buffer.alloc(32, fill);
const WORD64_MAX = (1n << 64n) - 1n;

const inner = hashMidgardCekEnvironmentNode({
  value: hash(1),
  tail: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  length: 1n,
});
const environment = hashMidgardCekEnvironmentNode({
  value: hash(2),
  tail: inner,
  length: 2n,
});

const state = (
  mode: MidgardCekMachineState["mode"],
  focusRoot: Uint8Array,
  environmentRoot: Uint8Array,
  auxiliary: bigint,
  cpu: bigint,
  memory: bigint,
): MidgardCekMachineState => ({
  mode,
  executionIndex: 0n,
  focusRoot,
  environmentRoot,
  continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  auxiliary,
  cpu,
  memory,
});

const computeVariable = (index: bigint): MidgardCekMachineState =>
  state(
    "compute",
    hashMidgardCekTermNode({ kind: "variable", index }),
    environment,
    0n,
    100n,
    100n,
  );

const lookupAt = (
  root: Uint8Array,
  auxiliary: bigint,
): MidgardCekMachineState =>
  state("lookup", root, root, auxiliary, 16_100n, 200n);

const unbound = (cpu: bigint, memory: bigint): MidgardCekMachineState =>
  state(
    "haltError",
    hashMidgardCekTermNode({ kind: "error" }),
    MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
    MidgardCekErrorCodes.UnboundVariable,
    cpu,
    memory,
  );

describe("CEK variable index", () => {
  it("uses Cardano's 1-based index: index 1 is the innermost binding", () => {
    const lookup = lookupAt(environment, 0n);
    expect(
      verifyMidgardCekCoreStep(computeVariable(1n), lookup, {
        kind: "computeVariable",
        index: 1n,
      }),
    ).toBe(true);
    expect(
      verifyMidgardCekCoreStep(
        lookup,
        state(
          "return",
          hash(2),
          MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          0n,
          16_100n,
          200n,
        ),
        { kind: "lookupEnvironment", value: hash(2), tail: inner, length: 2n },
      ),
    ).toBe(true);
    expect(
      verifyMidgardCekCoreStep(computeVariable(2n), lookupAt(environment, 1n), {
        kind: "computeVariable",
        index: 2n,
      }),
    ).toBe(true);
    expect(
      verifyMidgardCekCoreStep(computeVariable(2n), lookupAt(environment, 2n), {
        kind: "computeVariable",
        index: 2n,
      }),
    ).toBe(false);
  });

  it("fails index 0 in one uncharged compute step", () => {
    const witness = { kind: "computeVariable", index: 0n } as const;
    expect(
      verifyMidgardCekCoreStep(
        computeVariable(0n),
        unbound(100n, 100n),
        witness,
      ),
    ).toBe(true);
    for (const claimed of [
      lookupAt(environment, 0n),
      lookupAt(environment, -1n),
      unbound(16_100n, 200n),
    ]) {
      expect(
        verifyMidgardCekCoreStep(computeVariable(0n), claimed, witness),
      ).toBe(false);
    }
  });

  it("fails an index above the environment at the empty environment", () => {
    expect(
      verifyMidgardCekCoreStep(computeVariable(3n), lookupAt(environment, 2n), {
        kind: "computeVariable",
        index: 3n,
      }),
    ).toBe(true);
    expect(
      verifyMidgardCekCoreStep(
        lookupAt(MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT, 0n),
        unbound(16_100n, 200n),
        { kind: "lookupEmptyEnvironment" },
      ),
    ).toBe(true);
  });

  it("commits the full Word64 range and nothing above it", () => {
    expect(
      verifyMidgardCekCoreStep(
        computeVariable(WORD64_MAX),
        lookupAt(environment, WORD64_MAX - 1n),
        { kind: "computeVariable", index: WORD64_MAX },
      ),
    ).toBe(true);
    expect(() =>
      encodeMidgardCekTermNode({ kind: "variable", index: WORD64_MAX + 1n }),
    ).toThrow();
    expect(() =>
      encodeMidgardCekTermNode({ kind: "variable", index: -1n }),
    ).toThrow();
  });
});
