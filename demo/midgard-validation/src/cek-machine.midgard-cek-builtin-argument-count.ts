import {
  encodeCbor,
  hashMidgardCekEnvironmentNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  MIDGARD_CEK_MAX_BUILTIN_TAG,
  type MidgardCekMachineState,
  type MidgardCekValueNode,
} from "@al-ft/midgard-core";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  type Bytes,
  MACHINE_STEP_CPU,
  MACHINE_STEP_MEMORY,
  MAP_CONVERSION_CONTROL_DOMAIN,
  type MidgardCekEnvironmentSummary,
  type MidgardCekMapConversionControl,
  sameBytes,
  UINT32_MAX,
} from "./cek-machine.midgard-cek-core-step-witness.js";

export const sameState = (
  left: MidgardCekMachineState,
  right: MidgardCekMachineState,
): boolean =>
  left.mode === right.mode &&
  left.executionIndex === right.executionIndex &&
  sameBytes(left.focusRoot, right.focusRoot) &&
  sameBytes(left.environmentRoot, right.environmentRoot) &&
  sameBytes(left.continuationRoot, right.continuationRoot) &&
  left.auxiliary === right.auxiliary &&
  left.cpu === right.cpu &&
  left.memory === right.memory;

export const encodeMidgardCekMapConversionControl = (
  control: MidgardCekMapConversionControl,
): Buffer =>
  encodeCbor([
    control.tag,
    Buffer.from(control.resultRoot),
    Buffer.from(control.sourceRoot),
    control.sourceRemaining,
    control.sourcePayloadCborLength,
    control.sourceMemory,
    Buffer.from(control.destinationRoot),
    control.destinationRemaining,
    control.destinationPayloadCborLength,
    control.destinationMemory,
    control.budgetCpu,
    control.budgetMemory,
  ]);

export const mapConversionControlIsWellFormed = (
  control: MidgardCekMapConversionControl,
): boolean => {
  if (
    (control.tag !== 38n && control.tag !== 43n) ||
    control.resultRoot.length !== 32 ||
    control.sourceRoot.length !== 32 ||
    control.destinationRoot.length !== 32 ||
    control.sourceRemaining < 0n ||
    control.sourceRemaining !== control.destinationRemaining ||
    control.sourcePayloadCborLength < 0n ||
    control.sourceMemory < 0n ||
    control.destinationPayloadCborLength < 0n ||
    control.destinationMemory < 0n ||
    control.budgetCpu < 0n ||
    control.budgetMemory < 0n
  ) {
    return false;
  }
  if (control.sourceRemaining !== 0n) return true;
  return (
    control.sourcePayloadCborLength === 0n &&
    control.sourceMemory === 0n &&
    control.destinationPayloadCborLength === 0n &&
    control.destinationMemory === 0n &&
    (control.tag === 38n
      ? sameBytes(control.sourceRoot, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT) &&
        sameBytes(control.destinationRoot, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT)
      : sameBytes(control.sourceRoot, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT) &&
        sameBytes(control.destinationRoot, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT))
  );
};

export const hashMidgardCekMapConversionControl = (
  control: MidgardCekMapConversionControl,
): Bytes => {
  if (!mapConversionControlIsWellFormed(control)) {
    throw new Error("invalid V1 CEK map-conversion control");
  }
  return Buffer.from(
    blake2b(
      Buffer.concat([
        MAP_CONVERSION_CONTROL_DOMAIN,
        encodeMidgardCekMapConversionControl(control),
      ]),
      { dkLen: 32 },
    ),
  );
};

export const exactState = (
  pre: MidgardCekMachineState,
  update: {
    readonly mode: MidgardCekMachineState["mode"];
    readonly focusRoot: Bytes;
    readonly environmentRoot: Bytes;
    readonly continuationRoot: Bytes;
    readonly auxiliary: bigint;
    readonly cpuDelta?: bigint;
    readonly memoryDelta?: bigint;
  },
): MidgardCekMachineState => ({
  mode: update.mode,
  executionIndex: pre.executionIndex,
  focusRoot: update.focusRoot,
  environmentRoot: update.environmentRoot,
  continuationRoot: update.continuationRoot,
  auxiliary: update.auxiliary,
  cpu: pre.cpu + (update.cpuDelta ?? 0n),
  memory: pre.memory + (update.memoryDelta ?? 0n),
});

export const exactComputeSuccessor = (
  pre: MidgardCekMachineState,
  update: Omit<Parameters<typeof exactState>[1], "cpuDelta" | "memoryDelta">,
): MidgardCekMachineState =>
  exactState(pre, {
    ...update,
    cpuDelta: MACHINE_STEP_CPU,
    memoryDelta: MACHINE_STEP_MEMORY,
  });

export const errorSuccessor = (
  pre: MidgardCekMachineState,
  reason: bigint,
): MidgardCekMachineState =>
  exactState(pre, {
    mode: "haltError",
    focusRoot: hashMidgardCekTermNode({ kind: "error" }),
    environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
    continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
    auxiliary: reason,
  });

export const nonNegativeUint32 = (value: bigint): boolean =>
  value >= 0n && value <= UINT32_MAX;

export const linkedSequenceRootIsWellFormed = (
  root: Bytes,
  count: bigint,
): boolean =>
  nonNegativeUint32(count) &&
  (count === 0n) === sameBytes(root, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT);

export const linkedSequenceTailIsWellFormed = (
  tail: Bytes,
  length: bigint,
): boolean =>
  length > 0n &&
  length <= UINT32_MAX &&
  (length === 1n) === sameBytes(tail, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT);

export const valueHash = (value: MidgardCekValueNode): Bytes =>
  hashMidgardCekValueNode(value);

export const isConstant = (value: MidgardCekValueNode): boolean =>
  value.kind === "constant";

export const isLambdaOrBuiltin = (value: MidgardCekValueNode): boolean =>
  value.kind === "lambda" || value.kind === "builtin";

export const isDelayOrForceableBuiltin = (
  value: MidgardCekValueNode,
): boolean =>
  value.kind === "delay" ||
  (value.kind === "builtin" && value.forcesRemaining > 0n);

export const environmentSummaryLength = (
  summary: MidgardCekEnvironmentSummary,
): bigint => (summary.kind === "empty" ? 0n : summary.length);

export const environmentSummaryMatches = (
  root: Bytes,
  summary: MidgardCekEnvironmentSummary,
): boolean => {
  if (summary.kind === "empty") {
    return sameBytes(root, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT);
  }
  return (
    summary.length > 0n &&
    summary.length <= UINT32_MAX &&
    sameBytes(
      root,
      hashMidgardCekEnvironmentNode({
        value: summary.value,
        tail: summary.tail,
        length: summary.length,
      }),
    ) &&
    (summary.length === 1n) ===
      sameBytes(summary.tail, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT)
  );
};

export const midgardCekBuiltinForceCount = (tag: bigint): bigint => {
  if (tag < 0n || tag > MIDGARD_CEK_MAX_BUILTIN_TAG) {
    throw new RangeError("CEK builtin tag is outside the V1 table");
  }
  if (tag === 29n || tag === 30n || tag === 31n) return 2n;
  if (tag === 26n || tag === 27n || tag === 28n || (tag >= 32n && tag <= 36n)) {
    return 1n;
  }
  return 0n;
};

export const midgardCekBuiltinArgumentCount = (tag: bigint): bigint => {
  if (tag < 0n || tag > MIDGARD_CEK_MAX_BUILTIN_TAG) {
    throw new RangeError("CEK builtin tag is outside the V1 table");
  }
  if (tag === 36n) return 6n;
  if ([12n, 21n, 26n, 31n, 52n, 53n, 73n, 75n, 76n, 77n, 80n].includes(tag)) {
    return 3n;
  }
  if (
    tag <= 11n ||
    [
      14n,
      15n,
      16n,
      17n,
      22n,
      23n,
      27n,
      28n,
      32n,
      37n,
      47n,
      48n,
      54n,
      56n,
      57n,
      58n,
      61n,
      63n,
      64n,
      65n,
      68n,
      69n,
      70n,
      74n,
      79n,
      81n,
      82n,
      83n,
    ].includes(tag)
  ) {
    return 2n;
  }
  return 1n;
};
