import {
  advanceMidgardRedeemerItemProof,
  ensureHash32,
  hashMidgardRedeemerItemProofControl,
  isWellFormedMidgardRedeemerItemProofControl,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardBlake2b256TraceControl,
  type MidgardBoundedItemChunkProof,
  type MidgardCekDataBytesControl,
  type MidgardCekDataFrame,
  type MidgardCekDataIntegerControl,
  type MidgardCekDataSummary,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  type MidgardCekSourceBlobControl,
  type MidgardRedeemerItemProofControl,
  type MidgardRedeemerItemProofWitness,
} from "@al-ft/midgard-core";
import { Constr } from "@lucid-evolution/lucid";
type PlutusDataValue = unknown;
const requireConstr = ({
  value,
  index,
  fields,
  label,
}: {
  readonly value: PlutusDataValue;
  readonly index: number;
  readonly fields: number;
  readonly label: string;
}): Constr<PlutusDataValue> => {
  if (
    !(value instanceof Constr) ||
    value.index !== index ||
    value.fields.length !== fields
  ) {
    throw new Error(
      `${label} must be constructor ${index.toString()} with ${fields.toString()} fields`,
    );
  }
  return value;
};

const exactCborBigInt = (
  value: PlutusDataValue | undefined,
  label: string,
): bigint => {
  if (typeof value !== "bigint") {
    throw new Error(`${label} must be an exact CBOR integer`);
  }
  return value;
};

const requireOptionData = (
  value: PlutusDataValue,
  label: string,
): PlutusDataValue | null => {
  if (!(value instanceof Constr)) {
    throw new Error(`${label} must be an option constructor`);
  }
  if (value.index === 1 && value.fields.length === 0) return null;
  if (value.index === 0 && value.fields.length === 1) return value.fields[0]!;
  throw new Error(`${label} must be an exact Some or None`);
};

const dataSummaryCore = (
  value: PlutusDataValue,
  label: string,
): MidgardCekDataSummary => {
  const summary = requireConstr({ value, index: 0, fields: 3, label });
  if (typeof summary.fields[0] !== "string") {
    throw new Error(`${label}.root must be bytes`);
  }
  return {
    root: Buffer.from(summary.fields[0], "hex"),
    cborLength: exactCborBigInt(summary.fields[1], `${label}.cbor_length`),
    memory: exactCborBigInt(summary.fields[2], `${label}.memory`),
  };
};

const dataFrameCore = (
  value: PlutusDataValue,
  label: string,
): MidgardCekDataFrame => {
  const frame = requireConstr({ value, index: 0, fields: 11, label });
  const bytes = (index: number, field: string): Buffer => {
    const selected = frame.fields[index];
    if (typeof selected !== "string") {
      throw new Error(`${label}.${field} must be bytes`);
    }
    return Buffer.from(selected, "hex");
  };
  const integer = (index: number, field: string): bigint =>
    exactCborBigInt(frame.fields[index], `${label}.${field}`);
  const frontier = frame.fields[8];
  if (!Array.isArray(frontier)) {
    throw new Error(`${label}.child_peaks must be a frontier`);
  }
  const childPeaks = frontier.map((peak, index) => {
    const fields = requireConstr({
      value: peak,
      index: 0,
      fields: 2,
      label: `${label}.child_peaks[${index.toString()}]`,
    }).fields;
    if (typeof fields[1] !== "string") {
      throw new Error(`${label}.child_peaks hash must be bytes`);
    }
    return {
      height: exactSafeCborInteger(fields[0], `${label}.child_peaks height`),
      hash: Buffer.from(fields[1], "hex"),
    };
  });
  const sequence = requireConstr({
    value: frame.fields[10]!,
    index: 0,
    fields: 4,
    label: `${label}.sequence`,
  });
  if (typeof sequence.fields[0] !== "string") {
    throw new Error(`${label}.sequence.root must be bytes`);
  }
  const common = {
    tail: bytes(5, "tail"),
    expectedChildren: exactSafeCborInteger(
      frame.fields[6],
      `${label}.expected_children`,
    ),
    childCount: exactSafeCborInteger(frame.fields[7], `${label}.child_count`),
    childFrontier: {
      count: exactSafeCborInteger(frame.fields[7], `${label}.child_count`),
      peaks: childPeaks,
    },
    foldCursor: exactSafeCborInteger(frame.fields[9], `${label}.fold_cursor`),
    sequence: {
      root: Buffer.from(sequence.fields[0], "hex"),
      length: exactCborBigInt(sequence.fields[1], `${label}.sequence.length`),
      payloadCborLength: exactCborBigInt(
        sequence.fields[2],
        `${label}.sequence.payload_cbor_length`,
      ),
      memory: exactCborBigInt(sequence.fields[3], `${label}.sequence.memory`),
    },
  } as const;
  const kind = exactSafeCborInteger(frame.fields[0], `${label}.kind`);
  if (kind === 0)
    return {
      ...common,
      kind: "constrSmall",
      constructor: integer(1, "constructor"),
    };
  if (kind === 1) {
    return {
      ...common,
      kind: "constrLarge",
      constructorCborRoot: bytes(2, "constructor_cbor_root"),
      constructorCborLength: integer(3, "constructor_cbor_length"),
      constructorMemory: integer(4, "constructor_memory"),
    };
  }
  if (kind === 2) return { ...common, kind: "list" };
  if (kind === 3) return { ...common, kind: "map" };
  throw new Error(`${label}.kind is not a supported data frame`);
};

const exactSafeCborInteger = (value: unknown, label: string): number => {
  if (
    typeof value !== "bigint" ||
    value < BigInt(Number.MIN_SAFE_INTEGER) ||
    value > BigInt(Number.MAX_SAFE_INTEGER)
  )
    throw new Error(`${label} must be a safe integer`);
  return Number(value);
};

const fields = (value: unknown, count: number): unknown[] =>
  requireConstr({
    value,
    index: 0,
    fields: count,
    label: "redeemer item state",
  }).fields;
const integer = (value: unknown | undefined): number =>
  exactSafeCborInteger(value, "redeemer item integer");
const bigint = (value: unknown | undefined): bigint =>
  exactCborBigInt(value, "redeemer item integer");
const bytes = (value: unknown | undefined): Buffer => {
  if (typeof value !== "string" || !/^(?:[0-9a-fA-F]{2})*$/.test(value))
    throw new Error("redeemer item bytes are malformed");
  return Buffer.from(value, "hex");
};
const list = (value: unknown | undefined): unknown[] => {
  if (!Array.isArray(value)) throw new Error("redeemer item list is malformed");
  return value;
};
const option = <T>(value: unknown, decode: (value: unknown) => T): T | null => {
  const inner = requireOptionData(value, "redeemer item option");
  return inner === null ? null : decode(inner);
};
const hash = (value: unknown | undefined) =>
  ensureHash32(bytes(value), "redeemer item hash");
const blakeControl = (value: unknown): MidgardBlake2b256TraceControl => {
  const f = fields(value, 9);
  return {
    version: integer(f[0]) as 1,
    stage: integer(f[1]) as MidgardBlake2b256TraceControl["stage"],
    cursor: integer(f[2]),
    totalLength: integer(f[3]),
    chainingValue: bytes(f[4]),
    activeBlock: bytes(f[5]),
    activeBlockLength: integer(f[6]),
    workingValue: bytes(f[7]),
    round: integer(f[8]),
  };
};
const blobControl = (value: unknown): MidgardCekSourceBlobControl => {
  const f = fields(value, 6),
    frontier = fields(f[4]!, 3);
  return {
    version: integer(f[0]) as 1,
    stage: integer(f[1]) as MidgardCekSourceBlobControl["stage"],
    sourceStart: integer(f[2]),
    sourceLength: integer(f[3]),
    frontier: {
      count: integer(frontier[0]),
      byteLength: bigint(frontier[1]),
      peaks: list(frontier[2]).map((value) => {
        const p = fields(value, 3);
        return {
          height: integer(p[0]),
          root: hash(p[1]),
          byteLength: bigint(p[2]),
        };
      }),
    },
    activeHash: option(f[5]!, blakeControl),
  };
};
const integerControl = (value: unknown): MidgardCekDataIntegerControl => {
  const f = fields(value, 6);
  return {
    version: integer(f[0]) as 1,
    stage: integer(f[1]) as MidgardCekDataIntegerControl["stage"],
    sourceStart: integer(f[2]),
    sourceLength: integer(f[3]),
    memory: bigint(f[4]),
    blob: option(f[5]!, blobControl),
  };
};
const bytesControl = (value: unknown): MidgardCekDataBytesControl => {
  const f = fields(value, 6);
  return {
    version: integer(f[0]) as 1,
    stage: integer(f[1]) as MidgardCekDataBytesControl["stage"],
    sourceStart: integer(f[2]),
    sourceLength: integer(f[3]),
    bytesLength: integer(f[4]),
    blob: option(f[5]!, blobControl),
  };
};
const traversalControl = (value: unknown): MidgardCekDataTraverseControl => {
  const f = fields(value, 10);
  return {
    version: integer(f[0]) as 1,
    stage: integer(f[1]) as MidgardCekDataTraverseControl["stage"],
    sourceStart: integer(f[2]),
    sourceLength: integer(f[3]),
    offset: integer(f[4]),
    frameRoot: bytes(f[5]),
    pendingLargeExpectedChildren: option(f[6]!, integer),
    integer: option(f[7]!, integerControl),
    bytes: option(f[8]!, bytesControl),
    result: option(f[9]!, (value) =>
      dataSummaryCore(value, "redeemer item result"),
    ),
  };
};
/** Exact raw Data decoder shared by ScriptSources and CEK item consumers. */
export const decodeRedeemerItemControlData = (
  value: unknown,
): MidgardRedeemerItemProofControl => {
  const f = fields(value, 16);
  const control: MidgardRedeemerItemProofControl = {
    version: integer(f[0]) as 1,
    mode: integer(f[1]) as MidgardRedeemerItemProofControl["mode"],
    stage: integer(f[2]) as MidgardRedeemerItemProofControl["stage"],
    itemIndex: integer(f[3]),
    itemCount: integer(f[4]),
    totalLength: integer(f[5]),
    itemCommitment: bytes(f[6]),
    expectedPurposeTag: integer(f[7]),
    expectedPointerIndex: integer(f[8]),
    purposeTag: integer(f[9]),
    pointerIndex: integer(f[10]),
    dataOffset: integer(f[11]),
    dataLength: integer(f[12]),
    executionMemory: bigint(f[13]),
    executionSteps: bigint(f[14]),
    traversal: option(f[15]!, traversalControl),
  };
  if (!isWellFormedMidgardRedeemerItemProofControl(control))
    throw new Error("redeemer item control is not well formed");
  return control;
};
const traversalAction = (value: unknown): MidgardCekDataTraverseAction => {
  if (!(value instanceof Constr))
    throw new Error("redeemer item action is not a constructor");
  const arities = [0, 1, 1, 0, 2, 1, 4, 6, 2];
  if (arities[value.index] !== value.fields.length)
    throw new Error("redeemer item traversal action arity differs");
  const f = value.fields;
  const frame = (value: unknown) => dataFrameCore(value, "redeemer item frame");
  const summary = (value: unknown) =>
    dataSummaryCore(value, "redeemer item summary");
  switch (value.index) {
    case 0:
      return null;
    case 1:
      return { kind: "headScalar", itemLength: integer(f[0]) };
    case 2:
      return { kind: "headSequence", expectedChildren: integer(f[0]) };
    case 3:
      return { kind: "headMap" };
    case 4:
      return {
        kind: "headLargeConstructor",
        constructorCborLength: integer(f[0]),
        expectedChildren: integer(f[1]),
      };
    case 5:
      return { kind: "attachScalar", parent: option(f[0]!, frame) };
    case 6:
      return {
        kind: "foldList",
        frame: frame(f[0]!),
        childIndex: integer(f[1]),
        child: summary(f[2]!),
        siblings: list(f[3]).map(hash),
      };
    case 7:
      return {
        kind: "foldMap",
        frame: frame(f[0]!),
        pairIndex: integer(f[1]),
        key: summary(f[2]!),
        value: summary(f[3]!),
        keySiblings: list(f[4]).map(hash),
        valueSiblings: list(f[5]).map(hash),
      };
    case 8:
      return {
        kind: "finalizeFrame",
        frame: frame(f[0]!),
        parent: option(f[1]!, frame),
      };
    default:
      throw new Error("unsupported redeemer item action");
  }
};
const chunkProof = (value: unknown): MidgardBoundedItemChunkProof => {
  const f = fields(value, 8);
  return {
    version: integer(f[0]) as 1,
    fieldIndex: integer(f[1]),
    itemIndex: integer(f[2]),
    totalLength: integer(f[3]),
    chunkIndex: integer(f[4]),
    chunk: bytes(f[5]),
    frontier: {
      count: Math.max(
        1,
        Math.ceil(integer(f[3]) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES),
      ),
      peaks: list(f[6]).map((value) => {
        const p = fields(value, 2);
        return { height: integer(p[0]), hash: hash(p[1]) };
      }),
    },
    siblings: list(f[7]).map(hash),
  };
};
export const decodeRedeemerItemWitnessData = (
  value: unknown,
): MidgardRedeemerItemProofWitness => {
  const f = fields(value, 3),
    action = f[0];
  if (
    !(action instanceof Constr) ||
    [0, 0, 1, 0][action.index] !== action.fields.length
  )
    throw new Error("redeemer item action is malformed");
  return {
    action:
      action.index === 0
        ? { kind: "openHeader" }
        : action.index === 1
          ? { kind: "openTail" }
          : action.index === 2
            ? {
                kind: "traverseData",
                action: traversalAction(action.fields[0]!),
              }
            : { kind: "finishData" },
    chunkProof: option(f[1]!, chunkProof),
    nextChunkProof: option(f[2]!, chunkProof),
  };
};
/** Retained claimed output is untrusted: derive it again with the canonical machine. */
export const deriveRedeemerItemStepPlan = ({
  current,
  witness,
  claimedNext,
}: {
  readonly current: unknown;
  readonly witness: unknown;
  readonly claimedNext: unknown;
}) => {
  const control = decodeRedeemerItemControlData(current);
  const next = advanceMidgardRedeemerItemProof({
    control,
    witness: decodeRedeemerItemWitnessData(witness),
  });
  if (next === null) throw new Error("redeemer item witness does not advance");
  const claimed = decodeRedeemerItemControlData(claimedNext);
  const nextHash = hashMidgardRedeemerItemProofControl(next);
  if (!nextHash.equals(hashMidgardRedeemerItemProofControl(claimed)))
    throw new Error(
      "retained redeemer item successor differs from canonical advance",
    );
  return {
    control,
    next,
    currentControlHash: hashMidgardRedeemerItemProofControl(control),
    nextControlHash: nextHash,
    claimedNext,
  };
};
