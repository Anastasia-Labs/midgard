import {
  commitMidgardCekBlobV1,
  decodeMidgardCekDataListNodeV1,
  decodeMidgardCekDataNodeV1,
  decodeMidgardCekDataPairNodeV1,
  decodeMidgardCekProgramBlobPreimageV1,
  decodeMidgardCekProgramMaterialEntryV1,
  decodeMidgardCekProgramSequencePreimageV1,
  decodeMidgardCekProgramTermPreimageV1,
  decodeMidgardCekProgramValuePreimageV1,
  encodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekSequenceNodeV1,
  encodeMidgardCekTermNodeV1,
  encodeMidgardCekValueNodeV1,
  type Hash32,
  hashMidgardCekBlobChunkV1,
  hashMidgardCekContinuationFrameV1,
  hashMidgardCekEnvironmentNodeV1,
  hashMidgardCekSequenceNodeV1,
  hashMidgardCekTermNodeV1,
  hashMidgardCekValueNodeV1,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
  type MidgardCekContinuationFrameV1,
  type MidgardCekDataListNodeV1,
  type MidgardCekDataNodeV1,
  type MidgardCekDataPairNodeV1,
  type MidgardCekMachineStateV1,
  type MidgardCekProgramEnvelopeV1,
  type MidgardCekProgramMaterialEntryV1,
  type MidgardCekTermNodeV1,
  type MidgardCekValueNodeV1,
  verifyMidgardCekProgramMaterialV1,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { UPLCConst } from "@harmoniclabs/uplc";

import {
  evaluateMidgardCekBlsFinalV1,
  evaluateMidgardCekDirectBuiltinV1,
  hashMidgardCekDirectValueWitnessV1,
  hashMidgardCekRuntimeValueWitnessV1,
  type MidgardCekBlsExpressionWitnessV1,
  type MidgardCekConstantValueWitnessV1,
  midgardCekDirectBuiltinBudgetV1,
  type MidgardCekDirectValueWitnessV1,
  type MidgardCekRuntimeValueWitnessV1,
  verifyMidgardCekBuiltinTypeFailureV1,
} from "./cek-builtin.js";
import {
  decodeMidgardCekConstantTypeCborV1,
  decodeMidgardCekConstantWitnessV1,
  encodeMidgardCekCanonicalConstantV1,
  encodeMidgardCekConstantTypeCborV1,
  encodeMidgardCekPlutusDataV1,
  hashMidgardCekConstantWitnessV1,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES_V1,
  midgardCekConstantMemorySizeV1,
  type MidgardCekConstantTypeV1,
} from "./cek-constant.js";
import { commitMidgardCekDataTreeV1 } from "./cek-data-tree.js";
import {
  hashMidgardCekMapConversionControlV1,
  midgardCekBuiltinArgumentCount,
  midgardCekBuiltinForceCount,
  type MidgardCekCoreStepWitnessV1,
  type MidgardCekEnvironmentSummaryV1,
  MidgardCekErrorCodes,
  type MidgardCekMapConversionControlV1,
  verifyMidgardCekCoreStepV1,
} from "./cek-machine.js";

type Bytes = Uint8Array;

const MACHINE_STEP_CPU = 16_000n;
const MACHINE_STEP_MEMORY = 100n;

const rootHex = (root: Bytes): string => Buffer.from(root).toString("hex");

const sameBytes = (left: Bytes, right: Bytes): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

const exactState = (
  pre: MidgardCekMachineStateV1,
  update: {
    readonly mode: MidgardCekMachineStateV1["mode"];
    readonly focusRoot: Bytes;
    readonly environmentRoot: Bytes;
    readonly continuationRoot: Bytes;
    readonly auxiliary: bigint;
    readonly cpuDelta?: bigint;
    readonly memoryDelta?: bigint;
  },
): MidgardCekMachineStateV1 =>
  Object.freeze({
    mode: update.mode,
    executionIndex: pre.executionIndex,
    focusRoot: Buffer.from(update.focusRoot),
    environmentRoot: Buffer.from(update.environmentRoot),
    continuationRoot: Buffer.from(update.continuationRoot),
    auxiliary: update.auxiliary,
    cpu: pre.cpu + (update.cpuDelta ?? 0n),
    memory: pre.memory + (update.memoryDelta ?? 0n),
  });

const exactComputeSuccessor = (
  pre: MidgardCekMachineStateV1,
  update: Omit<Parameters<typeof exactState>[1], "cpuDelta" | "memoryDelta">,
): MidgardCekMachineStateV1 =>
  exactState(pre, {
    ...update,
    cpuDelta: MACHINE_STEP_CPU,
    memoryDelta: MACHINE_STEP_MEMORY,
  });

const errorSuccessor = (
  pre: MidgardCekMachineStateV1,
  reason: bigint,
): MidgardCekMachineStateV1 =>
  exactState(pre, {
    mode: "haltError",
    focusRoot: hashMidgardCekTermNodeV1({ kind: "error" }),
    environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
    continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
    auxiliary: reason,
  });

type EnvironmentNodeV1 = {
  readonly value: Hash32;
  readonly tail: Hash32;
  readonly length: bigint;
};

type SequenceNodeV1 = {
  readonly head: Hash32;
  readonly tail: Hash32;
  readonly length: bigint;
};

export type MidgardCekExecutionGraphV1 = {
  readonly root: Hash32;
  readonly contextTermRoot: Hash32;
  readonly contextValueRoot: Hash32;
  readonly material: ReadonlyMap<string, MidgardCekProgramMaterialEntryV1>;
  readonly constantWitnesses: ReadonlyMap<
    string,
    MidgardCekConstantValueWitnessV1
  >;
};

export type MidgardCekExecutionStepV1 = {
  readonly pre: MidgardCekMachineStateV1;
  readonly post: MidgardCekMachineStateV1;
  readonly witness: MidgardCekCoreStepWitnessV1;
};

export type MidgardCekStructuralExecutionV1 = {
  readonly initialState: MidgardCekMachineStateV1;
  readonly steps: readonly MidgardCekExecutionStepV1[];
  readonly terminalState: MidgardCekMachineStateV1;
  readonly stopReason: "halted" | "budgetExceeded";
};

/**
 * Builds the deterministic runtime application root
 * `program scriptContext`. The source envelope remains the script identity;
 * the context nodes are derived from already-authenticated transaction data
 * and are therefore runtime material rather than a second script payload.
 */
export const buildMidgardCekExecutionGraphV1 = (
  envelope: MidgardCekProgramEnvelopeV1,
  sourceMaterial: Iterable<MidgardCekProgramMaterialEntryV1>,
  contextCbor: Uint8Array,
): MidgardCekExecutionGraphV1 => {
  const source = [...sourceMaterial];
  const verifiedSource = verifyMidgardCekProgramMaterialV1(envelope, source, {
    allowUnreachable: true,
  });

  const material = new Map<string, MidgardCekProgramMaterialEntryV1>();
  const constantWitnesses = new Map<string, MidgardCekConstantValueWitnessV1>(
    verifiedSource.constants.map((constant) => {
      if (constant.payloadCbor.length <= 9_215) {
        return [
          rootHex(constant.valueRoot),
          Object.freeze({
            kind: "constant" as const,
            witness: Object.freeze({
              typeCbor: constant.typeCbor,
              payloadCbor: constant.payloadCbor,
            }),
          }),
        ];
      }
      const payload = dataFromCbor(constant.payloadCbor);
      const semantic = commitMidgardCekDataTreeV1(payload);
      if (!sameBytes(semantic.root, constant.semanticRoot)) {
        throw new Error(
          "CEK source constant semantic material does not match its authenticated value root",
        );
      }
      return [
        rootHex(constant.valueRoot),
        Object.freeze({
          kind: "semanticConstant" as const,
          witness: Object.freeze({
            typeCbor: constant.typeCbor,
            payload: Object.freeze({
              root: semantic.root,
              cborLength: semantic.cborLength,
              memory: semantic.memory,
            }),
            memory: constant.memory,
          }),
        }),
      ];
    }),
  );
  const addEntry = (entry: MidgardCekProgramMaterialEntryV1): void => {
    const exact = decodeMidgardCekProgramMaterialEntryV1(
      encodeMidgardCekProgramMaterialEntryV1(entry),
    );
    const key = rootHex(exact.root);
    const prior = material.get(key);
    if (prior !== undefined) {
      if (
        prior.kind !== exact.kind ||
        !Buffer.from(prior.preimage).equals(exact.preimage)
      ) {
        throw new Error(
          "CEK execution graph contains a material hash collision",
        );
      }
      return;
    }
    material.set(key, exact);
  };
  for (const entry of source) {
    if (verifiedSource.reachableRoots.has(rootHex(entry.root))) {
      addEntry(entry);
    }
  }

  const addBlob = (bytes: Uint8Array): Hash32 => {
    const committed = commitMidgardCekBlobV1(bytes);
    for (const [key, node] of committed.nodes) {
      addEntry({
        kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
        root: Buffer.from(key, "hex") as Hash32,
        preimage: node.preimage,
      });
    }
    return committed.root;
  };
  const addValue = (
    node: Extract<MidgardCekValueNodeV1, { readonly kind: "constant" }>,
  ): Hash32 => {
    const root = hashMidgardCekValueNodeV1(node);
    addEntry({
      kind: "value",
      root,
      preimage: encodeMidgardCekValueNodeV1(node),
    });
    return root;
  };
  const addTerm = (node: MidgardCekTermNodeV1): Hash32 => {
    const root = hashMidgardCekTermNodeV1(node);
    addEntry({
      kind: "term",
      root,
      preimage: encodeMidgardCekTermNodeV1(node),
    });
    return root;
  };

  const context = encodeMidgardCekCanonicalConstantV1(
    UPLCConst.data(dataFromCbor(Buffer.from(contextCbor))),
  );
  const contextPayload = dataFromCbor(context.payloadCbor);
  const contextSemantic = commitMidgardCekDataTreeV1(contextPayload);
  for (const [key, entry] of contextSemantic.dataNodes) {
    addEntry({
      kind: "dataNode",
      root: Buffer.from(key, "hex") as Hash32,
      preimage: entry.preimage,
    });
  }
  for (const [key, entry] of contextSemantic.listNodes) {
    addEntry({
      kind: "dataList",
      root: Buffer.from(key, "hex") as Hash32,
      preimage: entry.preimage,
    });
  }
  for (const [key, entry] of contextSemantic.pairNodes) {
    addEntry({
      kind: "dataPair",
      root: Buffer.from(key, "hex") as Hash32,
      preimage: entry.preimage,
    });
  }
  for (const [key, entry] of contextSemantic.blobNodes) {
    addEntry({
      kind: entry.kind === "chunk" ? "blobChunk" : "blobBranch",
      root: Buffer.from(key, "hex") as Hash32,
      preimage: entry.preimage,
    });
  }
  const contextValueRoot = addValue({
    kind: "constant",
    typeRoot: addBlob(context.typeCbor),
    payloadRoot: contextSemantic.root,
    payloadLength: contextSemantic.cborLength,
    semanticRoot: contextSemantic.root,
    memory: midgardCekConstantMemorySizeV1(context.type, contextPayload),
  });
  constantWitnesses.set(
    rootHex(contextValueRoot),
    Object.freeze({
      kind: "semanticConstant",
      witness: Object.freeze({
        typeCbor: context.typeCbor,
        payload: Object.freeze({
          root: contextSemantic.root,
          cborLength: contextSemantic.cborLength,
          memory: contextSemantic.memory,
        }),
        memory: midgardCekConstantMemorySizeV1(context.type, contextPayload),
      }),
    }),
  );
  const contextTermRoot = addTerm({
    kind: "contextConstant",
    value: contextValueRoot,
  });
  const root = addTerm({
    kind: "application",
    function: envelope.termRoot,
    argument: contextTermRoot,
  });
  return Object.freeze({
    root,
    contextTermRoot,
    contextValueRoot,
    material,
    constantWitnesses,
  });
};

class StructuralExecutorV1 {
  private state: MidgardCekMachineStateV1;
  private readonly steps: MidgardCekExecutionStepV1[] = [];
  private readonly material = new Map<
    string,
    MidgardCekProgramMaterialEntryV1
  >();
  private readonly values = new Map<string, MidgardCekValueNodeV1>();
  private readonly constants = new Map<
    string,
    MidgardCekConstantValueWitnessV1
  >();
  private readonly blsExpressions = new Map<
    string,
    MidgardCekBlsExpressionWitnessV1
  >();
  private readonly sequences = new Map<string, SequenceNodeV1>();
  private readonly dataNodes = new Map<string, MidgardCekDataNodeV1>();
  private readonly dataLists = new Map<string, MidgardCekDataListNodeV1>();
  private readonly dataPairs = new Map<string, MidgardCekDataPairNodeV1>();
  private readonly blobs = new Map<
    string,
    ReturnType<typeof decodeMidgardCekProgramBlobPreimageV1>
  >();
  private readonly mapControls = new Map<
    string,
    MidgardCekMapConversionControlV1
  >();
  private readonly environments = new Map<string, EnvironmentNodeV1>();
  private readonly continuations = new Map<
    string,
    MidgardCekContinuationFrameV1
  >();

  constructor(
    root: Bytes,
    material: Iterable<MidgardCekProgramMaterialEntryV1>,
    executionIndex: bigint,
    constantWitnesses: ReadonlyMap<string, MidgardCekConstantValueWitnessV1>,
  ) {
    for (const entry of material) {
      const exact = decodeMidgardCekProgramMaterialEntryV1(
        encodeMidgardCekProgramMaterialEntryV1(entry),
      );
      const key = rootHex(exact.root);
      const prior = this.material.get(key);
      if (prior !== undefined) {
        if (
          prior.kind !== exact.kind ||
          !Buffer.from(prior.preimage).equals(exact.preimage)
        ) {
          throw new Error("CEK execution material contains a hash collision");
        }
        continue;
      }
      this.material.set(key, exact);
      if (exact.kind === "value") {
        const value = decodeMidgardCekProgramValuePreimageV1(exact.preimage);
        this.values.set(key, {
          kind: "constant",
          typeRoot: value.typeRoot,
          payloadRoot: value.payloadRoot,
          payloadLength: value.payloadLength,
          semanticRoot: value.semanticRoot,
          memory: value.memory,
        });
      } else if (exact.kind === "sequence") {
        this.sequences.set(
          key,
          decodeMidgardCekProgramSequencePreimageV1(exact.preimage),
        );
      } else if (exact.kind === "dataNode") {
        this.dataNodes.set(key, decodeMidgardCekDataNodeV1(exact.preimage));
      } else if (exact.kind === "dataList") {
        this.dataLists.set(key, decodeMidgardCekDataListNodeV1(exact.preimage));
      } else if (exact.kind === "dataPair") {
        this.dataPairs.set(key, decodeMidgardCekDataPairNodeV1(exact.preimage));
      } else if (exact.kind === "blobChunk" || exact.kind === "blobBranch") {
        this.blobs.set(
          key,
          decodeMidgardCekProgramBlobPreimageV1(exact.kind, exact.preimage),
        );
      }
    }
    for (const [key, witness] of constantWitnesses) {
      const root =
        witness.kind === "constant"
          ? hashMidgardCekConstantWitnessV1(witness.witness)
          : hashMidgardCekDirectValueWitnessV1(witness);
      if (rootHex(root) !== key || this.values.get(key)?.kind !== "constant") {
        throw new Error(
          `CEK constant witness does not match authenticated value ${key}`,
        );
      }
      this.constants.set(key, witness);
    }
    this.state = Object.freeze({
      mode: "compute",
      executionIndex,
      focusRoot: Buffer.from(root),
      environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
      auxiliary: 0n,
      cpu: 0n,
      memory: 0n,
    });
  }

  public run(
    maxSteps: number,
    executionBudget?: {
      readonly cpu: bigint;
      readonly memory: bigint;
    },
  ): MidgardCekStructuralExecutionV1 {
    if (!Number.isSafeInteger(maxSteps) || maxSteps <= 0) {
      throw new Error("CEK maxSteps must be a positive safe integer");
    }
    if (
      executionBudget !== undefined &&
      (executionBudget.cpu < 0n || executionBudget.memory < 0n)
    ) {
      throw new Error("CEK execution budget must be non-negative");
    }
    const initialState = this.state;
    while (
      this.state.mode !== "haltSuccess" &&
      this.state.mode !== "haltError"
    ) {
      if (this.steps.length >= maxSteps) {
        throw new Error(
          `CEK execution exceeded the exact ${maxSteps.toString(10)}-step bound`,
        );
      }
      this.step();
      if (
        executionBudget !== undefined &&
        (this.state.cpu > executionBudget.cpu ||
          this.state.memory > executionBudget.memory)
      ) {
        return Object.freeze({
          initialState,
          steps: Object.freeze([...this.steps]),
          terminalState: this.state,
          stopReason: "budgetExceeded",
        });
      }
    }
    return Object.freeze({
      initialState,
      steps: Object.freeze([...this.steps]),
      terminalState: this.state,
      stopReason: "halted",
    });
  }

  private record(
    witness: MidgardCekCoreStepWitnessV1,
    post: MidgardCekMachineStateV1,
  ): void {
    const pre = this.state;
    if (!verifyMidgardCekCoreStepV1(pre, post, witness)) {
      throw new Error(
        `generated CEK ${witness.kind} transition does not pass the consensus verifier`,
      );
    }
    this.steps.push(Object.freeze({ pre, post, witness }));
    this.state = post;
  }

  private term(root: Bytes) {
    const entry = this.material.get(rootHex(root));
    if (entry?.kind !== "term") {
      throw new Error(`missing authenticated CEK term ${rootHex(root)}`);
    }
    return decodeMidgardCekProgramTermPreimageV1(entry.preimage);
  }

  private sequence(root: Bytes, length: bigint): SequenceNodeV1 {
    if (length === 0n) {
      if (!sameBytes(root, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1)) {
        throw new Error("empty CEK sequence has a non-canonical root");
      }
      throw new Error("an empty CEK sequence has no head");
    }
    const node = this.sequences.get(rootHex(root));
    if (node === undefined || node.length !== length) {
      throw new Error(`missing authenticated CEK sequence ${rootHex(root)}`);
    }
    return node;
  }

  private value(root: Bytes): MidgardCekValueNodeV1 {
    const value = this.values.get(rootHex(root));
    if (value === undefined) {
      throw new Error(`missing authenticated CEK value ${rootHex(root)}`);
    }
    return value;
  }

  private addValue(node: MidgardCekValueNodeV1): Hash32 {
    const root = hashMidgardCekValueNodeV1(node);
    const key = rootHex(root);
    const prior = this.values.get(key);
    if (
      prior !== undefined &&
      !Buffer.from(encodeMidgardCekValueNodeV1(prior)).equals(
        encodeMidgardCekValueNodeV1(node),
      )
    ) {
      throw new Error("CEK runtime value hash collision");
    }
    this.values.set(key, node);
    return root;
  }

  private addDirectResult(result: MidgardCekDirectValueWitnessV1): Bytes {
    const resultRoot = hashMidgardCekDirectValueWitnessV1(result);
    if (result.kind === "constant") {
      const witness = result.witness;
      const decoded = decodeMidgardCekConstantWitnessV1(witness);
      const semantic = commitMidgardCekDataTreeV1(decoded.payload);
      const node = {
        kind: "constant",
        typeRoot: hashMidgardCekBlobChunkV1(witness.typeCbor),
        payloadRoot: semantic.root,
        payloadLength: semantic.cborLength,
        semanticRoot: semantic.root,
        memory: midgardCekConstantMemorySizeV1(decoded.type, decoded.payload),
      } as const;
      if (!sameBytes(this.addValue(node), resultRoot)) {
        throw new Error("CEK builtin result constant root mismatch");
      }
      this.constants.set(rootHex(resultRoot), {
        kind: "constant",
        witness,
      });
    } else if (result.kind === "semanticConstant") {
      const witness = result.witness;
      const node = {
        kind: "constant",
        typeRoot: hashMidgardCekBlobChunkV1(witness.typeCbor),
        payloadRoot: witness.payload.root,
        payloadLength: witness.payload.cborLength,
        semanticRoot: witness.payload.root,
        memory: witness.memory,
      } as const;
      if (!sameBytes(this.addValue(node), resultRoot)) {
        throw new Error("CEK semantic builtin result constant root mismatch");
      }
      this.constants.set(rootHex(resultRoot), result);
    } else if (result.kind === "blsMillerLoop") {
      if (
        !sameBytes(
          this.addValue({
            kind: "blsMillerLoop",
            expressionRoot: result.expressionRoot,
          }),
          resultRoot,
        )
      ) {
        throw new Error("CEK builtin BLS expression root mismatch");
      }
    } else {
      this.value(resultRoot);
    }
    return resultRoot;
  }

  private blobBytes(
    root: Bytes,
    active: ReadonlySet<string> = new Set(),
  ): Buffer {
    const key = rootHex(root);
    if (active.has(key)) {
      throw new Error("cyclic CEK semantic blob commitment");
    }
    const node = this.blobs.get(key);
    if (node === undefined) {
      throw new Error(`missing authenticated CEK blob ${key}`);
    }
    if (node.kind === "chunk") return Buffer.from(node.bytes);
    const next = new Set(active);
    next.add(key);
    const bytes = Buffer.concat([
      this.blobBytes(node.left, next),
      this.blobBytes(node.right, next),
    ]);
    if (BigInt(bytes.length) !== node.byteLength) {
      throw new Error("CEK semantic blob length does not match its root");
    }
    return bytes;
  }

  private dataListValues(root: Bytes, count: bigint): Data[] {
    const values: Data[] = [];
    let cursor = Buffer.from(root);
    let remaining = count;
    while (remaining > 0n) {
      const node = this.dataLists.get(rootHex(cursor));
      if (node === undefined || node.length !== remaining) {
        throw new Error(
          `missing authenticated CEK Data-list node ${rootHex(cursor)}`,
        );
      }
      values.push(this.dataValue(node.head));
      cursor = Buffer.from(node.tail);
      remaining -= 1n;
    }
    if (!sameBytes(cursor, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1)) {
      throw new Error("CEK Data-list commitment has a non-empty tail");
    }
    return values;
  }

  private dataPairValues(root: Bytes, count: bigint): DataPair<Data, Data>[] {
    const values: DataPair<Data, Data>[] = [];
    let cursor = Buffer.from(root);
    let remaining = count;
    while (remaining > 0n) {
      const node = this.dataPairs.get(rootHex(cursor));
      if (node === undefined || node.length !== remaining) {
        throw new Error(
          `missing authenticated CEK Data-pair node ${rootHex(cursor)}`,
        );
      }
      values.push(
        new DataPair(this.dataValue(node.key), this.dataValue(node.value)),
      );
      cursor = Buffer.from(node.tail);
      remaining -= 1n;
    }
    if (!sameBytes(cursor, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1)) {
      throw new Error("CEK Data-map commitment has a non-empty tail");
    }
    return values;
  }

  private dataValue(root: Bytes): Data {
    const node = this.dataNodes.get(rootHex(root));
    if (node === undefined) {
      throw new Error(`missing authenticated CEK Data node ${rootHex(root)}`);
    }
    let value: Data;
    switch (node.kind) {
      case "constrSmall":
        value = new DataConstr(
          node.constructor,
          this.dataListValues(node.fieldsRoot, node.fieldsCount),
        );
        break;
      case "constrLarge": {
        const constructor = dataFromCbor(
          this.blobBytes(node.constructorCborRoot),
        );
        if (!(constructor instanceof DataI) || constructor.int <= 127n) {
          throw new Error("CEK large Data constructor is not canonical");
        }
        value = new DataConstr(
          constructor.int,
          this.dataListValues(node.fieldsRoot, node.fieldsCount),
        );
        break;
      }
      case "map":
        value = new DataMap(
          this.dataPairValues(node.entriesRoot, node.entriesCount),
        );
        break;
      case "list":
        value = new DataList(
          this.dataListValues(node.itemsRoot, node.itemsCount),
        );
        break;
      case "integer": {
        const integer = dataFromCbor(this.blobBytes(node.cborRoot));
        if (!(integer instanceof DataI)) {
          throw new Error("CEK integer Data leaf has a non-integer payload");
        }
        value = integer;
        break;
      }
      case "bytes": {
        const bytes = this.blobBytes(node.bytesRoot);
        if (BigInt(bytes.length) !== node.bytesLength) {
          throw new Error("CEK bytes Data leaf has the wrong length");
        }
        value = new DataB(bytes);
        break;
      }
    }
    const canonical = commitMidgardCekDataTreeV1(value);
    if (
      !sameBytes(canonical.root, root) ||
      canonical.cborLength !== node.cborLength ||
      canonical.memory !== node.memory
    ) {
      throw new Error("CEK semantic Data material is not self-consistent");
    }
    return value;
  }

  private installSemanticTree(
    tree: ReturnType<typeof commitMidgardCekDataTreeV1>,
  ): void {
    for (const [key, entry] of tree.dataNodes) {
      this.dataNodes.set(key, entry.node);
    }
    for (const [key, entry] of tree.listNodes) {
      this.dataLists.set(key, entry.node);
    }
    for (const [key, entry] of tree.pairNodes) {
      this.dataPairs.set(key, entry.node);
    }
    for (const [key, entry] of tree.blobNodes) {
      this.blobs.set(
        key,
        decodeMidgardCekProgramBlobPreimageV1(
          entry.kind === "chunk" ? "blobChunk" : "blobBranch",
          entry.preimage,
        ),
      );
    }
  }

  private addSemanticResult(
    type: MidgardCekConstantTypeV1,
    payload: Data,
  ): {
    readonly result: MidgardCekDirectValueWitnessV1;
    readonly root: Bytes;
    readonly tree: ReturnType<typeof commitMidgardCekDataTreeV1>;
  } {
    const tree = commitMidgardCekDataTreeV1(payload);
    this.installSemanticTree(tree);
    const typeCbor = encodeMidgardCekConstantTypeCborV1(type);
    const payloadCbor = encodeMidgardCekPlutusDataV1(payload);
    const result: MidgardCekDirectValueWitnessV1 =
      payloadCbor.length <= MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES_V1
        ? {
            kind: "constant",
            witness: {
              typeCbor,
              payloadCbor,
            },
          }
        : {
            kind: "semanticConstant",
            witness: {
              typeCbor,
              payload: {
                root: tree.root,
                cborLength: tree.cborLength,
                memory: tree.memory,
              },
              memory: midgardCekConstantMemorySizeV1(type, payload),
            },
          };
    return Object.freeze({
      result,
      root: this.addDirectResult(result),
      tree,
    });
  }

  private addSequence(node: SequenceNodeV1): Hash32 {
    const root = hashMidgardCekSequenceNodeV1(node);
    const key = rootHex(root);
    const prior = this.sequences.get(key);
    if (
      prior !== undefined &&
      !Buffer.from(encodeMidgardCekSequenceNodeV1(prior)).equals(
        encodeMidgardCekSequenceNodeV1(node),
      )
    ) {
      throw new Error("CEK runtime sequence hash collision");
    }
    this.sequences.set(key, node);
    return root;
  }

  private addEnvironment(node: EnvironmentNodeV1): Hash32 {
    const root = hashMidgardCekEnvironmentNodeV1(node);
    this.environments.set(rootHex(root), node);
    return root;
  }

  private environmentSummary(root: Bytes): MidgardCekEnvironmentSummaryV1 {
    if (sameBytes(root, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1)) {
      return { kind: "empty" };
    }
    const node = this.environments.get(rootHex(root));
    if (node === undefined) {
      throw new Error(`missing authenticated CEK environment ${rootHex(root)}`);
    }
    return {
      kind: "nonempty",
      value: node.value,
      tail: node.tail,
      length: node.length,
    };
  }

  private addContinuation(frame: MidgardCekContinuationFrameV1): Hash32 {
    const root = hashMidgardCekContinuationFrameV1(frame);
    this.continuations.set(rootHex(root), frame);
    return root;
  }

  private continuation(root: Bytes): MidgardCekContinuationFrameV1 | null {
    if (sameBytes(root, MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1)) {
      return null;
    }
    const frame = this.continuations.get(rootHex(root));
    if (frame === undefined) {
      throw new Error(
        `missing authenticated CEK continuation ${rootHex(root)}`,
      );
    }
    return frame;
  }

  private step(): void {
    switch (this.state.mode) {
      case "compute":
        this.compute();
        return;
      case "lookup":
        this.lookup();
        return;
      case "return":
        this.returnValue();
        return;
      case "caseSelect":
        this.selectCase();
        return;
      case "caseApply":
        this.applyCase();
        return;
      case "builtin":
        this.executeBuiltin();
        return;
      case "semanticBuiltin":
        this.executeSemanticBuiltinControl();
        return;
      case "haltSuccess":
      case "haltError":
        throw new Error("cannot step a halted CEK machine");
    }
  }

  private compute(): void {
    const pre = this.state;
    const term = this.term(pre.focusRoot);
    switch (term.kind) {
      case "variable":
        this.record(
          { kind: "computeVariable", index: term.index },
          exactComputeSuccessor(pre, {
            mode: "lookup",
            focusRoot: pre.environmentRoot,
            environmentRoot: pre.environmentRoot,
            continuationRoot: pre.continuationRoot,
            auxiliary: term.index,
          }),
        );
        return;
      case "constant":
        this.value(term.value);
        {
          const value = this.constants.get(rootHex(term.value));
          if (value?.kind !== "constant") {
            throw new Error(
              "source CEK constant is missing its exact bounded direct witness",
            );
          }
          this.record(
            { kind: "computeConstant", value: value.witness },
            exactComputeSuccessor(pre, {
              mode: "return",
              focusRoot: term.value,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
              continuationRoot: pre.continuationRoot,
              auxiliary: 0n,
            }),
          );
        }
        return;
      case "contextConstant":
        this.value(term.value);
        this.record(
          { kind: "computeContextConstant", valueRoot: term.value },
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: term.value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        );
        return;
      case "unaryTerm": {
        if (term.termKind === "lambda" || term.termKind === "delay") {
          const value = this.addValue({
            kind: term.termKind,
            body: term.child,
            environment: pre.environmentRoot,
          });
          this.record(
            term.termKind === "lambda"
              ? { kind: "computeLambda", body: term.child }
              : { kind: "computeDelay", body: term.child },
            exactComputeSuccessor(pre, {
              mode: "return",
              focusRoot: value,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
              continuationRoot: pre.continuationRoot,
              auxiliary: 0n,
            }),
          );
          return;
        }
        const continuation = this.addContinuation({
          kind: "force",
          tail: pre.continuationRoot,
        });
        this.record(
          { kind: "computeForce", term: term.child },
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: term.child,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "application": {
        const continuation = this.addContinuation({
          kind: "applyArgument",
          argument: term.argument,
          environment: pre.environmentRoot,
          tail: pre.continuationRoot,
        });
        this.record(
          {
            kind: "computeApplication",
            function: term.function,
            argument: term.argument,
          },
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: term.function,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "error":
        this.record(
          { kind: "computeError" },
          errorSuccessor(pre, MidgardCekErrorCodes.Explicit),
        );
        return;
      case "builtin": {
        const value = this.addValue({
          kind: "builtin",
          tag: term.tag,
          forcesRemaining: midgardCekBuiltinForceCount(term.tag),
          argumentsCount: 0n,
          argumentsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
        });
        this.record(
          { kind: "computeBuiltin", tag: term.tag },
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "constr": {
        if (term.count === 0n) {
          if (!sameBytes(term.sequence, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1)) {
            throw new Error("empty CEK constructor has a non-canonical root");
          }
          const value = this.addValue({
            kind: "constr",
            tag: term.tag,
            valuesCount: 0n,
            valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
          });
          this.record(
            { kind: "computeConstrEmpty", tag: term.tag },
            exactComputeSuccessor(pre, {
              mode: "return",
              focusRoot: value,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
              continuationRoot: pre.continuationRoot,
              auxiliary: 0n,
            }),
          );
          return;
        }
        const sequence = this.sequence(term.sequence, term.count);
        const continuation = this.addContinuation({
          kind: "constr",
          tag: term.tag,
          remainingTermsCount: term.count - 1n,
          remainingTermsRoot: sequence.tail,
          valuesCount: 0n,
          valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
          environment: pre.environmentRoot,
          tail: pre.continuationRoot,
        });
        this.record(
          {
            kind: "computeConstrNonempty",
            tag: term.tag,
            termsCount: term.count,
            firstTerm: sequence.head,
            remainingTermsRoot: sequence.tail,
          },
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: sequence.head,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "case": {
        if (term.count > 0n) this.sequence(term.sequence, term.count);
        if (
          term.count === 0n &&
          !sameBytes(term.sequence, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1)
        ) {
          throw new Error("empty CEK case has a non-canonical branch root");
        }
        const continuation = this.addContinuation({
          kind: "case",
          branchesCount: term.count,
          branchesRoot: term.sequence,
          environment: pre.environmentRoot,
          tail: pre.continuationRoot,
        });
        this.record(
          {
            kind: "computeCase",
            scrutinee: term.scrutinee,
            branchesCount: term.count,
            branchesRoot: term.sequence,
          },
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: term.scrutinee,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
    }
  }

  private lookup(): void {
    const pre = this.state;
    if (sameBytes(pre.focusRoot, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1)) {
      this.record(
        { kind: "lookupEmptyEnvironment" },
        errorSuccessor(pre, MidgardCekErrorCodes.UnboundVariable),
      );
      return;
    }
    const node = this.environments.get(rootHex(pre.focusRoot));
    if (node === undefined) {
      throw new Error(
        `missing authenticated CEK environment ${rootHex(pre.focusRoot)}`,
      );
    }
    this.record(
      {
        kind: "lookupEnvironment",
        value: node.value,
        tail: node.tail,
        length: node.length,
      },
      pre.auxiliary === 0n
        ? exactState(pre, {
            mode: "return",
            focusRoot: node.value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          })
        : exactState(pre, {
            mode: "lookup",
            focusRoot: node.tail,
            environmentRoot: node.tail,
            continuationRoot: pre.continuationRoot,
            auxiliary: pre.auxiliary - 1n,
          }),
    );
  }

  private applyBuiltin(
    pre: MidgardCekMachineStateV1,
    argument: Bytes,
    builtin: Extract<MidgardCekValueNodeV1, { readonly kind: "builtin" }>,
    tail: Bytes,
  ): MidgardCekMachineStateV1 {
    const required = midgardCekBuiltinArgumentCount(builtin.tag);
    if (builtin.forcesRemaining !== 0n || builtin.argumentsCount >= required) {
      throw new Error(
        `invalid CEK builtin application state tag=${builtin.tag.toString()} forces=${builtin.forcesRemaining.toString()} arguments=${builtin.argumentsCount.toString()} required=${required.toString()}`,
      );
    }
    const nextCount = builtin.argumentsCount + 1n;
    const nextRoot = this.addSequence({
      head: Buffer.from(argument) as Hash32,
      tail: Buffer.from(builtin.argumentsRoot) as Hash32,
      length: nextCount,
    });
    const nextValue = this.addValue({
      ...builtin,
      argumentsCount: nextCount,
      argumentsRoot: nextRoot,
    });
    return exactState(pre, {
      mode: nextCount === required ? "builtin" : "return",
      focusRoot: nextValue,
      environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      continuationRoot: tail,
      auxiliary: 0n,
    });
  }

  private runtimeValueWitness(root: Bytes): MidgardCekRuntimeValueWitnessV1 {
    const value = this.value(root);
    switch (value.kind) {
      case "constant": {
        const witness = this.constants.get(rootHex(root));
        if (witness === undefined) {
          throw new Error(
            `missing authenticated CEK constant preimage ${rootHex(root)}`,
          );
        }
        return witness;
      }
      case "lambda":
        return {
          kind: "lambda",
          body: value.body,
          environment: value.environment,
        };
      case "delay":
        return {
          kind: "delay",
          body: value.body,
          environment: value.environment,
        };
      case "constr":
        return {
          kind: "constr",
          tag: value.tag,
          valuesCount: value.valuesCount,
          valuesRoot: value.valuesRoot,
        };
      case "builtin":
        return {
          kind: "builtin",
          tag: value.tag,
          forcesRemaining: value.forcesRemaining,
          argumentsCount: value.argumentsCount,
          argumentsRoot: value.argumentsRoot,
        };
      case "blsMillerLoop":
        return {
          kind: "blsMillerLoop",
          expressionRoot: value.expressionRoot,
        };
    }
  }

  private builtinArguments(
    root: Bytes,
    count: bigint,
  ): readonly MidgardCekRuntimeValueWitnessV1[] {
    const reversed: MidgardCekRuntimeValueWitnessV1[] = [];
    let cursor = Buffer.from(root);
    let remaining = count;
    while (remaining > 0n) {
      const node = this.sequence(cursor, remaining);
      const witness = this.runtimeValueWitness(node.head);
      if (!sameBytes(hashMidgardCekRuntimeValueWitnessV1(witness), node.head)) {
        throw new Error("CEK runtime argument witness root mismatch");
      }
      reversed.push(witness);
      cursor = Buffer.from(node.tail);
      remaining -= 1n;
    }
    if (!sameBytes(cursor, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1)) {
      throw new Error("CEK builtin argument sequence has a non-empty tail");
    }
    return Object.freeze(reversed.reverse());
  }

  private mapControl(
    tag: 38n | 43n,
    resultRoot: Bytes,
    sourceNode: MidgardCekDataNodeV1,
    resultNode: MidgardCekDataNodeV1,
    arguments_: readonly MidgardCekDirectValueWitnessV1[],
  ): MidgardCekMapConversionControlV1 {
    const source =
      tag === 38n && sourceNode.kind === "list"
        ? {
            root: sourceNode.itemsRoot,
            length: sourceNode.itemsCount,
            payload:
              sourceNode.cborLength - (sourceNode.itemsCount === 0n ? 1n : 2n),
            memory: sourceNode.memory - 4n,
          }
        : tag === 43n && sourceNode.kind === "map"
          ? {
              root: sourceNode.entriesRoot,
              length: sourceNode.entriesCount,
              payload:
                sourceNode.cborLength -
                (sourceNode.entriesCount < 24n
                  ? 1n
                  : sourceNode.entriesCount <= 0xffn
                    ? 2n
                    : sourceNode.entriesCount <= 0xffffn
                      ? 3n
                      : 5n),
              memory: sourceNode.memory - 4n,
            }
          : null;
    const destination =
      tag === 38n && resultNode.kind === "map"
        ? {
            root: resultNode.entriesRoot,
            length: resultNode.entriesCount,
            payload:
              resultNode.cborLength -
              (resultNode.entriesCount < 24n
                ? 1n
                : resultNode.entriesCount <= 0xffn
                  ? 2n
                  : resultNode.entriesCount <= 0xffffn
                    ? 3n
                    : 5n),
            memory: resultNode.memory - 4n,
          }
        : tag === 43n && resultNode.kind === "list"
          ? {
              root: resultNode.itemsRoot,
              length: resultNode.itemsCount,
              payload:
                resultNode.cborLength -
                (resultNode.itemsCount === 0n ? 1n : 2n),
              memory: resultNode.memory - 4n,
            }
          : null;
    if (
      source === null ||
      destination === null ||
      source.length !== destination.length
    ) {
      throw new Error("CEK map conversion has incompatible semantic roots");
    }
    const budget = midgardCekDirectBuiltinBudgetV1(tag, arguments_);
    return Object.freeze({
      tag,
      resultRoot: Buffer.from(resultRoot),
      sourceRoot: Buffer.from(source.root),
      sourceRemaining: source.length,
      sourcePayloadCborLength: source.payload,
      sourceMemory: source.memory,
      destinationRoot: Buffer.from(destination.root),
      destinationRemaining: destination.length,
      destinationPayloadCborLength: destination.payload,
      destinationMemory: destination.memory,
      budgetCpu: budget.cpu,
      budgetMemory: budget.memory,
    });
  }

  private startMapConversion(
    pre: MidgardCekMachineStateV1,
    tag: 38n | 43n,
    arguments_: readonly MidgardCekDirectValueWitnessV1[],
  ): void {
    const source = arguments_[0];
    if (source?.kind !== "semanticConstant") {
      throw new Error("CEK semantic map conversion requires semantic source");
    }
    const sourcePayload = this.dataValue(source.witness.payload.root);
    let resultPayload: Data;
    let resultType: MidgardCekConstantTypeV1;
    if (tag === 38n) {
      if (!(sourcePayload instanceof DataList)) {
        throw new Error("mapData requires a list payload");
      }
      resultPayload = new DataMap(
        sourcePayload.list.map((item) => {
          if (
            !(item instanceof DataConstr) ||
            item.constr !== 0n ||
            item.fields.length !== 2
          ) {
            throw new Error("mapData requires canonical Data pairs");
          }
          return new DataPair(item.fields[0]!, item.fields[1]!);
        }),
      );
      resultType = { kind: "data" };
    } else {
      if (!(sourcePayload instanceof DataMap)) {
        throw new Error("unMapData requires a map Data payload");
      }
      resultPayload = new DataList(
        sourcePayload.map.map(
          (entry) => new DataConstr(0n, [entry.fst, entry.snd]),
        ),
      );
      resultType = {
        kind: "list",
        element: {
          kind: "pair",
          first: { kind: "data" },
          second: { kind: "data" },
        },
      };
    }
    const added = this.addSemanticResult(resultType, resultPayload);
    const sourceNode = this.dataNodes.get(rootHex(source.witness.payload.root));
    const resultNode = this.dataNodes.get(rootHex(added.tree.root));
    if (sourceNode === undefined || resultNode === undefined) {
      throw new Error("CEK map conversion is missing its top Data nodes");
    }
    const control = this.mapControl(
      tag,
      added.root,
      sourceNode,
      resultNode,
      arguments_,
    );
    const controlRoot = hashMidgardCekMapConversionControlV1(control);
    this.mapControls.set(rootHex(controlRoot), control);
    this.record(
      {
        kind: "startBuiltinMapConversion",
        tag,
        arguments: arguments_,
        result: added.result,
        material: {
          sourceNode,
          sourceList:
            sourceNode.kind === "list" && sourceNode.itemsCount > 0n
              ? (this.dataLists.get(rootHex(sourceNode.itemsRoot)) ?? null)
              : null,
          sourcePairs:
            sourceNode.kind === "map" && sourceNode.entriesCount > 0n
              ? (this.dataPairs.get(rootHex(sourceNode.entriesRoot)) ?? null)
              : null,
          resultNode,
          resultList:
            resultNode.kind === "list" && resultNode.itemsCount > 0n
              ? (this.dataLists.get(rootHex(resultNode.itemsRoot)) ?? null)
              : null,
          resultPairs:
            resultNode.kind === "map" && resultNode.entriesCount > 0n
              ? (this.dataPairs.get(rootHex(resultNode.entriesRoot)) ?? null)
              : null,
        },
      },
      exactState(pre, {
        mode: "semanticBuiltin",
        focusRoot: controlRoot,
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
      }),
    );
  }

  private advancedMapControl(
    control: MidgardCekMapConversionControlV1,
    sourcePayload: bigint,
    sourceMemory: bigint,
    sourceTail: Bytes,
    destinationPayload: bigint,
    destinationMemory: bigint,
    destinationTail: Bytes,
  ): MidgardCekMapConversionControlV1 {
    return Object.freeze({
      ...control,
      sourceRoot: Buffer.from(sourceTail),
      sourceRemaining: control.sourceRemaining - 1n,
      sourcePayloadCborLength: control.sourcePayloadCborLength - sourcePayload,
      sourceMemory: control.sourceMemory - sourceMemory,
      destinationRoot: Buffer.from(destinationTail),
      destinationRemaining: control.destinationRemaining - 1n,
      destinationPayloadCborLength:
        control.destinationPayloadCborLength - destinationPayload,
      destinationMemory: control.destinationMemory - destinationMemory,
    });
  }

  private mapPairMaterial(pairRoot: Bytes): {
    readonly pair: MidgardCekDataNodeV1;
    readonly first: MidgardCekDataListNodeV1;
    readonly second: MidgardCekDataListNodeV1;
    readonly key: MidgardCekDataNodeV1;
    readonly value: MidgardCekDataNodeV1;
  } {
    const pair = this.dataNodes.get(rootHex(pairRoot));
    if (
      pair?.kind !== "constrSmall" ||
      pair.constructor !== 0n ||
      pair.fieldsCount !== 2n
    ) {
      throw new Error("CEK map conversion pair wrapper is malformed");
    }
    const first = this.dataLists.get(rootHex(pair.fieldsRoot));
    const second =
      first === undefined ? undefined : this.dataLists.get(rootHex(first.tail));
    const key =
      first === undefined ? undefined : this.dataNodes.get(rootHex(first.head));
    const value =
      second === undefined
        ? undefined
        : this.dataNodes.get(rootHex(second.head));
    if (
      first === undefined ||
      second === undefined ||
      key === undefined ||
      value === undefined
    ) {
      throw new Error("CEK map conversion pair material is incomplete");
    }
    return { pair, first, second, key, value };
  }

  private executeSemanticBuiltinControl(): void {
    const pre = this.state;
    const control = this.mapControls.get(rootHex(pre.focusRoot));
    if (control === undefined) {
      throw new Error("missing authenticated CEK semantic control");
    }
    if (control.sourceRemaining === 0n) {
      this.record(
        { kind: "finishBuiltinMapConversion", control },
        exactState(pre, {
          mode: "return",
          focusRoot: control.resultRoot,
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
          cpuDelta: control.budgetCpu,
          memoryDelta: control.budgetMemory,
        }),
      );
      return;
    }
    if (control.tag === 38n) {
      const source = this.dataLists.get(rootHex(control.sourceRoot));
      const destination = this.dataPairs.get(rootHex(control.destinationRoot));
      if (source === undefined || destination === undefined) {
        throw new Error("CEK list-to-map control material is missing");
      }
      const material = this.mapPairMaterial(source.head);
      const next = this.advancedMapControl(
        control,
        source.headCborLength,
        source.headMemory,
        source.tail,
        destination.keyCborLength + destination.valueCborLength,
        destination.keyMemory + destination.valueMemory,
        destination.tail,
      );
      const nextRoot = hashMidgardCekMapConversionControlV1(next);
      this.mapControls.set(rootHex(nextRoot), next);
      this.record(
        {
          kind: "stepBuiltinListToMap",
          control,
          source,
          ...material,
          destination,
        },
        exactState(pre, {
          mode: "semanticBuiltin",
          focusRoot: nextRoot,
          environmentRoot: pre.environmentRoot,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
        }),
      );
      return;
    }
    const source = this.dataPairs.get(rootHex(control.sourceRoot));
    const destination = this.dataLists.get(rootHex(control.destinationRoot));
    if (source === undefined || destination === undefined) {
      throw new Error("CEK map-to-list control material is missing");
    }
    const material = this.mapPairMaterial(destination.head);
    const next = this.advancedMapControl(
      control,
      source.keyCborLength + source.valueCborLength,
      source.keyMemory + source.valueMemory,
      source.tail,
      destination.headCborLength,
      destination.headMemory,
      destination.tail,
    );
    const nextRoot = hashMidgardCekMapConversionControlV1(next);
    this.mapControls.set(rootHex(nextRoot), next);
    this.record(
      {
        kind: "stepBuiltinMapToList",
        control,
        source,
        destination,
        ...material,
      },
      exactState(pre, {
        mode: "semanticBuiltin",
        focusRoot: nextRoot,
        environmentRoot: pre.environmentRoot,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
      }),
    );
  }

  private resolvedConstant(value: MidgardCekDirectValueWitnessV1): {
    readonly type: MidgardCekConstantTypeV1;
    readonly payload: Data;
    readonly tree: ReturnType<typeof commitMidgardCekDataTreeV1>;
  } {
    if (value.kind === "constant") {
      const decoded = decodeMidgardCekConstantWitnessV1(value.witness);
      const tree = commitMidgardCekDataTreeV1(decoded.payload);
      this.installSemanticTree(tree);
      return { ...decoded, tree };
    }
    if (value.kind === "semanticConstant") {
      const payload = this.dataValue(value.witness.payload.root);
      const tree = commitMidgardCekDataTreeV1(payload);
      this.installSemanticTree(tree);
      return {
        type: decodeMidgardCekConstantTypeCborV1(value.witness.typeCbor),
        payload,
        tree,
      };
    }
    throw new Error("CEK semantic builtin requires a constant");
  }

  private topSemanticMaterial(
    tree: ReturnType<typeof commitMidgardCekDataTreeV1>,
  ): {
    readonly node: MidgardCekDataNodeV1;
    readonly lists: readonly MidgardCekDataListNodeV1[];
    readonly pairs: readonly MidgardCekDataPairNodeV1[];
  } {
    const node = this.dataNodes.get(rootHex(tree.root));
    if (node === undefined) {
      throw new Error("CEK semantic constant is missing its top node");
    }
    const listRoot =
      node.kind === "constrSmall" || node.kind === "constrLarge"
        ? node.fieldsCount > 0n
          ? node.fieldsRoot
          : null
        : node.kind === "list" && node.itemsCount > 0n
          ? node.itemsRoot
          : null;
    const pairRoot =
      node.kind === "map" && node.entriesCount > 0n ? node.entriesRoot : null;
    const list =
      listRoot === null ? null : this.dataLists.get(rootHex(listRoot));
    const pair =
      pairRoot === null ? null : this.dataPairs.get(rootHex(pairRoot));
    if (
      (listRoot !== null && list === undefined) ||
      (pairRoot !== null && pair === undefined)
    ) {
      throw new Error("CEK semantic constant has incomplete top material");
    }
    return {
      node,
      lists: list === undefined || list === null ? [] : [list],
      pairs: pair === undefined || pair === null ? [] : [pair],
    };
  }

  private recordSemanticResult(
    pre: MidgardCekMachineStateV1,
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitnessV1[],
    result: MidgardCekDirectValueWitnessV1,
    material: {
      readonly dataNodes: readonly MidgardCekDataNodeV1[];
      readonly listNodes: readonly MidgardCekDataListNodeV1[];
      readonly pairNodes: readonly MidgardCekDataPairNodeV1[];
      readonly scalarPreimages: readonly Bytes[];
    },
  ): void {
    const resultRoot = this.addDirectResult(result);
    const budget = midgardCekDirectBuiltinBudgetV1(tag, arguments_);
    this.record(
      {
        kind: "executeBuiltinSemantic",
        tag,
        arguments: arguments_,
        result,
        material,
      },
      exactState(pre, {
        mode: "return",
        focusRoot: resultRoot,
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: budget.cpu,
        memoryDelta: budget.memory,
      }),
    );
  }

  private recordSemanticFailure(
    pre: MidgardCekMachineStateV1,
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitnessV1[],
    source: ReturnType<StructuralExecutorV1["resolvedConstant"]>,
  ): void {
    const top = this.topSemanticMaterial(source.tree);
    this.record(
      {
        kind: "executeBuiltinSemanticFailure",
        tag,
        arguments: arguments_,
        material: {
          dataNodes: [top.node],
          listNodes: top.lists,
          pairNodes: top.pairs,
          scalarPreimages: [],
        },
      },
      exactState(pre, {
        mode: "haltError",
        focusRoot: hashMidgardCekTermNodeV1({ kind: "error" }),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
        auxiliary: MidgardCekErrorCodes.BuiltinFailure,
      }),
    );
  }

  private executeSemanticBuiltin(
    pre: MidgardCekMachineStateV1,
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitnessV1[],
  ): void {
    const emptyMaterial = {
      dataNodes: [],
      listNodes: [],
      pairNodes: [],
      scalarPreimages: [],
    } as const;
    if (tag === 33n || tag === 34n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (
        source.type.kind === "list" &&
        source.payload instanceof DataList &&
        source.payload.list.length === 0
      ) {
        this.recordSemanticFailure(pre, tag, arguments_, source);
        return;
      }
    }
    if (tag >= 42n && tag <= 46n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (source.type.kind !== "data") {
        throw new Error("semantic Data failure has a non-Data source");
      }
      const wrongVariant =
        tag === 42n
          ? !(source.payload instanceof DataConstr)
          : tag === 43n
            ? !(source.payload instanceof DataMap)
            : tag === 44n
              ? !(source.payload instanceof DataList)
              : tag === 45n
                ? !(source.payload instanceof DataI)
                : !(source.payload instanceof DataB);
      if (wrongVariant) {
        this.recordSemanticFailure(pre, tag, arguments_, source);
        return;
      }
    }
    if (tag === 29n || tag === 30n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (
        source.type.kind !== "pair" ||
        !(source.payload instanceof DataConstr) ||
        source.payload.constr !== 0n ||
        source.payload.fields.length !== 2
      ) {
        throw new Error("fstPair/sndPair requires a canonical pair");
      }
      const top = this.topSemanticMaterial(source.tree);
      const firstLink = top.lists[0];
      const secondLink =
        firstLink === undefined
          ? undefined
          : this.dataLists.get(rootHex(firstLink.tail));
      const firstNode =
        firstLink === undefined
          ? undefined
          : this.dataNodes.get(rootHex(firstLink.head));
      const secondNode =
        secondLink === undefined
          ? undefined
          : this.dataNodes.get(rootHex(secondLink.head));
      if (
        firstLink === undefined ||
        secondLink === undefined ||
        firstNode === undefined ||
        secondNode === undefined
      ) {
        throw new Error("CEK semantic pair material is incomplete");
      }
      const selectedType = tag === 29n ? source.type.first : source.type.second;
      const selectedPayload =
        tag === 29n ? source.payload.fields[0]! : source.payload.fields[1]!;
      const added = this.addSemanticResult(selectedType, selectedPayload);
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes: [top.node, firstNode, secondNode],
        listNodes: [firstLink, secondLink],
        pairNodes: [],
        scalarPreimages: [],
      });
      return;
    }
    if (tag >= 31n && tag <= 35n) {
      const sourceIndex = tag === 32n ? 1 : 0;
      const source = this.resolvedConstant(arguments_[sourceIndex]!);
      if (
        source.type.kind !== "list" ||
        !(source.payload instanceof DataList)
      ) {
        throw new Error("CEK semantic list builtin requires a list");
      }
      const top = this.topSemanticMaterial(source.tree);
      if (tag === 31n) {
        const result =
          source.payload.list.length === 0 ? arguments_[1]! : arguments_[2]!;
        this.recordSemanticResult(pre, tag, arguments_, result, {
          dataNodes: [top.node],
          listNodes: top.lists,
          pairNodes: [],
          scalarPreimages: [],
        });
        return;
      }
      if (tag === 32n) {
        const item = this.resolvedConstant(arguments_[0]!);
        const added = this.addSemanticResult(
          source.type,
          new DataList([item.payload, ...source.payload.list]),
        );
        this.recordSemanticResult(pre, tag, arguments_, added.result, {
          dataNodes: [top.node],
          listNodes: top.lists,
          pairNodes: [],
          scalarPreimages: [],
        });
        return;
      }
      if (tag === 35n) {
        const added = this.addSemanticResult(
          { kind: "boolean" },
          new DataConstr(source.payload.list.length === 0 ? 1n : 0n, []),
        );
        this.recordSemanticResult(pre, tag, arguments_, added.result, {
          dataNodes: [top.node],
          listNodes: top.lists,
          pairNodes: [],
          scalarPreimages: [],
        });
        return;
      }
      if (source.payload.list.length === 0) {
        throw new Error("headList/tailList failed on an empty list");
      }
      const firstLink = top.lists[0];
      const headNode =
        firstLink === undefined
          ? undefined
          : this.dataNodes.get(rootHex(firstLink.head));
      const tailLink =
        firstLink === undefined || firstLink.length === 1n
          ? undefined
          : this.dataLists.get(rootHex(firstLink.tail));
      if (firstLink === undefined || headNode === undefined) {
        throw new Error("CEK semantic list head material is incomplete");
      }
      const added =
        tag === 33n
          ? this.addSemanticResult(source.type.element, source.payload.list[0]!)
          : this.addSemanticResult(
              source.type,
              new DataList(source.payload.list.slice(1)),
            );
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes: [top.node, headNode],
        listNodes: tailLink === undefined ? [firstLink] : [firstLink, tailLink],
        pairNodes: [],
        scalarPreimages: [],
      });
      return;
    }
    if (tag === 36n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (source.type.kind !== "data") {
        throw new Error("chooseData requires Data");
      }
      const top = this.topSemanticMaterial(source.tree);
      const result =
        source.payload instanceof DataConstr
          ? arguments_[1]!
          : source.payload instanceof DataMap
            ? arguments_[2]!
            : source.payload instanceof DataList
              ? arguments_[3]!
              : source.payload instanceof DataI
                ? arguments_[4]!
                : arguments_[5]!;
      this.recordSemanticResult(pre, tag, arguments_, result, {
        dataNodes: [top.node],
        listNodes: top.lists,
        pairNodes: top.pairs,
        scalarPreimages: [],
      });
      return;
    }
    if (tag === 37n) {
      const index = this.resolvedConstant(arguments_[0]!);
      const fields = this.resolvedConstant(arguments_[1]!);
      if (
        index.type.kind !== "integer" ||
        !(index.payload instanceof DataI) ||
        fields.type.kind !== "list" ||
        fields.type.element.kind !== "data" ||
        !(fields.payload instanceof DataList) ||
        index.payload.int < 0n
      ) {
        throw new Error("constrData requires non-negative index and Data list");
      }
      const fieldsTop = this.topSemanticMaterial(fields.tree);
      const indexIsSemantic = arguments_[0]?.kind === "semanticConstant";
      const indexTop = indexIsSemantic
        ? this.topSemanticMaterial(index.tree)
        : null;
      const added = this.addSemanticResult(
        { kind: "data" },
        new DataConstr(index.payload.int, fields.payload.list),
      );
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes:
          indexTop === null
            ? [fieldsTop.node]
            : [indexTop.node, fieldsTop.node],
        listNodes: fieldsTop.lists,
        pairNodes: [],
        scalarPreimages:
          indexTop === null
            ? []
            : [encodeMidgardCekPlutusDataV1(index.payload)],
      });
      return;
    }
    if (tag === 39n) {
      const items = this.resolvedConstant(arguments_[0]!);
      if (
        items.type.kind !== "list" ||
        items.type.element.kind !== "data" ||
        !(items.payload instanceof DataList)
      ) {
        throw new Error("listData requires a Data list");
      }
      const top = this.topSemanticMaterial(items.tree);
      const added = this.addSemanticResult({ kind: "data" }, items.payload);
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes: [top.node],
        listNodes: top.lists,
        pairNodes: [],
        scalarPreimages: [],
      });
      return;
    }
    if (tag === 40n || tag === 45n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (
        !(source.payload instanceof DataI) ||
        source.type.kind !== (tag === 40n ? "integer" : "data")
      ) {
        throw new Error("iData/unIData requires an integer payload");
      }
      const top = this.topSemanticMaterial(source.tree);
      const added = this.addSemanticResult(
        { kind: tag === 40n ? "data" : "integer" },
        source.payload,
      );
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes: [top.node],
        listNodes: [],
        pairNodes: [],
        scalarPreimages: [encodeMidgardCekPlutusDataV1(source.payload)],
      });
      return;
    }
    if (tag === 41n || tag === 46n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (
        !(source.payload instanceof DataB) ||
        source.type.kind !== (tag === 41n ? "bytes" : "data")
      ) {
        throw new Error("bData/unBData requires a bytes payload");
      }
      const top = this.topSemanticMaterial(source.tree);
      const added = this.addSemanticResult(
        { kind: tag === 41n ? "data" : "bytes" },
        source.payload,
      );
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes: [top.node],
        listNodes: [],
        pairNodes: [],
        scalarPreimages: [source.payload.bytes.toBuffer()],
      });
      return;
    }
    if (tag === 42n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (
        source.type.kind !== "data" ||
        !(source.payload instanceof DataConstr)
      ) {
        throw new Error("unConstrData requires constructor Data");
      }
      const top = this.topSemanticMaterial(source.tree);
      const added = this.addSemanticResult(
        {
          kind: "pair",
          first: { kind: "integer" },
          second: {
            kind: "list",
            element: { kind: "data" },
          },
        },
        new DataConstr(0n, [
          new DataI(source.payload.constr),
          new DataList(source.payload.fields),
        ]),
      );
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes: [top.node],
        listNodes: top.lists,
        pairNodes: [],
        scalarPreimages:
          source.payload.constr <= 127n
            ? []
            : [encodeMidgardCekPlutusDataV1(new DataI(source.payload.constr))],
      });
      return;
    }
    if (tag === 44n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (
        source.type.kind !== "data" ||
        !(source.payload instanceof DataList)
      ) {
        throw new Error("unListData requires list Data");
      }
      const top = this.topSemanticMaterial(source.tree);
      const added = this.addSemanticResult(
        {
          kind: "list",
          element: { kind: "data" },
        },
        source.payload,
      );
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes: [top.node],
        listNodes: top.lists,
        pairNodes: [],
        scalarPreimages: [],
      });
      return;
    }
    if (tag === 47n) {
      const left = this.resolvedConstant(arguments_[0]!);
      const right = this.resolvedConstant(arguments_[1]!);
      if (left.type.kind !== "data" || right.type.kind !== "data") {
        throw new Error("equalsData requires Data arguments");
      }
      const added = this.addSemanticResult(
        { kind: "boolean" },
        new DataConstr(
          sameBytes(left.tree.root, right.tree.root) ? 1n : 0n,
          [],
        ),
      );
      this.recordSemanticResult(
        pre,
        tag,
        arguments_,
        added.result,
        emptyMaterial,
      );
      return;
    }
    if (tag === 48n) {
      const first = this.resolvedConstant(arguments_[0]!);
      const second = this.resolvedConstant(arguments_[1]!);
      if (first.type.kind !== "data" || second.type.kind !== "data") {
        throw new Error("mkPairData requires Data arguments");
      }
      const added = this.addSemanticResult(
        {
          kind: "pair",
          first: { kind: "data" },
          second: { kind: "data" },
        },
        new DataConstr(0n, [first.payload, second.payload]),
      );
      this.recordSemanticResult(
        pre,
        tag,
        arguments_,
        added.result,
        emptyMaterial,
      );
      return;
    }
    if (tag === 49n || tag === 50n) {
      const unit = this.resolvedConstant(arguments_[0]!);
      if (unit.type.kind !== "unit") {
        throw new Error("mkNilData requires unit");
      }
      const element: MidgardCekConstantTypeV1 =
        tag === 49n
          ? { kind: "data" }
          : {
              kind: "pair",
              first: { kind: "data" },
              second: { kind: "data" },
            };
      const added = this.addSemanticResult(
        { kind: "list", element },
        new DataList([]),
      );
      this.recordSemanticResult(
        pre,
        tag,
        arguments_,
        added.result,
        emptyMaterial,
      );
      return;
    }
    if (tag === 51n) {
      const source = this.resolvedConstant(arguments_[0]!);
      if (source.type.kind !== "data") {
        throw new Error("serialiseData requires Data");
      }
      const raw = encodeMidgardCekPlutusDataV1(source.payload);
      if (raw.length > 9_215) {
        throw new Error(
          "serialiseData source exceeds the exact L1 revealed-preimage envelope",
        );
      }
      const added = this.addSemanticResult({ kind: "bytes" }, new DataB(raw));
      this.recordSemanticResult(pre, tag, arguments_, added.result, {
        dataNodes: [],
        listNodes: [],
        pairNodes: [],
        scalarPreimages: [raw],
      });
      return;
    }
    throw new Error(`unsupported CEK semantic builtin ${tag.toString()}`);
  }

  private executeBuiltin(): void {
    const pre = this.state;
    const builtin = this.value(pre.focusRoot);
    if (
      builtin.kind !== "builtin" ||
      builtin.forcesRemaining !== 0n ||
      builtin.argumentsCount !== midgardCekBuiltinArgumentCount(builtin.tag)
    ) {
      throw new Error("CEK builtin mode has an incomplete builtin value");
    }
    const arguments_ = this.builtinArguments(
      builtin.argumentsRoot,
      builtin.argumentsCount,
    );
    if (
      verifyMidgardCekBuiltinTypeFailureV1(
        builtin.tag,
        pre.focusRoot,
        arguments_,
      )
    ) {
      this.record(
        {
          kind: "executeBuiltinTypeFailure",
          tag: builtin.tag,
          arguments: arguments_,
        },
        errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure),
      );
      return;
    }
    const directArguments = arguments_.map(
      (argument): MidgardCekDirectValueWitnessV1 => {
        if (argument.kind === "constant") {
          return { kind: "constant", witness: argument.witness };
        }
        if (argument.kind === "semanticConstant") {
          return {
            kind: "semanticConstant",
            witness: argument.witness,
          };
        }
        if (argument.kind === "blsMillerLoop") {
          return {
            kind: "blsMillerLoop",
            expressionRoot: argument.expressionRoot,
          };
        }
        return {
          kind: "opaque",
          root: hashMidgardCekRuntimeValueWitnessV1(argument),
        };
      },
    );
    if (
      builtin.tag === 43n &&
      directArguments[0]?.kind === "semanticConstant"
    ) {
      const source = this.resolvedConstant(directArguments[0]);
      if (!(source.payload instanceof DataMap)) {
        this.recordSemanticFailure(pre, builtin.tag, directArguments, source);
        return;
      }
    }
    if (
      (builtin.tag === 38n || builtin.tag === 43n) &&
      directArguments[0]?.kind === "semanticConstant"
    ) {
      this.startMapConversion(pre, builtin.tag, directArguments);
      return;
    }
    if (
      builtin.tag === 51n &&
      directArguments[0]?.kind === "semanticConstant"
    ) {
      this.executeSemanticBuiltin(pre, builtin.tag, directArguments);
      return;
    }
    if (
      builtin.tag >= 29n &&
      builtin.tag <= 50n &&
      builtin.tag !== 38n &&
      builtin.tag !== 43n
    ) {
      this.executeSemanticBuiltin(pre, builtin.tag, directArguments);
      return;
    }
    if (builtin.tag === 70n) {
      const left = directArguments[0];
      const right = directArguments[1];
      if (left?.kind !== "blsMillerLoop" || right?.kind !== "blsMillerLoop") {
        throw new Error(
          "CEK BLS finalVerify requires two authenticated expression values",
        );
      }
      const leftExpression = this.blsExpressions.get(
        rootHex(left.expressionRoot),
      );
      const rightExpression = this.blsExpressions.get(
        rootHex(right.expressionRoot),
      );
      if (leftExpression === undefined || rightExpression === undefined) {
        throw new Error(
          "CEK BLS finalVerify is missing an authenticated expression witness",
        );
      }
      const evaluated = evaluateMidgardCekBlsFinalV1(
        left.expressionRoot,
        right.expressionRoot,
        leftExpression,
        rightExpression,
      );
      const resultRoot = this.addDirectResult(evaluated.result);
      this.record(
        {
          kind: "executeBuiltinBlsFinal",
          leftRoot: left.expressionRoot,
          rightRoot: right.expressionRoot,
          leftExpression,
          rightExpression,
          result: evaluated.result,
        },
        exactState(pre, {
          mode: "return",
          focusRoot: resultRoot,
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
          cpuDelta: evaluated.budget.cpu,
          memoryDelta: evaluated.budget.memory,
        }),
      );
      return;
    }
    const evaluated = evaluateMidgardCekDirectBuiltinV1(
      builtin.tag,
      directArguments,
    );
    if (evaluated.kind === "failure") {
      this.record(
        {
          kind: "executeBuiltinFailure",
          tag: builtin.tag,
          arguments: directArguments,
        },
        exactState(pre, {
          mode: "haltError",
          focusRoot: hashMidgardCekTermNodeV1({ kind: "error" }),
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
          auxiliary: MidgardCekErrorCodes.BuiltinFailure,
          cpuDelta: evaluated.budget.cpu,
          memoryDelta: evaluated.budget.memory,
        }),
      );
      return;
    }
    if (builtin.tag === 51n && evaluated.result.kind === "semanticConstant") {
      this.executeSemanticBuiltin(pre, builtin.tag, directArguments);
      return;
    }
    const resultRoot = this.addDirectResult(evaluated.result);
    if (builtin.tag === 68n) {
      const left = directArguments[0];
      const right = directArguments[1];
      if (
        evaluated.result.kind !== "blsMillerLoop" ||
        left?.kind !== "constant" ||
        right?.kind !== "constant"
      ) {
        throw new Error("CEK millerLoop returned an invalid expression value");
      }
      this.blsExpressions.set(
        rootHex(evaluated.result.expressionRoot),
        Object.freeze({
          kind: "millerLoop",
          g1: left.witness,
          g2: right.witness,
        }),
      );
    } else if (builtin.tag === 69n) {
      const left = directArguments[0];
      const right = directArguments[1];
      if (
        evaluated.result.kind !== "blsMillerLoop" ||
        left?.kind !== "blsMillerLoop" ||
        right?.kind !== "blsMillerLoop"
      ) {
        throw new Error("CEK mulMlResult returned an invalid expression value");
      }
      const leftExpression = this.blsExpressions.get(
        rootHex(left.expressionRoot),
      );
      const rightExpression = this.blsExpressions.get(
        rootHex(right.expressionRoot),
      );
      if (leftExpression === undefined || rightExpression === undefined) {
        throw new Error(
          "CEK mulMlResult is missing an authenticated child expression",
        );
      }
      this.blsExpressions.set(
        rootHex(evaluated.result.expressionRoot),
        Object.freeze({
          kind: "multiply",
          left: leftExpression,
          right: rightExpression,
        }),
      );
    }
    this.record(
      {
        kind: "executeBuiltinDirect",
        tag: builtin.tag,
        arguments: directArguments,
        result: evaluated.result,
      },
      exactState(pre, {
        mode: "return",
        focusRoot: resultRoot,
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: evaluated.budget.cpu,
        memoryDelta: evaluated.budget.memory,
      }),
    );
  }

  private returnValue(): void {
    const pre = this.state;
    const value = this.value(pre.focusRoot);
    const frame = this.continuation(pre.continuationRoot);
    if (frame === null) {
      this.record(
        { kind: "returnEmptyContinuation", value },
        value.kind === "constant"
          ? exactState(pre, {
              mode: "haltSuccess",
              focusRoot: pre.focusRoot,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
              continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
              auxiliary: 0n,
            })
          : errorSuccessor(pre, MidgardCekErrorCodes.NonconstantHalt),
      );
      return;
    }
    switch (frame.kind) {
      case "applyArgument": {
        const continuation = this.addContinuation({
          kind: "applyFunction",
          functionValue: pre.focusRoot,
          tail: frame.tail,
        });
        this.record(
          {
            kind: "returnApplyArgument",
            argument: frame.argument,
            capturedEnvironment: frame.environment,
            tail: frame.tail,
          },
          exactState(pre, {
            mode: "compute",
            focusRoot: frame.argument,
            environmentRoot: frame.environment,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "applyFunction": {
        const functionValue = this.value(frame.functionValue);
        if (functionValue.kind === "lambda") {
          const summary = this.environmentSummary(functionValue.environment);
          const environment = this.addEnvironment({
            value: Buffer.from(pre.focusRoot) as Hash32,
            tail: Buffer.from(functionValue.environment) as Hash32,
            length: (summary.kind === "empty" ? 0n : summary.length) + 1n,
          });
          this.record(
            {
              kind: "returnApplyLambda",
              body: functionValue.body,
              closureEnvironment: functionValue.environment,
              closureSummary: summary,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "compute",
              focusRoot: functionValue.body,
              environmentRoot: environment,
              continuationRoot: frame.tail,
              auxiliary: 0n,
            }),
          );
          return;
        }
        if (functionValue.kind === "builtin") {
          this.record(
            {
              kind: "returnApplyBuiltin",
              tag: functionValue.tag,
              forcesRemaining: functionValue.forcesRemaining,
              argumentsCount: functionValue.argumentsCount,
              argumentsRoot: functionValue.argumentsRoot,
              tail: frame.tail,
            },
            this.applyBuiltin(pre, pre.focusRoot, functionValue, frame.tail),
          );
          return;
        }
        this.record(
          {
            kind: "returnApplyInvalid",
            function: functionValue,
            tail: frame.tail,
          },
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidApplication),
        );
        return;
      }
      case "applyValue": {
        if (value.kind === "lambda") {
          const summary = this.environmentSummary(value.environment);
          const environment = this.addEnvironment({
            value: Buffer.from(frame.value) as Hash32,
            tail: Buffer.from(value.environment) as Hash32,
            length: (summary.kind === "empty" ? 0n : summary.length) + 1n,
          });
          this.record(
            {
              kind: "returnApplyValueLambda",
              argument: frame.value,
              body: value.body,
              closureEnvironment: value.environment,
              closureSummary: summary,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "compute",
              focusRoot: value.body,
              environmentRoot: environment,
              continuationRoot: frame.tail,
              auxiliary: 0n,
            }),
          );
          return;
        }
        if (value.kind === "builtin") {
          this.record(
            {
              kind: "returnApplyValueBuiltin",
              argument: frame.value,
              tag: value.tag,
              forcesRemaining: value.forcesRemaining,
              argumentsCount: value.argumentsCount,
              argumentsRoot: value.argumentsRoot,
              tail: frame.tail,
            },
            this.applyBuiltin(pre, frame.value, value, frame.tail),
          );
          return;
        }
        this.record(
          {
            kind: "returnApplyValueInvalid",
            argument: frame.value,
            function: value,
            tail: frame.tail,
          },
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidApplication),
        );
        return;
      }
      case "force":
        if (value.kind === "delay") {
          this.record(
            {
              kind: "returnForceDelay",
              body: value.body,
              closureEnvironment: value.environment,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "compute",
              focusRoot: value.body,
              environmentRoot: value.environment,
              continuationRoot: frame.tail,
              auxiliary: 0n,
            }),
          );
          return;
        }
        if (value.kind === "builtin" && value.forcesRemaining > 0n) {
          const nextValue = this.addValue({
            ...value,
            forcesRemaining: value.forcesRemaining - 1n,
          });
          this.record(
            {
              kind: "returnForceBuiltin",
              tag: value.tag,
              forcesRemaining: value.forcesRemaining,
              argumentsCount: value.argumentsCount,
              argumentsRoot: value.argumentsRoot,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "return",
              focusRoot: nextValue,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
              continuationRoot: frame.tail,
              auxiliary: 0n,
            }),
          );
          return;
        }
        this.record(
          {
            kind: "returnForceInvalid",
            value,
            tail: frame.tail,
          },
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidForce),
        );
        return;
      case "constr": {
        if (frame.remainingTermsCount > 0n) {
          const next = this.sequence(
            frame.remainingTermsRoot,
            frame.remainingTermsCount,
          );
          const nextValuesCount = frame.valuesCount + 1n;
          const nextValuesRoot = this.addSequence({
            head: Buffer.from(pre.focusRoot) as Hash32,
            tail: Buffer.from(frame.valuesRoot) as Hash32,
            length: nextValuesCount,
          });
          const continuation = this.addContinuation({
            ...frame,
            remainingTermsCount: frame.remainingTermsCount - 1n,
            remainingTermsRoot: next.tail,
            valuesCount: nextValuesCount,
            valuesRoot: nextValuesRoot,
          });
          this.record(
            {
              kind: "returnConstrNext",
              tag: frame.tag,
              remainingTermsCount: frame.remainingTermsCount,
              nextTerm: next.head,
              remainingTermsTail: next.tail,
              valuesCount: frame.valuesCount,
              valuesRoot: frame.valuesRoot,
              capturedEnvironment: frame.environment,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "compute",
              focusRoot: next.head,
              environmentRoot: frame.environment,
              continuationRoot: continuation,
              auxiliary: 0n,
            }),
          );
          return;
        }
        const nextValuesCount = frame.valuesCount + 1n;
        const nextValuesRoot = this.addSequence({
          head: Buffer.from(pre.focusRoot) as Hash32,
          tail: Buffer.from(frame.valuesRoot) as Hash32,
          length: nextValuesCount,
        });
        const constr = this.addValue({
          kind: "constr",
          tag: frame.tag,
          valuesCount: nextValuesCount,
          valuesRoot: nextValuesRoot,
        });
        this.record(
          {
            kind: "returnConstrDone",
            tag: frame.tag,
            valuesCount: frame.valuesCount,
            valuesRoot: frame.valuesRoot,
            capturedEnvironment: frame.environment,
            tail: frame.tail,
          },
          exactState(pre, {
            mode: "return",
            focusRoot: constr,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
            continuationRoot: frame.tail,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "case":
        if (value.kind !== "constr") {
          this.record(
            {
              kind: "returnCaseInvalid",
              value,
              branchesCount: frame.branchesCount,
              branchesRoot: frame.branchesRoot,
              capturedEnvironment: frame.environment,
              tail: frame.tail,
            },
            errorSuccessor(pre, MidgardCekErrorCodes.InvalidCaseScrutinee),
          );
          return;
        }
        if (value.tag >= frame.branchesCount) {
          this.record(
            {
              kind: "returnCaseConstr",
              tag: value.tag,
              valuesCount: value.valuesCount,
              valuesRoot: value.valuesRoot,
              branchesCount: frame.branchesCount,
              branchesRoot: frame.branchesRoot,
              capturedEnvironment: frame.environment,
              tail: frame.tail,
            },
            errorSuccessor(pre, MidgardCekErrorCodes.CaseBranchMissing),
          );
          return;
        }
        const work = this.addContinuation({
          kind: "caseSelect",
          environment: frame.environment,
          tail: frame.tail,
          valuesCount: value.valuesCount,
        });
        this.record(
          {
            kind: "returnCaseConstr",
            tag: value.tag,
            valuesCount: value.valuesCount,
            valuesRoot: value.valuesRoot,
            branchesCount: frame.branchesCount,
            branchesRoot: frame.branchesRoot,
            capturedEnvironment: frame.environment,
            tail: frame.tail,
          },
          exactState(pre, {
            mode: "caseSelect",
            focusRoot: frame.branchesRoot,
            environmentRoot: value.valuesRoot,
            continuationRoot: work,
            auxiliary: value.tag,
          }),
        );
        return;
      case "caseSelect":
      case "caseApply":
        throw new Error("case work continuation cannot be returned directly");
    }
  }

  private selectCase(): void {
    const pre = this.state;
    const frame = this.continuation(pre.continuationRoot);
    if (frame?.kind !== "caseSelect") {
      throw new Error("CEK case-select state has the wrong continuation");
    }
    const branch = this.sequence(pre.focusRoot, pre.auxiliary + 1n);
    const witness: MidgardCekCoreStepWitnessV1 = {
      kind: "selectCaseBranch",
      branch: branch.head,
      remainingBranchesRoot: branch.tail,
      length: branch.length,
      capturedEnvironment: frame.environment,
      tail: frame.tail,
      valuesCount: frame.valuesCount,
    };
    if (pre.auxiliary > 0n) {
      this.record(
        witness,
        exactState(pre, {
          mode: "caseSelect",
          focusRoot: branch.tail,
          environmentRoot: pre.environmentRoot,
          continuationRoot: pre.continuationRoot,
          auxiliary: pre.auxiliary - 1n,
        }),
      );
      return;
    }
    if (frame.valuesCount === 0n) {
      this.record(
        witness,
        exactState(pre, {
          mode: "compute",
          focusRoot: branch.head,
          environmentRoot: frame.environment,
          continuationRoot: frame.tail,
          auxiliary: 0n,
        }),
      );
      return;
    }
    const continuation = this.addContinuation({
      kind: "caseApply",
      environment: frame.environment,
      builtContinuation: frame.tail,
    });
    this.record(
      witness,
      exactState(pre, {
        mode: "caseApply",
        focusRoot: pre.environmentRoot,
        environmentRoot: branch.head,
        continuationRoot: continuation,
        auxiliary: frame.valuesCount,
      }),
    );
  }

  private applyCase(): void {
    const pre = this.state;
    const frame = this.continuation(pre.continuationRoot);
    if (frame?.kind !== "caseApply") {
      throw new Error("CEK case-apply state has the wrong continuation");
    }
    const value = this.sequence(pre.focusRoot, pre.auxiliary);
    const nextContinuation = this.addContinuation({
      kind: "applyValue",
      value: value.head,
      tail: frame.builtContinuation,
    });
    const witness: MidgardCekCoreStepWitnessV1 = {
      kind: "applyCaseValue",
      value: value.head,
      remainingValuesRoot: value.tail,
      length: value.length,
      capturedEnvironment: frame.environment,
      builtContinuation: frame.builtContinuation,
    };
    if (value.length === 1n) {
      this.record(
        witness,
        exactState(pre, {
          mode: "compute",
          focusRoot: pre.environmentRoot,
          environmentRoot: frame.environment,
          continuationRoot: nextContinuation,
          auxiliary: 0n,
        }),
      );
      return;
    }
    const continuation = this.addContinuation({
      kind: "caseApply",
      environment: frame.environment,
      builtContinuation: nextContinuation,
    });
    this.record(
      witness,
      exactState(pre, {
        mode: "caseApply",
        focusRoot: value.tail,
        environmentRoot: pre.environmentRoot,
        continuationRoot: continuation,
        auxiliary: value.length - 1n,
      }),
    );
  }
}

/**
 * Executes every structural CEK rule and proves each generated transition
 * through the same verifier mirrored on L1. Builtin mode remains fail-closed
 * here until semantic success/failure witnesses are generated.
 */
export const executeMidgardCekStructuralProgramV1 = (input: {
  readonly root: Bytes;
  readonly material: Iterable<MidgardCekProgramMaterialEntryV1>;
  readonly constantWitnesses: ReadonlyMap<
    string,
    MidgardCekConstantValueWitnessV1
  >;
  readonly executionIndex?: bigint;
  readonly maxSteps: number;
  readonly executionBudget?: {
    readonly cpu: bigint;
    readonly memory: bigint;
  };
}): MidgardCekStructuralExecutionV1 =>
  new StructuralExecutorV1(
    input.root,
    input.material,
    input.executionIndex ?? 0n,
    input.constantWitnesses,
  ).run(input.maxSteps, input.executionBudget);
