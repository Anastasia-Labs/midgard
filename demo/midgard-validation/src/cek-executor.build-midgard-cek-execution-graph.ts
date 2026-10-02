import {
  commitMidgardCekBlob,
  decodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  type Hash32,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  type MidgardCekMachineState,
  type MidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  type MidgardCekTermNode,
  type MidgardCekValueNode,
  verifyMidgardCekProgramMaterial,
} from "@al-ft/midgard-core";

import { type MidgardCekConstantValueWitness } from "./cek-builtin.js";
import {
  encodeMidgardCekCanonicalDataConstant,
  midgardCekConstantMemorySize,
} from "./cek-constant.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import { type MidgardCekCoreStepWitness } from "./cek-machine.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";

export type Bytes = Uint8Array;

const MACHINE_STEP_CPU = 16_000n;

export const MACHINE_STEP_MEMORY = 100n;

export const rootHex = (root: Bytes): string =>
  Buffer.from(root).toString("hex");

export const sameBytes = (left: Bytes, right: Bytes): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

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
): MidgardCekMachineState =>
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

export type EnvironmentNode = {
  readonly value: Hash32;
  readonly tail: Hash32;
  readonly length: bigint;
};

export type SequenceNode = {
  readonly head: Hash32;
  readonly tail: Hash32;
  readonly length: bigint;
};

export type MidgardCekExecutionGraph = {
  readonly root: Hash32;
  readonly contextTermRoot: Hash32;
  readonly contextValueRoot: Hash32;
  readonly material: ReadonlyMap<string, MidgardCekProgramMaterialEntry>;
  readonly constantWitnesses: ReadonlyMap<
    string,
    MidgardCekConstantValueWitness
  >;
};

export type MidgardCekExecutionStep = {
  readonly pre: MidgardCekMachineState;
  readonly post: MidgardCekMachineState;
  readonly witness: MidgardCekCoreStepWitness;
};

export type MidgardCekStructuralExecution = {
  readonly initialState: MidgardCekMachineState;
  readonly steps: readonly MidgardCekExecutionStep[];
  readonly terminalState: MidgardCekMachineState;
  readonly stopReason: "halted" | "budgetExceeded";
};

/**
 * Builds the deterministic runtime application root
 * `program scriptContext`. The source envelope remains the script identity;
 * the context nodes are derived from already-authenticated transaction data
 * and are therefore runtime material rather than a second script payload.
 */
export const buildMidgardCekExecutionGraph = (
  envelope: MidgardCekProgramEnvelope,
  sourceMaterial: Iterable<MidgardCekProgramMaterialEntry>,
  contextCbor: Uint8Array,
): MidgardCekExecutionGraph => {
  const source = [...sourceMaterial];
  const verifiedSource = verifyMidgardCekProgramMaterial(envelope, source, {
    allowUnreachable: true,
  });

  const material = new Map<string, MidgardCekProgramMaterialEntry>();
  const constantWitnesses = new Map<string, MidgardCekConstantValueWitness>(
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
      const payload = plutusDataFromCborIterative(constant.payloadCbor);
      const semantic = commitMidgardCekDataTree(payload);
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
  const addEntry = (entry: MidgardCekProgramMaterialEntry): void => {
    const exact = decodeMidgardCekProgramMaterialEntry(
      encodeMidgardCekProgramMaterialEntry(entry),
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
    const committed = commitMidgardCekBlob(bytes);
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
    node: Extract<MidgardCekValueNode, { readonly kind: "constant" }>,
  ): Hash32 => {
    const root = hashMidgardCekValueNode(node);
    addEntry({
      kind: "value",
      root,
      preimage: encodeMidgardCekValueNode(node),
    });
    return root;
  };
  const addTerm = (node: MidgardCekTermNode): Hash32 => {
    const root = hashMidgardCekTermNode(node);
    addEntry({
      kind: "term",
      root,
      preimage: encodeMidgardCekTermNode(node),
    });
    return root;
  };

  const context = encodeMidgardCekCanonicalDataConstant(
    plutusDataFromCborIterative(contextCbor),
  );
  const contextPayload = plutusDataFromCborIterative(context.payloadCbor);
  const contextSemantic = commitMidgardCekDataTree(contextPayload);
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
    memory: midgardCekConstantMemorySize(context.type, contextPayload),
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
        memory: midgardCekConstantMemorySize(context.type, contextPayload),
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
