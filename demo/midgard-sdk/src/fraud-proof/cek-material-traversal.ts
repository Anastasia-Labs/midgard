import {
  decodeMidgardCekProgramBlobPreimage,
  decodeMidgardCekProgramSequencePreimage,
  decodeMidgardCekProgramTermPreimage,
  decodeMidgardCekProgramValuePreimage,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardCekDataListNode,
  decodeMidgardCekDataNode,
  decodeMidgardCekDataPairNode,
} from "@al-ft/midgard-core/cek-semantic";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { ProofSchema } from "../common.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

export const CekMaterialTaskSchema = Data.Object({
  kind: Data.Integer(),
  root: Data.Bytes(),
  expected_length: Data.Integer(),
});
export type CekMaterialTask = Data.Static<typeof CekMaterialTaskSchema>;
const StackNode = asDataType<{ task: CekMaterialTask; tail: string }>(
  Data.Object({ task: CekMaterialTaskSchema, tail: Data.Bytes() }),
);
export const CekMaterialTraversalStateSchema = Data.Object({
  pending_root: Data.Bytes(),
  visited_root: Data.Bytes(),
  node_count: Data.Integer(),
  byte_length: Data.Integer(),
  expected_node_count: Data.Integer(),
  expected_byte_length: Data.Integer(),
});
export type CekMaterialTraversalState = Data.Static<
  typeof CekMaterialTraversalStateSchema
>;
export const CekMaterialTraversalState = asDataType<CekMaterialTraversalState>(
  CekMaterialTraversalStateSchema,
);
export const CekMaterialTraversalDatumSchema = faultProofStepDatumSchema(
  CekMaterialTraversalStateSchema,
);
export type CekMaterialTraversalDatum = Data.Static<
  typeof CekMaterialTraversalDatumSchema
>;
export const CekMaterialTraversalDatum = asDataType<CekMaterialTraversalDatum>(
  CekMaterialTraversalDatumSchema,
);
export const CekMaterialVisitSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  task: CekMaterialTaskSchema,
  tail_root: Data.Bytes(),
  entry: Data.Object({
    kind: Data.Integer(),
    root: Data.Bytes(),
    preimage: Data.Bytes(),
  }),
  already_seen: Data.Boolean(),
  visited_proof: ProofSchema,
  yield_reference_input_index: Data.Integer(),
  next_pending_root: Data.Bytes(),
});
export const CekMaterialTraversalRedeemerSchema = faultProofStepRedeemerSchema(
  CekMaterialVisitSchema,
);
export type CekMaterialTraversalRedeemer = Data.Static<
  typeof CekMaterialTraversalRedeemerSchema
>;
export const CekMaterialTraversalRedeemer =
  asDataType<CekMaterialTraversalRedeemer>(CekMaterialTraversalRedeemerSchema);
const digest = (bytes: Uint8Array) =>
  Buffer.from(blake2b(bytes, { dkLen: 32 })).toString("hex");
export const CEK_MATERIAL_EMPTY_STACK = digest(
  Buffer.from("midgard/cek-material-stack/empty"),
);
export const pushCekMaterialTask = (
  task: CekMaterialTask,
  tail: string,
): string =>
  digest(
    Buffer.concat([
      Buffer.from("midgard/cek-material-stack/item"),
      Buffer.from(
        aikenSerialisedPlutusDataCbor(Data.to({ task, tail }, StackNode)),
        "hex",
      ),
    ]),
  );
export const pushCekMaterialTasks = (
  tasks: readonly CekMaterialTask[],
  tail: string,
): string =>
  tasks.reduceRight((root, task) => pushCekMaterialTask(task, root), tail);
const task = (
  kind: bigint,
  root: Uint8Array,
  expected_length = -1n,
): CekMaterialTask => ({
  kind,
  root: Buffer.from(root).toString("hex"),
  expected_length,
});

/** Called only after complete SDK material admission; on-chain independently checks each edge. */
export const cekMaterialChildren = (
  entry: MidgardCekProgramMaterialEntry,
): readonly CekMaterialTask[] => {
  switch (entry.kind) {
    case "term": {
      const node = decodeMidgardCekProgramTermPreimage(entry.preimage);
      switch (node.kind) {
        case "variable":
        case "error":
        case "builtin":
          return [];
        case "unaryTerm":
          return [task(0n, node.child)];
        case "application":
          return [task(0n, node.function), task(0n, node.argument)];
        case "contextConstant":
          throw new Error(
            "context constants are forbidden in program material",
          );
        case "constant":
          return [task(1n, node.value)];
        case "constr":
          return node.count === 0n ? [] : [task(2n, node.sequence, node.count)];
        case "case":
          return [
            task(0n, node.scrutinee),
            ...(node.count === 0n ? [] : [task(2n, node.sequence, node.count)]),
          ];
        default:
          return node satisfies never;
      }
    }
    case "value": {
      const node = decodeMidgardCekProgramValuePreimage(entry.preimage);
      return [task(3n, node.typeRoot), task(5n, node.semanticRoot)];
    }
    case "sequence": {
      const node = decodeMidgardCekProgramSequencePreimage(entry.preimage);
      return [
        task(0n, node.head),
        ...(node.length === 1n ? [] : [task(2n, node.tail, node.length - 1n)]),
      ];
    }
    case "blobChunk":
    case "blobBranch": {
      const node = decodeMidgardCekProgramBlobPreimage(
        entry.kind,
        entry.preimage,
      );
      return node.kind === "chunk"
        ? []
        : [task(3n, node.left), task(3n, node.right)];
    }
    case "dataNode": {
      const node = decodeMidgardCekDataNode(entry.preimage);
      switch (node.kind) {
        case "constrSmall":
          return node.fieldsCount === 0n
            ? []
            : [task(6n, node.fieldsRoot, node.fieldsCount)];
        case "constrLarge":
          return [
            task(3n, node.constructorCborRoot),
            ...(node.fieldsCount === 0n
              ? []
              : [task(6n, node.fieldsRoot, node.fieldsCount)]),
          ];
        case "map":
          return node.entriesCount === 0n
            ? []
            : [task(7n, node.entriesRoot, node.entriesCount)];
        case "list":
          return node.itemsCount === 0n
            ? []
            : [task(6n, node.itemsRoot, node.itemsCount)];
        case "integer":
          return [task(3n, node.cborRoot)];
        case "bytes":
          return [task(3n, node.bytesRoot)];
        default:
          return node satisfies never;
      }
    }
    case "dataList": {
      const node = decodeMidgardCekDataListNode(entry.preimage);
      return [
        task(5n, node.head),
        ...(node.length === 1n ? [] : [task(6n, node.tail, node.length - 1n)]),
      ];
    }
    case "dataPair": {
      const node = decodeMidgardCekDataPairNode(entry.preimage);
      return [
        task(5n, node.key),
        task(5n, node.value),
        ...(node.length === 1n ? [] : [task(7n, node.tail, node.length - 1n)]),
      ];
    }
  }
};
export const CEK_MATERIAL_TASK_YIELD_ROLES = [
  {
    contract: "cekMaterialProgramTask",
    deployment: "validationTraceDisputeCekMaterialProgramTaskWithdraw",
    role: "V1 validation-trace CEK material program task yield",
    token: "V1VtCekMatProgramTask",
  },
  {
    contract: "cekMaterialDataTask",
    deployment: "validationTraceDisputeCekMaterialDataTaskWithdraw",
    role: "V1 validation-trace CEK material Data task yield",
    token: "V1VtCekMatDataTask",
  },
] as const;
