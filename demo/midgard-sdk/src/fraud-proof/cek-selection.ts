import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialSidecar,
  verifyMidgardCekProgramMaterial,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardCekDataNode } from "@al-ft/midgard-core/cek-semantic";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

export const CekSelectionEnvelopeFactsSchema = Data.Object({
  term_root: Data.Bytes(),
  node_count: Data.Integer(),
  material_byte_length: Data.Integer(),
});
export type CekSelectionEnvelopeFacts = Data.Static<
  typeof CekSelectionEnvelopeFactsSchema
>;
export const CekSelectionEnvelopeFacts = asDataType<CekSelectionEnvelopeFacts>(
  CekSelectionEnvelopeFactsSchema,
);
export const CekSelectionMaterialFactsSchema = Data.Object({
  program_node_count: Data.Integer(),
  program_byte_length: Data.Integer(),
  data_node_count: Data.Integer(),
  data_byte_length: Data.Integer(),
  data_roots: Data.Array(Data.Bytes()),
  data_blob_roots: Data.Array(Data.Bytes()),
});
export type CekSelectionMaterialFacts = Data.Static<
  typeof CekSelectionMaterialFactsSchema
>;
export const CekSelectionMaterialFacts = asDataType<CekSelectionMaterialFacts>(
  CekSelectionMaterialFactsSchema,
);

/** Reconstruct both material frontiers from the authenticated complete graph. */
export const deriveCekSelectionFacts = (material?: {
  readonly envelopeCbor: Uint8Array;
  readonly programMaterialSidecarCbor: Uint8Array;
}): {
  envelope: CekSelectionEnvelopeFacts;
  material: CekSelectionMaterialFacts;
} => {
  if (material === undefined)
    return {
      envelope: { term_root: "", node_count: 0n, material_byte_length: 0n },
      material: {
        program_node_count: 0n,
        program_byte_length: 0n,
        data_node_count: 0n,
        data_byte_length: 0n,
        data_roots: [],
        data_blob_roots: [],
      },
    };
  const envelope = decodeMidgardCekProgramEnvelope(
    Buffer.from(material.envelopeCbor),
  );
  const entries = decodeMidgardCekProgramMaterialSidecar(
    Buffer.from(material.programMaterialSidecarCbor),
  );
  const verified = verifyMidgardCekProgramMaterial(envelope, entries);
  const facts: CekSelectionMaterialFacts = {
    program_node_count: 0n,
    program_byte_length: 0n,
    data_node_count: 0n,
    data_byte_length: 0n,
    data_roots: [],
    data_blob_roots: [],
  };
  const dataRoots = new Set(
    verified.constants.map((constant) =>
      Buffer.from(constant.semanticRoot).toString("hex"),
    ),
  );
  const blobs = new Set<string>();
  for (const entry of entries) {
    if (
      entry.kind === "dataNode" ||
      entry.kind === "dataList" ||
      entry.kind === "dataPair"
    ) {
      facts.data_node_count += 1n;
      facts.data_byte_length += BigInt(entry.preimage.length);
      if (entry.kind === "dataNode") {
        const node = decodeMidgardCekDataNode(entry.preimage);
        const root =
          node.kind === "integer"
            ? node.cborRoot
            : node.kind === "bytes"
              ? node.bytesRoot
              : node.kind === "constrLarge"
                ? node.constructorCborRoot
                : undefined;
        if (root !== undefined) blobs.add(Buffer.from(root).toString("hex"));
      }
    } else {
      facts.program_node_count += 1n;
      facts.program_byte_length += BigInt(entry.preimage.length);
    }
  }
  facts.data_roots = [...dataRoots].sort();
  facts.data_blob_roots = [...blobs].sort();
  return {
    envelope: {
      term_root: Buffer.from(envelope.termRoot).toString("hex"),
      node_count: verified.nodeCount,
      material_byte_length: verified.materialByteLength,
    },
    material: facts,
  };
};

export const CEK_SELECTION_YIELD_ROLES = [
  {
    contract: "cekSelectionAuthenticate",
    deployment: "validationTraceDisputeCekSelectionAuthenticateWithdraw",
    role: "V1 validation-trace CEK selection authenticate yield",
  },
  {
    contract: "cekSelectionSuccessor",
    deployment: "validationTraceDisputeCekSelectionSuccessorWithdraw",
    role: "V1 validation-trace CEK selection successor yield",
  },
  {
    contract: "cekSelectionMaterialProgram",
    deployment: "validationTraceDisputeCekSelectionMaterialProgramWithdraw",
    role: "V1 validation-trace CEK selection material program yield",
  },
  {
    contract: "cekSelectionMaterialData",
    deployment: "validationTraceDisputeCekSelectionMaterialDataWithdraw",
    role: "V1 validation-trace CEK selection material data yield",
  },
] as const;
