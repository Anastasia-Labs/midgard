import {
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramMaterialSidecar,
  type Hash32,
  hashMidgardVersionedScript,
  type MidgardCekProgramMaterialEntry,
  type MidgardVersionedScript,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core";

import type { MidgardCekConstantValueWitness } from "./cek-builtin.js";
import {
  buildMidgardCanonicalCekProgram,
  copyProgramEnvelope,
  copyProgramMaterialEntry,
} from "./cek-program.build-midgard-canonical-cek-program.js";
import {
  type MidgardCanonicalCekProgram,
  type MidgardCanonicalScriptArtifact,
  type MidgardCanonicalScriptArtifactInput,
  type MidgardCanonicalScriptArtifactLanguage,
} from "./cek-program.unwrap-canonical-cbor-byte-string.js";

const copyConstantValueWitness = (
  value: MidgardCekConstantValueWitness,
): MidgardCekConstantValueWitness => {
  if (value.kind === "constant") {
    const typeCbor = Buffer.from(value.witness.typeCbor);
    const payloadCbor = Buffer.from(value.witness.payloadCbor);
    return Object.freeze({
      kind: "constant",
      get witness() {
        return Object.freeze({
          get typeCbor(): Buffer {
            return Buffer.from(typeCbor);
          },
          get payloadCbor(): Buffer {
            return Buffer.from(payloadCbor);
          },
        });
      },
    });
  }
  const typeCbor = Buffer.from(value.witness.typeCbor);
  const payloadRoot = Buffer.from(value.witness.payload.root);
  return Object.freeze({
    kind: "semanticConstant",
    get witness() {
      return Object.freeze({
        get typeCbor(): Buffer {
          return Buffer.from(typeCbor);
        },
        get payload() {
          return Object.freeze({
            get root(): Buffer {
              return Buffer.from(payloadRoot);
            },
            cborLength: value.witness.payload.cborLength,
            memory: value.witness.payload.memory,
          });
        },
        memory: value.witness.memory,
      });
    },
  });
};

const copyCanonicalProgram = (
  program: MidgardCanonicalCekProgram,
): MidgardCanonicalCekProgram =>
  Object.freeze({
    envelope: copyProgramEnvelope(program.envelope),
    envelopeCbor: Buffer.from(program.envelopeCbor),
    envelopeHash: Buffer.from(program.envelopeHash) as Hash32,
    material: new Map(
      [...program.material].map(([key, entry]) => [
        key,
        copyProgramMaterialEntry(entry),
      ]),
    ),
    constantWitnesses: new Map(
      [...program.constantWitnesses].map(([key, value]) => [
        key,
        copyConstantValueWitness(value),
      ]),
    ),
  });

const copyCanonicalCredentialScript = (
  language: MidgardCanonicalScriptArtifactLanguage,
  envelopeCbor: Uint8Array,
): MidgardVersionedScript => {
  const scriptBytes = Buffer.from(envelopeCbor);
  return Object.freeze({
    language,
    get scriptBytes(): Buffer {
      return Buffer.from(scriptBytes);
    },
  });
};

/**
 * Builds the exact canonical V1 script artifact used for Midgard credentials
 * from raw PlutusV3 or MidgardV1 Flat authoring input.
 */
export const buildMidgardCanonicalScriptArtifact = ({
  language,
  sourceRawFlatProgramBytes,
}: MidgardCanonicalScriptArtifactInput): MidgardCanonicalScriptArtifact => {
  const sourceBytes = Buffer.from(sourceRawFlatProgramBytes);
  const canonicalProgram = buildMidgardCanonicalCekProgram(sourceBytes);
  const sourceRawScriptAuditHash = hashMidgardVersionedScript({
    language,
    scriptBytes: sourceBytes,
  });
  const canonicalCredentialScript = copyCanonicalCredentialScript(
    language,
    canonicalProgram.envelopeCbor,
  );
  const canonicalMidgardCredentialScriptHash = hashMidgardVersionedScript(
    canonicalCredentialScript,
  );
  const encodedSidecar = encodeMidgardCekProgramMaterialSidecar([
    ...canonicalProgram.material.values(),
  ]);
  const canonicalMaterialEntries =
    decodeMidgardCekProgramMaterialSidecar(encodedSidecar);
  verifyMidgardCekProgramMaterialBundle(
    [canonicalProgram.envelope],
    canonicalMaterialEntries,
  );

  return Object.freeze({
    get canonicalMidgardCredentialScript(): MidgardVersionedScript {
      return copyCanonicalCredentialScript(
        language,
        canonicalProgram.envelopeCbor,
      );
    },
    canonicalMidgardCredentialScriptHash,
    sourceRawScriptAuditHash,
    get canonicalProgram(): MidgardCanonicalCekProgram {
      return copyCanonicalProgram(canonicalProgram);
    },
    get canonicalMaterialEntries(): readonly MidgardCekProgramMaterialEntry[] {
      return Object.freeze(
        canonicalMaterialEntries.map(copyProgramMaterialEntry),
      );
    },
    get canonicalMaterialSidecarCbor(): Buffer {
      return Buffer.from(encodedSidecar);
    },
  });
};
