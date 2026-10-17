import type { MidgardValidationMachineState } from "@al-ft/midgard-core";
import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialSidecar,
  hashMidgardCekProgramEnvelope,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { Constr } from "@lucid-evolution/lucid";

import {
  type DeterministicValidationMachineTrace,
  type ValidationMachineWorkWitness,
} from "./validation-machine/index.js";
import {
  bytes,
  type ConstructorData,
  int,
  record,
} from "./validation-machine-data.validation-machine-carriage-tier-mismatch-error.js";
import {
  CEK_ROUTE_MATERIAL_KEYS,
  type CekRouteMaterial,
} from "./validation-machine-data.validation-semantic-resolver-index.js";

const exactCekRouteMaterialObject = (
  value: unknown,
): Record<(typeof CEK_ROUTE_MATERIAL_KEYS)[number], unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("CEK route material must be an object");
  }
  const actual = Object.keys(value).sort();
  const expected = [...CEK_ROUTE_MATERIAL_KEYS].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      `CEK route material must contain exactly ${CEK_ROUTE_MATERIAL_KEYS.join(", ")}`,
    );
  }
  return value as Record<(typeof CEK_ROUTE_MATERIAL_KEYS)[number], unknown>;
};

const exactBytes = (value: unknown, label: string): Buffer => {
  if (!(value instanceof Uint8Array)) {
    throw new Error(`${label} must be bytes`);
  }
  return Buffer.from(value);
};

export const extractCekProgramEnvelopeFromFirstSourceChunk = ({
  chunk,
  languageTag,
}: {
  readonly chunk: Uint8Array;
  readonly languageTag: 3 | 128;
}): Buffer => {
  const source = Buffer.from(chunk);
  const outer = readCborArrayHeader(source, 0, "CEK selected versioned script");
  if (outer.length !== 2) {
    throw new Error("CEK selected versioned script must contain two fields");
  }
  const language = readCborUnsigned(
    source,
    outer.nextOffset,
    "CEK selected versioned script language",
  );
  const payload = readCborBytes(
    source,
    language.nextOffset,
    "CEK selected versioned script payload",
  );
  if (
    language.value !== BigInt(languageTag) ||
    payload.nextOffset !== source.length
  ) {
    throw new Error(
      "CEK selected versioned script language or payload length is invalid",
    );
  }
  return Buffer.from(payload.value);
};

/**
 * Validates and defensively copies the complete C28 route material. The
 * envelope must be the exact selected script payload, both retained forms
 * must be canonical, and the sidecar must be exactly the complete graph for
 * that one envelope.
 */
export const validateCekRouteMaterial = ({
  value,
  firstSourceChunk,
  languageTag,
}: {
  readonly value: unknown;
  readonly firstSourceChunk: Uint8Array;
  readonly languageTag: 3 | 128;
}): CekRouteMaterial => {
  const routeMaterial = exactCekRouteMaterialObject(value);
  const envelopeCbor = exactBytes(
    routeMaterial.envelopeCbor,
    "CEK route envelope CBOR",
  );
  const selectedEnvelopeCbor = extractCekProgramEnvelopeFromFirstSourceChunk({
    chunk: firstSourceChunk,
    languageTag,
  });
  if (!envelopeCbor.equals(selectedEnvelopeCbor)) {
    throw new Error(
      "CEK route envelope must equal the selected first-source-chunk payload",
    );
  }
  const envelope = decodeMidgardCekProgramEnvelope(envelopeCbor);
  if (!encodeMidgardCekProgramEnvelope(envelope).equals(envelopeCbor)) {
    throw new Error("CEK route envelope CBOR is not canonical");
  }
  const programMaterialSidecarCbor = exactBytes(
    routeMaterial.programMaterialSidecarCbor,
    "CEK route program-material sidecar CBOR",
  );
  const entries = decodeMidgardCekProgramMaterialSidecar(
    programMaterialSidecarCbor,
  );
  if (
    !encodeMidgardCekProgramMaterialSidecar(entries).equals(
      programMaterialSidecarCbor,
    )
  ) {
    throw new Error("CEK route program-material sidecar CBOR is not canonical");
  }
  verifyMidgardCekProgramMaterialBundle([envelope], entries);
  const programEnvelopeHash = exactBytes(
    routeMaterial.programEnvelopeHash,
    "CEK route program-envelope hash",
  );
  const canonicalEnvelopeHash = Buffer.from(
    hashMidgardCekProgramEnvelope(envelope),
  );
  if (
    programEnvelopeHash.length !== 32 ||
    !programEnvelopeHash.equals(canonicalEnvelopeHash)
  ) {
    throw new Error("CEK route program-envelope hash is invalid");
  }
  return Object.freeze({
    envelopeCbor: Buffer.from(envelopeCbor),
    programMaterialSidecarCbor: Buffer.from(programMaterialSidecarCbor),
    programEnvelopeHash: Buffer.from(programEnvelopeHash),
  });
};

export const buildCekRouteMaterial = ({
  trace,
  witness,
}: {
  readonly trace: DeterministicValidationMachineTrace;
  readonly witness: ValidationMachineWorkWitness;
}): CekRouteMaterial | undefined => {
  if (
    witness.phase !== "cek" ||
    witness.auxiliary?.kind !== "nativeExecutionScan" ||
    witness.auxiliary.languageTag === 0
  ) {
    return undefined;
  }
  const envelopeCbor = extractCekProgramEnvelopeFromFirstSourceChunk({
    chunk: witness.auxiliary.firstChunkProof.chunk,
    languageTag: witness.auxiliary.languageTag,
  });
  const envelope = decodeMidgardCekProgramEnvelope(envelopeCbor);
  return validateCekRouteMaterial({
    value: {
      envelopeCbor,
      programMaterialSidecarCbor: trace.programMaterialSidecarCbor,
      programEnvelopeHash: hashMidgardCekProgramEnvelope(envelope),
    },
    firstSourceChunk: witness.auxiliary.firstChunkProof.chunk,
    languageTag: witness.auxiliary.languageTag,
  });
};

export const validationMachineStateData = (
  state: MidgardValidationMachineState,
): ConstructorData =>
  record([
    int(state.machineVersion),
    bytes(state.eventKeyHash),
    bytes(state.transactionId),
    bytes(state.transactionCommitment),
    bytes(state.validationContextHash),
    new Constr(state.sourceKind === "normal" ? 0 : 1, []),
    bytes(state.priorLedgerRoot),
    new Constr(
      {
        canonicalDecode: 0,
        compactBinding: 1,
        staticLedgerRules: 2,
        inputSets: 3,
        signatures: 4,
        phaseANativeScripts: 5,
        phaseAScriptPreconditions: 6,
        resolveInputs: 7,
        scriptSources: 8,
        nativeScripts: 9,
        scriptIntegrity: 10,
        cek: 11,
        valueAndMint: 12,
        ledgerDelta: 13,
        terminal: 14,
      }[state.phase],
      [],
    ),
    int(state.programCounter),
    bytes(state.workRoot),
    state.executionCpu,
    state.executionMemory,
    new Constr(
      state.verdict === "pending" ? 0 : state.verdict === "accepted" ? 1 : 2,
      [],
    ),
    bytes(state.rejectionCodeHash),
    bytes(state.ledgerDeltaRoot),
  ]);

export const validationOneStepWitnessData = ({
  witness,
  claimedSuccessor,
}: {
  readonly witness: ValidationMachineWorkWitness;
  readonly claimedSuccessor: MidgardValidationMachineState;
}): ConstructorData =>
  record([bytes(witness.cbor), validationMachineStateData(claimedSuccessor)]);
