import type {
  MidgardBlake2b256TraceControl,
  MidgardCekDataBytesControl,
  MidgardCekDataIntegerControl,
  MidgardCekDataTraverseControl,
  MidgardCekSourceBlobControl,
  MidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofWitness,
  MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import { Constr } from "@lucid-evolution/lucid";

import { type ValidationMachineWorkWitness } from "./validation-machine/index.js";
import {
  chunkProofData,
  dataTraverseActionData,
  summaryData,
} from "./validation-machine-data.ledger-output-proof-witness-data.js";
import {
  bool,
  bytes,
  type ConstructorData,
  int,
  option,
  proofData,
  record,
} from "./validation-machine-data.validation-machine-carriage-tier-mismatch-error.js";

export const redeemerItemControlData = (
  control: MidgardRedeemerItemProofControl,
): ConstructorData => {
  const blake2b256ControlData = (
    hash: MidgardBlake2b256TraceControl,
  ): ConstructorData =>
    record([
      int(hash.version),
      int(hash.stage),
      int(hash.cursor),
      int(hash.totalLength),
      bytes(hash.chainingValue),
      bytes(hash.activeBlock),
      int(hash.activeBlockLength),
      bytes(hash.workingValue),
      int(hash.round),
    ]);
  const sourceBlobData = (blob: MidgardCekSourceBlobControl): ConstructorData =>
    record([
      int(blob.version),
      int(blob.stage),
      int(blob.sourceStart),
      int(blob.sourceLength),
      record([
        int(blob.frontier.count),
        blob.frontier.byteLength,
        blob.frontier.peaks.map((peak) =>
          record([int(peak.height), bytes(peak.root), peak.byteLength]),
        ),
      ]),
      option(blob.activeHash, blake2b256ControlData),
    ]);
  const integerControlData = (
    integer: MidgardCekDataIntegerControl,
  ): ConstructorData =>
    record([
      int(integer.version),
      int(integer.stage),
      int(integer.sourceStart),
      int(integer.sourceLength),
      integer.memory,
      option(integer.blob, sourceBlobData),
    ]);
  const bytesControlData = (
    byteControl: MidgardCekDataBytesControl,
  ): ConstructorData =>
    record([
      int(byteControl.version),
      int(byteControl.stage),
      int(byteControl.sourceStart),
      int(byteControl.sourceLength),
      int(byteControl.bytesLength),
      option(byteControl.blob, sourceBlobData),
    ]);
  const traversalData = (
    traversal: MidgardCekDataTraverseControl,
  ): ConstructorData =>
    record([
      int(traversal.version),
      int(traversal.stage),
      int(traversal.sourceStart),
      int(traversal.sourceLength),
      int(traversal.offset),
      bytes(traversal.frameRoot),
      option(traversal.pendingLargeExpectedChildren, int),
      option(traversal.integer, integerControlData),
      option(traversal.bytes, bytesControlData),
      option(traversal.result, summaryData),
    ]);
  return record([
    int(control.version),
    int(control.mode),
    int(control.stage),
    int(control.itemIndex),
    int(control.itemCount),
    int(control.totalLength),
    bytes(control.itemCommitment),
    int(control.expectedPurposeTag),
    int(control.expectedPointerIndex),
    int(control.purposeTag),
    int(control.pointerIndex),
    int(control.dataOffset),
    int(control.dataLength),
    control.executionMemory,
    control.executionSteps,
    option(control.traversal, traversalData),
  ]);
};

export const redeemerItemProofWitnessData = (
  witness: MidgardRedeemerItemProofWitness,
): ConstructorData => {
  const action =
    witness.action.kind === "openHeader"
      ? new Constr(0, [])
      : witness.action.kind === "openTail"
        ? new Constr(1, [])
        : witness.action.kind === "traverseData"
          ? new Constr(2, [dataTraverseActionData(witness.action.action)])
          : new Constr(3, []);
  return record([
    action,
    option(witness.chunkProof, chunkProofData),
    option(witness.nextChunkProof, chunkProofData),
  ]);
};

export const valueMutationData = (
  mutation: Extract<
    NonNullable<ValidationMachineWorkWitness["auxiliary"]>,
    {
      readonly kind: "valueInputAsset" | "valueOutputAsset" | "valueMintAsset";
    }
  >["mutationStep"],
): ConstructorData =>
  record([
    bool(mutation.oldDelta !== null),
    mutation.oldDelta ?? 0n,
    proofData(mutation.proofCbor),
  ]);

export const sourceKind = (kind: "spend" | "reference"): bigint =>
  kind === "spend" ? 0n : 1n;

export const originKind = (kind: "inline" | "reference"): bigint =>
  kind === "inline" ? 0n : 1n;

export const resolverPhaseIndex = (
  phase: MidgardValidationPhaseName,
): number => {
  const index = {
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
    terminal: -1,
  }[phase];
  if (index < 0) {
    throw new Error(`validation phase ${phase} has no resolver`);
  }
  return index;
};

export const scanStage = (
  witness: ValidationMachineWorkWitness,
  label: string,
): number => {
  const outer = readCborArrayHeader(witness.cbor, 0, label);
  if (outer.length < 6) {
    throw new Error(`${label} control has too few fields`);
  }
  let offset = outer.nextOffset;
  for (let index = 0; index < 5; index += 1) {
    offset = readCborBytes(
      witness.cbor,
      offset,
      `${label}.binding_${index.toString()}`,
    ).nextOffset;
  }
  const stage = readCborInteger(witness.cbor, offset, `${label}.stage`).value;
  const exact = Number(stage);
  if (!Number.isSafeInteger(exact) || exact < 0) {
    throw new Error(`${label} stage is invalid`);
  }
  return exact;
};

export const scriptSourcesControlStatus = (
  witness: ValidationMachineWorkWitness,
): {
  readonly stage: number;
  readonly pendingHashStage: number | null;
} => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_sources_control",
  );
  if (control.length !== 30 && control.length !== 31) {
    throw new Error("script_sources_control has an invalid field count");
  }
  const stage = Number(asBigInt(control[9], "script_sources_control.stage"));
  if (!Number.isSafeInteger(stage) || stage < 0) {
    throw new Error("script_sources_control stage is invalid");
  }
  if (stage !== 0 || control.length === 30) {
    return { stage, pendingHashStage: null };
  }
  const pendingCbor = asBytes(
    control[30],
    "script_sources_control.pending_source",
  );
  if (pendingCbor.length === 0) {
    throw new Error("script_sources_control pending source is empty");
  }
  const pending = asArray(
    decodeSingleCbor(pendingCbor),
    "script_sources_pending_source",
  );
  if (
    pending.length !== 9 ||
    asBigInt(pending[0], "script_sources_pending_source.version") !== 1n
  ) {
    throw new Error("script_sources_control pending source is invalid");
  }
  const hashControlCbor = asBytes(
    pending[8],
    "script_sources_pending_source.hash_control",
  );
  const hashControl = readCborArrayHeader(
    hashControlCbor,
    0,
    "script_sources_pending_source.hash_control",
  );
  const hashVersion = readCborInteger(
    hashControlCbor,
    hashControl.nextOffset,
    "script_sources_pending_source.hash_control.version",
  );
  const hashStage = readCborInteger(
    hashControlCbor,
    hashVersion.nextOffset,
    "script_sources_pending_source.hash_control.stage",
  );
  if (hashControl.length !== 9 || hashVersion.value !== 1n) {
    throw new Error("script_sources pending hash control is invalid");
  }
  const pendingHashStage = Number(hashStage.value);
  if (
    !Number.isSafeInteger(pendingHashStage) ||
    pendingHashStage < 0 ||
    pendingHashStage > 3
  ) {
    throw new Error("script_sources pending hash stage is invalid");
  }
  return { stage, pendingHashStage };
};

export const scriptSourcesDiscoveryCurrentScriptHash = (
  witness: ValidationMachineWorkWitness,
): Buffer => {
  const control = asArray(
    decodeSingleCbor(witness.cbor),
    "script_sources_control",
  );
  if (
    control.length !== 31 ||
    asBigInt(control[9], "script_sources_control.stage") !== 9n
  ) {
    throw new Error("script_sources_control is not at discovery stage 9");
  }
  const discovery = asArray(
    decodeSingleCbor(asBytes(control[30], "script_sources_control.discovery")),
    "script_sources_discovery",
  );
  if (discovery.length !== 15) {
    throw new Error("script_sources discovery has an invalid field count");
  }
  const scriptHash = Buffer.from(
    asBytes(discovery[5], "script_sources_discovery.current_script_hash"),
  );
  if (scriptHash.length !== 28) {
    throw new Error(
      "script_sources discovery current script hash has an invalid length",
    );
  }
  return scriptHash;
};
