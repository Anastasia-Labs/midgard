import {
  decodeMidgardNativeScript,
  type MidgardValue,
} from "@al-ft/midgard-core/codec";
import type { Address } from "@lucid-evolution/lucid";

import { LedgerColumns, type LedgerEntry } from "./ledger.js";
import type {
  MidgardLedgerOutput,
  MidgardLedgerTx,
} from "./ledger-tx/types.js";
import { redeemerDataFromCborHex } from "./midgard-redeemers.js";
import { plutusDataToCborHex } from "./plutus-data.js";
import type { MidgardValueDelta, PhaseAValidatedTx } from "./types.js";

type Bytes = Uint8Array;

type WireVersionedScript =
  | { readonly language: "NativeCardano"; readonly scriptBytes: Bytes }
  | { readonly language: "PlutusV3"; readonly scriptBytes: Bytes }
  | { readonly language: "MidgardV1"; readonly scriptBytes: Bytes };

type WireLedgerOutput = {
  readonly address: Bytes;
  readonly value: MidgardValue;
  readonly datum?: { readonly kind: "inline"; readonly cbor: Bytes };
  readonly scriptRef?: WireVersionedScript;
};

type WireLedgerEntry = {
  readonly txId: Bytes;
  readonly outref: Bytes;
  readonly output: Bytes;
  readonly address: Address;
  readonly timeStampTz?: Date;
};

type WireLedgerTx = {
  readonly txId: Bytes;
  readonly validity: MidgardLedgerTx["validity"];
  readonly fee: bigint;
  readonly networkId?: bigint;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly auxiliaryDataHash: Bytes;
  readonly scriptIntegrityHash: Bytes;
  readonly spendInputs: ReadonlyArray<{
    readonly txId: Bytes;
    readonly index: bigint;
  }>;
  readonly referenceInputs: ReadonlyArray<{
    readonly txId: Bytes;
    readonly index: bigint;
  }>;
  readonly outputs: readonly WireLedgerOutput[];
  readonly requiredSignerHashes: readonly Bytes[];
  readonly requiredObserverHashes: readonly Bytes[];
  readonly vkeyWitnesses: null;
  readonly witnessKeyHashes: readonly Bytes[];
  readonly scriptWitnesses: ReadonlyArray<{
    readonly index: number;
    readonly hash: Bytes;
    readonly script: WireVersionedScript;
  }>;
  readonly nativeScriptHashes: readonly Bytes[];
  readonly plutusScriptHashes: readonly Bytes[];
  readonly redeemers: ReadonlyArray<{
    readonly tag: number;
    readonly index: bigint;
    readonly dataCborHex: string;
    readonly exUnits: { readonly memory: bigint; readonly steps: bigint };
  }>;
  readonly mint: {
    readonly assets: ReadonlyArray<{
      readonly policyId: Bytes;
      readonly assetName: Bytes;
      readonly quantity: bigint;
    }>;
  };
  readonly requiresPlutusEvaluation: boolean;
};

export type WirePhaseACandidateV1 = {
  readonly ledgerTx: WireLedgerTx;
  readonly submission: {
    readonly txCbor: Bytes;
    readonly programMaterialSidecarCbor: Bytes | null;
    readonly arrivalSeq: bigint;
    readonly createdAt: Date;
  };
  readonly graph: {
    readonly spentOutRefHexes: readonly string[];
    readonly referenceOutRefHexes: readonly string[];
    readonly produced: readonly WireLedgerEntry[];
  };
  readonly derived: {
    readonly expectedNetworkId: bigint;
    readonly outputSum: MidgardValue;
    readonly mintDelta: MidgardValueDelta;
    readonly witnessKeyHashHexes: readonly string[];
    readonly nativeScriptHashHexes: readonly string[];
    readonly plutusScriptHashHexes: readonly string[];
    readonly requiredObserverHashHexes: readonly string[];
    readonly mintPolicyHashHexes: readonly string[];
    readonly redeemerWitnessHash: Bytes;
    readonly requiresLocalScriptDiscovery: boolean;
  };
};

type PhaseACandidateWireFields =
  | "ledgerTx"
  | "submission"
  | "graph"
  | "derived";
const phaseACandidateWireIsExhaustive: Exclude<
  keyof PhaseAValidatedTx,
  PhaseACandidateWireFields
> extends never
  ? true
  : never = true;
void phaseACandidateWireIsExhaustive;

type DerivedWireFields =
  | "expectedNetworkId"
  | "outputSum"
  | "mintDelta"
  | "witnessKeyHashHexes"
  | "nativeScriptHashHexes"
  | "plutusScriptHashHexes"
  | "requiredObserverHashHexes"
  | "mintPolicyHashHexes"
  | "redeemerWitnessHash"
  | "requiresLocalScriptDiscovery";
const derivedWireIsExhaustive: Exclude<
  keyof PhaseAValidatedTx["derived"],
  DerivedWireFields
> extends never
  ? true
  : never = true;
void derivedWireIsExhaustive;

type LedgerTxWireFields =
  | "txId"
  | "validity"
  | "fee"
  | "networkId"
  | "validityIntervalStart"
  | "validityIntervalEnd"
  | "auxiliaryDataHash"
  | "scriptIntegrityHash"
  | "spendInputs"
  | "referenceInputs"
  | "outputs"
  | "requiredSignerHashes"
  | "requiredObserverHashes"
  | "vkeyWitnesses"
  | "witnessKeyHashes"
  | "scriptWitnesses"
  | "nativeScriptHashes"
  | "plutusScriptHashes"
  | "redeemers"
  | "mint"
  | "requiresPlutusEvaluation";

// Adding a semantic ledger field must force an explicit wire-format decision.
const ledgerTxWireIsExhaustive: Exclude<
  keyof MidgardLedgerTx,
  LedgerTxWireFields
> extends never
  ? true
  : never = true;
void ledgerTxWireIsExhaustive;

const bytesView = (value: Buffer): Uint8Array => {
  // Worker responses use structured clone (not transfer lists). Exact-length
  // copies prevent a tiny slice from cloning its entire pooled backing store.
  const bytes = new Uint8Array(value.byteLength);
  bytes.set(value);
  return bytes;
};

const bufferView = (value: Uint8Array): Buffer =>
  Buffer.from(value.buffer, value.byteOffset, value.byteLength);

const serializeScript = (
  script: MidgardLedgerTx["scriptWitnesses"][number]["script"],
): WireVersionedScript => ({
  language: script.language,
  scriptBytes: bytesView(script.scriptBytes),
});

const deserializeScript = (
  script: WireVersionedScript,
): MidgardLedgerTx["scriptWitnesses"][number]["script"] => {
  const scriptBytes = bufferView(script.scriptBytes);
  if (script.language === "NativeCardano") {
    return {
      language: script.language,
      scriptBytes,
      nativeScript: decodeMidgardNativeScript(scriptBytes).script,
    };
  }
  return { language: script.language, scriptBytes };
};

const serializeOutput = (output: MidgardLedgerOutput): WireLedgerOutput => ({
  address: bytesView(output.address),
  value: output.value,
  ...(output.datum === undefined
    ? {}
    : {
        datum: { kind: "inline" as const, cbor: bytesView(output.datum.cbor) },
      }),
  ...(output.scriptRef === undefined
    ? {}
    : { scriptRef: serializeScript(output.scriptRef) }),
});

const deserializeOutput = (output: WireLedgerOutput): MidgardLedgerOutput => ({
  address: bufferView(output.address),
  value: output.value,
  ...(output.datum === undefined
    ? {}
    : {
        datum: { kind: "inline" as const, cbor: bufferView(output.datum.cbor) },
      }),
  ...(output.scriptRef === undefined
    ? {}
    : { scriptRef: deserializeScript(output.scriptRef) }),
});

const serializeLedgerEntry = (entry: LedgerEntry): WireLedgerEntry => ({
  txId: bytesView(entry[LedgerColumns.TX_ID]),
  outref: bytesView(entry[LedgerColumns.OUTREF]),
  output: bytesView(entry[LedgerColumns.OUTPUT]),
  address: entry[LedgerColumns.ADDRESS],
  ...(LedgerColumns.TIMESTAMPTZ in entry
    ? { timeStampTz: entry[LedgerColumns.TIMESTAMPTZ] }
    : {}),
});

const deserializeLedgerEntry = (entry: WireLedgerEntry): LedgerEntry => ({
  [LedgerColumns.TX_ID]: bufferView(entry.txId),
  [LedgerColumns.OUTREF]: bufferView(entry.outref),
  [LedgerColumns.OUTPUT]: bufferView(entry.output),
  [LedgerColumns.ADDRESS]: entry.address,
  ...(entry.timeStampTz === undefined
    ? {}
    : { [LedgerColumns.TIMESTAMPTZ]: entry.timeStampTz }),
});

export const serializePhaseACandidate = (
  candidate: PhaseAValidatedTx,
): WirePhaseACandidateV1 => {
  const tx = candidate.ledgerTx;
  return {
    ledgerTx: {
      txId: bytesView(tx.txId),
      validity: tx.validity,
      fee: tx.fee,
      networkId: tx.networkId,
      validityIntervalStart: tx.validityIntervalStart,
      validityIntervalEnd: tx.validityIntervalEnd,
      auxiliaryDataHash: bytesView(tx.auxiliaryDataHash),
      scriptIntegrityHash: bytesView(tx.scriptIntegrityHash),
      spendInputs: tx.spendInputs.map((input) => ({
        txId: bytesView(input.txId),
        index: input.index,
      })),
      referenceInputs: tx.referenceInputs.map((input) => ({
        txId: bytesView(input.txId),
        index: input.index,
      })),
      outputs: tx.outputs.map(serializeOutput),
      requiredSignerHashes: tx.requiredSignerHashes.map(bytesView),
      requiredObserverHashes: tx.requiredObserverHashes.map(bytesView),
      vkeyWitnesses: null,
      witnessKeyHashes: tx.witnessKeyHashes.map(bytesView),
      scriptWitnesses: tx.scriptWitnesses.map((witness) => ({
        index: witness.index,
        hash: bytesView(witness.hash),
        script: serializeScript(witness.script),
      })),
      nativeScriptHashes: tx.nativeScriptHashes.map(bytesView),
      plutusScriptHashes: tx.plutusScriptHashes.map(bytesView),
      redeemers: tx.redeemers.map((redeemer) => ({
        tag: redeemer.tag,
        index: redeemer.index,
        dataCborHex: plutusDataToCborHex(redeemer.data, { canonical: true }),
        exUnits: redeemer.exUnits,
      })),
      mint: {
        assets: tx.mint.assets.map((asset) => ({
          policyId: bytesView(asset.policyId),
          assetName: bytesView(asset.assetName),
          quantity: asset.quantity,
        })),
      },
      requiresPlutusEvaluation: tx.requiresPlutusEvaluation,
    },
    submission: {
      txCbor: bytesView(candidate.submission.txCbor),
      programMaterialSidecarCbor:
        candidate.submission.programMaterialSidecarCbor === null
          ? null
          : bytesView(candidate.submission.programMaterialSidecarCbor),
      arrivalSeq: candidate.submission.arrivalSeq,
      createdAt: candidate.submission.createdAt,
    },
    graph: {
      spentOutRefHexes: candidate.graph.spentOutRefHexes,
      referenceOutRefHexes: candidate.graph.referenceOutRefHexes,
      produced: candidate.graph.produced.map(serializeLedgerEntry),
    },
    derived: {
      expectedNetworkId: candidate.derived.expectedNetworkId,
      outputSum: candidate.derived.outputSum,
      mintDelta: candidate.derived.mintDelta,
      witnessKeyHashHexes: candidate.derived.witnessKeyHashHexes,
      nativeScriptHashHexes: candidate.derived.nativeScriptHashHexes,
      plutusScriptHashHexes: candidate.derived.plutusScriptHashHexes,
      requiredObserverHashHexes: candidate.derived.requiredObserverHashHexes,
      mintPolicyHashHexes: candidate.derived.mintPolicyHashHexes,
      redeemerWitnessHash: bytesView(candidate.derived.redeemerWitnessHash),
      requiresLocalScriptDiscovery:
        candidate.derived.requiresLocalScriptDiscovery,
    },
  } satisfies WirePhaseACandidateV1;
};

export const deserializePhaseACandidate = (
  candidate: WirePhaseACandidateV1,
): PhaseAValidatedTx => ({
  ledgerTx: {
    txId: bufferView(candidate.ledgerTx.txId),
    validity: candidate.ledgerTx.validity,
    fee: candidate.ledgerTx.fee,
    networkId: candidate.ledgerTx.networkId,
    validityIntervalStart: candidate.ledgerTx.validityIntervalStart,
    validityIntervalEnd: candidate.ledgerTx.validityIntervalEnd,
    auxiliaryDataHash: bufferView(candidate.ledgerTx.auxiliaryDataHash),
    scriptIntegrityHash: bufferView(candidate.ledgerTx.scriptIntegrityHash),
    spendInputs: candidate.ledgerTx.spendInputs.map((input) => ({
      txId: bufferView(input.txId),
      index: input.index,
    })),
    referenceInputs: candidate.ledgerTx.referenceInputs.map((input) => ({
      txId: bufferView(input.txId),
      index: input.index,
    })),
    outputs: candidate.ledgerTx.outputs.map(deserializeOutput),
    requiredSignerHashes:
      candidate.ledgerTx.requiredSignerHashes.map(bufferView),
    requiredObserverHashes:
      candidate.ledgerTx.requiredObserverHashes.map(bufferView),
    vkeyWitnesses: [],
    witnessKeyHashes: candidate.ledgerTx.witnessKeyHashes.map(bufferView),
    scriptWitnesses: candidate.ledgerTx.scriptWitnesses.map((witness) => ({
      index: witness.index,
      hash: bufferView(witness.hash),
      script: deserializeScript(witness.script),
    })),
    nativeScriptHashes: candidate.ledgerTx.nativeScriptHashes.map(bufferView),
    plutusScriptHashes: candidate.ledgerTx.plutusScriptHashes.map(bufferView),
    redeemers: candidate.ledgerTx.redeemers.map((redeemer) => ({
      tag: redeemer.tag,
      index: redeemer.index,
      data: redeemerDataFromCborHex(redeemer.dataCborHex),
      exUnits: redeemer.exUnits,
    })),
    mint: {
      assets: candidate.ledgerTx.mint.assets.map((asset) => ({
        policyId: bufferView(asset.policyId),
        assetName: bufferView(asset.assetName),
        quantity: asset.quantity,
      })),
    },
    requiresPlutusEvaluation: candidate.ledgerTx.requiresPlutusEvaluation,
  },
  submission: {
    txCbor: bufferView(candidate.submission.txCbor),
    programMaterialSidecarCbor:
      candidate.submission.programMaterialSidecarCbor === null
        ? null
        : bufferView(candidate.submission.programMaterialSidecarCbor),
    arrivalSeq: candidate.submission.arrivalSeq,
    createdAt: candidate.submission.createdAt,
  },
  graph: {
    spentOutRefHexes: candidate.graph.spentOutRefHexes,
    referenceOutRefHexes: candidate.graph.referenceOutRefHexes,
    produced: candidate.graph.produced.map(deserializeLedgerEntry),
  },
  derived: {
    expectedNetworkId: candidate.derived.expectedNetworkId,
    outputSum: candidate.derived.outputSum,
    mintDelta: candidate.derived.mintDelta,
    witnessKeyHashHexes: candidate.derived.witnessKeyHashHexes,
    nativeScriptHashHexes: candidate.derived.nativeScriptHashHexes,
    plutusScriptHashHexes: candidate.derived.plutusScriptHashHexes,
    requiredObserverHashHexes: candidate.derived.requiredObserverHashHexes,
    mintPolicyHashHexes: candidate.derived.mintPolicyHashHexes,
    redeemerWitnessHash: bufferView(candidate.derived.redeemerWitnessHash),
    requiresLocalScriptDiscovery:
      candidate.derived.requiresLocalScriptDiscovery,
  },
});
