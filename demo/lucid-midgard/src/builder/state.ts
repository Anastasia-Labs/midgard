import { CML } from "@lucid-evolution/lucid";
import { BuilderInvariantError } from "../core/errors.js";
import type {
  AuthoredOutput,
  PlutusDataLike,
  ScriptRefLike,
} from "../core/output.js";
import { outRefLabel } from "../core/out-ref.js";
import type {
  BuilderScriptState,
  DatumWitness,
  MintIntent,
  MintingPolicy,
  ObserverIntent,
  ObserverValidator,
  ReceiveRedeemerIntent,
  Redeemer,
  ScriptSource,
  SpendInputIntent,
  SpendingValidator,
  TrustedReferenceScriptMetadata,
} from "../core/scripts.js";
import { emptyBuilderScriptState, type MidgardUtxo } from "../core/types.js";
import type { BuilderState } from "./context.js";

const cloneBytesLike = <T extends Uint8Array | string>(value: T): T =>
  (typeof value === "string" ? value : Buffer.from(value)) as T;

export const clonePlutusDataLike = (data: PlutusDataLike): PlutusDataLike => {
  if (typeof data === "string" || data instanceof Uint8Array) {
    return cloneBytesLike(data);
  }
  return Buffer.from(data.to_cbor_bytes());
};

const cloneScriptRefLike = (scriptRef: ScriptRefLike): ScriptRefLike => {
  if (typeof scriptRef === "string" || scriptRef instanceof Uint8Array) {
    return cloneBytesLike(scriptRef);
  }
  if (!(scriptRef instanceof CML.Script)) {
    return { ...scriptRef };
  }
  return CML.Script.from_cbor_bytes(scriptRef.to_cbor_bytes());
};

export const validatorScriptSource = (
  validator: SpendingValidator | MintingPolicy | ObserverValidator,
  fieldName: string,
): ScriptSource => {
  if (typeof validator !== "object" || validator === null) {
    throw new BuilderInvariantError(`${fieldName} must be an object`);
  }
  const candidate = validator as {
    readonly language?: unknown;
    readonly script?: unknown;
  };
  if (candidate.language === "PlutusV3") {
    const script = candidate.script;
    if (
      !(
        typeof script === "string" ||
        script instanceof Uint8Array ||
        (typeof script === "object" &&
          script !== null &&
          "to_cbor_bytes" in script)
      )
    ) {
      throw new BuilderInvariantError(
        `${fieldName}.script is not PlutusV3 script bytes`,
      );
    }
    if (script instanceof CML.Script && script.as_plutus_v3() === undefined) {
      throw new BuilderInvariantError(
        `${fieldName}.script is not a PlutusV3 script`,
      );
    }
    return {
      kind: "plutus-v3",
      language: "PlutusV3",
      script: cloneScriptRefLike(script as ScriptRefLike),
    };
  }
  if (candidate.language === "MidgardV1") {
    const script = candidate.script;
    if (!(typeof script === "string" || script instanceof Uint8Array)) {
      throw new BuilderInvariantError(
        `${fieldName}.script is not MidgardV1 bytes`,
      );
    }
    return {
      kind: "midgard-v1",
      language: "MidgardV1",
      script: cloneBytesLike(script),
    };
  }
  throw new BuilderInvariantError(
    `${fieldName} language must be PlutusV3 or MidgardV1`,
    String(candidate.language),
  );
};

export const cloneRedeemer = (redeemer: Redeemer): Redeemer => ({
  data: clonePlutusDataLike(redeemer.data),
  exUnits:
    redeemer.exUnits === undefined
      ? undefined
      : {
          mem: redeemer.exUnits.mem,
          steps: redeemer.exUnits.steps,
        },
});

export const cloneScriptSource = (script: ScriptSource): ScriptSource => {
  switch (script.kind) {
    case "native":
      return {
        ...script,
        script:
          typeof script.script === "string" ||
          script.script instanceof Uint8Array
            ? cloneBytesLike(script.script)
            : Buffer.from(script.script.to_cbor_bytes()),
      };
    case "plutus-v3":
      return {
        ...script,
        script: cloneScriptRefLike(script.script),
      };
    case "midgard-v1":
    case "dual-plutus-v3-midgard-v1":
      return {
        ...script,
        script: cloneBytesLike(script.script),
      };
  }
};

const cloneDatumWitness = (datum: DatumWitness): DatumWitness => ({
  data: clonePlutusDataLike(datum.data),
  hash: datum.hash,
});

const cloneSpendIntent = (intent: SpendInputIntent): SpendInputIntent => ({
  txHash: intent.txHash,
  outputIndex: intent.outputIndex,
  redeemer:
    intent.redeemer === undefined ? undefined : cloneRedeemer(intent.redeemer),
});

const cloneMintIntent = (intent: MintIntent): MintIntent => ({
  policyId: intent.policyId,
  assets: { ...intent.assets },
  redeemer:
    intent.redeemer === undefined ? undefined : cloneRedeemer(intent.redeemer),
});

const cloneObserverIntent = (intent: ObserverIntent): ObserverIntent => ({
  scriptHash: intent.scriptHash,
  redeemer:
    intent.redeemer === undefined ? undefined : cloneRedeemer(intent.redeemer),
});

const cloneReceiveRedeemerIntent = (
  intent: ReceiveRedeemerIntent,
): ReceiveRedeemerIntent => ({
  scriptHash: intent.scriptHash,
  redeemer: cloneRedeemer(intent.redeemer),
});

const cloneTrustedReferenceScriptMetadata = (
  metadata: TrustedReferenceScriptMetadata,
): TrustedReferenceScriptMetadata => ({
  txHash: metadata.txHash,
  outputIndex: metadata.outputIndex,
  language: metadata.language,
  scriptHash: metadata.scriptHash,
  scriptCborHash: metadata.scriptCborHash,
});

export const cloneScripts = (
  scripts: BuilderScriptState,
): BuilderScriptState => ({
  spendRedeemers: scripts.spendRedeemers.map(cloneSpendIntent),
  referenceScriptMetadata: scripts.referenceScriptMetadata.map(
    cloneTrustedReferenceScriptMetadata,
  ),
  scripts: scripts.scripts.map(cloneScriptSource),
  datumWitnesses: scripts.datumWitnesses.map(cloneDatumWitness),
  mints: scripts.mints.map(cloneMintIntent),
  observers: scripts.observers.map(cloneObserverIntent),
  receiveRedeemers: scripts.receiveRedeemers.map(cloneReceiveRedeemerIntent),
});

const cloneOutputDatum = (
  datum: AuthoredOutput["datum"],
): AuthoredOutput["datum"] => {
  if (datum === undefined || datum.kind === "none") {
    return datum;
  }
  if (datum.kind === "hash") {
    return { ...datum };
  }
  return { kind: "inline", data: clonePlutusDataLike(datum.data) };
};

export const cloneOutput = (output: AuthoredOutput): AuthoredOutput => ({
  ...output,
  assets: { ...output.assets },
  datum: cloneOutputDatum(output.datum),
  scriptRef:
    output.scriptRef === undefined
      ? undefined
      : cloneScriptRefLike(output.scriptRef),
});

export const cloneUtxo = (utxo: MidgardUtxo): MidgardUtxo => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
  output: {
    address: utxo.output.address,
    assets: { ...utxo.output.assets },
    datum:
      utxo.output.datum === undefined || utxo.output.datum === null
        ? utxo.output.datum
        : { ...utxo.output.datum },
    scriptRef:
      utxo.output.scriptRef === undefined || utxo.output.scriptRef === null
        ? utxo.output.scriptRef
        : { ...utxo.output.scriptRef },
  },
  cbor:
    utxo.cbor === undefined
      ? undefined
      : {
          outRef:
            utxo.cbor.outRef === undefined
              ? undefined
              : Buffer.from(utxo.cbor.outRef),
          output:
            utxo.cbor.output === undefined
              ? undefined
              : Buffer.from(utxo.cbor.output),
        },
});

export const emptyState = (networkId?: bigint): BuilderState => ({
  spendInputs: [],
  referenceInputs: [],
  outputs: [],
  requiredSigners: [],
  minimumFee: undefined,
  networkId,
  scripts: emptyBuilderScriptState(),
});

export const cloneState = (state: BuilderState): BuilderState => ({
  spendInputs: state.spendInputs.map(cloneUtxo),
  referenceInputs: state.referenceInputs.map(cloneUtxo),
  outputs: state.outputs.map(cloneOutput),
  requiredSigners: [...state.requiredSigners],
  validityIntervalStart: state.validityIntervalStart,
  validityIntervalEnd: state.validityIntervalEnd,
  minimumFee: state.minimumFee,
  networkId: state.networkId,
  scripts: cloneScripts(state.scripts),
  composition:
    state.composition === undefined ? undefined : { ...state.composition },
});

export const assertUniqueUtxos = (
  spendInputs: readonly MidgardUtxo[],
  referenceInputs: readonly MidgardUtxo[],
): void => {
  const spend = new Set<string>();
  for (const input of spendInputs) {
    const label = outRefLabel(input);
    if (spend.has(label)) {
      throw new BuilderInvariantError("Duplicate spend input", label);
    }
    spend.add(label);
  }

  const refs = new Set<string>();
  for (const input of referenceInputs) {
    const label = outRefLabel(input);
    if (refs.has(label)) {
      throw new BuilderInvariantError("Duplicate reference input", label);
    }
    if (spend.has(label)) {
      throw new BuilderInvariantError(
        "Input cannot be both spend and reference",
        label,
      );
    }
    refs.add(label);
  }
};

export const assertNoDuplicateStrings = (
  values: readonly string[],
  message: string,
): void => {
  const seen = new Set<string>();
  for (const value of values) {
    if (seen.has(value)) {
      throw new BuilderInvariantError(message, value);
    }
    seen.add(value);
  }
};
