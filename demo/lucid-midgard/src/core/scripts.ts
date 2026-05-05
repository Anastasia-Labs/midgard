import type { CML } from "@lucid-evolution/lucid";
import type { ScriptLanguageName } from "@al-ft/midgard-core/codec";
import type { AssetUnit, Assets } from "./assets.js";
import type { OutRef } from "./out-ref.js";
import type { PlutusDataLike, ScriptRefLike } from "./output.js";

export type ScriptLanguage = "NativeCardano" | ScriptLanguageName;
export type ScriptPurpose = "spend" | "mint" | "observe" | "receive";
export type SupportedValidatorLanguage = "PlutusV3" | "MidgardV1";

export type PlutusV3Validator = {
  readonly language: "PlutusV3";
  readonly script: ScriptRefLike;
};

export type MidgardV1Validator = {
  readonly language: "MidgardV1";
  readonly script: Uint8Array | string;
};

export type SupportedValidator = PlutusV3Validator | MidgardV1Validator;
export type SpendingValidator = SupportedValidator;
export type MintingPolicy = SupportedValidator;
export type ObserverValidator = SupportedValidator;

export type TrustedReferenceScriptMetadata = OutRef & {
  readonly language: ScriptLanguage;
  readonly scriptHash: string;
  readonly scriptCborHash?: string;
};

export type ScriptSource =
  | {
      readonly kind: "native";
      readonly language: "NativeCardano";
      readonly script: InstanceType<typeof CML.NativeScript> | Uint8Array | string;
    }
  | {
      readonly kind: "plutus-v3";
      readonly language: "PlutusV3";
      readonly script: ScriptRefLike;
    }
  | {
      readonly kind: "midgard-v1";
      readonly language: "MidgardV1";
      readonly script: Uint8Array | string;
    }
  | {
      readonly kind: "dual-plutus-v3-midgard-v1";
      readonly languages: readonly ["PlutusV3", "MidgardV1"];
      readonly script: Uint8Array | string;
    };

export type ExUnits = {
  readonly mem: bigint;
  readonly steps: bigint;
};

export type Redeemer = {
  readonly data: PlutusDataLike;
  readonly exUnits?: ExUnits;
};

export type DatumWitness = {
  readonly data: PlutusDataLike;
  readonly hash?: string;
};

export type SpendInputIntent = OutRef & {
  readonly redeemer?: Redeemer;
};

export type ReferenceInputIntent = OutRef & {
  readonly script?: ScriptSource;
};

export type MintIntent = {
  readonly policyId: string;
  readonly assets: Readonly<Record<AssetUnit, bigint>>;
  readonly redeemer?: Redeemer;
};

export type ObserverIntent = {
  readonly scriptHash: string;
  readonly redeemer?: Redeemer;
};

export type ReceiveRedeemerIntent = {
  readonly scriptHash: string;
  readonly redeemer: Redeemer;
};

export type BuilderScriptState = {
  readonly spendRedeemers: readonly SpendInputIntent[];
  readonly referenceScriptMetadata: readonly TrustedReferenceScriptMetadata[];
  readonly scripts: readonly ScriptSource[];
  readonly datumWitnesses: readonly DatumWitness[];
  readonly mints: readonly MintIntent[];
  readonly observers: readonly ObserverIntent[];
  readonly receiveRedeemers: readonly ReceiveRedeemerIntent[];
};

export type MintAssets = Readonly<Record<string, Assets>>;
