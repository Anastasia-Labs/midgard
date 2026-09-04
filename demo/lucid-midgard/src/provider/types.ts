import type { MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core/cek-proof";
import type {
  ScriptLanguageName,
  ScriptLanguageTag,
} from "@al-ft/midgard-core/codec";
import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import type {
  Address,
  MidgardProtocolParameters,
  MidgardUtxo,
  OutRef,
  SubmitTxResult,
  TxHash,
  TxStatus,
} from "../core/index.js";

export type ProtocolScriptLanguage = {
  readonly name: ScriptLanguageName;
  readonly tag: ScriptLanguageTag;
};

type MidgardProtocolInfoCommon = {
  readonly network: string;
  readonly midgardNativeTxVersion: number;
  readonly currentSlot: bigint;
  readonly supportedScriptLanguages: readonly ProtocolScriptLanguage[];
  readonly codecSupportedScriptLanguages: readonly ProtocolScriptLanguage[];
  readonly protocolFeeParameters: {
    readonly minFeeA: bigint;
    readonly minFeeB: bigint;
  };
  readonly submissionLimits: {
    readonly maxSubmitTxCborBytes: number;
  };
  readonly validation: {
    readonly strictnessProfile: string;
    readonly localValidationIsAuthoritative: false;
  };
};

export type MidgardProtocolInfo = MidgardProtocolInfoCommon & {
  readonly apiVersion: 1;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly deploymentMarker?: DeploymentMarker;
};

export type ProviderDiagnostics = {
  readonly endpoint: string;
  readonly protocolInfoSource: "node" | "offline" | "unknown";
};

export type MidgardProvider = {
  getUtxos(address: Address): Promise<readonly MidgardUtxo[]>;
  getUtxoByOutRef(outRef: OutRef): Promise<MidgardUtxo | undefined>;
  getUtxosByOutRefs?(
    outRefs: readonly OutRef[],
  ): Promise<readonly MidgardUtxo[]>;
  getUtxosByUnit?(unit: string): Promise<readonly MidgardUtxo[]>;
  getProtocolInfo(): Promise<MidgardProtocolInfo>;
  getProtocolParameters(): Promise<MidgardProtocolParameters>;
  getCurrentSlot(): Promise<bigint>;
  submitTx(
    txCanonicalCborHex: string,
    programMaterial?: readonly MidgardCekProgramMaterialEntry[],
  ): Promise<SubmitTxResult>;
  getTxStatus(txId: TxHash): Promise<TxStatus>;
  diagnostics(): ProviderDiagnostics;
};

export type MidgardFetch = (
  input: string | URL,
  init?: RequestInit,
) => Promise<Response>;

export type MidgardNodeProviderOptions = {
  readonly endpoint: string;
  readonly fetch?: MidgardFetch;
};
