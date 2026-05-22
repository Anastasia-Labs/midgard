import type {
  ScriptLanguageName,
  ScriptLanguageTag,
} from "@al-ft/midgard-core/codec";

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

export type MidgardProtocolInfo = {
  readonly apiVersion: number;
  readonly network: string;
  readonly midgardNativeTxVersion: number;
  readonly currentSlot: bigint;
  readonly supportedScriptLanguages: readonly ProtocolScriptLanguage[];
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

export type ProviderDiagnostics = {
  readonly endpoint: string;
  readonly protocolInfoSource: "node" | "fallback" | "unknown";
  readonly protocolInfoFallbackReason?: string;
};

export type ProtocolInfoFallback = {
  readonly protocolInfo: MidgardProtocolInfo;
  readonly reason: string;
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
  submitTx(txCanonicalCborHex: string): Promise<SubmitTxResult>;
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
  readonly protocolInfoFallback?: ProtocolInfoFallback;
};
