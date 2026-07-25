import type { MidgardCekProgramMaterialEntryV1 } from "@al-ft/midgard-core/cek-proof";
import type { MidgardConsensusProfileV1 } from "@al-ft/midgard-core/consensus-profile-v1";

import type { Assets } from "../core/assets.js";
import {
  outputAddressPaymentKeyHash,
  outputAddressProtected,
  utxoAddress,
} from "../core/output.js";
import type {
  Address,
  LocalValidationReport,
  MidgardUtxo,
  WalletInputSource,
} from "../core/types.js";
import type { MidgardProvider, ProviderDiagnostics } from "../provider.js";
import type { MidgardWallet } from "../wallet.js";
import {
  type BuilderState,
  cloneProviderDiagnostics,
  type ProviderSnapshot,
} from "./context.js";

export type CompleteTxMetadata = {
  readonly fee: bigint;
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly outputCount: number;
  readonly requiredSignerCount: number;
  readonly txByteLength: number;
  readonly feeIterations: number;
  readonly balanced: boolean;
  readonly changeAddress?: Address;
  readonly changeAssets?: Assets;
  readonly changeOutputIndex?: number;
  readonly expectedAddrWitnessCount?: number;
  readonly expectedAddrWitnessKeyHashes?: readonly string[];
  readonly expectedAddrWitnessesComplete?: boolean;
  readonly estimatedSignedTxByteLength?: number;
  readonly addrWitnessCount?: number;
  readonly signedBy?: readonly string[];
  readonly localValidation?: LocalValidationReport;
  readonly providerGeneration?: number;
  readonly providerDiagnostics?: ProviderDiagnostics;
  readonly walletInputSource?: WalletInputSource;
  readonly walletInputCount?: number;
  readonly utxoOverrideGeneration?: number;
};

export type CompleteTxContext = {
  readonly provider?: MidgardProvider;
  readonly wallet?: () => MidgardWallet | undefined;
  readonly networkId?: bigint;
  readonly maxSubmitTxCborBytes?: number;
  readonly consensusProfile?: MidgardConsensusProfileV1;
  readonly programMaterial?: readonly MidgardCekProgramMaterialEntryV1[];
};

export const cloneLocalValidationReport = (
  report: LocalValidationReport | undefined,
): LocalValidationReport | undefined =>
  report === undefined
    ? undefined
    : {
        ...report,
        acceptedTxIds: [...report.acceptedTxIds],
        rejected: report.rejected.map((entry) => ({ ...entry })),
        statePatch:
          report.statePatch === undefined
            ? undefined
            : {
                deletedOutRefs: [...report.statePatch.deletedOutRefs],
                upsertedOutRefs: report.statePatch.upsertedOutRefs.map(
                  ([outRef, output]) => [outRef, output] as const,
                ),
              },
      };

export const cloneCompleteTxMetadata = (
  metadata: CompleteTxMetadata,
): CompleteTxMetadata => ({
  ...metadata,
  changeAssets:
    metadata.changeAssets === undefined
      ? undefined
      : { ...metadata.changeAssets },
  expectedAddrWitnessKeyHashes:
    metadata.expectedAddrWitnessKeyHashes === undefined
      ? undefined
      : [...metadata.expectedAddrWitnessKeyHashes],
  signedBy:
    metadata.signedBy === undefined ? undefined : [...metadata.signedBy],
  localValidation: cloneLocalValidationReport(metadata.localValidation),
  providerDiagnostics:
    metadata.providerDiagnostics === undefined
      ? undefined
      : cloneProviderDiagnostics(metadata.providerDiagnostics),
});

export const attachProviderMetadata = (
  metadata: Omit<CompleteTxMetadata, "localValidation">,
  provider: Pick<ProviderSnapshot, "diagnostics" | "generation">,
): CompleteTxMetadata => ({
  ...metadata,
  providerGeneration: provider.generation,
  providerDiagnostics: cloneProviderDiagnostics(provider.diagnostics),
});

export const paymentPubKeyHashFromUtxo = (
  utxo: MidgardUtxo,
): string | undefined => outputAddressPaymentKeyHash(utxoAddress(utxo));

export const expectedAddrWitnessKeyHashes = (
  state: BuilderState,
): readonly string[] => {
  const keyHashes = new Set(state.requiredSigners);
  for (const input of state.spendInputs) {
    const keyHash = paymentPubKeyHashFromUtxo(input);
    if (keyHash !== undefined) {
      keyHashes.add(keyHash);
    }
  }
  for (const output of state.outputs) {
    if (!outputAddressProtected(output.address)) {
      continue;
    }
    const keyHash = outputAddressPaymentKeyHash(output.address);
    if (keyHash !== undefined) {
      keyHashes.add(keyHash);
    }
  }
  return [...keyHashes].sort();
};
