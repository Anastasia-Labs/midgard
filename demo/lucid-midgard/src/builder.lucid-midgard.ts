import { type MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core/cek-proof";
import { type Network } from "@lucid-evolution/lucid";

import {
  CompleteTx,
  makeCompleteTx,
  makePartiallySignedTx,
  PartiallySignedTx,
} from "./builder.complete-tx.js";
import {
  type DatumOfOptions,
  type FromTxInput,
  normalizeUtxo,
  normalizeWalletInputUtxos,
  referenceOutputMapsEqual,
  type UtxosByOutRefOptions,
} from "./builder.compose-states.js";
import {
  assertTxNetworkMatchesExpected,
  type AwaitTxOptions,
  type MidgardEffect,
  midgardProgram,
  midgardSafe,
  trustedCompleteTxs,
} from "./builder.midgard-json-safe.js";
import {
  cloneUtxoOverrideSnapshot,
  inlineDatumFromUtxo,
  networkForSeedWallet,
  normalizeAddressQuery,
  normalizeAssetUnit,
  normalizeLucidMidgardConfig,
  normalizeProviderUtxos,
  orderedUtxosByOutRef,
  readOnlyWalletFromAddress,
  validateSelectedWalletForConfig,
} from "./builder.ordered-utxos-by-out-ref.js";
import { makeTxBuilder, TxBuilder } from "./builder.tx-builder.js";
import type {
  LucidMidgardConfig,
  LucidMidgardConfigSnapshot,
  ProviderSnapshot,
  SwitchProviderOptions,
  UtxoOverrideSnapshot,
} from "./builder/context.js";
import {
  buildConfigSnapshot,
  cloneProtocolInfo,
  cloneProviderDiagnostics,
  configNetworkId,
  readProviderSnapshot,
} from "./builder/context.js";
import {
  decodeFromTxInput,
  type FromTxOptions,
  importedTxMetadata,
  resolveImportedReferenceInputs,
} from "./builder/imported-tx.js";
import {
  attachProviderMetadata,
  type CompleteTxContext,
} from "./builder/metadata.js";
import { mergeCanonicalProofProgramMaterial } from "./builder/script-materialization.js";
import { cloneUtxo, emptyState } from "./builder/state.js";
import { assertTxStatusMatches, pollTxStatus } from "./builder/status.js";
import { assetQuantity, type AssetUnit } from "./core/assets.js";
import {
  BuilderInvariantError,
  LucidMidgardError,
  ProviderCapabilityError,
  ProviderPayloadError,
} from "./core/errors.js";
import {
  compareOutRefs,
  normalizeTxHash,
  type OutRef,
  outRefLabel,
} from "./core/out-ref.js";
import { utxoAddress, utxoAssets as utxoOutputAssets } from "./core/output.js";
import {
  type Address,
  type MidgardResult,
  type MidgardUtxo,
  type TxStatus,
} from "./core/types.js";
import type { MidgardProvider } from "./provider.js";
import {
  type ExternalBodyHashSigner,
  type MidgardWallet,
  type PrivateKey,
  walletFromExternalSigner,
  walletFromPrivateKey,
  walletFromSeedPhrase,
} from "./wallet.js";

export class LucidMidgard {
  selectedWallet: MidgardWallet | undefined;
  #providerSnapshot: ProviderSnapshot;
  #config: LucidMidgardConfigSnapshot;
  #baseConfig: LucidMidgardConfig;
  #utxoOverrideGeneration = 0;
  #utxoOverrides: UtxoOverrideSnapshot | undefined;

  readonly selectWallet = {
    fromSeed: (seedPhrase: string): LucidMidgard => {
      this.selectedWallet = walletFromSeedPhrase(seedPhrase, {
        network: networkForSeedWallet(this.#config.network),
        expectedNetworkId: this.#config.networkId,
      });
      return this;
    },
    fromPrivateKey: (
      privateKey: PrivateKey | string,
      address: Address,
    ): LucidMidgard => {
      this.selectedWallet = walletFromPrivateKey(privateKey, address, {
        expectedNetworkId: this.#config.networkId,
      });
      return this;
    },
    fromExternalSigner: (signer: ExternalBodyHashSigner): LucidMidgard => {
      this.selectedWallet = walletFromExternalSigner(signer, {
        expectedNetworkId: this.#config.networkId,
      });
      return this;
    },
    fromAddress: (
      address: Address,
      utxos?: readonly MidgardUtxo[],
    ): LucidMidgard => {
      const wallet = readOnlyWalletFromAddress(address, this.#config.networkId);
      let normalizedOverrides: readonly MidgardUtxo[] | undefined;
      if (utxos !== undefined) {
        const normalized = normalizeWalletInputUtxos(
          utxos,
          "instance-override",
          this.#config.networkId,
        );
        for (const utxo of normalized) {
          if (utxoAddress(utxo) !== address) {
            throw new BuilderInvariantError(
              "fromAddress UTxO does not belong to the selected address",
              outRefLabel(utxo),
            );
          }
        }
        normalizedOverrides = normalized;
      }
      this.selectedWallet = wallet;
      if (normalizedOverrides !== undefined) {
        this.setUtxoOverrides(normalizedOverrides);
      }
      return this;
    },
  };

  private constructor({
    providerSnapshot,
    configSnapshot,
    baseConfig,
  }: {
    readonly providerSnapshot: ProviderSnapshot;
    readonly configSnapshot: LucidMidgardConfigSnapshot;
    readonly baseConfig: LucidMidgardConfig;
  }) {
    this.#providerSnapshot = providerSnapshot;
    this.#config = configSnapshot;
    this.#baseConfig = baseConfig;
  }

  static async new(
    provider: MidgardProvider,
    networkOrConfig?: Network | LucidMidgardConfig,
  ): Promise<LucidMidgard> {
    const baseConfig = normalizeLucidMidgardConfig(networkOrConfig);
    const { snapshot, config } = await readProviderSnapshot({
      provider,
      generation: 0,
      config: baseConfig,
    });
    return new LucidMidgard({
      providerSnapshot: snapshot,
      configSnapshot: config,
      baseConfig,
    });
  }

  get provider(): MidgardProvider {
    return this.#providerSnapshot.provider;
  }

  config(): LucidMidgardConfigSnapshot {
    return this.#config;
  }

  private refreshConfigSnapshot(): void {
    this.#config = buildConfigSnapshot({
      input: this.#baseConfig,
      protocolInfo: this.#providerSnapshot.protocolInfo,
      diagnostics: this.#providerSnapshot.diagnostics,
      providerGeneration: this.#providerSnapshot.generation,
      utxoOverrideGeneration: this.#utxoOverrideGeneration,
      hasUtxoOverrides: this.#utxoOverrides !== undefined,
    });
  }

  private setUtxoOverrides(utxos: readonly MidgardUtxo[]): void {
    this.#utxoOverrideGeneration += 1;
    this.#utxoOverrides = {
      generation: this.#utxoOverrideGeneration,
      utxos: utxos.map(cloneUtxo),
    };
    this.refreshConfigSnapshot();
  }

  async switchProvider(
    provider: MidgardProvider,
    options: SwitchProviderOptions = {},
  ): Promise<LucidMidgard> {
    const nextGeneration = this.#providerSnapshot.generation + 1;
    const { snapshot, config } = await readProviderSnapshot({
      provider,
      generation: nextGeneration,
      config: this.#baseConfig,
      currentConfig: this.#config,
      options,
    });
    await validateSelectedWalletForConfig(this.selectedWallet, config);
    this.#providerSnapshot = snapshot;
    this.#config = buildConfigSnapshot({
      input: this.#baseConfig,
      protocolInfo: snapshot.protocolInfo,
      diagnostics: snapshot.diagnostics,
      providerGeneration: snapshot.generation,
      utxoOverrideGeneration: this.#utxoOverrideGeneration,
      hasUtxoOverrides: this.#utxoOverrides !== undefined,
    });
    return this;
  }

  overrideUTxOs(utxos: readonly MidgardUtxo[]): LucidMidgard {
    this.setUtxoOverrides(
      normalizeWalletInputUtxos(
        utxos,
        "instance-override",
        this.#config.networkId,
      ),
    );
    return this;
  }

  clearUTxOOverrides(): LucidMidgard {
    this.#utxoOverrideGeneration += 1;
    this.#utxoOverrides = undefined;
    this.refreshConfigSnapshot();
    return this;
  }

  wallet(): MidgardWallet {
    if (this.selectedWallet === undefined) {
      throw new BuilderInvariantError("No Midgard wallet selected");
    }
    return this.selectedWallet;
  }

  txStatus(txId: string): Promise<TxStatus> {
    const normalizedTxId = normalizeTxHash(txId);
    return this.provider
      .getTxStatus(normalizedTxId)
      .then((status) => assertTxStatusMatches(normalizedTxId, status));
  }

  txStatusProgram(txId: string): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.txStatus(txId));
  }

  txStatusSafe(
    txId: string,
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.txStatus(txId));
  }

  currentSlot(): Promise<bigint> {
    return this.provider.getCurrentSlot();
  }

  async utxosAt(address: Address): Promise<readonly MidgardUtxo[]> {
    const endpoint = "/utxos";
    const normalizedAddress = normalizeAddressQuery(
      address,
      this.#config.networkId,
      endpoint,
    );
    const utxos = normalizeProviderUtxos(
      await this.provider.getUtxos(normalizedAddress),
      endpoint,
    );
    for (const utxo of utxos) {
      if (utxoAddress(utxo) !== normalizedAddress) {
        throw new ProviderPayloadError(
          endpoint,
          "Provider returned a UTxO for a different address",
          `expected=${normalizedAddress} actual=${utxoAddress(utxo)}`,
        );
      }
    }
    return [...utxos].sort(compareOutRefs).map(cloneUtxo);
  }

  async utxosAtWithUnit(
    address: Address,
    unit: AssetUnit,
  ): Promise<readonly MidgardUtxo[]> {
    const normalizedUnit = normalizeAssetUnit(unit);
    return (await this.utxosAt(address)).filter(
      (utxo) => assetQuantity(utxoOutputAssets(utxo), normalizedUnit) > 0n,
    );
  }

  utxosByOutRef(
    outRefs: readonly OutRef[],
    options?: UtxosByOutRefOptions,
  ): Promise<readonly MidgardUtxo[]> {
    return orderedUtxosByOutRef(this.provider, outRefs, options);
  }

  async utxoByUnit(unit: AssetUnit): Promise<MidgardUtxo> {
    const endpoint = "/utxo-by-unit";
    const normalizedUnit = normalizeAssetUnit(unit);
    if (this.provider.getUtxosByUnit === undefined) {
      throw new ProviderCapabilityError(
        endpoint,
        "Midgard provider does not expose a native unit index",
      );
    }
    const hits = [
      ...normalizeProviderUtxos(
        await this.provider.getUtxosByUnit(normalizedUnit),
        endpoint,
      ),
    ].sort(compareOutRefs);
    for (const hit of hits) {
      if (assetQuantity(utxoOutputAssets(hit), normalizedUnit) <= 0n) {
        throw new ProviderPayloadError(
          endpoint,
          "Provider unit index returned a UTxO without the requested unit",
          outRefLabel(hit),
        );
      }
    }
    if (hits.length === 0) {
      throw new ProviderPayloadError(
        endpoint,
        "No UTxO found for requested unit",
        normalizedUnit,
      );
    }
    if (hits.length > 1) {
      throw new ProviderPayloadError(
        endpoint,
        "Multiple UTxOs found for requested unit",
        normalizedUnit,
      );
    }
    return cloneUtxo(hits[0]!);
  }

  async datumOf(utxo: MidgardUtxo, options?: DatumOfOptions): Promise<Buffer> {
    return inlineDatumFromUtxo(utxo, options);
  }

  awaitTx(txId: string, options: AwaitTxOptions = {}): Promise<TxStatus> {
    return pollTxStatus(
      options.provider ?? this.provider,
      normalizeTxHash(txId),
      options,
    );
  }

  awaitTxProgram(
    txId: string,
    options: AwaitTxOptions = {},
  ): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.awaitTx(txId, options));
  }

  awaitTxSafe(
    txId: string,
    options: AwaitTxOptions = {},
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.awaitTx(txId, options));
  }

  private completeTxContext(
    programMaterial: readonly MidgardCekProgramMaterialEntry[] = [],
    resolvedReferenceOutputsByOutRef: ReadonlyMap<
      string,
      Uint8Array
    > = new Map(),
  ): CompleteTxContext {
    return {
      provider: this.#providerSnapshot.provider,
      wallet: () => this.selectedWallet,
      networkId: configNetworkId(this.#config),
      maxSubmitTxCborBytes: this.#config.submissionLimits.maxSubmitTxCborBytes,
      consensusProfile: this.#config.consensusProfile,
      programMaterial,
      resolvedReferenceOutputsByOutRef,
    };
  }

  newTx(): TxBuilder {
    const providerSnapshot = {
      ...this.#providerSnapshot,
      protocolInfo: cloneProtocolInfo(this.#providerSnapshot.protocolInfo),
      diagnostics: cloneProviderDiagnostics(this.#providerSnapshot.diagnostics),
    };
    return makeTxBuilder(
      {
        provider: providerSnapshot,
        wallet: this.selectedWallet,
        config: this.#config,
        utxoOverrides: cloneUtxoOverrideSnapshot(this.#utxoOverrides),
      },
      emptyState(configNetworkId(this.#config)),
    );
  }

  fromTx(
    input: FromTxInput,
    options: FromTxOptions & { readonly partial: true },
  ): PartiallySignedTx;
  fromTx(input: FromTxInput, options?: FromTxOptions): CompleteTx;
  fromTx(
    input: FromTxInput,
    options: FromTxOptions = {},
  ): CompleteTx | PartiallySignedTx {
    const expectedBodyNetworkId = configNetworkId(this.#config);
    const tx =
      input instanceof CompleteTx ? input.tx : decodeFromTxInput(input);
    const carriedReferenceOutputsByOutRef =
      input instanceof CompleteTx && trustedCompleteTxs.has(input)
        ? input.resolvedReferenceOutputsByOutRef
        : undefined;
    const explicitReferenceInputs =
      options.resolvedReferenceInputs === undefined
        ? undefined
        : resolveImportedReferenceInputs(
            tx,
            options,
            normalizeUtxo,
            this.#config.networkId,
          );
    if (
      explicitReferenceInputs !== undefined &&
      carriedReferenceOutputsByOutRef !== undefined &&
      !referenceOutputMapsEqual(
        explicitReferenceInputs.outputsByOutRef,
        carriedReferenceOutputsByOutRef,
      )
    ) {
      throw new BuilderInvariantError(
        "Conflicting resolved reference inputs",
        "explicit resolution does not match carried canonical reference outputs",
      );
    }
    const resolvedReferenceOutputsByOutRef =
      explicitReferenceInputs?.outputsByOutRef ??
      carriedReferenceOutputsByOutRef ??
      resolveImportedReferenceInputs(
        tx,
        options,
        normalizeUtxo,
        this.#config.networkId,
      ).outputsByOutRef;
    const programMaterial = mergeCanonicalProofProgramMaterial(
      input instanceof CompleteTx ? input.programMaterial : [],
      options.programMaterial ?? [],
    );
    assertTxNetworkMatchesExpected(
      tx,
      expectedBodyNetworkId,
      "Imported transaction",
    );
    const metadata = attachProviderMetadata(
      importedTxMetadata(tx, options, normalizeUtxo, this.#config.networkId),
      this.#providerSnapshot,
    );
    return options.partial === true
      ? makePartiallySignedTx(
          tx,
          metadata,
          this.completeTxContext(
            programMaterial,
            resolvedReferenceOutputsByOutRef,
          ),
        )
      : makeCompleteTx(
          tx,
          metadata,
          this.completeTxContext(
            programMaterial,
            resolvedReferenceOutputsByOutRef,
          ),
        );
  }

  async walletAddress(): Promise<Address> {
    return this.wallet().address();
  }
}
