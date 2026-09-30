import { type MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { isMidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import { CML } from "@lucid-evolution/lucid";

import {
  CompleteTx,
  makeCompleteTx,
  runSharedLocalPreflight,
} from "./builder.complete-tx.js";
import {
  assertValidityInterval,
  type ChainResult,
  composeStates,
  normalizeSignerKeyHash,
  normalizeUtxo,
  normalizeWalletInputUtxos,
  type ReadFromOptions,
} from "./builder.compose-states.js";
import {
  type MidgardEffect,
  midgardProgram,
  midgardSafe,
  txBuilderConstructorToken,
} from "./builder.midgard-json-safe.js";
import {
  assertAcceptedLocalValidation,
  assertWalletOwnsInputs,
  normalizeFeePolicy,
  normalizeTrustedReferenceScriptMetadataList,
  rejectRuntimeKindOption,
  resolveInitialFee,
  resolveMaxFeeIterations,
  shouldBalanceWithWalletDefault,
  validationLevel,
} from "./builder.normalize-trusted-reference-script-metadata.js";
import {
  assertBalancedWithoutChange,
  assertNoPresetInputOverlap,
  type BalancedCompletionInputs,
  buildBalancedCompletion,
  type FeePolicy,
  type ResolvedWalletInputs,
  sumAssets,
} from "./builder/balancing.js";
import type {
  BuilderContextSnapshot,
  BuilderState,
  LucidMidgardConfigSnapshot,
} from "./builder/context.js";
import {
  assertBuilderContextsComposable,
  cloneProtocolInfo,
  configNetworkId,
} from "./builder/context.js";
import {
  nativeInputOutRefs,
  referenceOutputsByOutRef,
} from "./builder/imported-tx.js";
import {
  attachProviderMetadata,
  type CompleteTxContext,
  type CompleteTxMetadata,
  expectedAddrWitnessKeyHashes,
  paymentPubKeyHashFromUtxo,
} from "./builder/metadata.js";
import {
  normalizeHashHex,
  normalizeNonNegativeBigInt,
} from "./builder/normalizers.js";
import {
  deriveScriptMaterialization,
  normalizeMintAssetsForNormalizedPolicy,
  normalizePolicyId,
  normalizeScriptHash,
  prepareProofBuilderState,
} from "./builder/script-materialization.js";
import {
  assertNoDuplicateStrings,
  assertUniqueUtxos,
  cloneOutput,
  clonePlutusDataLike,
  cloneRedeemer,
  cloneScripts,
  cloneScriptSource,
  cloneState,
  cloneUtxo,
  validatorScriptSource,
} from "./builder/state.js";
import { buildCanonicalUnsignedTx } from "./builder/unsigned-tx.js";
import { estimatedSignedTxByteLength } from "./builder/witness-bundle.js";
import { type Assets, type ValueLike } from "./core/assets.js";
import { BuilderInvariantError, LucidMidgardError } from "./core/errors.js";
import { compareOutRefs, outRefLabel } from "./core/out-ref.js";
import {
  type AuthoredOutput,
  authoredOutput,
  normalizePlutusData,
  type OutputOptions,
  type PlutusDataLike,
  utxoAssets as utxoOutputAssets,
} from "./core/output.js";
import type {
  MintAssets,
  MintingPolicy,
  ObserverValidator,
  Redeemer,
  ScriptSource,
  SpendingValidator,
  TrustedReferenceScriptMetadata,
} from "./core/scripts.js";
import {
  type Address,
  type BuilderSnapshot,
  type CompleteOptions,
  type LocalValidationReport,
  type MidgardResult,
  type MidgardUtxo,
  type WalletInputSource,
} from "./core/types.js";
import type { MidgardProtocolInfo } from "./provider.js";
import { assertAddressNetwork, paymentKeyHashFromAddress } from "./wallet.js";

export type PayApi = {
  readonly ToAddress: (
    address: Address,
    value: ValueLike,
    options?: Omit<OutputOptions, "kind">,
  ) => TxBuilder;
  readonly ToContract: (
    address: Address,
    datum: PlutusDataLike,
    value: ValueLike,
    options?: Omit<OutputOptions, "datum" | "kind">,
  ) => TxBuilder;
  readonly ToProtectedAddress: (
    address: Address,
    value: ValueLike,
    options?: Omit<OutputOptions, "kind">,
  ) => TxBuilder;
};

export type AttachApi = {
  readonly Script: (source: ScriptSource) => TxBuilder;
  readonly NativeScript: (
    script: CML.NativeScript | Uint8Array | string,
  ) => TxBuilder;
  readonly SpendingValidator: (validator: SpendingValidator) => TxBuilder;
  readonly MintingPolicy: (policy: MintingPolicy) => TxBuilder;
  readonly ObserverValidator: (validator: ObserverValidator) => TxBuilder;
  readonly ReferenceScriptMetadata: (
    metadata:
      | TrustedReferenceScriptMetadata
      | readonly TrustedReferenceScriptMetadata[],
  ) => TxBuilder;
  readonly Datum: (data: PlutusDataLike, hash?: string) => TxBuilder;
};

export class TxBuilder {
  readonly pay: PayApi;
  readonly attach: AttachApi;

  constructor(
    private readonly context: BuilderContextSnapshot,
    private readonly state: BuilderState,
    token?: symbol,
  ) {
    if (token !== txBuilderConstructorToken) {
      throw new BuilderInvariantError(
        "TxBuilder constructor is internal; use LucidMidgard.newTx()",
      );
    }
    this.attach = {
      Script: (source) => {
        return this.next({
          scripts: {
            ...this.state.scripts,
            scripts: [...this.state.scripts.scripts, cloneScriptSource(source)],
          },
        });
      },
      NativeScript: (script) =>
        this.attach.Script({
          kind: "native",
          language: "NativeCardano",
          script,
        }),
      SpendingValidator: (validator) =>
        this.attach.Script(
          validatorScriptSource(validator, "SpendingValidator"),
        ),
      MintingPolicy: (policy) =>
        this.attach.Script(validatorScriptSource(policy, "MintingPolicy")),
      ObserverValidator: (validator) =>
        this.attach.Script(
          validatorScriptSource(validator, "ObserverValidator"),
        ),
      ReferenceScriptMetadata: (metadata) => {
        return this.attachReferenceScriptMetadata(metadata);
      },
      Datum: (data, hash) => this.attachDatum(data, hash),
    };
    this.pay = {
      ToAddress: (address, value, options) => {
        rejectRuntimeKindOption(options, "pay.ToAddress");
        return this.addOutput(
          authoredOutput({
            kind: "ordinary",
            address,
            value,
            datum: options?.datum,
            scriptRef: options?.scriptRef,
          }),
        );
      },
      ToContract: (address, datum, value, options) => {
        rejectRuntimeKindOption(options, "pay.ToContract");
        return this.addOutput(
          authoredOutput({
            kind: "ordinary",
            address,
            value,
            datum,
            scriptRef: options?.scriptRef,
          }),
        );
      },
      ToProtectedAddress: (address, value, options) => {
        rejectRuntimeKindOption(options, "pay.ToProtectedAddress");
        return this.addOutput(
          authoredOutput({
            kind: "protected",
            address,
            value,
            datum: options?.datum,
            scriptRef: options?.scriptRef,
          }),
        );
      },
    };
  }

  private attachDatum(data: PlutusDataLike, hash?: string): TxBuilder {
    const normalizedData = clonePlutusDataLike(data);
    const normalizedHash =
      hash === undefined
        ? CML.hash_plutus_data(normalizePlutusData(normalizedData)).to_hex()
        : normalizeHashHex(hash, "datum hash", 32);
    if (
      this.state.scripts.datumWitnesses.some(
        (datum) => datum.hash === normalizedHash,
      )
    ) {
      throw new BuilderInvariantError(
        "Duplicate datum witness",
        normalizedHash,
      );
    }
    return this.next({
      scripts: {
        ...this.state.scripts,
        datumWitnesses: [
          ...this.state.scripts.datumWitnesses,
          { data: normalizedData, hash: normalizedHash },
        ],
      },
    });
  }

  private attachReferenceScriptMetadata(
    metadata:
      | TrustedReferenceScriptMetadata
      | readonly TrustedReferenceScriptMetadata[],
  ): TxBuilder {
    const nextMetadata = [
      ...this.state.scripts.referenceScriptMetadata,
      ...normalizeTrustedReferenceScriptMetadataList(metadata),
    ];
    assertNoDuplicateStrings(
      nextMetadata.map(outRefLabel),
      "Duplicate trusted reference script metadata",
    );
    return this.next({
      scripts: {
        ...this.state.scripts,
        referenceScriptMetadata: nextMetadata,
      },
    });
  }

  private next(patch: Partial<BuilderState>): TxBuilder {
    const nextState: BuilderState = {
      ...cloneState(this.state),
      ...patch,
      scripts:
        patch.scripts === undefined
          ? cloneScripts(this.state.scripts)
          : cloneScripts(patch.scripts),
    };
    assertUniqueUtxos(nextState.spendInputs, nextState.referenceInputs);
    assertValidityInterval(nextState);
    return makeTxBuilder(this.context, nextState);
  }

  private addOutput(output: AuthoredOutput): TxBuilder {
    assertAddressNetwork(output.address, this.context.config.networkId);
    return this.next({
      outputs: [...this.state.outputs.map(cloneOutput), cloneOutput(output)],
    });
  }

  collectFrom(utxos: readonly MidgardUtxo[], redeemer?: Redeemer): TxBuilder {
    const normalizedUtxos = utxos.map((utxo) => normalizeUtxo(utxo));
    const spendRedeemers =
      redeemer === undefined
        ? this.state.scripts.spendRedeemers
        : [
            ...this.state.scripts.spendRedeemers,
            ...normalizedUtxos.map((utxo) => ({
              txHash: utxo.txHash,
              outputIndex: utxo.outputIndex,
              redeemer: cloneRedeemer(redeemer),
            })),
          ];
    return this.next({
      spendInputs: [...this.state.spendInputs, ...normalizedUtxos],
      scripts: {
        ...this.state.scripts,
        spendRedeemers,
      },
    });
  }

  readFrom(
    utxos: readonly MidgardUtxo[],
    options: ReadFromOptions = {},
  ): TxBuilder {
    const normalizedUtxos = utxos.map((utxo) => normalizeUtxo(utxo));
    const trustedReferenceScripts =
      options.trustedReferenceScripts === undefined
        ? []
        : normalizeTrustedReferenceScriptMetadataList(
            options.trustedReferenceScripts,
          );
    if (trustedReferenceScripts.length > 0) {
      const referenceLabels = new Set(normalizedUtxos.map(outRefLabel));
      for (const metadata of trustedReferenceScripts) {
        const label = outRefLabel(metadata);
        if (!referenceLabels.has(label)) {
          throw new BuilderInvariantError(
            "readFrom trusted reference script metadata must match a supplied reference input",
            label,
          );
        }
      }
    }
    const nextReferenceScriptMetadata = [
      ...this.state.scripts.referenceScriptMetadata,
      ...trustedReferenceScripts,
    ];
    assertNoDuplicateStrings(
      nextReferenceScriptMetadata.map(outRefLabel),
      "Duplicate trusted reference script metadata",
    );
    return this.next({
      referenceInputs: [...this.state.referenceInputs, ...normalizedUtxos],
      scripts: {
        ...this.state.scripts,
        referenceScriptMetadata: nextReferenceScriptMetadata,
      },
    });
  }

  mintAssets(policyId: string, assets: Assets, redeemer?: Redeemer): TxBuilder {
    const normalizedPolicy = normalizePolicyId(policyId);
    return this.next({
      scripts: {
        ...this.state.scripts,
        mints: [
          ...this.state.scripts.mints,
          {
            policyId: normalizedPolicy,
            assets: normalizeMintAssetsForNormalizedPolicy(
              normalizedPolicy,
              assets,
            ),
            redeemer:
              redeemer === undefined ? undefined : cloneRedeemer(redeemer),
          },
        ],
      },
    });
  }

  mint(mints: MintAssets, redeemer?: Redeemer): TxBuilder {
    return Object.entries(mints).reduce<TxBuilder>(
      (builder, [policyId, assets]) =>
        builder.mintAssets(policyId, assets, redeemer),
      this,
    );
  }

  observe(scriptHash: string, redeemer?: Redeemer): TxBuilder {
    const normalized = normalizeScriptHash(scriptHash, "observer script hash");
    if (
      this.state.scripts.observers.some(
        (observer) => observer.scriptHash === normalized,
      )
    ) {
      throw new BuilderInvariantError("Duplicate observer intent", normalized);
    }
    return this.next({
      scripts: {
        ...this.state.scripts,
        observers: [
          ...this.state.scripts.observers,
          {
            scriptHash: normalized,
            redeemer:
              redeemer === undefined ? undefined : cloneRedeemer(redeemer),
          },
        ],
      },
    });
  }

  receiveRedeemer(scriptHash: string, redeemer: Redeemer): TxBuilder {
    const normalized = normalizeScriptHash(scriptHash, "receive script hash");
    if (
      this.state.scripts.receiveRedeemers.some(
        (entry) => entry.scriptHash === normalized,
      )
    ) {
      throw new BuilderInvariantError("Duplicate receive redeemer", normalized);
    }
    return this.next({
      scripts: {
        ...this.state.scripts,
        receiveRedeemers: [
          ...this.state.scripts.receiveRedeemers,
          {
            scriptHash: normalized,
            redeemer: cloneRedeemer(redeemer),
          },
        ],
      },
    });
  }

  addSigner(keyHashOrAddress: string): TxBuilder {
    const keyHash = normalizeSignerKeyHash(keyHashOrAddress);
    if (keyHash !== undefined) {
      return this.addNormalizedSigner(keyHash);
    }
    assertAddressNetwork(keyHashOrAddress, this.context.config.networkId);
    return this.addNormalizedSigner(
      paymentKeyHashFromAddress(keyHashOrAddress.trim()),
    );
  }

  private addNormalizedSigner(signer: string): TxBuilder {
    const requiredSigners = [...this.state.requiredSigners, signer];
    if (new Set(requiredSigners).size !== requiredSigners.length) {
      throw new BuilderInvariantError("Duplicate required signer", signer);
    }
    return this.next({ requiredSigners });
  }

  addSignerKey(keyHash: string): TxBuilder {
    return this.addNormalizedSigner(
      normalizeHashHex(keyHash, "required signer key hash", 28),
    );
  }

  setMinFee(fee: bigint | number): TxBuilder {
    return this.next({
      minimumFee: normalizeNonNegativeBigInt(fee, "minimumFee"),
    });
  }

  validFrom(slotOrPosix: bigint | number): TxBuilder {
    return this.next({
      validityIntervalStart: normalizeNonNegativeBigInt(
        slotOrPosix,
        "validityIntervalStart",
      ),
    });
  }

  validTo(slotOrPosix: bigint | number): TxBuilder {
    return this.next({
      validityIntervalEnd: normalizeNonNegativeBigInt(
        slotOrPosix,
        "validityIntervalEnd",
      ),
    });
  }

  debugSnapshot(): BuilderSnapshot {
    return {
      ...cloneState(this.state),
      providerGeneration: this.context.provider.generation,
      utxoOverrideGeneration: this.context.utxoOverrides?.generation,
      hasUtxoOverrides: this.context.utxoOverrides !== undefined,
    };
  }

  snapshot(): BuilderSnapshot {
    return this.debugSnapshot();
  }

  config(): LucidMidgardConfigSnapshot {
    return this.context.config;
  }

  rawConfig(): MidgardProtocolInfo {
    return cloneProtocolInfo(this.context.provider.protocolInfo);
  }

  compose(other: TxBuilder, ...others: readonly TxBuilder[]): TxBuilder {
    const fragments = [other, ...others];
    for (const fragment of fragments) {
      assertBuilderContextsComposable(this.context, fragment.context);
    }
    return makeTxBuilder(
      this.context,
      composeStates([
        this.state,
        ...fragments.map((fragment) => fragment.state),
      ]),
    );
  }

  private async resolveChangeAddress(
    options: CompleteOptions,
  ): Promise<Address> {
    const changeAddress =
      options.changeAddress ?? (await this.context.wallet?.address());
    if (changeAddress === undefined) {
      throw new BuilderInvariantError(
        "Balancing requires a change address or a selected wallet",
      );
    }
    assertAddressNetwork(changeAddress, this.context.config.networkId);
    return changeAddress;
  }

  private async resolveFeePolicy(
    option: CompleteOptions["feePolicy"],
  ): Promise<FeePolicy> {
    if (option !== undefined && option !== "provider") {
      return normalizeFeePolicy(option);
    }
    const params = await this.context.provider.provider.getProtocolParameters();
    return normalizeFeePolicy({
      minFeeA: params.minFeeA,
      minFeeB: params.minFeeB,
    });
  }

  private async runLocalValidation(
    completed: CompleteTx,
    options: CompleteOptions,
  ): Promise<LocalValidationReport | undefined> {
    const level = validationLevel(options);
    if (level === "none") {
      return undefined;
    }
    const report = await runSharedLocalPreflight({
      completed,
      phase: level,
      provider: this.context.provider.provider,
      options,
      missingPreStateMessage:
        'complete({ localValidation: "phase-b" }) requires localPreState',
    });
    assertAcceptedLocalValidation(report, completed.txIdHex);
    return report;
  }

  private async finalizeCompleteTx(
    tx: MidgardNativeTxFull,
    metadata: Omit<CompleteTxMetadata, "localValidation">,
    options: CompleteOptions,
    programMaterial: readonly MidgardCekProgramMaterialEntry[] = [],
    resolvedReferenceOutputsByOutRef: ReadonlyMap<
      string,
      Uint8Array
    > = new Map(),
  ): Promise<CompleteTx> {
    const context: CompleteTxContext = {
      provider: this.context.provider.provider,
      wallet: () => this.context.wallet,
      networkId: configNetworkId(this.context.config),
      maxSubmitTxCborBytes:
        this.context.config.submissionLimits.maxSubmitTxCborBytes,
      consensusProfile: this.context.config.consensusProfile,
      programMaterial,
      resolvedReferenceOutputsByOutRef,
    };
    const enrichedMetadata = attachProviderMetadata(
      metadata,
      this.context.provider,
    );
    const completed = makeCompleteTx(tx, enrichedMetadata, context);
    const localValidation = await this.runLocalValidation(completed, options);
    if (localValidation === undefined) {
      return completed;
    }
    return makeCompleteTx(
      tx,
      { ...enrichedMetadata, localValidation },
      context,
    );
  }

  private async resolveWalletInputs(
    state: BuilderState,
    changeAddress: Address,
    options: CompleteOptions,
  ): Promise<ResolvedWalletInputs> {
    const referenceLabels = new Set(state.referenceInputs.map(outRefLabel));
    const normalizeCandidates = (
      inputs: readonly MidgardUtxo[],
      source: WalletInputSource,
    ): readonly MidgardUtxo[] =>
      normalizeWalletInputUtxos(
        inputs,
        source,
        this.context.config.networkId,
      ).filter((utxo) => !referenceLabels.has(outRefLabel(utxo)));

    if (options.presetWalletInputs !== undefined) {
      const inputs = normalizeCandidates(
        options.presetWalletInputs,
        "completion-preset",
      );
      assertNoPresetInputOverlap(state.spendInputs, inputs);
      await assertWalletOwnsInputs(
        this.context.wallet,
        inputs,
        "completion-preset",
      );
      return { source: "completion-preset", inputs };
    }

    if (this.context.utxoOverrides !== undefined) {
      const inputs = normalizeCandidates(
        this.context.utxoOverrides.utxos,
        "instance-override",
      );
      await assertWalletOwnsInputs(
        this.context.wallet,
        inputs,
        "instance-override",
      );
      return {
        source: "instance-override",
        inputs,
        overrideGeneration: this.context.utxoOverrides.generation,
      };
    }

    const fetchedUtxos =
      await this.context.provider.provider.getUtxos(changeAddress);
    return {
      source: "provider",
      inputs: fetchedUtxos
        .map(normalizeUtxo)
        .filter((utxo) => !referenceLabels.has(outRefLabel(utxo))),
    };
  }

  private async completeBalanced(
    state: BuilderState,
    options: CompleteOptions,
    resolved?: BalancedCompletionInputs,
    programMaterial: readonly MidgardCekProgramMaterialEntry[] = [],
    resolvedReferenceOutputsByOutRef: ReadonlyMap<
      string,
      Uint8Array
    > = new Map(),
  ): Promise<CompleteTx> {
    let completionInputs = resolved;
    if (completionInputs === undefined) {
      const changeAddress = await this.resolveChangeAddress(options);
      completionInputs = {
        changeAddress,
        feePolicy: await this.resolveFeePolicy(options.feePolicy),
        walletInputs: await this.resolveWalletInputs(
          state,
          changeAddress,
          options,
        ),
      };
    }
    const balanced = buildBalancedCompletion({
      state,
      resolved: completionInputs,
      initialFee: resolveInitialFee(state, options.fee),
      maxFeeIterations: resolveMaxFeeIterations(options.maxFeeIterations),
      nativeTxVersion: BigInt(this.context.config.midgardNativeTxVersion),
      deriveScriptMaterialization,
    });
    return this.finalizeCompleteTx(
      balanced.tx,
      balanced.metadata,
      options,
      programMaterial,
      resolvedReferenceOutputsByOutRef,
    );
  }

  async complete(options: CompleteOptions = {}): Promise<CompleteTx> {
    const clonedState = cloneState(this.state);
    const prepared = isMidgardConsensusProfile(
      this.context.config.consensusProfile,
    )
      ? prepareProofBuilderState(clonedState, options.programMaterial)
      : { state: clonedState, programMaterial: [] };
    const state = prepared.state;
    const balanceRequested = shouldBalanceWithWalletDefault(
      options,
      this.context.wallet !== undefined,
    );
    if (state.spendInputs.length === 0 && !balanceRequested) {
      throw new BuilderInvariantError(
        "Cannot complete a transaction with no spend inputs",
      );
    }
    if (balanceRequested) {
      return this.completeBalanced(
        state,
        options,
        undefined,
        prepared.programMaterial,
        referenceOutputsByOutRef(state.referenceInputs),
      );
    }

    const fee = resolveInitialFee(state, options.fee);
    const scriptMaterialization = deriveScriptMaterialization(state);
    const inputTotal = sumAssets(state.spendInputs.map(utxoOutputAssets));
    const outputTotal = sumAssets(state.outputs.map((output) => output.assets));
    assertBalancedWithoutChange(
      inputTotal,
      outputTotal,
      fee,
      scriptMaterialization.mintDelta,
    );

    const canonical = buildCanonicalUnsignedTx(
      state,
      fee,
      scriptMaterialization,
      BigInt(this.context.config.midgardNativeTxVersion),
    );
    const tx = materializeMidgardNativeTxFromCanonical(canonical);
    const txCbor = encodeMidgardNativeTxCanonical(tx);
    const expectedWitnessKeyHashes = expectedAddrWitnessKeyHashes(state);
    const expectedAddrWitnessCount = expectedWitnessKeyHashes.length;

    return this.finalizeCompleteTx(
      tx,
      {
        fee,
        inputCount: state.spendInputs.length,
        referenceInputCount: state.referenceInputs.length,
        outputCount: state.outputs.length,
        requiredSignerCount: state.requiredSigners.length,
        txByteLength: txCbor.length,
        feeIterations: 0,
        balanced: false,
        expectedAddrWitnessCount,
        expectedAddrWitnessKeyHashes: expectedWitnessKeyHashes,
        estimatedSignedTxByteLength: estimatedSignedTxByteLength(
          tx,
          expectedAddrWitnessCount,
        ),
      },
      options,
      prepared.programMaterial,
      referenceOutputsByOutRef(state.referenceInputs),
    );
  }

  completeProgram(options: CompleteOptions = {}): MidgardEffect<CompleteTx> {
    return midgardProgram(() => this.complete(options));
  }

  completeSafe(
    options: CompleteOptions = {},
  ): Promise<MidgardResult<CompleteTx, LucidMidgardError>> {
    return midgardSafe(() => this.complete(options));
  }

  private async resolveBalancedChainInputs(
    state: BuilderState,
    options: CompleteOptions,
  ): Promise<BalancedCompletionInputs> {
    const changeAddress = await this.resolveChangeAddress(options);
    const [feePolicy, walletInputs] = await Promise.all([
      this.resolveFeePolicy(options.feePolicy),
      this.resolveWalletInputs(state, changeAddress, options),
    ]);
    return { changeAddress, feePolicy, walletInputs };
  }

  private async explicitChainBaseWalletInputs(
    state: BuilderState,
    options: CompleteOptions,
  ): Promise<readonly MidgardUtxo[]> {
    if (this.context.wallet === undefined) {
      return [];
    }
    const referenceLabels = new Set(state.referenceInputs.map(outRefLabel));
    const normalizeBase = async (
      inputs: readonly MidgardUtxo[],
      source: WalletInputSource,
    ): Promise<readonly MidgardUtxo[]> => {
      const normalized = normalizeWalletInputUtxos(
        inputs,
        source,
        this.context.config.networkId,
      ).filter((utxo) => !referenceLabels.has(outRefLabel(utxo)));
      await assertWalletOwnsInputs(this.context.wallet, normalized, source);
      return normalized;
    };
    if (options.presetWalletInputs !== undefined) {
      return normalizeBase(options.presetWalletInputs, "completion-preset");
    }
    if (this.context.utxoOverrides !== undefined) {
      return normalizeBase(
        this.context.utxoOverrides.utxos,
        "instance-override",
      );
    }
    return [];
  }

  private async newWalletUtxosAfterChain(
    completed: CompleteTx,
    baseWalletInputs: readonly MidgardUtxo[],
  ): Promise<readonly MidgardUtxo[]> {
    if (this.context.wallet === undefined) {
      return [];
    }
    const walletKeyHash = await this.context.wallet.keyHash();
    const spent = new Set(nativeInputOutRefs(completed.tx).map(outRefLabel));
    const unspent = baseWalletInputs.filter(
      (utxo) => !spent.has(outRefLabel(utxo)),
    );
    const producedForWallet = completed
      .producedOutputs()
      .filter((utxo) => paymentPubKeyHashFromUtxo(utxo) === walletKeyHash);
    return [
      ...unspent.map(cloneUtxo),
      ...producedForWallet.map(cloneUtxo),
    ].sort(compareOutRefs);
  }

  async chain(options: CompleteOptions = {}): Promise<ChainResult> {
    const clonedState = cloneState(this.state);
    const prepared = isMidgardConsensusProfile(
      this.context.config.consensusProfile,
    )
      ? prepareProofBuilderState(clonedState, options.programMaterial)
      : { state: clonedState, programMaterial: [] };
    const state = prepared.state;
    const balanceRequested = shouldBalanceWithWalletDefault(
      options,
      this.context.wallet !== undefined,
    );
    if (state.spendInputs.length === 0 && !balanceRequested) {
      throw new BuilderInvariantError(
        "Cannot chain a transaction with no spend inputs",
      );
    }

    const balancedInputs = balanceRequested
      ? await this.resolveBalancedChainInputs(state, options)
      : undefined;
    const completed =
      balancedInputs === undefined
        ? await this.complete(options)
        : await this.completeBalanced(
            state,
            options,
            balancedInputs,
            prepared.programMaterial,
            referenceOutputsByOutRef(state.referenceInputs),
          );
    const baseWalletInputs =
      balancedInputs?.walletInputs.inputs ??
      (await this.explicitChainBaseWalletInputs(state, options));
    const derivedOutputs = completed.producedOutputs();
    const newWalletUtxos = await this.newWalletUtxosAfterChain(
      completed,
      baseWalletInputs,
    );
    return [newWalletUtxos, derivedOutputs, completed] as const;
  }

  chainProgram(options: CompleteOptions = {}): MidgardEffect<ChainResult> {
    return midgardProgram(() => this.chain(options));
  }

  chainSafe(
    options: CompleteOptions = {},
  ): Promise<MidgardResult<ChainResult, LucidMidgardError>> {
    return midgardSafe(() => this.chain(options));
  }
}

export const makeTxBuilder = (
  context: BuilderContextSnapshot,
  state: BuilderState,
): TxBuilder => new TxBuilder(context, state, txBuilderConstructorToken);
