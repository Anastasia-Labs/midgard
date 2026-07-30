import { assetsEqual } from "@al-ft/midgard-core/assets";
import {
  encodeMidgardCekProgramMaterialSidecarV1,
  type MidgardCekProgramMaterialEntryV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeMidgardAddressText,
  encodeMidgardNativeTxCanonicalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  midgardAddressFromText,
  type MidgardNativeTxFullV1,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { isMidgardConsensusProfileV1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { validateMidgardConsensusV1TxCbor } from "@al-ft/midgard-core/consensus-validation-v1";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import {
  LedgerColumns,
  type PhaseAConfig,
  type PhaseAResult,
  type PhaseBConfig,
  type PhaseBResultWithPatch,
  type QueuedTx,
  type RejectedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { CML, type Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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
  LucidMidgardConfig,
  LucidMidgardConfigSnapshot,
  ProviderSnapshot,
  SwitchProviderOptions,
  UtxoOverrideSnapshot,
} from "./builder/context.js";
import {
  assertBuilderContextsComposable,
  buildConfigSnapshot,
  cloneProtocolInfo,
  cloneProviderDiagnostics,
  configNetworkId,
  readProviderSnapshot,
} from "./builder/context.js";
import {
  assertExpectedAddrWitnesses,
  decodeFromTxInput,
  type FromTxOptions,
  type ImportedTxInput,
  importedTxMetadata,
  localUtxoAt,
  localUtxosFromTx,
  nativeInputOutRefs,
} from "./builder/imported-tx.js";
import {
  attachProviderMetadata,
  cloneCompleteTxMetadata,
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
  mergeCanonicalProofProgramMaterial,
  normalizeMintAssetsForNormalizedPolicy,
  normalizePolicyId,
  normalizeScriptHash,
  normalizeScriptLanguage,
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
  emptyState,
  validatorScriptSource,
} from "./builder/state.js";
import {
  assertSubmitAdmissionMatches,
  assertSubmitSizeWithinLimit,
  assertTxStatusMatches,
  pollTxStatus,
  resolveProvider,
  resolveSubmitSizeLimit,
} from "./builder/status.js";
import { buildCanonicalUnsignedTx } from "./builder/unsigned-tx.js";
import {
  addrWitnessKeyHashes,
  addrWitnessMetadata,
  applyAddrWitnessesToTx,
  assertPartialBundleMatchesTx,
  decodeAddrWitnesses,
  decodeImportAddrWitnesses,
  encodePartialWitnessBundle,
  estimatedSignedTxByteLength,
  type MidgardPartialWitnessBundleV1,
  normalizeVKeyWitnessInput,
  parsePartialWitnessBundle,
  partialWitnessBundleFromWitnesses,
  type PartialWitnessBundleInput,
  signMidgardNativeTx,
  type VKeyWitnessInput,
} from "./builder/witness-bundle.js";
import {
  assetQuantity,
  type Assets,
  type AssetUnit,
  type ValueLike,
} from "./core/assets.js";
import {
  BuilderInvariantError,
  LucidMidgardError,
  ProviderCapabilityError,
  ProviderPayloadError,
  SigningError,
} from "./core/errors.js";
import {
  compareOutRefs,
  normalizeOutRef,
  normalizeTxHash,
  type OutRef,
  outRefLabel,
} from "./core/out-ref.js";
import {
  type AuthoredOutput,
  authoredOutput,
  decodeMidgardTxOutput,
  normalizePlutusData,
  type OutputOptions,
  type PlutusDataLike,
  utxoAddress,
  utxoAssets as utxoOutputAssets,
  utxoOutputCbor,
  utxoOutRefCbor,
} from "./core/output.js";
import type {
  BuilderScriptState,
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
  type LocalValidationPreState,
  type LocalValidationPreStateSource,
  type LocalValidationReport,
  type MidgardResult,
  type MidgardUtxo,
  type SubmitTxResult,
  type TxStatus,
  type WalletInputSource,
} from "./core/types.js";
import type { MidgardProtocolInfo, MidgardProvider } from "./provider.js";
import {
  assertAddressNetwork,
  assertVKeyWitness,
  type ExternalBodyHashSigner,
  makeVKeyWitness,
  type MidgardWallet,
  paymentKeyHashFromAddress,
  type PrivateKey,
  type VKeyWitness,
  walletFromExternalSigner,
  walletFromPrivateKey,
  walletFromSeedPhrase,
} from "./wallet.js";

export type {
  LucidMidgardConfig,
  LucidMidgardConfigSnapshot,
  SwitchProviderOptions,
} from "./builder/context.js";
export type { FromTxOptions } from "./builder/imported-tx.js";
export type { CompleteTxMetadata } from "./builder/metadata.js";
export type {
  MidgardPartialWitnessBundleV1,
  PartialWitnessBundleInput,
  VKeyWitnessInput,
} from "./builder/witness-bundle.js";
export {
  decodePartialWitnessBundle,
  encodePartialWitnessBundle,
  parsePartialWitnessBundle,
} from "./builder/witness-bundle.js";

export type TxStatusKind = TxStatus["kind"];

export type SubmitOptions = {
  readonly provider?: MidgardProvider;
};

export type AwaitTxOptions = {
  readonly provider?: MidgardProvider;
  readonly until?: TxStatusKind | readonly TxStatusKind[];
  readonly pollIntervalMs?: number;
  readonly timeoutMs?: number;
  readonly signal?: AbortSignal;
};

export type LocalPreflightPhase = "phase-a" | "phase-b";

export type LocalPreflightOptions = {
  readonly provider?: MidgardProvider;
  readonly localPreState?: LocalValidationPreState;
  readonly localPreStateSource?: LocalValidationPreStateSource;
  readonly nowCardanoSlotNo?: bigint | number;
  readonly validationConcurrency?: number;
  readonly enforceScriptBudget?: boolean;
};

export type FromTxInput = CompleteTx | ImportedTxInput;

export type AssemblePartialWitnessOptions = {
  readonly allowPartial?: boolean;
};

export type PartialSignApi = {
  readonly withWallet: (wallet?: MidgardWallet) => TxPartialSignBuilder;
  readonly withWalletSafe: (
    wallet?: MidgardWallet,
  ) => Promise<MidgardResult<MidgardPartialWitnessBundleV1, LucidMidgardError>>;
  readonly withWalletProgram: (
    wallet?: MidgardWallet,
  ) => MidgardEffect<MidgardPartialWitnessBundleV1>;
  readonly withPrivateKey: (
    privateKey: PrivateKey | string,
  ) => TxPartialSignBuilder;
  readonly withPrivateKeySafe: (
    privateKey: PrivateKey | string,
  ) => Promise<MidgardResult<MidgardPartialWitnessBundleV1, LucidMidgardError>>;
  readonly withPrivateKeyProgram: (
    privateKey: PrivateKey | string,
  ) => MidgardEffect<MidgardPartialWitnessBundleV1>;
  readonly withExternalSigner: (
    signer: ExternalBodyHashSigner,
  ) => TxPartialSignBuilder;
  readonly withExternalSignerSafe: (
    signer: ExternalBodyHashSigner,
  ) => Promise<MidgardResult<MidgardPartialWitnessBundleV1, LucidMidgardError>>;
  readonly withExternalSignerProgram: (
    signer: ExternalBodyHashSigner,
  ) => MidgardEffect<MidgardPartialWitnessBundleV1>;
  readonly withWitness: (witness: VKeyWitnessInput) => TxPartialSignBuilder;
  readonly withWitnesses: (
    witnesses: readonly VKeyWitnessInput[],
  ) => TxPartialSignBuilder;
};

export type ChainResult = readonly [
  newWalletUtxos: readonly MidgardUtxo[],
  derivedOutputs: readonly MidgardUtxo[],
  tx: CompleteTx,
];

export type MidgardEffect<T> = Effect.Effect<T, LucidMidgardError>;

const midgardSafe = async <T>(
  operation: () => Promise<T>,
): Promise<MidgardResult<T, LucidMidgardError>> => {
  try {
    return { ok: true, value: await operation() };
  } catch (error) {
    if (error instanceof LucidMidgardError) {
      return { ok: false, error };
    }
    throw error;
  }
};

const midgardProgram = <T>(operation: () => Promise<T>): MidgardEffect<T> =>
  Effect.tryPromise({
    try: () => operation(),
    catch: (error) => {
      if (error instanceof LucidMidgardError) {
        return error;
      }
      throw error;
    },
  });

export type UtxosByOutRefOptions = {
  readonly missing?: "error" | "omit";
};

export type DatumOfOptions =
  | {
      readonly expectedHash?: string;
    }
  | string;

const txBuilderConstructorToken = Symbol("TxBuilder.constructor");

const assertConsensusTransaction = (
  txCbor: Uint8Array,
  consensusProfile: LucidMidgardConfigSnapshot["consensusProfile"],
): void => {
  if (!isMidgardConsensusProfileV1(consensusProfile)) {
    throw new BuilderInvariantError(
      "Transaction uses an unsupported consensus profile",
    );
  }
  const violation = validateMidgardConsensusV1TxCbor(txCbor);
  if (violation !== null) {
    throw new BuilderInvariantError(
      `Transaction violates the V1 consensus profile: ${violation.code}`,
      `${violation.featureId}: ${violation.detail}`,
    );
  }
};
const submittedTxConstructorToken = Symbol("SubmittedTx.constructor");
const partiallySignedTxConstructorToken = Symbol(
  "PartiallySignedTx.constructor",
);

const assertTxNetworkMatchesExpected = (
  tx: MidgardNativeTxFullV1,
  expectedNetworkId: bigint | undefined,
  context: string,
): void => {
  if (expectedNetworkId === undefined) {
    return;
  }
  if (tx.body.networkId !== expectedNetworkId) {
    throw new BuilderInvariantError(
      `${context} network id mismatch`,
      `expected=${expectedNetworkId.toString()} actual=${tx.body.networkId.toString()}`,
    );
  }
};

const trustedCompleteTxs = new WeakSet<object>();

const midgardJsonSafe = (value: unknown): unknown => {
  if (typeof value === "bigint") {
    return value.toString(10);
  }
  if (Buffer.isBuffer(value) || value instanceof Uint8Array) {
    return Buffer.from(value).toString("hex");
  }
  if (Array.isArray(value)) {
    return value.map(midgardJsonSafe);
  }
  if (typeof value === "object" && value !== null) {
    return Object.fromEntries(
      Object.entries(value).map(([key, child]) => [
        key,
        midgardJsonSafe(child),
      ]),
    );
  }
  return value;
};

export type MidgardTxJson = {
  readonly txId: string;
  readonly txCbor: string;
  readonly metadata: unknown;
};

const completeTxMetadataWithAddrWitnesses = (
  tx: MidgardNativeTxFullV1,
  metadata: CompleteTxMetadata,
): CompleteTxMetadata => {
  const witnesses = decodeAddrWitnesses(tx.witnessSet.addrTxWitsPreimageCbor);
  return {
    ...metadata,
    txByteLength: encodeMidgardNativeTxCanonicalV1(tx).length,
    ...addrWitnessMetadata(witnesses),
  };
};

const privateKeyFromInput = (privateKey: PrivateKey | string): PrivateKey =>
  typeof privateKey === "string"
    ? CML.PrivateKey.from_bech32(privateKey)
    : privateKey;

export class TxPartialSignBuilder {
  readonly #tx: CompleteTx;
  readonly #witnesses: readonly VKeyWitnessInput[];
  readonly #wallet?: MidgardWallet;
  readonly #privateKey?: PrivateKey | string;
  readonly #externalSigner?: ExternalBodyHashSigner;
  readonly #expectedNetworkId?: number;

  constructor({
    tx,
    witnesses = [],
    wallet,
    privateKey,
    externalSigner,
    expectedNetworkId,
  }: {
    readonly tx: CompleteTx;
    readonly witnesses?: readonly VKeyWitnessInput[];
    readonly wallet?: MidgardWallet;
    readonly privateKey?: PrivateKey | string;
    readonly externalSigner?: ExternalBodyHashSigner;
    readonly expectedNetworkId?: number;
  }) {
    this.#tx = tx;
    this.#witnesses = witnesses;
    this.#wallet = wallet;
    this.#privateKey = privateKey;
    this.#externalSigner = externalSigner;
    this.#expectedNetworkId = expectedNetworkId;
  }

  async partial(): Promise<MidgardPartialWitnessBundleV1> {
    return partialWitnessBundleFromWitnesses(
      this.#tx.tx,
      await this.collectWitnesses(),
    );
  }

  async complete(): Promise<CompleteTx> {
    const assembled = this.#tx.assemble(await this.partial());
    if (assembled instanceof CompleteTx) {
      return assembled;
    }
    throw new SigningError("Incomplete partial witness assembly");
  }

  completeProgram(): MidgardEffect<CompleteTx> {
    return midgardProgram(() => this.complete());
  }

  completeSafe(): Promise<MidgardResult<CompleteTx, LucidMidgardError>> {
    return midgardSafe(() => this.complete());
  }

  partialProgram(): MidgardEffect<MidgardPartialWitnessBundleV1> {
    return midgardProgram(() => this.partial());
  }

  partialSafe(): Promise<
    MidgardResult<MidgardPartialWitnessBundleV1, LucidMidgardError>
  > {
    return midgardSafe(() => this.partial());
  }

  private async collectWitnesses(): Promise<readonly VKeyWitness[]> {
    const nativeTx = this.#tx.tx;
    const bodyHash = computeMidgardNativeTxIdV1(nativeTx);
    const witnesses: VKeyWitness[] = this.#witnesses.map((witness, index) =>
      normalizeVKeyWitnessInput(
        witness,
        bodyHash,
        `partial witness[${index.toString()}]`,
      ),
    );
    if (this.#wallet !== undefined) {
      witnesses.push(
        assertVKeyWitness(bodyHash, await this.#wallet.signBodyHash(bodyHash)),
      );
    }
    if (this.#privateKey !== undefined) {
      witnesses.push(
        makeVKeyWitness(bodyHash, privateKeyFromInput(this.#privateKey)),
      );
    }
    if (this.#externalSigner !== undefined) {
      const wallet = walletFromExternalSigner(this.#externalSigner, {
        expectedNetworkId: this.#expectedNetworkId,
      });
      witnesses.push(
        assertVKeyWitness(bodyHash, await wallet.signBodyHash(bodyHash)),
      );
    }
    if (witnesses.length === 0) {
      throw new SigningError("No partial witnesses supplied");
    }
    return witnesses;
  }
}

export type CompleteTxSignApi = ((
  wallet?: MidgardWallet,
) => Promise<CompleteTx>) &
  PartialSignApi;

export class CompleteTx {
  readonly #txCbor: Buffer;
  readonly #txId: Buffer;
  readonly #metadata: CompleteTxMetadata;
  readonly #context?: CompleteTxContext;
  readonly txHex: string;
  readonly txIdHex: string;
  readonly sign: CompleteTxSignApi;
  readonly partialSign: PartialSignApi;

  constructor(
    tx: MidgardNativeTxFullV1,
    metadata: CompleteTxMetadata,
    context?: CompleteTxContext,
  ) {
    this.#txCbor = encodeMidgardNativeTxCanonicalV1(tx);
    assertConsensusTransaction(
      this.#txCbor,
      context?.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE_V1,
    );
    this.txHex = this.#txCbor.toString("hex");
    this.#txId = computeMidgardNativeTxIdV1(tx);
    this.txIdHex = this.#txId.toString("hex");
    this.#metadata = cloneCompleteTxMetadata(metadata);
    this.#context = context;
    this.partialSign = this.makePartialSignApi();
    this.sign = Object.assign(
      (wallet?: MidgardWallet): Promise<CompleteTx> =>
        this.signWithWallet(wallet),
      this.partialSign,
    );
  }

  get tx(): MidgardNativeTxFullV1 {
    return decodeMidgardNativeTxFullV1FromCanonicalCbor(this.#txCbor);
  }

  get txCbor(): Buffer {
    return Buffer.from(this.#txCbor);
  }

  get txId(): Buffer {
    return Buffer.from(this.#txId);
  }

  get metadata(): CompleteTxMetadata {
    return cloneCompleteTxMetadata(this.#metadata);
  }

  get programMaterial(): readonly MidgardCekProgramMaterialEntryV1[] {
    return (this.#context?.programMaterial ?? []).map((entry) => ({
      ...entry,
      root: Buffer.from(entry.root) as MidgardCekProgramMaterialEntryV1["root"],
      preimage: Buffer.from(entry.preimage),
    }));
  }

  toCBOR(): string {
    return this.txHex;
  }

  toHash(): string {
    return this.txIdHex;
  }

  toJSON(): MidgardTxJson {
    return {
      txId: this.txIdHex,
      txCbor: this.txHex,
      metadata: midgardJsonSafe(this.metadata),
    };
  }

  producedOutputs(): readonly MidgardUtxo[] {
    assertTrustedCompleteTx(this, "derive produced outputs");
    return localUtxosFromTx(this.tx, this.expectedNetworkIdNumber()).map(
      cloneUtxo,
    );
  }

  producedOutput(outputIndex: number): MidgardUtxo {
    assertTrustedCompleteTx(this, "derive a produced output");
    return localUtxoAt(this.tx, outputIndex, this.expectedNetworkIdNumber());
  }

  assemble(
    bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
    options: AssemblePartialWitnessOptions = {},
  ): CompleteTx | PartiallySignedTx {
    assertTrustedCompleteTx(this, "assemble partial witnesses");
    return assemblePartialWitnessBundles(
      this.tx,
      this.#metadata,
      this.#context,
      bundles,
      options,
    );
  }

  toPartialWitnessBundle(): MidgardPartialWitnessBundleV1 {
    assertTrustedCompleteTx(this, "export partial witnesses");
    return partialWitnessBundleFromWitnesses(
      this.tx,
      decodeAddrWitnesses(this.tx.witnessSet.addrTxWitsPreimageCbor),
    );
  }

  toPartialWitnessBundleCbor(): Buffer {
    return encodePartialWitnessBundle(this.toPartialWitnessBundle());
  }

  async validate(
    phase: LocalPreflightPhase,
    options: LocalPreflightOptions = {},
  ): Promise<LocalValidationReport> {
    assertTrustedCompleteTx(this, "run local preflight validation");
    assertTxNetworkMatchesExpected(
      this.tx,
      this.#context?.networkId,
      "Transaction",
    );
    return runSharedLocalPreflight({
      completed: this,
      phase: normalizeLocalPreflightPhase(phase),
      provider: resolveProvider(options.provider, this.#context),
      options,
      missingPreStateMessage: 'validate("phase-b") requires localPreState',
    });
  }

  validateProgram(
    phase: LocalPreflightPhase,
    options: LocalPreflightOptions = {},
  ): MidgardEffect<LocalValidationReport> {
    return midgardProgram(() => this.validate(phase, options));
  }

  validateSafe(
    phase: LocalPreflightPhase,
    options: LocalPreflightOptions = {},
  ): Promise<MidgardResult<LocalValidationReport, LucidMidgardError>> {
    return midgardSafe(() => this.validate(phase, options));
  }

  private expectedNetworkIdNumber(): number | undefined {
    if (this.#context?.networkId === undefined) {
      return undefined;
    }
    return Number(this.#context.networkId);
  }

  private makePartialSignApi(): PartialSignApi {
    const withWallet = (wallet?: MidgardWallet): TxPartialSignBuilder => {
      const signer = wallet ?? this.#context?.wallet?.();
      return new TxPartialSignBuilder({
        tx: this,
        wallet: signer,
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    };
    const withPrivateKey = (
      privateKey: PrivateKey | string,
    ): TxPartialSignBuilder =>
      new TxPartialSignBuilder({
        tx: this,
        privateKey,
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    const withExternalSigner = (
      signer: ExternalBodyHashSigner,
    ): TxPartialSignBuilder =>
      new TxPartialSignBuilder({
        tx: this,
        externalSigner: signer,
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    const withWitness = (witness: VKeyWitnessInput): TxPartialSignBuilder =>
      new TxPartialSignBuilder({
        tx: this,
        witnesses: [witness],
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    const withWitnesses = (
      witnesses: readonly VKeyWitnessInput[],
    ): TxPartialSignBuilder =>
      new TxPartialSignBuilder({
        tx: this,
        witnesses,
        expectedNetworkId: this.expectedNetworkIdNumber(),
      });
    return {
      withWallet,
      withWalletSafe: (wallet?: MidgardWallet) =>
        midgardSafe(() => withWallet(wallet).partial()),
      withWalletProgram: (wallet?: MidgardWallet) =>
        midgardProgram(() => withWallet(wallet).partial()),
      withPrivateKey,
      withPrivateKeySafe: (privateKey: PrivateKey | string) =>
        midgardSafe(() => withPrivateKey(privateKey).partial()),
      withPrivateKeyProgram: (privateKey: PrivateKey | string) =>
        midgardProgram(() => withPrivateKey(privateKey).partial()),
      withExternalSigner,
      withExternalSignerSafe: (signer: ExternalBodyHashSigner) =>
        midgardSafe(() => withExternalSigner(signer).partial()),
      withExternalSignerProgram: (signer: ExternalBodyHashSigner) =>
        midgardProgram(() => withExternalSigner(signer).partial()),
      withWitness,
      withWitnesses,
    };
  }

  private async signWithWallet(wallet?: MidgardWallet): Promise<CompleteTx> {
    assertTrustedCompleteTx(this, "sign");
    assertTxNetworkMatchesExpected(
      this.tx,
      this.#context?.networkId,
      "Transaction",
    );
    const signer = wallet ?? this.#context?.wallet?.();
    if (signer === undefined) {
      throw new SigningError("No Midgard wallet available for signing");
    }
    const signedTx = await signMidgardNativeTx(this.tx, signer);
    const signedWitnesses = decodeAddrWitnesses(
      signedTx.witnessSet.addrTxWitsPreimageCbor,
    );
    assertExpectedAddrWitnesses({
      actual: addrWitnessKeyHashes(signedWitnesses),
      expected: this.#metadata.expectedAddrWitnessKeyHashes,
      expectedComplete: this.#metadata.expectedAddrWitnessesComplete,
      requireComplete: false,
    });
    return makeCompleteTx(
      signedTx,
      {
        ...this.#metadata,
        txByteLength: encodeMidgardNativeTxCanonicalV1(signedTx).length,
        ...addrWitnessMetadata(signedWitnesses),
      },
      this.#context,
    );
  }

  async submit(options: SubmitOptions = {}): Promise<SubmittedTx> {
    assertTrustedCompleteTx(this, "submit");
    const provider = resolveProvider(options.provider, this.#context);
    assertTxNetworkMatchesExpected(
      this.tx,
      this.#context?.networkId,
      "Transaction",
    );
    const providerParams = await provider.getProtocolParameters();
    assertTxNetworkMatchesExpected(
      this.tx,
      providerParams.networkId,
      "Provider",
    );
    const maxSubmitTxCborBytes = await resolveSubmitSizeLimit({
      provider,
      providerParams,
      context: this.#context,
    });
    assertSubmitSizeWithinLimit(this.txCbor, maxSubmitTxCborBytes);
    const verifiedWitnesses = decodeImportAddrWitnesses(this.tx);
    assertExpectedAddrWitnesses({
      actual: addrWitnessKeyHashes(verifiedWitnesses),
      expected: this.#metadata.expectedAddrWitnessKeyHashes,
      expectedComplete: this.#metadata.expectedAddrWitnessesComplete,
      requireComplete: true,
    });
    const admission = assertSubmitAdmissionMatches(
      this.txIdHex,
      await provider.submitTx(this.txHex, this.programMaterial),
    );
    return makeSubmittedTx(this, admission, provider);
  }

  submitProgram(options: SubmitOptions = {}): MidgardEffect<SubmittedTx> {
    return midgardProgram(() => this.submit(options));
  }

  submitSafe(
    options: SubmitOptions = {},
  ): Promise<MidgardResult<SubmittedTx, LucidMidgardError>> {
    return midgardSafe(() => this.submit(options));
  }

  async status(options: SubmitOptions = {}): Promise<TxStatus> {
    assertTrustedCompleteTx(this, "query status");
    const provider = resolveProvider(options.provider, this.#context);
    return assertTxStatusMatches(
      this.txIdHex,
      await provider.getTxStatus(this.txIdHex),
    );
  }

  statusProgram(options: SubmitOptions = {}): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.status(options));
  }

  statusSafe(
    options: SubmitOptions = {},
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.status(options));
  }

  async awaitStatus(options: AwaitTxOptions = {}): Promise<TxStatus> {
    assertTrustedCompleteTx(this, "poll status");
    const provider = resolveProvider(options.provider, this.#context);
    return pollTxStatus(provider, this.txIdHex, options);
  }

  awaitStatusProgram(options: AwaitTxOptions = {}): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.awaitStatus(options));
  }

  awaitStatusSafe(
    options: AwaitTxOptions = {},
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.awaitStatus(options));
  }
}

const assertTrustedCompleteTx = (tx: CompleteTx, action: string): void => {
  if (!trustedCompleteTxs.has(tx)) {
    throw new BuilderInvariantError(
      `Untrusted CompleteTx cannot ${action}`,
      "construct transactions through LucidMidgard.newTx(), LucidMidgard.fromTx(), or signing APIs",
    );
  }
};

const makeCompleteTx = (
  tx: MidgardNativeTxFullV1,
  metadata: CompleteTxMetadata,
  context?: CompleteTxContext,
): CompleteTx => {
  const completed = new CompleteTx(tx, metadata, context);
  trustedCompleteTxs.add(completed);
  return completed;
};

const partialBundleInputs = (
  bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
): readonly PartialWitnessBundleInput[] =>
  Array.isArray(bundles) ? bundles : [bundles];

const completeWitnessSet = (
  actual: readonly string[],
  metadata: CompleteTxMetadata,
): boolean => {
  const expected = metadata.expectedAddrWitnessKeyHashes;
  if (
    expected === undefined ||
    metadata.expectedAddrWitnessesComplete === false
  ) {
    return false;
  }
  const actualSet = new Set(actual);
  return expected.every((keyHash) => actualSet.has(keyHash));
};

const assemblePartialWitnessBundles = (
  tx: MidgardNativeTxFullV1,
  metadata: CompleteTxMetadata,
  context: CompleteTxContext | undefined,
  bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
  options: AssemblePartialWitnessOptions,
): CompleteTx | PartiallySignedTx => {
  const inputs = partialBundleInputs(bundles);
  if (inputs.length === 0) {
    throw new SigningError("At least one partial witness bundle is required");
  }
  const bodyHash = computeMidgardNativeTxIdV1(tx);
  const witnesses = inputs.flatMap((input) => {
    const bundle = parsePartialWitnessBundle(input);
    assertPartialBundleMatchesTx(tx, bundle);
    return bundle.witnesses.map((witnessHex, index) =>
      normalizeVKeyWitnessInput(
        witnessHex,
        bodyHash,
        `partial bundle witness[${index.toString()}]`,
      ),
    );
  });
  const assembled = applyAddrWitnessesToTx(tx, witnesses);
  const nextMetadata = completeTxMetadataWithAddrWitnesses(
    assembled.tx,
    metadata,
  );
  const actual = addrWitnessKeyHashes(assembled.witnesses);
  assertExpectedAddrWitnesses({
    actual,
    expected: nextMetadata.expectedAddrWitnessKeyHashes,
    expectedComplete: nextMetadata.expectedAddrWitnessesComplete,
    requireComplete: false,
  });
  if (completeWitnessSet(actual, nextMetadata)) {
    return makeCompleteTx(assembled.tx, nextMetadata, context);
  }
  if (options.allowPartial === true) {
    return makePartiallySignedTx(assembled.tx, nextMetadata, context);
  }
  assertExpectedAddrWitnesses({
    actual,
    expected: nextMetadata.expectedAddrWitnessKeyHashes,
    expectedComplete: nextMetadata.expectedAddrWitnessesComplete,
    requireComplete: true,
  });
  throw new SigningError("Incomplete partial witness assembly");
};

export class PartiallySignedTx {
  readonly #txCbor: Buffer;
  readonly #txId: Buffer;
  readonly #metadata: CompleteTxMetadata;
  readonly #context?: CompleteTxContext;
  readonly txHex: string;
  readonly txIdHex: string;

  constructor(
    tx: MidgardNativeTxFullV1,
    metadata: CompleteTxMetadata,
    context?: CompleteTxContext,
    token?: symbol,
  ) {
    if (token !== partiallySignedTxConstructorToken) {
      throw new BuilderInvariantError(
        "PartiallySignedTx constructor is internal; use CompleteTx.assemble(..., { allowPartial: true })",
      );
    }
    this.#txCbor = encodeMidgardNativeTxCanonicalV1(tx);
    assertConsensusTransaction(
      this.#txCbor,
      context?.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE_V1,
    );
    this.txHex = this.#txCbor.toString("hex");
    this.#txId = computeMidgardNativeTxIdV1(tx);
    this.txIdHex = this.#txId.toString("hex");
    this.#metadata = cloneCompleteTxMetadata(metadata);
    this.#context = context;
  }

  get tx(): MidgardNativeTxFullV1 {
    return decodeMidgardNativeTxFullV1FromCanonicalCbor(this.#txCbor);
  }

  get txCbor(): Buffer {
    return Buffer.from(this.#txCbor);
  }

  get txId(): Buffer {
    return Buffer.from(this.#txId);
  }

  get metadata(): CompleteTxMetadata {
    return cloneCompleteTxMetadata(this.#metadata);
  }

  toCBOR(): string {
    return this.txHex;
  }

  toHash(): string {
    return this.txIdHex;
  }

  toJSON(): MidgardTxJson {
    return {
      txId: this.txIdHex,
      txCbor: this.txHex,
      metadata: midgardJsonSafe(this.metadata),
    };
  }

  producedOutputs(): readonly MidgardUtxo[] {
    return localUtxosFromTx(this.tx, this.expectedNetworkIdNumber()).map(
      cloneUtxo,
    );
  }

  producedOutput(outputIndex: number): MidgardUtxo {
    return localUtxoAt(this.tx, outputIndex, this.expectedNetworkIdNumber());
  }

  private expectedNetworkIdNumber(): number | undefined {
    if (this.#context?.networkId === undefined) {
      return undefined;
    }
    return Number(this.#context.networkId);
  }

  assemble(
    bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
    options: AssemblePartialWitnessOptions = {},
  ): CompleteTx | PartiallySignedTx {
    return assemblePartialWitnessBundles(
      this.tx,
      this.#metadata,
      this.#context,
      bundles,
      options,
    );
  }

  toPartialWitnessBundle(): MidgardPartialWitnessBundleV1 {
    return partialWitnessBundleFromWitnesses(
      this.tx,
      decodeAddrWitnesses(this.tx.witnessSet.addrTxWitsPreimageCbor),
    );
  }

  toPartialWitnessBundleCbor(): Buffer {
    return encodePartialWitnessBundle(this.toPartialWitnessBundle());
  }
}

const makePartiallySignedTx = (
  tx: MidgardNativeTxFullV1,
  metadata: CompleteTxMetadata,
  context?: CompleteTxContext,
): PartiallySignedTx =>
  new PartiallySignedTx(
    tx,
    metadata,
    context,
    partiallySignedTxConstructorToken,
  );

export class SubmittedTx {
  readonly #tx: CompleteTx;
  readonly #admission: SubmitTxResult;
  readonly #provider: MidgardProvider;
  readonly txIdHex: string;

  constructor(
    tx: CompleteTx,
    admission: SubmitTxResult,
    provider: MidgardProvider,
    token?: symbol,
  ) {
    if (token !== submittedTxConstructorToken) {
      throw new BuilderInvariantError(
        "SubmittedTx constructor is internal; use CompleteTx.submit()",
      );
    }
    this.#tx = tx;
    this.#admission = admission;
    this.#provider = provider;
    this.txIdHex = tx.txIdHex;
  }

  get tx(): CompleteTx {
    return this.#tx;
  }

  get admission(): SubmitTxResult {
    return { ...this.#admission };
  }

  toCBOR(): string {
    return this.#tx.toCBOR();
  }

  toHash(): string {
    return this.txIdHex;
  }

  toJSON(): MidgardTxJson & { readonly admission: SubmitTxResult } {
    return {
      ...this.#tx.toJSON(),
      admission: this.admission,
    };
  }

  producedOutputs(): readonly MidgardUtxo[] {
    return this.#tx.producedOutputs();
  }

  producedOutput(outputIndex: number): MidgardUtxo {
    return this.#tx.producedOutput(outputIndex);
  }

  async status(): Promise<TxStatus> {
    return assertTxStatusMatches(
      this.txIdHex,
      await this.#provider.getTxStatus(this.txIdHex),
    );
  }

  statusProgram(): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.status());
  }

  statusSafe(): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.status());
  }

  async awaitStatus(options: AwaitTxOptions = {}): Promise<TxStatus> {
    return pollTxStatus(
      options.provider ?? this.#provider,
      this.txIdHex,
      options,
    );
  }

  awaitStatusProgram(options: AwaitTxOptions = {}): MidgardEffect<TxStatus> {
    return midgardProgram(() => this.awaitStatus(options));
  }

  awaitStatusSafe(
    options: AwaitTxOptions = {},
  ): Promise<MidgardResult<TxStatus, LucidMidgardError>> {
    return midgardSafe(() => this.awaitStatus(options));
  }
}

const makeSubmittedTx = (
  tx: CompleteTx,
  admission: SubmitTxResult,
  provider: MidgardProvider,
): SubmittedTx =>
  new SubmittedTx(tx, admission, provider, submittedTxConstructorToken);

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

export type ReadFromOptions = {
  readonly trustedReferenceScripts?: readonly TrustedReferenceScriptMetadata[];
};

const maxOptionalBigInt = (
  left: bigint | undefined,
  right: bigint | undefined,
): bigint | undefined =>
  left === undefined
    ? right
    : right === undefined
      ? left
      : left > right
        ? left
        : right;

const minOptionalBigInt = (
  left: bigint | undefined,
  right: bigint | undefined,
): bigint | undefined =>
  left === undefined
    ? right
    : right === undefined
      ? left
      : left < right
        ? left
        : right;

const assertComposableScriptState = (scripts: BuilderScriptState): void => {
  assertNoDuplicateStrings(
    scripts.datumWitnesses
      .map((datum) => datum.hash)
      .filter((hash): hash is string => hash !== undefined),
    "Duplicate datum witness",
  );
  assertNoDuplicateStrings(
    scripts.observers.map((observer) => observer.scriptHash),
    "Duplicate observer intent",
  );
  assertNoDuplicateStrings(
    scripts.receiveRedeemers.map((entry) => entry.scriptHash),
    "Duplicate receive redeemer",
  );
  assertNoDuplicateStrings(
    scripts.spendRedeemers.map(outRefLabel),
    "Duplicate spend redeemer",
  );
  assertNoDuplicateStrings(
    scripts.referenceScriptMetadata.map(outRefLabel),
    "Duplicate trusted reference script metadata",
  );
  assertNoDuplicateStrings(
    scripts.mints
      .filter((mint) => mint.redeemer !== undefined)
      .map((mint) => mint.policyId),
    "Duplicate mint redeemer for policy",
  );
};

const composeStates = (states: readonly BuilderState[]): BuilderState => {
  const [first, ...rest] = states;
  if (first === undefined) {
    throw new BuilderInvariantError("compose requires at least one builder");
  }
  let validityIntervalStart = first.validityIntervalStart;
  let validityIntervalEnd = first.validityIntervalEnd;
  let minimumFee = first.minimumFee;
  for (const state of rest) {
    if (state.networkId !== first.networkId) {
      throw new BuilderInvariantError(
        "Cannot compose builders with different state network ids",
        `left=${String(first.networkId)} right=${String(state.networkId)}`,
      );
    }
    validityIntervalStart = maxOptionalBigInt(
      validityIntervalStart,
      state.validityIntervalStart,
    );
    validityIntervalEnd = minOptionalBigInt(
      validityIntervalEnd,
      state.validityIntervalEnd,
    );
    minimumFee = maxOptionalBigInt(minimumFee, state.minimumFee);
  }
  const requiredSigners = states.flatMap((state) => state.requiredSigners);
  assertNoDuplicateStrings(requiredSigners, "Duplicate required signer");
  const scriptStates = states.map((state) => cloneScripts(state.scripts));
  const scripts: BuilderScriptState = {
    spendRedeemers: scriptStates.flatMap((scripts) => scripts.spendRedeemers),
    referenceScriptMetadata: scriptStates.flatMap(
      (scripts) => scripts.referenceScriptMetadata,
    ),
    scripts: scriptStates.flatMap((scripts) => scripts.scripts),
    datumWitnesses: scriptStates.flatMap((scripts) => scripts.datumWitnesses),
    mints: scriptStates.flatMap((scripts) => scripts.mints),
    observers: scriptStates.flatMap((scripts) => scripts.observers),
    receiveRedeemers: scriptStates.flatMap(
      (scripts) => scripts.receiveRedeemers,
    ),
  };
  assertComposableScriptState(scripts);
  const composed: BuilderState = {
    spendInputs: states.flatMap((state) => state.spendInputs.map(cloneUtxo)),
    referenceInputs: states.flatMap((state) =>
      state.referenceInputs.map(cloneUtxo),
    ),
    outputs: states.flatMap((state) => state.outputs.map(cloneOutput)),
    requiredSigners,
    validityIntervalStart,
    validityIntervalEnd,
    minimumFee,
    networkId: first.networkId,
    scripts,
    composition: {
      fragmentCount: states.reduce(
        (count, state) => count + (state.composition?.fragmentCount ?? 1),
        0,
      ),
    },
  };
  assertUniqueUtxos(composed.spendInputs, composed.referenceInputs);
  assertValidityInterval(composed);
  return composed;
};

const hasDefinedProperty = <K extends PropertyKey>(
  value: object,
  property: K,
): value is object & Record<K, unknown> =>
  Object.prototype.hasOwnProperty.call(value, property) &&
  (value as Record<K, unknown>)[property] !== undefined;

const scriptHex = (
  scriptRef: NonNullable<MidgardUtxo["output"]["scriptRef"]>,
): string => {
  try {
    return normalizeHex(scriptRef.script, {
      fieldName: "scriptRef.script",
      allowEmpty: true,
    });
  } catch {
    throw new BuilderInvariantError("UTxO scriptRef script must be hex");
  }
};

const scriptRefsCompatible = (
  supplied: MidgardUtxo["output"]["scriptRef"],
  decoded: MidgardUtxo["output"]["scriptRef"],
): boolean => {
  if (supplied === undefined) {
    return true;
  }
  if (supplied === null) {
    return decoded === undefined || decoded === null;
  }
  if (decoded === undefined || decoded === null) {
    return false;
  }
  return (
    scriptHex(supplied) === scriptHex(decoded) && supplied.type === decoded.type
  );
};

const normalizeUtxo = (utxo: MidgardUtxo): MidgardUtxo => {
  const normalized = normalizeOutRef(utxo);
  const outRefCbor = utxoOutRefCbor(utxo);

  let decodedOutput: ReturnType<typeof decodeMidgardTxOutput>;
  try {
    decodedOutput = decodeMidgardTxOutput(utxoOutputCbor(utxo));
  } catch (cause) {
    throw new BuilderInvariantError(
      "Invalid UTxO outputCbor",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  if (utxo.output.address !== decodedOutput.address) {
    throw new BuilderInvariantError("UTxO address does not match output CBOR");
  }
  if (!assetsEqual(utxo.output.assets, decodedOutput.assets)) {
    throw new BuilderInvariantError("UTxO assets do not match output CBOR");
  }
  const datum = hasDefinedProperty(utxo.output, "datum")
    ? utxo.output.datum
    : undefined;
  const decodedDatum = decodedOutput.txOutput.datum;
  if (
    datum !== undefined &&
    JSON.stringify(datum) !== JSON.stringify(decodedDatum ?? null)
  ) {
    throw new BuilderInvariantError("UTxO datum does not match output CBOR");
  }
  const scriptRef = hasDefinedProperty(utxo.output, "scriptRef")
    ? utxo.output.scriptRef
    : undefined;
  if (!scriptRefsCompatible(scriptRef, decodedOutput.txOutput.scriptRef)) {
    throw new BuilderInvariantError(
      "UTxO scriptRef does not match output CBOR",
    );
  }

  return {
    txHash: normalized.txHash,
    outputIndex: normalized.outputIndex,
    output: {
      address: decodedOutput.txOutput.address,
      assets: { ...decodedOutput.txOutput.assets },
      datum: datum === undefined ? decodedOutput.txOutput.datum : datum,
      scriptRef:
        scriptRef === undefined ? decodedOutput.txOutput.scriptRef : scriptRef,
    },
    cbor: {
      outRef: outRefCbor,
      output: Buffer.from(decodedOutput.outputCbor),
    },
  };
};

const normalizeProviderUtxo = (
  utxo: MidgardUtxo,
  endpoint: string,
): MidgardUtxo => {
  try {
    return normalizeUtxo(utxo);
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "Provider returned invalid Midgard UTxO",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const normalizeProviderUtxos = (
  utxos: readonly MidgardUtxo[],
  endpoint: string,
): readonly MidgardUtxo[] => {
  if (!Array.isArray(utxos)) {
    throw new ProviderPayloadError(
      endpoint,
      "Provider UTxO result must be an array",
    );
  }
  return utxos.map((utxo) => normalizeProviderUtxo(utxo, endpoint));
};

const normalizeWalletInputUtxos = (
  utxos: readonly MidgardUtxo[],
  source: WalletInputSource,
  expectedNetworkId: number | undefined,
): readonly MidgardUtxo[] => {
  const seen = new Set<string>();
  return utxos.map((utxo) => {
    const normalized = normalizeUtxo(utxo);
    assertAddressNetwork(utxoAddress(normalized), expectedNetworkId);
    const label = outRefLabel(normalized);
    if (seen.has(label)) {
      throw new BuilderInvariantError(`Duplicate ${source} UTxO outref`, label);
    }
    seen.add(label);
    return normalized;
  });
};

const cloneUtxoOverrideSnapshot = (
  snapshot: UtxoOverrideSnapshot | undefined,
): UtxoOverrideSnapshot | undefined =>
  snapshot === undefined
    ? undefined
    : {
        generation: snapshot.generation,
        utxos: snapshot.utxos.map(cloneUtxo),
      };

const normalizeSignerKeyHash = (signer: string): string | undefined => {
  try {
    return normalizeHex(signer, {
      fieldName: "required signer key hash",
      byteLength: 28,
    });
  } catch {
    return undefined;
  }
};

const normalizeAssetUnit = (unit: AssetUnit): AssetUnit => {
  if (typeof unit !== "string") {
    throw new BuilderInvariantError("Asset unit must be a string");
  }
  if (unit.trim().toLowerCase() === "lovelace") {
    return "lovelace";
  }
  let normalized: string;
  try {
    normalized = normalizeHex(unit, { fieldName: "asset unit" });
  } catch {
    throw new BuilderInvariantError(
      "Asset unit must be lovelace or policy-id plus hex asset-name",
      unit,
    );
  }
  if (normalized.length < 56 || normalized.length > 120) {
    throw new BuilderInvariantError(
      "Asset unit must be lovelace or policy-id plus hex asset-name",
      unit,
    );
  }
  return normalized;
};

const normalizeAddressQuery = (
  address: Address,
  expectedNetworkId: number | undefined,
  endpoint: string,
): Address => {
  if (typeof address !== "string") {
    throw new ProviderPayloadError(
      endpoint,
      "Midgard address queries require a bech32 address string",
    );
  }
  let normalized: Address;
  try {
    normalized = encodeMidgardAddressText(midgardAddressFromText(address));
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "address must be a valid Midgard bech32 address",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  assertAddressNetwork(normalized, expectedNetworkId);
  return normalized;
};

const normalizeMissingMode = (
  options: UtxosByOutRefOptions | undefined,
): "error" | "omit" => {
  const mode = options?.missing ?? "error";
  if (mode !== "error" && mode !== "omit") {
    throw new BuilderInvariantError(
      'utxosByOutRef missing option must be "error" or "omit"',
      String(mode),
    );
  }
  return mode;
};

const assertRequestedOutRefsUnique = (
  outRefs: readonly OutRef[],
  endpoint: string,
): readonly OutRef[] => {
  const normalized = outRefs.map(normalizeOutRef);
  assertNoDuplicateStrings(
    normalized.map(({ txHash, outputIndex }) => `${txHash}#${outputIndex}`),
    "Duplicate requested outref",
  );
  if (normalized.length === 0) {
    throw new BuilderInvariantError(
      "OutRef query must include at least one outref",
      endpoint,
    );
  }
  return normalized;
};

const orderedUtxosByOutRef = async (
  provider: MidgardProvider,
  outRefs: readonly OutRef[],
  options?: UtxosByOutRefOptions,
): Promise<readonly MidgardUtxo[]> => {
  const endpoint = "/utxos?by-outrefs";
  const requested = assertRequestedOutRefsUnique(outRefs, endpoint);
  const missingMode = normalizeMissingMode(options);
  const raw =
    provider.getUtxosByOutRefs === undefined
      ? await Promise.all(
          requested.map(async (outRef) => provider.getUtxoByOutRef(outRef)),
        ).then((items) =>
          items.filter((item): item is MidgardUtxo => item !== undefined),
        )
      : await provider.getUtxosByOutRefs(requested);
  const requestedLabels = requested.map(
    ({ txHash, outputIndex }) => `${txHash}#${outputIndex}`,
  );
  const requestedLabelSet = new Set(requestedLabels);
  const byLabel = new Map<string, MidgardUtxo>();
  for (const utxo of normalizeProviderUtxos(raw, endpoint)) {
    const label = outRefLabel(utxo);
    if (!requestedLabelSet.has(label)) {
      throw new ProviderPayloadError(
        endpoint,
        "Provider returned an unrequested outref",
        label,
      );
    }
    if (byLabel.has(label)) {
      throw new ProviderPayloadError(
        endpoint,
        "Provider returned a duplicate outref",
        label,
      );
    }
    byLabel.set(label, utxo);
  }
  const ordered: MidgardUtxo[] = [];
  for (const label of requestedLabels) {
    const found = byLabel.get(label);
    if (found === undefined) {
      if (missingMode === "error") {
        throw new ProviderPayloadError(
          endpoint,
          "Missing requested UTxO",
          label,
        );
      }
      continue;
    }
    ordered.push(cloneUtxo(found));
  }
  return ordered;
};

const datumExpectedHash = (
  options: DatumOfOptions | undefined,
): string | undefined => {
  const expected =
    typeof options === "string" ? options : options?.expectedHash;
  return expected === undefined
    ? undefined
    : normalizeHashHex(expected, "datum hash", 32);
};

const inlineDatumFromUtxo = (
  utxo: MidgardUtxo,
  options?: DatumOfOptions,
): Buffer => {
  const endpoint = "/datum";
  const normalized = normalizeProviderUtxo(utxo, endpoint);
  if (
    normalized.output.datum === undefined ||
    normalized.output.datum === null
  ) {
    throw new ProviderPayloadError(endpoint, "UTxO does not contain a datum");
  }
  const datumCbor = Buffer.from(normalized.output.datum.cbor, "hex");
  const datum = CML.PlutusData.from_cbor_bytes(datumCbor);
  const expectedHash = datumExpectedHash(options);
  const actualHash = CML.hash_plutus_data(datum).to_hex();
  if (expectedHash !== undefined && expectedHash !== actualHash) {
    throw new ProviderPayloadError(
      endpoint,
      "Datum hash mismatch",
      `expected=${expectedHash} actual=${actualHash}`,
    );
  }
  return datumCbor;
};

const assertValidityInterval = (state: BuilderState): void => {
  if (
    state.validityIntervalStart !== undefined &&
    state.validityIntervalEnd !== undefined &&
    state.validityIntervalStart > state.validityIntervalEnd
  ) {
    throw new BuilderInvariantError(
      "validityIntervalStart must be less than or equal to validityIntervalEnd",
      `${state.validityIntervalStart.toString()} > ${state.validityIntervalEnd.toString()}`,
    );
  }
};

const normalizeValidationConcurrency = (
  value: number | undefined,
  fieldName: string,
): number => {
  if (value === undefined) {
    return 1;
  }
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new BuilderInvariantError(
      `${fieldName} must be a non-negative safe integer`,
    );
  }
  return value;
};

const rejectRuntimeKindOption = (
  options: object | undefined,
  methodName: string,
): void => {
  if (options !== undefined && "kind" in options) {
    throw new BuilderInvariantError(
      `${methodName} does not accept an output kind option; use the explicit helper`,
    );
  }
};

const normalizeTrustedReferenceScriptMetadata = (
  metadata: TrustedReferenceScriptMetadata,
): TrustedReferenceScriptMetadata => {
  if (typeof metadata !== "object" || metadata === null) {
    throw new BuilderInvariantError(
      "Reference script metadata must be an object",
    );
  }
  const candidate = metadata as {
    readonly txHash?: unknown;
    readonly outputIndex?: unknown;
    readonly language?: unknown;
    readonly scriptHash?: unknown;
    readonly scriptCborHash?: unknown;
  };
  if (typeof candidate.txHash !== "string") {
    throw new BuilderInvariantError(
      "Reference script metadata txHash must be a string",
    );
  }
  if (
    typeof candidate.outputIndex !== "number" ||
    !Number.isSafeInteger(candidate.outputIndex)
  ) {
    throw new BuilderInvariantError(
      "Reference script metadata outputIndex must be a safe integer",
    );
  }
  if (typeof candidate.scriptHash !== "string") {
    throw new BuilderInvariantError(
      "Reference script metadata scriptHash must be a string",
    );
  }
  if (
    candidate.scriptCborHash !== undefined &&
    typeof candidate.scriptCborHash !== "string"
  ) {
    throw new BuilderInvariantError(
      "Reference script metadata scriptCborHash must be a string when present",
    );
  }
  const outRef = normalizeOutRef({
    txHash: candidate.txHash,
    outputIndex: candidate.outputIndex,
  } as OutRef);
  return {
    ...outRef,
    language: normalizeScriptLanguage(
      candidate.language,
      "Reference script metadata language",
    ),
    scriptHash: normalizeScriptHash(
      candidate.scriptHash,
      "reference script metadata scriptHash",
    ),
    scriptCborHash:
      candidate.scriptCborHash === undefined
        ? undefined
        : normalizeHashHex(
            candidate.scriptCborHash,
            "reference script metadata scriptCborHash",
            32,
          ),
  };
};

const normalizeTrustedReferenceScriptMetadataList = (
  metadata:
    | TrustedReferenceScriptMetadata
    | readonly TrustedReferenceScriptMetadata[],
): readonly TrustedReferenceScriptMetadata[] => {
  const entries = Array.isArray(metadata) ? metadata : [metadata];
  return entries.map(normalizeTrustedReferenceScriptMetadata);
};

const assertWalletOwnsInputs = async (
  wallet: MidgardWallet | undefined,
  inputs: readonly MidgardUtxo[],
  source: WalletInputSource,
): Promise<void> => {
  if (inputs.length === 0) {
    return;
  }
  if (wallet === undefined) {
    throw new BuilderInvariantError(
      `${source} wallet inputs require a selected wallet`,
    );
  }
  const walletKeyHash = await wallet.keyHash();
  for (const input of inputs) {
    const inputKeyHash = paymentPubKeyHashFromUtxo(input);
    if (inputKeyHash === undefined) {
      throw new BuilderInvariantError(
        `${source} wallet input is not spendable by a public-key wallet`,
        outRefLabel(input),
      );
    }
    if (inputKeyHash !== walletKeyHash) {
      throw new BuilderInvariantError(
        `${source} wallet input does not belong to the selected wallet`,
        `outref=${outRefLabel(input)} wallet_key_hash=${walletKeyHash} input_key_hash=${inputKeyHash}`,
      );
    }
  }
};

const shouldBalance = (options: CompleteOptions): boolean =>
  options.changeAddress !== undefined ||
  options.feePolicy !== undefined ||
  options.maxFeeIterations !== undefined;

const shouldBalanceWithWalletDefault = (
  options: CompleteOptions,
  hasSelectedWallet: boolean,
): boolean => shouldBalance(options) || hasSelectedWallet;

const resolveMaxFeeIterations = (value: number | undefined): number => {
  if (value === undefined) {
    return 20;
  }
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new BuilderInvariantError(
      "maxFeeIterations must be a non-negative safe integer",
    );
  }
  return value;
};

const normalizeFeePolicy = (feePolicy: {
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
}): FeePolicy => ({
  minFeeA: normalizeNonNegativeBigInt(feePolicy.minFeeA, "minFeeA"),
  minFeeB: normalizeNonNegativeBigInt(feePolicy.minFeeB, "minFeeB"),
});

const maxBigInt = (left: bigint, right: bigint): bigint =>
  left > right ? left : right;

const resolveInitialFee = (
  state: BuilderState,
  fee: bigint | number | undefined,
): bigint =>
  maxBigInt(
    normalizeNonNegativeBigInt(fee ?? 0n, "fee"),
    state.minimumFee ?? 0n,
  );

const validationLevel = (
  options: CompleteOptions,
): "none" | LocalPreflightPhase => options.localValidation ?? "none";

const normalizeLocalPreflightPhase = (
  phase: LocalPreflightPhase,
): LocalPreflightPhase => {
  if (phase === "phase-a" || phase === "phase-b") {
    return phase;
  }
  throw new BuilderInvariantError(
    'local preflight phase must be "phase-a" or "phase-b"',
    String(phase),
  );
};

const queuedTxFromComplete = (tx: CompleteTx): QueuedTx => ({
  txId: tx.txId,
  txCbor: tx.txCbor,
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecarV1(
    tx.programMaterial,
  ),
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

const localValidationRejected = (
  rejected: readonly RejectedTx[],
): LocalValidationReport["rejected"] =>
  rejected.map((item) => ({
    txId: item.txId.toString("hex"),
    code: item.code,
    detail: item.detail,
  }));

const localValidationReportFromPhaseA = (
  result: PhaseAResult,
  preStateSource?: LocalValidationPreStateSource,
): LocalValidationReport => ({
  phase: "phase-a",
  acceptedTxIds: result.accepted.map((item) =>
    item.ledgerTx.txId.toString("hex"),
  ),
  rejected: localValidationRejected(result.rejected),
  ...(preStateSource === undefined
    ? {}
    : { preStateSource, preStateAuthoritative: false as const }),
});

const localValidationReportFromPhaseB = (
  result: PhaseBResultWithPatch,
  preStateSource?: LocalValidationPreStateSource,
): LocalValidationReport => ({
  phase: "phase-b",
  acceptedTxIds: result.accepted.map((item) =>
    item.ledgerTx.txId.toString("hex"),
  ),
  rejected: localValidationRejected(result.rejected),
  ...(preStateSource === undefined
    ? {}
    : { preStateSource, preStateAuthoritative: false as const }),
  statePatch: {
    deletedOutRefs: result.statePatch.deletedOutRefs,
    upsertedOutRefs: result.statePatch.upsertedOutRefs.map(
      ([outRefHex, output]) => [outRefHex, Buffer.from(output).toString("hex")],
    ),
  },
});

const assertAcceptedLocalValidation = (
  report: LocalValidationReport,
  txIdHex: string,
): void => {
  if (
    report.rejected.length === 0 &&
    report.acceptedTxIds.length === 1 &&
    report.acceptedTxIds[0] === txIdHex
  ) {
    return;
  }
  throw new BuilderInvariantError(
    `Local ${report.phase} validation rejected transaction`,
    JSON.stringify(
      {
        expectedTxId: txIdHex,
        acceptedTxIds: report.acceptedTxIds,
        rejected: report.rejected,
      },
      null,
      2,
    ),
  );
};

const runSharedLocalPreflight = async ({
  completed,
  phase,
  provider,
  options,
  missingPreStateMessage,
}: {
  readonly completed: CompleteTx;
  readonly phase: LocalPreflightPhase;
  readonly provider: MidgardProvider;
  readonly options: LocalPreflightOptions;
  readonly missingPreStateMessage: string;
}): Promise<LocalValidationReport> => {
  if (phase === "phase-b" && options.localPreState === undefined) {
    throw new BuilderInvariantError(missingPreStateMessage);
  }

  const params = await provider.getProtocolParameters();
  const protocolInfo = await provider.getProtocolInfo();
  assertTxNetworkMatchesExpected(completed.tx, params.networkId, "Provider");
  const concurrency = normalizeValidationConcurrency(
    options.validationConcurrency,
    "validationConcurrency",
  );
  const phaseAConfig: PhaseAConfig = {
    expectedNetworkId: params.networkId,
    minFeeA: params.minFeeA,
    minFeeB: params.minFeeB,
    concurrency,
    strictnessProfile: params.strictnessProfile ?? "production",
    consensusProfile: protocolInfo.consensusProfile,
  };
  const phaseA = await Effect.runPromise(
    runPhaseAValidation([queuedTxFromComplete(completed)], phaseAConfig),
  );
  if (phase === "phase-a" || phaseA.rejected.length > 0) {
    return localValidationReportFromPhaseA(phaseA, options.localPreStateSource);
  }

  const preState = materializeLocalPreState(options.localPreState!);
  const nowCardanoSlotNo =
    options.nowCardanoSlotNo === undefined
      ? (params.currentSlot ?? (await provider.getCurrentSlot()))
      : normalizeNonNegativeBigInt(
          options.nowCardanoSlotNo,
          "nowCardanoSlotNo",
        );
  const phaseBConfig: PhaseBConfig = {
    nowCardanoSlotNo,
    bucketConcurrency: concurrency,
    enforceScriptBudget: options.enforceScriptBudget ?? true,
  };
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(phaseA.accepted, preState, phaseBConfig),
  );
  return localValidationReportFromPhaseB(
    phaseB,
    options.localPreStateSource ?? "explicit",
  );
};

const materializeLocalPreState = (
  preState: LocalValidationPreState,
): Map<string, Buffer> => {
  if (Array.isArray(preState)) {
    return new Map(
      preState.map((entry) => [
        entry[LedgerColumns.OUTREF].toString("hex"),
        Buffer.from(entry[LedgerColumns.OUTPUT]),
      ]),
    );
  }
  const mapPreState = preState as ReadonlyMap<string, Uint8Array>;
  return new Map(
    [...mapPreState.entries()].map(([outRefHex, output]) => [
      outRefHex,
      Buffer.from(output),
    ]),
  );
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
    tx: MidgardNativeTxFullV1,
    metadata: Omit<CompleteTxMetadata, "localValidation">,
    options: CompleteOptions,
    programMaterial: readonly MidgardCekProgramMaterialEntryV1[] = [],
  ): Promise<CompleteTx> {
    const context: CompleteTxContext = {
      provider: this.context.provider.provider,
      wallet: () => this.context.wallet,
      networkId: configNetworkId(this.context.config),
      maxSubmitTxCborBytes:
        this.context.config.submissionLimits.maxSubmitTxCborBytes,
      consensusProfile: this.context.config.consensusProfile,
      programMaterial,
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
    programMaterial: readonly MidgardCekProgramMaterialEntryV1[] = [],
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
    );
  }

  async complete(options: CompleteOptions = {}): Promise<CompleteTx> {
    const clonedState = cloneState(this.state);
    const prepared = isMidgardConsensusProfileV1(
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
    const tx = materializeMidgardNativeTxFromCanonicalV1(canonical);
    const txCbor = encodeMidgardNativeTxCanonicalV1(tx);
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
    const prepared = isMidgardConsensusProfileV1(
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

const makeTxBuilder = (
  context: BuilderContextSnapshot,
  state: BuilderState,
): TxBuilder => new TxBuilder(context, state, txBuilderConstructorToken);

const readOnlyWalletFromAddress = (
  address: Address,
  expectedNetworkId: number | undefined,
): MidgardWallet => {
  assertAddressNetwork(address, expectedNetworkId);
  const keyHash = paymentKeyHashFromAddress(address);
  return {
    address: async () => address,
    keyHash: async () => keyHash,
    signBodyHash: async () => {
      throw new SigningError(
        "Read-only Midgard wallet cannot sign body hashes",
      );
    },
  };
};

const normalizeLucidMidgardConfig = (
  networkOrConfig?: Network | LucidMidgardConfig,
): LucidMidgardConfig =>
  typeof networkOrConfig === "string"
    ? { network: networkOrConfig }
    : { ...(networkOrConfig ?? {}) };

const networkForSeedWallet = (network: string | undefined): Network => {
  if (network === "Mainnet" || network === "Preprod" || network === "Preview") {
    return network;
  }
  throw new BuilderInvariantError(
    "Selecting a seed wallet requires a known Cardano network",
    network ?? "undefined",
  );
};

const validateSelectedWalletForConfig = async (
  wallet: MidgardWallet | undefined,
  config: LucidMidgardConfigSnapshot,
): Promise<void> => {
  if (wallet === undefined || config.networkId === undefined) {
    return;
  }
  try {
    assertAddressNetwork(await wallet.address(), config.networkId);
  } catch (cause) {
    if (
      cause instanceof SigningError &&
      cause.message.includes("does not expose an address")
    ) {
      return;
    }
    throw cause;
  }
};

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
    programMaterial: readonly MidgardCekProgramMaterialEntryV1[] = [],
  ): CompleteTxContext {
    return {
      provider: this.#providerSnapshot.provider,
      wallet: () => this.selectedWallet,
      networkId: configNetworkId(this.#config),
      maxSubmitTxCborBytes: this.#config.submissionLimits.maxSubmitTxCborBytes,
      consensusProfile: this.#config.consensusProfile,
      programMaterial,
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
          this.completeTxContext(programMaterial),
        )
      : makeCompleteTx(tx, metadata, this.completeTxContext(programMaterial));
  }

  async walletAddress(): Promise<Address> {
    return this.wallet().address();
  }
}
