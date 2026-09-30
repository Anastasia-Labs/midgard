import {
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  type PhaseAConfig,
  type PhaseBConfig,
  type QueuedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import {
  type AssemblePartialWitnessOptions,
  assertConsensusTransaction,
  assertTxNetworkMatchesExpected,
  type AwaitTxOptions,
  completeTxMetadataWithAddrWitnesses,
  completeWitnessSet,
  type LocalPreflightOptions,
  type LocalPreflightPhase,
  localValidationReportFromPhaseA,
  localValidationReportFromPhaseB,
  materializeLocalPreState,
  type MidgardEffect,
  midgardJsonSafe,
  midgardProgram,
  midgardSafe,
  type MidgardTxJson,
  normalizeLocalPreflightPhase,
  normalizeValidationConcurrency,
  partialBundleInputs,
  partiallySignedTxConstructorToken,
  privateKeyFromInput,
  type SubmitOptions,
  submittedTxConstructorToken,
  trustedCompleteTxs,
} from "./builder.midgard-json-safe.js";
import {
  assertExpectedAddrWitnesses,
  localUtxoAt,
  localUtxosFromTx,
} from "./builder/imported-tx.js";
import {
  cloneCompleteTxContext,
  cloneCompleteTxMetadata,
  cloneReferenceOutputsByOutRef,
  type CompleteTxContext,
  type CompleteTxMetadata,
} from "./builder/metadata.js";
import { normalizeNonNegativeBigInt } from "./builder/normalizers.js";
import { assertCompleteTxProgramMaterial } from "./builder/script-materialization.js";
import { cloneUtxo } from "./builder/state.js";
import {
  assertSubmitAdmissionMatches,
  assertSubmitSizeWithinLimit,
  assertTxStatusMatches,
  pollTxStatus,
  resolveProvider,
  resolveSubmitSizeLimit,
} from "./builder/status.js";
import {
  addrWitnessKeyHashes,
  addrWitnessMetadata,
  applyAddrWitnessesToTx,
  assertPartialBundleMatchesTx,
  decodeAddrWitnesses,
  decodeImportAddrWitnesses,
  encodePartialWitnessBundle,
  type MidgardPartialWitnessBundleV1,
  normalizeVKeyWitnessInput,
  parsePartialWitnessBundle,
  partialWitnessBundleFromWitnesses,
  type PartialWitnessBundleInput,
  signMidgardNativeTx,
  type VKeyWitnessInput,
} from "./builder/witness-bundle.js";
import {
  BuilderInvariantError,
  LucidMidgardError,
  SigningError,
} from "./core/errors.js";
import {
  type LocalValidationReport,
  type MidgardResult,
  type MidgardUtxo,
  type SubmitTxResult,
  type TxStatus,
} from "./core/types.js";
import type { MidgardProvider } from "./provider.js";
import {
  assertVKeyWitness,
  type ExternalBodyHashSigner,
  makeVKeyWitness,
  type MidgardWallet,
  type PrivateKey,
  type VKeyWitness,
  walletFromExternalSigner,
} from "./wallet.js";

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
    const bodyHash = computeMidgardNativeTxId(nativeTx);
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
    tx: MidgardNativeTxFull,
    metadata: CompleteTxMetadata,
    context?: CompleteTxContext,
  ) {
    this.#txCbor = encodeMidgardNativeTxCanonical(tx);
    assertConsensusTransaction(
      this.#txCbor,
      context?.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE,
    );
    this.txHex = this.#txCbor.toString("hex");
    this.#txId = computeMidgardNativeTxId(tx);
    this.txIdHex = this.#txId.toString("hex");
    this.#metadata = cloneCompleteTxMetadata(metadata);
    this.#context = cloneCompleteTxContext(context);
    this.partialSign = this.makePartialSignApi();
    this.sign = Object.assign(
      (wallet?: MidgardWallet): Promise<CompleteTx> =>
        this.signWithWallet(wallet),
      this.partialSign,
    );
  }

  get tx(): MidgardNativeTxFull {
    return decodeMidgardNativeTxFullFromCanonicalCbor(this.#txCbor);
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

  get programMaterial(): readonly MidgardCekProgramMaterialEntry[] {
    return (this.#context?.programMaterial ?? []).map((entry) => ({
      ...entry,
      root: Buffer.from(entry.root) as MidgardCekProgramMaterialEntry["root"],
      preimage: Buffer.from(entry.preimage),
    }));
  }

  get resolvedReferenceOutputsByOutRef():
    | ReadonlyMap<string, Uint8Array>
    | undefined {
    return cloneReferenceOutputsByOutRef(
      this.#context?.resolvedReferenceOutputsByOutRef,
    );
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
        txByteLength: encodeMidgardNativeTxCanonical(signedTx).length,
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

export const makeCompleteTx = (
  tx: MidgardNativeTxFull,
  metadata: CompleteTxMetadata,
  context?: CompleteTxContext,
): CompleteTx => {
  assertCompleteTxProgramMaterial(
    tx,
    context?.resolvedReferenceOutputsByOutRef,
    context?.programMaterial ?? [],
  );
  const completed = new CompleteTx(tx, metadata, context);
  trustedCompleteTxs.add(completed);
  return completed;
};

const assemblePartialWitnessBundles = (
  tx: MidgardNativeTxFull,
  metadata: CompleteTxMetadata,
  context: CompleteTxContext | undefined,
  bundles: PartialWitnessBundleInput | readonly PartialWitnessBundleInput[],
  options: AssemblePartialWitnessOptions,
): CompleteTx | PartiallySignedTx => {
  const inputs = partialBundleInputs(bundles);
  if (inputs.length === 0) {
    throw new SigningError("At least one partial witness bundle is required");
  }
  const bodyHash = computeMidgardNativeTxId(tx);
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
    tx: MidgardNativeTxFull,
    metadata: CompleteTxMetadata,
    context?: CompleteTxContext,
    token?: symbol,
  ) {
    if (token !== partiallySignedTxConstructorToken) {
      throw new BuilderInvariantError(
        "PartiallySignedTx constructor is internal; use CompleteTx.assemble(..., { allowPartial: true })",
      );
    }
    this.#txCbor = encodeMidgardNativeTxCanonical(tx);
    assertConsensusTransaction(
      this.#txCbor,
      context?.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE,
    );
    this.txHex = this.#txCbor.toString("hex");
    this.#txId = computeMidgardNativeTxId(tx);
    this.txIdHex = this.#txId.toString("hex");
    this.#metadata = cloneCompleteTxMetadata(metadata);
    this.#context = cloneCompleteTxContext(context);
  }

  get tx(): MidgardNativeTxFull {
    return decodeMidgardNativeTxFullFromCanonicalCbor(this.#txCbor);
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

export const makePartiallySignedTx = (
  tx: MidgardNativeTxFull,
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

const queuedTxFromComplete = (tx: CompleteTx): QueuedTx => ({
  txId: tx.txId,
  txCbor: tx.txCbor,
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
    tx.programMaterial,
  ),
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

export const runSharedLocalPreflight = async ({
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
