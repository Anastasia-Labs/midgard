/**
 * `da-bond`: operate the pooled DA committee bond (spec #685, ticket #691).
 *
 * - `status` reads the pool: state, backing against `da_bond_lovelace`, and
 *   `unlock_at` while a withdrawal is pending.
 * - `top-up` adds lovelace from any wallet (`TopUp` is permissionless).
 * - `withdraw begin|cancel|complete --build-unsigned <file>` builds an owner
 *   quorum transaction and writes it, unsigned, to a file; nothing is
 *   submitted. Each owner witnesses it offline with `da-bond witness`
 *   (`da-bond-files.ts`), and `assemble` checks the quorum and submits.
 *
 * The chain logic takes a `DaBondContext`, so emulator tests drive it without
 * a manifest; `loadDaBondContext` builds the production context from a
 * verified manifest and authenticates only what these commands read: the pool
 * reference scripts and the DA params governor.
 */
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  type Network,
  type Provider,
  type SlotConfig,
  toUnit,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect, Either } from "effect";

import {
  fetchLocalOgmiosShelleyGenesisSlotConfig,
  fetchLocalOgmiosSubmitSlotSnapshot,
} from "../local-ledger-slot.js";
import { customSlotConfigFromShelleyGenesis } from "../lucid-time.js";
import {
  makeNodeKupmios,
  nativeLedgerSettingsFromEnv,
} from "../services/native-ledger.js";
import { awaitExactTransactionConfirmation } from "../transactions/utils.js";
import { parseNetworkArgument } from "./address-from-seed.js";
import {
  authenticatedManifestReference,
  availabilityParametersFromManifest,
  manifestReferenceScriptAuthPolicy,
  mintingValidatorOf,
  spendingValidatorOf,
} from "./availability-challenge-deployment.js";
import { readDeploymentManifestFile } from "./contract-deployment-info.js";
import {
  DA_BOND_UNSIGNED_FORMAT,
  daBondSigningKeyFromSecret,
  type DaBondUnsignedFile,
  type DaBondWithdrawAction,
  readDaBondSecretEnv,
  readDaBondUnsignedFile,
  readDaBondWitnessFile,
  writeNewJsonFile,
} from "./da-bond-files.js";
import { resolveKupmiosUrls } from "./l1-utxos.js";

/** Everything the chain-facing `da-bond` commands read or call. */
export type DaBondContext = Readonly<{
  lucid: LucidEvolution;
  /** The deployment's network, recorded in and checked against every file. */
  network: string;
  manifestId: string;
  poolValidator: SDK.AuthenticatedValidator;
  /** The authenticated `da-bond-pool spending` reference-script UTxO. */
  poolSpendingReference: UTxO;
  parameters: SDK.DaAvailabilityParameters;
  /** Where the DA params UTxO lives and the governor NFT that marks it. */
  daParamsGovernor: Readonly<{ address: string; unit: string }>;
  /** The deployment profile's `timing.da_bond_withdraw_delay_ms`. */
  withdrawDelayMs: bigint;
  /** The current POSIX time in milliseconds (the emulator's clock in tests). */
  now: () => number;
  /**
   * Submits a signed transaction, waits for it to land, returns its id. A
   * failure after the node accepted it names the transaction
   * (`daBondAfterSubmitError`).
   */
  submit: (txCbor: string) => Promise<string>;
}>;

const KEY_HASH = /^[0-9a-f]{56}$/u;
const LOVELACE = /^[1-9][0-9]*$/u;

const outRefLabel = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const parseOutRef = (
  value: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const [txHash, outputIndex] = value.split("#");
  return { txHash: txHash!, outputIndex: Number(outputIndex) };
};

const isoOf = (ms: bigint): string => new Date(Number(ms)).toISOString();

const parseLovelace = (value: string | undefined, flag: string): bigint => {
  if (value === undefined || !LOVELACE.test(value.trim())) {
    throw new Error(`${flag} must be a positive whole number of lovelace`);
  }
  return BigInt(value.trim());
};

/** `--valid-for-ms`: a positive whole number of milliseconds. */
const parsePositiveMs = (value: string, flag: string): bigint => {
  if (!LOVELACE.test(value.trim())) {
    throw new Error(`${flag} must be a positive whole number of milliseconds`);
  }
  return BigInt(value.trim());
};

const requireAddress = (value: string, flag: string): string => {
  try {
    getAddressDetails(value);
  } catch {
    throw new Error(`${flag} must be a bech32 Cardano address`);
  }
  if (!value.startsWith("addr")) {
    throw new Error(`${flag} must be a bech32 Cardano address`);
  }
  return value;
};

const paymentKeyHashOf = (address: string, flag: string): string => {
  const payment = getAddressDetails(
    requireAddress(address, flag),
  ).paymentCredential;
  if (payment?.type !== "Key") {
    throw new Error(`${flag} must have a payment key credential`);
  }
  return payment.hash;
};

const parseSigners = (value: string): readonly string[] => {
  const signers = value
    .split(",")
    .map((signer) => signer.trim().toLowerCase())
    .filter((signer) => signer.length > 0);
  if (signers.length === 0 || signers.some((s) => !KEY_HASH.test(s))) {
    throw new Error(
      "--signers must list the signing owners' 28-byte payment key hashes, comma separated",
    );
  }
  return signers;
};

type PoolView = Readonly<{ utxo: UTxO; datum: SDK.DaBondPoolDatum }>;

const fetchPool = async (ctx: DaBondContext): Promise<PoolView> => {
  const { utxo, datum } = await SDK.fetchDaBondPool(ctx.lucid, {
    policyId: ctx.poolValidator.policyId,
    address: ctx.poolValidator.spendingScriptAddress,
  });
  return { utxo, datum };
};

/** The status readout, with bigints as decimal strings. */
const statusJson = (ctx: DaBondContext, pool: PoolView) => {
  const status = SDK.daBondPoolStatus({
    lovelace: pool.utxo.assets.lovelace ?? 0n,
    datum: pool.datum,
    parameters: ctx.parameters,
  });
  return {
    poolOutRef: outRefLabel(pool.utxo),
    state: status.state,
    lovelace: status.lovelace.toString(),
    backing: status.backing.toString(),
    requiredBacking: status.requiredBacking.toString(),
    belowBond: status.belowBond,
    ...(status.unlockAt === undefined
      ? {}
      : {
          unlockAt: status.unlockAt.toString(),
          unlockAtIso: isoOf(status.unlockAt),
          unlockable: BigInt(ctx.now()) >= status.unlockAt,
        }),
  };
};

export type DaBondStatus = ReturnType<typeof statusJson>;

/** `da-bond status`. */
export const daBondStatusCommand = async (
  ctx: DaBondContext,
): Promise<DaBondStatus> => statusJson(ctx, await fetchPool(ctx));

/**
 * A failure after the node accepted a transaction, fatal like any other. It
 * names the transaction and what is known of it (`submitted`: its
 * confirmation was not seen; `confirmed`: it landed and only the status read
 * failed), so the operator does not run the command again: a second `top-up`
 * would pay the pool twice, and the funder cannot take lovelace back out.
 */
export const daBondAfterSubmitError = (
  txHash: string,
  state: "submitted" | "confirmed",
  cause: unknown,
): Error => {
  const reason = cause instanceof Error ? cause.message : String(cause);
  return new Error(
    state === "submitted"
      ? `Transaction ${txHash} was submitted, but waiting for its confirmation failed: ${reason}. Check whether ${txHash} landed before retrying; do not submit it again`
      : `Transaction ${txHash} is confirmed, but reading the pool status failed: ${reason}; do not submit it again`,
    { cause },
  );
};

/**
 * The production `submit`: send, then wait for exact confirmation. A failed
 * wait (a Kupo poll error aborts it) still names the submitted transaction.
 */
export const daBondSubmitAndConfirm =
  (
    send: (txCbor: string) => Promise<string>,
    confirm: (txHash: string) => Promise<unknown>,
  ) =>
  async (txCbor: string): Promise<string> => {
    const txHash = await send(txCbor);
    try {
      await confirm(txHash);
    } catch (error) {
      throw daBondAfterSubmitError(txHash, "submitted", error);
    }
    return txHash;
  };

/**
 * The `submit` `loadDaBondContext` installs: send through the chain provider,
 * then wait for `lucid`'s exact confirmation, a failed wait naming the hash.
 */
export const daBondChainSubmit = (
  provider: Readonly<{ submitTx: (txCbor: string) => Promise<string> }>,
  lucid: LucidEvolution,
): ((txCbor: string) => Promise<string>) =>
  daBondSubmitAndConfirm(
    (txCbor) => provider.submitTx(txCbor),
    (txHash) => awaitExactTransactionConfirmation(lucid, txHash),
  );

/** The status readout after `txHash` is confirmed; a failed read names it. */
const statusAfterSubmit = (
  ctx: DaBondContext,
  txHash: string,
): Promise<DaBondStatus> =>
  daBondStatusCommand(ctx).catch((error: unknown) => {
    throw daBondAfterSubmitError(txHash, "confirmed", error);
  });

type DaParamsView = Readonly<{ utxo: UTxO; datum: SDK.DaParamsDatum }>;

const isDaParamsUtxo = (ctx: DaBondContext, utxo: UTxO): boolean =>
  utxo.address === ctx.daParamsGovernor.address &&
  utxo.assets[ctx.daParamsGovernor.unit] === 1n;

const daParamsDatumOf = (utxo: UTxO): SDK.DaParamsDatum => {
  if (typeof utxo.datum !== "string" || utxo.datumHash != null) {
    throw new Error(
      `DA params UTxO ${outRefLabel(utxo)} carries no inline datum`,
    );
  }
  return Data.from(utxo.datum, SDK.DaParamsDatum);
};

/** The one live DA params UTxO: the governor NFT at the governor address. */
const fetchDaParams = async (ctx: DaBondContext): Promise<DaParamsView> => {
  const candidates = (
    await ctx.lucid.utxosAtWithUnit(
      ctx.daParamsGovernor.address,
      ctx.daParamsGovernor.unit,
    )
  ).filter((utxo) => isDaParamsUtxo(ctx, utxo));
  if (candidates.length !== 1) {
    throw new Error(
      `Expected exactly one DA params UTxO at the governor address, found ${candidates.length.toString()}`,
    );
  }
  const utxo = candidates[0]!;
  return { utxo, datum: daParamsDatumOf(utxo) };
};

/** Runs an SDK pool builder, mapping its refusal to a one-line message. */
const runPoolBuilder = async (
  program: Effect.Effect<TxBuilder, SDK.DaBondPoolBuildError>,
  refusal: (error: SDK.DaBondPoolBuildError) => string,
): Promise<TxBuilder> => {
  const result = await Effect.runPromise(Effect.either(program));
  if (Either.isLeft(result)) {
    throw new Error(refusal(result.left), { cause: result.left });
  }
  return result.right;
};

type RefusalFacts = Readonly<{
  action: string;
  pool: PoolView;
  parameters: SDK.DaAvailabilityParameters;
  daParams?: SDK.DaParamsDatum;
  signers?: readonly string[];
  amount?: bigint;
  validFrom?: bigint;
}>;

/**
 * One line per validator refusal the SDK prechecks mirror; any other reason
 * keeps the SDK's own message.
 */
const refusalMessage = (
  error: SDK.DaBondPoolBuildError,
  facts: RefusalFacts,
): string => {
  const { action, pool, parameters } = facts;
  switch (error.reason) {
    case "below_min_top_up":
      return `Refusing ${action}: ${(facts.amount ?? 0n).toString()} lovelace is below da_bond_min_top_up_lovelace ${parameters.da_bond_min_top_up_lovelace.toString()}; nothing was submitted`;
    case "before_unlock": {
      // The SDK compares the slot-aligned lower bound; report the values it
      // compared.
      const compared = /validFrom=(\d+),unlock_at=(\d+)/u.exec(
        String(error.cause),
      );
      const lower = BigInt(compared?.[1] ?? facts.validFrom ?? 0n);
      const unlockAt = BigInt(
        compared?.[2] ??
          (pool.datum === "Bonded" ? 0n : pool.datum.Withdrawing.unlock_at),
      );
      return `Refusing ${action}: the pool unlocks at unlock_at ${unlockAt.toString()} (${isoOf(unlockAt)}), after this transaction's lower bound ${lower.toString()} (${isoOf(lower)}); retry at or after unlock_at`;
    }
    case "insufficient_signers":
      return SDK.daBondPoolQuorumShortfallMessage(
        new Set(facts.signers ?? []).size,
        facts.daParams?.update_threshold ?? 0n,
      );
    case "pool_not_bonded":
      return `Refusing ${action}: the pool is already Withdrawing (unlock_at ${pool.datum === "Bonded" ? "" : pool.datum.Withdrawing.unlock_at.toString()}); cancel or complete that withdrawal first`;
    case "pool_not_withdrawing":
      return `Refusing ${action}: the pool is Bonded; begin a withdrawal first`;
    case "amount_exceeds_backing":
      return `Refusing ${action}: amount ${(facts.amount ?? 0n).toString()} lovelace exceeds the pool backing ${SDK.daBondPoolBacking(
        {
          lovelace: pool.utxo.assets.lovelace ?? 0n,
          parameters,
        },
      ).toString()} (lovelace above the ${parameters.da_bond_pool_floor_lovelace.toString()} floor)`;
    case "signer_not_owner":
      return `Refusing ${action}: signer ${String(error.cause)} is not a DA params owner (owners: ${(facts.daParams?.owners ?? []).join(", ")})`;
    case "duplicate_signer":
      return `Refusing ${action}: signer ${String(error.cause)} is listed twice in --signers`;
    case "below_floor":
    case "invalid_amount":
    case "invalid_da_params":
    case "invalid_pool":
    case "invalid_pool_address":
    case "invalid_validity_range":
    case "invalid_witness":
    case "missing_witness":
    case "reference_script_mismatch":
      return `Refusing ${action}: ${error.message}`;
  }
};

export type DaBondTopUpOptions = Readonly<{
  amount: string;
  /**
   * The funding wallet: a bech32 payment key (its enterprise address), or a
   * mnemonic selected as the node selects `L1_OPERATOR_SEED_PHRASE` (its base
   * address, account 0).
   */
  walletSecret: string;
}>;

/** `da-bond top-up`: the wallet adds `amount` lovelace and submits. */
export const daBondTopUpCommand = async (
  ctx: DaBondContext,
  options: DaBondTopUpOptions,
) => {
  const amount = parseLovelace(options.amount, "--amount");
  const pool = await fetchPool(ctx);
  const secret = options.walletSecret.trim();
  if (secret.startsWith("ed25519_sk1") || secret.startsWith("ed25519e_sk1")) {
    ctx.lucid.selectWallet.fromPrivateKey(secret);
  } else {
    // Validates the secret without echoing it.
    daBondSigningKeyFromSecret(secret);
    ctx.lucid.selectWallet.fromSeed(secret);
  }
  const tx = await runPoolBuilder(
    SDK.buildTopUpDaBondPoolTxProgram(ctx.lucid, {
      poolValidator: ctx.poolValidator,
      parameters: ctx.parameters,
      pool: { utxo: pool.utxo },
      amount,
      referenceScripts: { daBondPoolSpending: ctx.poolSpendingReference },
    }),
    (error) =>
      refusalMessage(error, {
        action: "top-up",
        pool,
        parameters: ctx.parameters,
        amount,
      }),
  );
  const signed = await (await tx.complete({ localUPLCEval: true })).sign
    .withWallet()
    .complete();
  const txHash = await ctx.submit(signed.toCBOR());
  return {
    action: "top-up",
    txHash,
    amount: amount.toString(),
    previousPoolOutRef: outRefLabel(pool.utxo),
    status: await statusAfterSubmit(ctx, txHash),
  };
};

export type DaBondWithdrawStep = "begin" | "cancel" | "complete";

const WITHDRAW_ACTION: Readonly<
  Record<DaBondWithdrawStep, DaBondWithdrawAction>
> = {
  begin: "BeginWithdraw",
  cancel: "CancelWithdraw",
  complete: "CompleteWithdraw",
};

export type DaBondWithdrawBuildOptions = Readonly<{
  feeAddress: string;
  signers: string;
  buildUnsigned: string;
  validForMs?: string;
  amount?: string;
  to?: string;
}>;

/** The body's validity bounds, in POSIX milliseconds. */
const txValidity = (
  lucid: LucidEvolution,
  txCbor: string,
): { readonly validFrom?: bigint; readonly validTo?: bigint } => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  try {
    const body = tx.body();
    try {
      const start = body.validity_interval_start();
      const ttl = body.ttl();
      return {
        ...(start === undefined
          ? {}
          : { validFrom: BigInt(lucid.slotToUnixTime(Number(start))) }),
        ...(ttl === undefined
          ? {}
          : { validTo: BigInt(lucid.slotToUnixTime(Number(ttl))) }),
      };
    } finally {
      body.free();
    }
  } finally {
    tx.free();
  }
};

/** The body's spent inputs and reference inputs, as `txHash#index`. */
const txInputs = (
  txCbor: string,
): { readonly inputs: string[]; readonly referenceInputs: string[] } => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  const labels = (list: CML.TransactionInputList | undefined): string[] => {
    if (list === undefined) return [];
    const out: string[] = [];
    for (let i = 0; i < list.len(); i += 1) {
      const input = list.get(i);
      out.push(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
      input.free();
    }
    list.free();
    return out;
  };
  try {
    const body = tx.body();
    try {
      return {
        inputs: labels(body.inputs()),
        referenceInputs: labels(body.reference_inputs()),
      };
    } finally {
      body.free();
    }
  } finally {
    tx.free();
  }
};

/**
 * `da-bond withdraw begin|cancel|complete --build-unsigned <file>`: builds the
 * owner quorum transaction with `--signers` as its required signers and the
 * `--fee-address` wallet paying fee and collateral, writes the
 * unsigned-transaction file, and submits nothing.
 */
export const daBondWithdrawBuildCommand = async (
  ctx: DaBondContext,
  step: DaBondWithdrawStep,
  options: DaBondWithdrawBuildOptions,
) => {
  const action = WITHDRAW_ACTION[step];
  const label = `withdraw ${step}`;
  const feePayerKeyHash = paymentKeyHashOf(options.feeAddress, "--fee-address");
  const signers = parseSigners(options.signers);
  if (step !== "complete" && (options.amount ?? options.to) !== undefined) {
    throw new Error(`--amount and --to apply only to withdraw complete`);
  }
  if (step !== "begin" && options.validForMs !== undefined) {
    throw new Error(`--valid-for-ms applies only to withdraw begin`);
  }
  const pool = await fetchPool(ctx);
  const daParams = await fetchDaParams(ctx);
  const now = BigInt(ctx.now());
  const quorum = {
    poolValidator: ctx.poolValidator,
    parameters: ctx.parameters,
    pool: { utxo: pool.utxo },
    daParamsUtxo: daParams.utxo,
    signerKeyHashes: signers,
    referenceScripts: { daBondPoolSpending: ctx.poolSpendingReference },
  };
  const facts = {
    action: label,
    pool,
    parameters: ctx.parameters,
    daParams: daParams.datum,
    signers,
  };
  let program: Effect.Effect<TxBuilder, SDK.DaBondPoolBuildError>;
  let amount: bigint | undefined;
  let destination: string | undefined;
  if (step === "begin") {
    const validForMs =
      options.validForMs === undefined
        ? SDK.MAX_VALIDITY_RANGE_LENGTH_MS
        : parsePositiveMs(options.validForMs, "--valid-for-ms");
    if (validForMs > SDK.MAX_VALIDITY_RANGE_LENGTH_MS) {
      throw new Error(
        `--valid-for-ms must be at most ${SDK.MAX_VALIDITY_RANGE_LENGTH_MS.toString()} (the maximum validity range)`,
      );
    }
    program = SDK.buildBeginDaBondPoolWithdrawTxProgram(ctx.lucid, {
      ...quorum,
      withdrawDelayMs: ctx.withdrawDelayMs,
      validity: { validFrom: now, validTo: now + validForMs },
    });
  } else if (step === "cancel") {
    program = SDK.buildCancelDaBondPoolWithdrawTxProgram(ctx.lucid, quorum);
  } else {
    if (options.amount === undefined || options.to === undefined) {
      throw new Error("withdraw complete needs both --amount and --to");
    }
    amount = parseLovelace(options.amount, "--amount");
    destination = requireAddress(options.to, "--to");
    program = SDK.buildCompleteDaBondPoolWithdrawTxProgram(ctx.lucid, {
      ...quorum,
      amount,
      destination,
      validity: { validFrom: now },
    });
  }
  ctx.lucid.selectWallet.fromAddress(
    options.feeAddress,
    await ctx.lucid.utxosAt(options.feeAddress),
  );
  const tx = await runPoolBuilder(program, (error) =>
    refusalMessage(error, { ...facts, amount, validFrom: now }),
  );
  const txCbor = (await tx.complete({ localUPLCEval: true })).toCBOR();
  const validity = txValidity(ctx.lucid, txCbor);
  const unsigned: DaBondUnsignedFile = {
    format: DA_BOND_UNSIGNED_FORMAT,
    network: ctx.network,
    manifestId: ctx.manifestId,
    action,
    txCbor,
    txBodyHash: SDK.daBondPoolTxBodyHash(txCbor),
    requiredSigners: SDK.daBondPoolTxRequiredSigners(txCbor),
    feePayerKeyHash,
    poolOutRef: outRefLabel(pool.utxo),
    daParamsOutRef: outRefLabel(daParams.utxo),
    ...(validity.validFrom === undefined
      ? {}
      : { validFrom: validity.validFrom.toString() }),
    ...(validity.validTo === undefined
      ? {}
      : { validTo: validity.validTo.toString() }),
  };
  writeNewJsonFile(options.buildUnsigned, unsigned);
  const unlockAt =
    step === "begin" && validity.validTo !== undefined
      ? SDK.daBondPoolUnlockAt({
          validToMs: validity.validTo,
          withdrawDelayMs: ctx.withdrawDelayMs,
        })
      : undefined;
  return {
    action,
    unsignedFile: options.buildUnsigned,
    txBodyHash: unsigned.txBodyHash,
    requiredSigners: unsigned.requiredSigners,
    feePayerKeyHash,
    updateThreshold: daParams.datum.update_threshold.toString(),
    poolOutRef: unsigned.poolOutRef,
    daParamsOutRef: unsigned.daParamsOutRef,
    ...(unsigned.validFrom === undefined
      ? {}
      : { validFrom: unsigned.validFrom }),
    ...(validity.validTo === undefined
      ? {}
      : {
          validTo: validity.validTo.toString(),
          validToIso: isoOf(validity.validTo),
        }),
    ...(unlockAt === undefined
      ? {}
      : { unlockAt: unlockAt.toString(), unlockAtIso: isoOf(unlockAt) }),
    ...(amount === undefined || destination === undefined
      ? {}
      : { amount: amount.toString(), to: destination }),
    submitted: false,
  };
};

const rebuild = "rebuild the unsigned transaction";

/**
 * The DA params UTxO among the transaction's reference inputs, live and
 * authenticated by the governor NFT at the governor address.
 */
const referencedDaParams = async (
  ctx: DaBondContext,
  unsigned: DaBondUnsignedFile,
  referenceInputs: readonly string[],
): Promise<DaParamsView> => {
  if (!referenceInputs.includes(unsigned.daParamsOutRef)) {
    throw new Error(
      `Refusing to assemble: the file's DA params ${unsigned.daParamsOutRef} is not a reference input of its transaction`,
    );
  }
  const live = (
    await ctx.lucid.utxosByOutRef(referenceInputs.map(parseOutRef))
  ).filter((utxo) => isDaParamsUtxo(ctx, utxo));
  const utxo = live.find(
    (candidate) => outRefLabel(candidate) === unsigned.daParamsOutRef,
  );
  if (utxo === undefined) {
    throw new Error(
      `Refusing to assemble: the DA params reference input ${unsigned.daParamsOutRef} is no longer live; ${rebuild}`,
    );
  }
  return { utxo, datum: daParamsDatumOf(utxo) };
};

/**
 * `da-bond assemble`: checks the witnesses against the owner quorum of the
 * live DA params the transaction references (plus the fee payer's witness),
 * refuses a transaction whose pool input or DA params reference is spent or
 * whose validity has passed, then merges the witnesses and submits.
 */
export const daBondAssembleCommand = async (
  ctx: DaBondContext,
  unsignedPath: string,
  witnessPaths: readonly string[],
) => {
  if (witnessPaths.length === 0) {
    throw new Error("assemble needs at least one witness file");
  }
  const unsigned = await readDaBondUnsignedFile(unsignedPath);
  if (unsigned.network !== ctx.network) {
    throw new Error(
      `Refusing to assemble: the file is for network ${unsigned.network}, the deployment is on ${ctx.network}`,
    );
  }
  if (unsigned.manifestId !== ctx.manifestId) {
    throw new Error(
      `Refusing to assemble: the file is for deployment ${unsigned.manifestId}, not ${ctx.manifestId}`,
    );
  }
  const witnessSetCbors: string[] = [];
  for (const path of witnessPaths) {
    const witness = await readDaBondWitnessFile(path);
    if (witness.txBodyHash !== unsigned.txBodyHash) {
      throw new Error(
        `Refusing to assemble: witness ${path} is for transaction ${witness.txBodyHash}, not ${unsigned.txBodyHash}`,
      );
    }
    const keys = SDK.verifiedDaBondPoolWitnessKeyHashes(
      unsigned.txCbor,
      witness.witnessSetCbor,
    );
    if (!keys.includes(witness.keyHash)) {
      throw new Error(
        `Refusing to assemble: witness ${path} claims key ${witness.keyHash} but carries ${keys.join(", ") || "no witness"}`,
      );
    }
    witnessSetCbors.push(witness.witnessSetCbor);
  }
  const { inputs, referenceInputs } = txInputs(unsigned.txCbor);
  if (!inputs.includes(unsigned.poolOutRef)) {
    throw new Error(
      `Refusing to assemble: the file's pool ${unsigned.poolOutRef} is not an input of its transaction`,
    );
  }
  const daParams = await referencedDaParams(ctx, unsigned, referenceInputs);
  let owners: readonly string[];
  try {
    owners = SDK.assertDaBondPoolWitnessQuorum({
      txCbor: unsigned.txCbor,
      witnessSetCbors,
      daParams: daParams.datum,
      requiredKeyHashes: [unsigned.feePayerKeyHash],
    }).ownerKeyHashes;
  } catch (error) {
    // insufficient_signers carries exactly daBondPoolQuorumShortfallMessage.
    if (error instanceof SDK.DaBondPoolBuildError) {
      throw new Error(error.message, { cause: error });
    }
    throw error;
  }
  const [livePool] = await ctx.lucid.utxosByOutRef([
    parseOutRef(unsigned.poolOutRef),
  ]);
  if (livePool === undefined) {
    throw new Error(
      `Refusing to assemble: the pool input ${unsigned.poolOutRef} is no longer live; ${rebuild}`,
    );
  }
  if (
    livePool.address !== ctx.poolValidator.spendingScriptAddress ||
    livePool.assets[SDK.daBondPoolUnit(ctx.poolValidator.policyId)] !== 1n
  ) {
    throw new Error(
      `Refusing to assemble: ${unsigned.poolOutRef} is not this deployment's DA bond pool`,
    );
  }
  const validity = txValidity(ctx.lucid, unsigned.txCbor);
  const now = BigInt(ctx.now());
  if (validity.validTo !== undefined && now >= validity.validTo) {
    throw new Error(
      `Refusing to assemble: the transaction's validity ended at ${validity.validTo.toString()} (${isoOf(validity.validTo)}); ${rebuild}`,
    );
  }
  if (validity.validFrom !== undefined && now < validity.validFrom) {
    throw new Error(
      `Refusing to assemble: the transaction is valid only from ${validity.validFrom.toString()} (${isoOf(validity.validFrom)}); retry then`,
    );
  }
  const assembled = SDK.assembleDaBondPoolTx(unsigned.txCbor, witnessSetCbors);
  const txHash = await ctx.submit(assembled);
  return {
    action: unsigned.action,
    txHash,
    ownerWitnesses: owners,
    updateThreshold: daParams.datum.update_threshold.toString(),
    status: await statusAfterSubmit(ctx, txHash),
  };
};

export type DaBondChainOptions = Readonly<{
  manifest: string;
  kupoUrl?: string;
  ogmiosUrl?: string;
}>;

/**
 * A `Custom` deployment whose slot mapping the local Ogmios did not give:
 * the command stops before it builds anything.
 */
export class DaBondCustomSlotMappingError extends Error {
  override readonly name = "DaBondCustomSlotMappingError";
}

/**
 * The network a verified manifest names. The public networks go through the
 * CLI network parser unchanged; `Custom` (a local devnet) is admitted here,
 * and only here, because `daBondLucid` gives it the node's slot mapping.
 */
const daBondNetwork = (manifestNetwork: string): Network =>
  manifestNetwork === "Custom"
    ? "Custom"
    : parseNetworkArgument(manifestNetwork);

const runOrThrow = async <A>(program: Effect.Effect<A, Error>): Promise<A> =>
  Either.getOrThrowWith(
    await Effect.runPromise(Effect.either(program)),
    (error) => error,
  );

/**
 * The `Custom` slot mapping, derived exactly as the node derives it
 * (`services/lucid.ts`): a live submit-slot snapshot and the Shelley genesis
 * from the local Ogmios, checked against each other. Never a configured
 * `zeroTime`: a wrong one shifts every validity interval, and the pool's
 * `unlock_at` is anchored at one.
 */
const daBondCustomSlotConfig = async (
  ogmiosUrl: string,
): Promise<SlotConfig> => {
  const stage = async <A>(label: string, run: () => Promise<A>) => {
    try {
      return await run();
    } catch (cause) {
      throw new DaBondCustomSlotMappingError(
        `Refusing the Custom deployment: ${label} from the local Ogmios at ${ogmiosUrl} failed: ${cause instanceof Error ? cause.message : String(cause)}`,
        { cause },
      );
    }
  };
  const snapshot = await stage("the submit-slot snapshot", () =>
    runOrThrow(fetchLocalOgmiosSubmitSlotSnapshot({ ogmiosUrl })),
  );
  const genesis = await stage("the Shelley genesis query", () =>
    runOrThrow(fetchLocalOgmiosShelleyGenesisSlotConfig({ ogmiosUrl })),
  );
  return stage("the slot mapping check", async () =>
    customSlotConfigFromShelleyGenesis(genesis, snapshot),
  );
};

/**
 * Lucid on the deployment's network. Mainnet, Preprod and Preview keep
 * Lucid's built-in slot mapping and never query Ogmios for it; `Custom` has
 * none built in and takes `daBondCustomSlotConfig`'s, or the command stops
 * before Lucid is built.
 */
export const daBondLucid = async (input: {
  readonly provider: Provider;
  readonly network: Network;
  readonly ogmiosUrl: string;
}): Promise<LucidEvolution> => {
  const slotConfig =
    input.network === "Custom"
      ? await daBondCustomSlotConfig(input.ogmiosUrl)
      : undefined;
  return Lucid(input.provider, input.network, {
    evaluator: createScalusEvaluator(),
    ...(slotConfig === undefined ? {} : { slotConfig }),
  });
};

/**
 * The production context: a verified finalized manifest, local Kupmios, and
 * only the references these commands read, each authenticated: the pool's
 * reference scripts (manifest role outputs carrying their
 * reference-script-auth token) and the DA params governor's address and NFT.
 */
export const loadDaBondContext = async (
  options: DaBondChainOptions,
  env: NodeJS.ProcessEnv = process.env,
): Promise<DaBondContext> => {
  const manifest = readDeploymentManifestFile(options.manifest);
  verifyFinalizedDeploymentManifest(manifest);
  const manifestNetwork = daBondNetwork(manifest.network);
  const connection = resolveKupmiosUrls({
    kupoUrl: options.kupoUrl,
    ogmiosUrl: options.ogmiosUrl,
    env,
  });
  const provider = makeNodeKupmios({
    kupoUrl: connection.kupoUrl,
    ogmiosUrl: connection.ogmiosUrl,
    network: manifestNetwork,
    nativeLedger: nativeLedgerSettingsFromEnv(env),
  });
  const lucid = await daBondLucid({
    provider,
    network: manifestNetwork,
    ogmiosUrl: connection.ogmiosUrl,
  });
  const network = lucid.config().network;
  if (network === undefined || network !== manifest.network) {
    throw new Error("da-bond network differs from the verified deployment");
  }
  const authPolicy = manifestReferenceScriptAuthPolicy(manifest);
  const spending = await authenticatedManifestReference(
    lucid,
    manifest,
    authPolicy,
    "daBondPoolSpend",
    "da-bond-pool spending",
  );
  const minting = await authenticatedManifestReference(
    lucid,
    manifest,
    authPolicy,
    "daBondPoolMint",
    "da-bond-pool minting",
  );
  const governorSpend = manifest.contracts.daParamsGovernorSpend?.scriptHash;
  const governorMint = manifest.contracts.daParamsGovernorMint?.scriptHash;
  if (governorSpend === undefined || governorMint === undefined) {
    throw new Error("Deployment omits the DA params governor");
  }
  return {
    lucid,
    network,
    manifestId: manifest.manifestId,
    poolValidator: {
      ...spendingValidatorOf(network, spending.scriptRef),
      ...mintingValidatorOf(minting.scriptRef),
    },
    poolSpendingReference: spending,
    parameters: availabilityParametersFromManifest(manifest),
    daParamsGovernor: {
      address: credentialToAddress(network, {
        type: "Script",
        hash: governorSpend,
      }),
      unit: toUnit(governorMint, SDK.DA_PARAMS_ASSET_NAME),
    },
    withdrawDelayMs: BigInt(
      manifest.deploymentProfile.timing.da_bond_withdraw_delay_ms,
    ),
    now: () => Date.now(),
    submit: daBondChainSubmit(provider, lucid),
  };
};

/** `da-bond top-up` from the CLI: the funding secret comes from one env var. */
export const runDaBondTopUp = async (
  options: DaBondChainOptions & { amount: string; walletSeedEnv: string },
  env: NodeJS.ProcessEnv = process.env,
) => {
  const walletSecret = readDaBondSecretEnv(
    env,
    options.walletSeedEnv,
    "--wallet-seed-env",
  );
  daBondSigningKeyFromSecret(walletSecret);
  parseLovelace(options.amount, "--amount");
  return daBondTopUpCommand(await loadDaBondContext(options, env), {
    amount: options.amount,
    walletSecret,
  });
};

/** `da-bond status` from the CLI. */
export const runDaBondStatus = async (
  options: DaBondChainOptions,
  env: NodeJS.ProcessEnv = process.env,
): Promise<DaBondStatus> =>
  daBondStatusCommand(await loadDaBondContext(options, env));

/** `da-bond withdraw begin|cancel|complete --build-unsigned` from the CLI. */
export const runDaBondWithdrawBuild = async (
  step: DaBondWithdrawStep,
  options: DaBondChainOptions & DaBondWithdrawBuildOptions,
  env: NodeJS.ProcessEnv = process.env,
) =>
  daBondWithdrawBuildCommand(
    await loadDaBondContext(options, env),
    step,
    options,
  );

/** `da-bond assemble` from the CLI. */
export const runDaBondAssemble = async (
  options: DaBondChainOptions,
  unsignedPath: string,
  witnessPaths: readonly string[],
  env: NodeJS.ProcessEnv = process.env,
) =>
  daBondAssembleCommand(
    await loadDaBondContext(options, env),
    unsignedPath,
    witnessPaths,
  );
