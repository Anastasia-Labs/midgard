import * as SDK from "@al-ft/midgard-sdk";
import { type Network, type TxBuilder } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import { parseNetworkArgument } from "./address-from-seed.js";
import {
  type DaBondContext,
  daParamsDatumOf,
  type DaParamsView,
  fetchDaParams,
  fetchPool,
  isDaParamsUtxo,
  isoOf,
  outRefLabel,
  parseLovelace,
  parseOutRef,
  parsePositiveMs,
  parseSigners,
  paymentKeyHashOf,
  requireAddress,
  runPoolBuilder,
  statusAfterSubmit,
} from "./da-bond.da-bond-context.js";
import {
  type DaBondWithdrawBuildOptions,
  type DaBondWithdrawStep,
  refusalMessage,
  txInputs,
  txValidity,
  WITHDRAW_ACTION,
} from "./da-bond.refusal-message.js";
import {
  DA_BOND_UNSIGNED_FORMAT,
  type DaBondUnsignedFile,
  readDaBondUnsignedFile,
  readDaBondWitnessFile,
  writeNewJsonFile,
} from "./da-bond-files.js";

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
export const daBondNetwork = (manifestNetwork: string): Network =>
  manifestNetwork === "Custom"
    ? "Custom"
    : parseNetworkArgument(manifestNetwork);

export const runOrThrow = async <A>(
  program: Effect.Effect<A, Error>,
): Promise<A> =>
  Either.getOrThrowWith(
    await Effect.runPromise(Effect.either(program)),
    (error) => error,
  );
