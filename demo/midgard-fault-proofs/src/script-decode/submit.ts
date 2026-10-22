/**
 * Transaction builders for the `script_decode_v1` validator: stage a script's
 * bytes, Bind a run, Walk it, and Finish it into a record. A Walk spends the
 * run and withdraws zero from each class staker that steps one of its
 * segments; each staker's redeemer carries its segments and their links.
 *
 * Coin selection spends only plain coins (no datum, no script, nothing but
 * ada), so a staged chunk (at the wallet address, read by Bind and Walk)
 * stays put. Each `build*Tx` signs without submitting, so a journaled caller can record
 * the exact transaction before it is sent; an optional `validTo` (POSIX ms)
 * makes a transaction the ledger never takes expire instead of staying
 * pending.
 */
import {
  requireReferenceInputIndex,
  type ScriptDecodeStakerClassName,
  type ScriptDecodeValidator,
  type WithdrawalValidator,
} from "@al-ft/midgard-sdk";
import {
  type ScriptDecodeChainedWalk,
  type ScriptDecodeRecord,
  type ScriptDecodeRun,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  CML,
  type LucidEvolution,
  type OutRef,
  type TxBuilder,
  type TxOutput,
  type TxSigned,
  type UTxO,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { isPlainFundingOutput } from "../funding/plain-funding-output.js";
import { DEFAULT_CONFIRMATION_POLL_MS } from "../runtime.js";
import {
  claimOwnOutputs,
  inlineDatumCarriageSlot,
  readOwnOutputs,
} from "../workflow/own-outputs.js";
import {
  SCRIPT_DECODE_CLOSE_REDEEMER,
  scriptDecodeBindRedeemer,
  scriptDecodeFinishRedeemer,
  type ScriptDecodePositionedSegment,
  scriptDecodeRecordDatum,
  scriptDecodeRunDatum,
  scriptDecodeRunTokenName,
  scriptDecodeStagingDatum,
  scriptDecodeStakeRedeemer,
  scriptDecodeWalkRedeemer,
} from "./data.js";

/** Staged bytes per UTxO, leaving room in a 16,384-byte transaction. */
export const SCRIPT_DECODE_STAGING_UTXO_BYTES = 12_288;

/**
 * The validator and a UTxO carrying it as a reference script, and for each
 * class staker a UTxO carrying it. Each staker's reward account must be
 * registered for a Walk to withdraw zero from it.
 */
export type ScriptDecodeDeployment = Readonly<{
  validator: ScriptDecodeValidator;
  reference: UTxO;
  stakerReferences: Readonly<Record<ScriptDecodeStakerClassName, UTxO>>;
}>;

/**
 * One withdraw-zero of a Walk: the staker, a UTxO carrying it as a reference
 * script, and the redeemer's run and segments at their chain positions.
 */
export type ScriptDecodeStake = Readonly<{
  staker: WithdrawalValidator;
  reference: UTxO;
  run: OutRef;
  segments: readonly ScriptDecodePositionedSegment[];
}>;

/** The execution units a built transaction's redeemers declare, summed. */
export type ScriptDecodeExUnits = Readonly<{ mem: bigint; steps: bigint }>;

/**
 * `complete`'s wallet inputs, read fresh (an earlier `complete` pins the
 * wallet's UTxO view): the plain-ADA coins only (`isPlainFundingOutput`, the
 * one funding predicate). A wallet offering nothing else, such as the
 * watcher's reserved wallet (`restrictWorkflowFundingSigner`), gets no
 * preset, so its own collateral survives: Lucid takes collateral only from
 * among preset inputs.
 */
const freshWallet = async (
  lucid: LucidEvolution,
): Promise<{ presetWalletInputs?: UTxO[] }> => {
  lucid.clearUTxOOverride();
  const utxos = await lucid.wallet().getUtxos();
  const plain = utxos.filter(isPlainFundingOutput);
  return plain.length === utxos.length ? {} : { presetWalletInputs: plain };
};

export const transactionExUnits = (txCbor: string): ScriptDecodeExUnits => {
  const transaction = CML.Transaction.from_cbor_hex(txCbor);
  const redeemers = transaction.witness_set().redeemers()?.to_flat_format();
  let mem = 0n;
  let steps = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const exUnits = redeemers!.get(index).ex_units();
    mem += exUnits.mem();
    steps += exUnits.steps();
  }
  return { mem, steps };
};

const requireReference = (reference: UTxO, scriptHash: string): UTxO => {
  if (
    reference.scriptRef == null ||
    validatorToScriptHash(reference.scriptRef) !== scriptHash
  ) {
    throw new Error(
      "script decode: the reference UTxO does not carry the script",
    );
  }
  return reference;
};

const requireScriptReference = (deployment: ScriptDecodeDeployment): UTxO =>
  requireReference(
    deployment.reference,
    deployment.validator.spendingScriptHash,
  );

/**
 * The withdraw-zeros of `walk` from the run at `run`: one per class with a
 * segment, holding that class's segments at their chain positions.
 */
export const scriptDecodeWalkStakes = (
  deployment: ScriptDecodeDeployment,
  run: OutRef,
  walk: ScriptDecodeChainedWalk,
): ScriptDecodeStake[] => {
  const byClass = new Map<
    ScriptDecodeStakerClassName,
    ScriptDecodePositionedSegment[]
  >();
  walk.segments.forEach((segment, position) => {
    const own = byClass.get(segment.within) ?? [];
    own.push({ position, segment });
    byClass.set(segment.within, own);
  });
  return [...byClass].map(([name, segments]) => ({
    staker: deployment.validator.stakers[name],
    reference: deployment.stakerReferences[name],
    run: { txHash: run.txHash, outputIndex: run.outputIndex },
    segments,
  }));
};

const ownTokenOutput =
  (address: string, unit: string) =>
  (output: TxOutput): boolean =>
    output.address === address && (output.assets[unit] ?? 0n) === 1n;

const requireOutputIndex = (
  outputs: readonly TxOutput[],
  matches: (output: TxOutput) => boolean,
  label: string,
): bigint => {
  const indices = outputs.flatMap((output, index) =>
    matches(output) ? [BigInt(index)] : [],
  );
  if (indices.length !== 1) {
    throw new Error(`script decode: ${label} output is not unique`);
  }
  return indices[0]!;
};

const stagingIndices = (
  ctx: Parameters<BuildTxWithRedeemer>[0],
  staging: readonly UTxO[],
): bigint[] =>
  staging.map((utxo) =>
    requireReferenceInputIndex(ctx, utxo, "script decode staging"),
  );

/**
 * A signed script-decode transaction, not yet submitted, and how to find the
 * one output of it a caller continues from.
 */
export type ScriptDecodeSignedTx = Readonly<{
  signed: TxSigned;
  address: string;
  matches: (output: TxOutput) => boolean;
  label: string;
}>;

const until = (tx: TxBuilder, validTo: number | undefined): TxBuilder =>
  validTo === undefined ? tx : tx.validTo(validTo);

/**
 * Submits `built`, awaits it, and returns the output it was built for: read
 * by the out-ref its signed body gives that output, never by a search of
 * the address.
 */
export const submitScriptDecodeTx = async (
  lucid: LucidEvolution,
  { signed, address, matches, label }: ScriptDecodeSignedTx,
): Promise<UTxO> => {
  const [claim] = claimOwnOutputs({ signed, slots: [{ address, matches }] });
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  const found = await readOwnOutputs(
    (outRefs) => lucid.utxosByOutRef([...outRefs]),
    [claim!.outRef],
  );
  const output = found.get(claim!.outRef);
  if (output === undefined || !matches(output)) {
    throw new Error(`script decode: ${label} output not found`);
  }
  return output;
};

/** The staging datums of `scriptBytes`, in order: one per staged UTxO. */
export const scriptDecodeStagingDatums = (
  scriptBytes: Uint8Array,
): string[] => {
  const datums: string[] = [];
  for (
    let offset = 0;
    offset < scriptBytes.length || datums.length === 0;
    offset += SCRIPT_DECODE_STAGING_UTXO_BYTES
  ) {
    datums.push(
      scriptDecodeStagingDatum(
        scriptBytes.subarray(offset, offset + SCRIPT_DECODE_STAGING_UTXO_BYTES),
      ),
    );
  }
  return datums;
};

/** Builds and signs one staging transaction: `datum` paid to `address`. */
export const buildScriptDecodeStageTx = async ({
  lucid,
  datum,
  address,
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly datum: string;
  readonly address: string;
  readonly validTo?: number;
}): Promise<ScriptDecodeSignedTx> => {
  const unsigned = await until(
    lucid
      .newTx()
      .pay.ToAddressWithData(address, { kind: "inline", value: datum }, {}),
    validTo,
  ).complete(await freshWallet(lucid));
  return {
    signed: await unsigned.sign.withWallet().complete(),
    address,
    matches: inlineDatumCarriageSlot(address, datum).matches,
    label: "staging",
  };
};

type BindInput = {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly language: number;
  readonly scriptHashHex: string;
  readonly initialRun: ScriptDecodeRun;
  readonly staging: readonly UTxO[];
};

/** Builds and signs the Bind that opens a run for the staged bytes. */
export const buildScriptDecodeBindTx = async ({
  lucid,
  deployment,
  language,
  scriptHashHex,
  initialRun,
  staging,
  validTo,
}: BindInput & {
  readonly validTo?: number;
}): Promise<ScriptDecodeSignedTx> => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const runUnit = `${policyId}${scriptDecodeRunTokenName(scriptHashHex)}`;
  const matches = ownTokenOutput(spendingScriptAddress, runUnit);
  const redeemer = ((ctx) =>
    scriptDecodeBindRedeemer({
      language,
      staging: stagingIndices(ctx, staging),
      runOutput: requireOutputIndex(ctx.outputs, matches, "run"),
    })) satisfies BuildTxWithRedeemer;
  const unsigned = await until(
    lucid
      .newTx()
      .mintAssets({ [runUnit]: 1n }, redeemer)
      .readFrom([...staging, requireScriptReference(deployment)])
      .pay.ToContract(
        spendingScriptAddress,
        { kind: "inline", value: scriptDecodeRunDatum(initialRun) },
        { [runUnit]: 1n },
      ),
    validTo,
  ).complete({
    localUPLCEval: true,
    ...(await freshWallet(lucid)),
  });
  return {
    signed: await unsigned.sign.withWallet().complete(),
    address: spendingScriptAddress,
    matches,
    label: "run",
  };
};

/** Opens the run for the staged bytes; returns the run UTxO. */
export const bindScriptDecodeRun = async (input: BindInput): Promise<UTxO> =>
  submitScriptDecodeTx(input.lucid, await buildScriptDecodeBindTx(input));

/**
 * Builds and signs one Walk of `runUtxo` to `after` along `stakes`. Throws
 * when the transaction cannot be built, for instance when its segments
 * overrun the transaction budget. `inputs` are further wallet inputs to
 * spend.
 */
export const buildScriptDecodeWalkTx = async ({
  lucid,
  deployment,
  scriptHashHex,
  runUtxo,
  staging,
  after,
  stakes,
  inputs = [],
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly scriptHashHex: string;
  readonly runUtxo: UTxO;
  readonly staging: readonly UTxO[];
  readonly after: ScriptDecodeRun;
  readonly stakes: readonly ScriptDecodeStake[];
  readonly inputs?: readonly UTxO[];
  readonly validTo?: number;
}) => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const runUnit = `${policyId}${scriptDecodeRunTokenName(scriptHashHex)}`;
  const matches = ownTokenOutput(spendingScriptAddress, runUnit);
  const network = lucid.config().network ?? "Custom";
  let tx = lucid
    .newTx()
    .collectFrom([runUtxo], ((ctx) =>
      scriptDecodeWalkRedeemer(
        requireOutputIndex(ctx.outputs, matches, "run"),
      )) satisfies BuildTxWithRedeemer)
    .readFrom([
      ...staging,
      requireScriptReference(deployment),
      ...stakes.map(({ staker, reference }) =>
        requireReference(reference, staker.withdrawalScriptHash),
      ),
    ]);
  if (inputs.length > 0) tx = tx.collectFrom([...inputs]);
  for (const { staker, run, segments } of stakes) {
    tx = tx.withdraw(
      validatorToRewardAddress(network, staker.withdrawalScript),
      0n,
      ((ctx) =>
        scriptDecodeStakeRedeemer({
          run,
          staging: stagingIndices(ctx, staging),
          segments,
        })) satisfies BuildTxWithRedeemer,
    );
  }
  const unsigned = await until(
    tx.pay.ToContract(
      spendingScriptAddress,
      { kind: "inline", value: scriptDecodeRunDatum(after) },
      { [runUnit]: 1n },
    ),
    validTo,
  ).complete({
    localUPLCEval: true,
    ...(await freshWallet(lucid)),
  });
  const signed = await unsigned.sign.withWallet().complete();
  const cbor = signed.toCBOR();
  const built: ScriptDecodeSignedTx = {
    signed,
    address: spendingScriptAddress,
    matches,
    label: "run",
  };
  return {
    exUnits: transactionExUnits(cbor),
    txBytes: cbor.length / 2,
    tx: built,
    submit: () => submitScriptDecodeTx(lucid, built),
  };
};

/** Builds and signs the honest Walk `walk` of `runUtxo`. */
export const buildScriptDecodeWalk = ({
  walk,
  ...rest
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly scriptHashHex: string;
  readonly runUtxo: UTxO;
  readonly staging: readonly UTxO[];
  readonly walk: ScriptDecodeChainedWalk;
  readonly validTo?: number;
}) =>
  buildScriptDecodeWalkTx({
    ...rest,
    after: walk.after,
    stakes: scriptDecodeWalkStakes(rest.deployment, rest.runUtxo, walk),
  });

type FinishInput = {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly scriptHashHex: string;
  readonly runUtxo: UTxO;
  readonly record: ScriptDecodeRecord;
};

/** Builds and signs the Finish: burns the run's token, mints its record. */
export const buildScriptDecodeFinishTx = async ({
  lucid,
  deployment,
  scriptHashHex,
  runUtxo,
  record,
  validTo,
}: FinishInput & {
  readonly validTo?: number;
}): Promise<ScriptDecodeSignedTx> => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const runUnit = `${policyId}${scriptDecodeRunTokenName(scriptHashHex)}`;
  const recordUnit = `${policyId}${scriptHashHex}`;
  const matches = ownTokenOutput(spendingScriptAddress, recordUnit);
  const redeemer = ((ctx) =>
    scriptDecodeFinishRedeemer(
      requireOutputIndex(ctx.outputs, matches, "record"),
    )) satisfies BuildTxWithRedeemer;
  const unsigned = await until(
    lucid
      .newTx()
      .collectFrom([runUtxo], SCRIPT_DECODE_CLOSE_REDEEMER)
      .mintAssets({ [recordUnit]: 1n, [runUnit]: -1n }, redeemer)
      .readFrom([requireScriptReference(deployment)])
      .pay.ToContract(
        spendingScriptAddress,
        { kind: "inline", value: scriptDecodeRecordDatum(record) },
        { [recordUnit]: 1n },
      ),
    validTo,
  ).complete({
    localUPLCEval: true,
    ...(await freshWallet(lucid)),
  });
  return {
    signed: await unsigned.sign.withWallet().complete(),
    address: spendingScriptAddress,
    matches,
    label: "record",
  };
};

/** Burns the finished run's token and mints its record; returns the record. */
export const finishScriptDecodeRun = async (
  input: FinishInput,
): Promise<UTxO> =>
  submitScriptDecodeTx(input.lucid, await buildScriptDecodeFinishTx(input));
