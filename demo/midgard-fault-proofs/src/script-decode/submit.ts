/**
 * Transaction builders for the `script_decode_v1` validator: stage a script's
 * bytes, Bind a run, Walk it, and Finish it into a record. A Walk spends the
 * run and withdraws zero from each class staker that steps one of its
 * segments; each staker's redeemer carries its segments and their links.
 *
 * Every transaction is funded from the wallet's plain-Ada UTxOs only, so a
 * staged chunk (which sits at the wallet address with a datum) or a token is
 * never spent by coin selection.
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
  type TxOutput,
  type UTxO,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { DEFAULT_CONFIRMATION_POLL_MS } from "../runtime.js";
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

const isPlainAda = (utxo: UTxO): boolean =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).every((unit) => unit === "lovelace");

/**
 * The wallet's UTxOs that hold only Ada and carry no datum or script, read
 * fresh: a view pinned by an earlier submission is dropped first.
 */
export const plainAdaWalletUtxos = async (
  lucid: LucidEvolution,
): Promise<UTxO[]> => {
  lucid.clearUTxOOverride();
  return (await lucid.wallet().getUtxos()).filter(isPlainAda);
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

const submitAndFind = async (
  lucid: LucidEvolution,
  signed: { submit: () => Promise<string> },
  address: string,
  matches: (output: TxOutput) => boolean,
  label: string,
): Promise<UTxO> => {
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  const found = (await lucid.utxosAt(address)).filter(
    (utxo) => utxo.txHash === txHash && matches(utxo),
  );
  if (found.length !== 1) {
    throw new Error(`script decode: ${label} output not found`);
  }
  return found[0]!;
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

/**
 * Stages `scriptBytes` in order across UTxOs at `address` (the wallet's own by
 * default, so their Ada stays reclaimable), one transaction each.
 */
export const stageScriptDecodeBytes = async ({
  lucid,
  scriptBytes,
  address,
}: {
  readonly lucid: LucidEvolution;
  readonly scriptBytes: Uint8Array;
  readonly address?: string;
}): Promise<UTxO[]> => {
  const target = address ?? (await lucid.wallet().address());
  const staged: UTxO[] = [];
  for (const datum of scriptDecodeStagingDatums(scriptBytes)) {
    const unsigned = await lucid
      .newTx()
      .pay.ToAddressWithData(target, { kind: "inline", value: datum }, {})
      .complete({ presetWalletInputs: await plainAdaWalletUtxos(lucid) });
    const signed = await unsigned.sign.withWallet().complete();
    staged.push(
      await submitAndFind(
        lucid,
        signed,
        target,
        (output) => output.datum === datum,
        "staging",
      ),
    );
  }
  return staged;
};

/** Opens the run for the staged bytes; returns the run UTxO. */
export const bindScriptDecodeRun = async ({
  lucid,
  deployment,
  language,
  scriptHashHex,
  initialRun,
  staging,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly language: number;
  readonly scriptHashHex: string;
  readonly initialRun: ScriptDecodeRun;
  readonly staging: readonly UTxO[];
}): Promise<UTxO> => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const runUnit = `${policyId}${scriptDecodeRunTokenName(scriptHashHex)}`;
  const matches = ownTokenOutput(spendingScriptAddress, runUnit);
  const redeemer = ((ctx) =>
    scriptDecodeBindRedeemer({
      language,
      staging: stagingIndices(ctx, staging),
      runOutput: requireOutputIndex(ctx.outputs, matches, "run"),
    })) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .mintAssets({ [runUnit]: 1n }, redeemer)
    .readFrom([...staging, requireScriptReference(deployment)])
    .pay.ToContract(
      spendingScriptAddress,
      { kind: "inline", value: scriptDecodeRunDatum(initialRun) },
      { [runUnit]: 1n },
    )
    .complete({
      localUPLCEval: true,
      presetWalletInputs: await plainAdaWalletUtxos(lucid),
    });
  const signed = await unsigned.sign.withWallet().complete();
  return submitAndFind(lucid, signed, spendingScriptAddress, matches, "run");
};

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
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly scriptHashHex: string;
  readonly runUtxo: UTxO;
  readonly staging: readonly UTxO[];
  readonly after: ScriptDecodeRun;
  readonly stakes: readonly ScriptDecodeStake[];
  readonly inputs?: readonly UTxO[];
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
  const unsigned = await tx.pay
    .ToContract(
      spendingScriptAddress,
      { kind: "inline", value: scriptDecodeRunDatum(after) },
      { [runUnit]: 1n },
    )
    .complete({
      localUPLCEval: true,
      presetWalletInputs: await plainAdaWalletUtxos(lucid),
    });
  const signed = await unsigned.sign.withWallet().complete();
  const cbor = signed.toCBOR();
  return {
    exUnits: transactionExUnits(cbor),
    txBytes: cbor.length / 2,
    submit: () =>
      submitAndFind(lucid, signed, spendingScriptAddress, matches, "run"),
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
}) =>
  buildScriptDecodeWalkTx({
    ...rest,
    after: walk.after,
    stakes: scriptDecodeWalkStakes(rest.deployment, rest.runUtxo, walk),
  });

/** Burns the finished run's token and mints its record; returns the record. */
export const finishScriptDecodeRun = async ({
  lucid,
  deployment,
  scriptHashHex,
  runUtxo,
  record,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly scriptHashHex: string;
  readonly runUtxo: UTxO;
  readonly record: ScriptDecodeRecord;
}): Promise<UTxO> => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const runUnit = `${policyId}${scriptDecodeRunTokenName(scriptHashHex)}`;
  const recordUnit = `${policyId}${scriptHashHex}`;
  const matches = ownTokenOutput(spendingScriptAddress, recordUnit);
  const redeemer = ((ctx) =>
    scriptDecodeFinishRedeemer(
      requireOutputIndex(ctx.outputs, matches, "record"),
    )) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([runUtxo], SCRIPT_DECODE_CLOSE_REDEEMER)
    .mintAssets({ [recordUnit]: 1n, [runUnit]: -1n }, redeemer)
    .readFrom([requireScriptReference(deployment)])
    .pay.ToContract(
      spendingScriptAddress,
      { kind: "inline", value: scriptDecodeRecordDatum(record) },
      { [recordUnit]: 1n },
    )
    .complete({
      localUPLCEval: true,
      presetWalletInputs: await plainAdaWalletUtxos(lucid),
    });
  const signed = await unsigned.sign.withWallet().complete();
  return submitAndFind(lucid, signed, spendingScriptAddress, matches, "record");
};
