/**
 * Transaction builders for the `script_decode_v1` validator: stage a script's
 * bytes, Bind a run, Walk it, and Finish it into a record.
 *
 * Every transaction is funded from the wallet's plain-Ada UTxOs only, so a
 * staged chunk (which sits at the wallet address with a datum) or a token is
 * never spent by coin selection.
 */
import type { AuthenticatedValidator } from "@al-ft/midgard-sdk";
import { requireReferenceInputIndex } from "@al-ft/midgard-sdk";
import {
  type ScriptDecodeRecord,
  type ScriptDecodeRun,
  type ScriptDecodeWalkStep,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  CML,
  type LucidEvolution,
  type TxOutput,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { DEFAULT_CONFIRMATION_POLL_MS } from "../runtime.js";
import {
  SCRIPT_DECODE_CLOSE_REDEEMER,
  scriptDecodeBindRedeemer,
  scriptDecodeFinishRedeemer,
  scriptDecodeRecordDatum,
  scriptDecodeRunDatum,
  scriptDecodeRunTokenName,
  scriptDecodeStagingDatum,
  scriptDecodeWalkRedeemer,
} from "./data.js";

/** Staged bytes per UTxO, leaving room in a 16,384-byte transaction. */
export const SCRIPT_DECODE_STAGING_UTXO_BYTES = 12_288;

/** The validator and a UTxO carrying it as a reference script. */
export type ScriptDecodeDeployment = Readonly<{
  validator: AuthenticatedValidator;
  reference: UTxO;
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

const requireScriptReference = (deployment: ScriptDecodeDeployment): UTxO => {
  const { reference, validator } = deployment;
  if (
    reference.scriptRef == null ||
    validatorToScriptHash(reference.scriptRef) !== validator.spendingScriptHash
  ) {
    throw new Error(
      "script decode: the reference UTxO does not carry the validator",
    );
  }
  return reference;
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
  for (
    let offset = 0;
    offset < scriptBytes.length || staged.length === 0;
    offset += SCRIPT_DECODE_STAGING_UTXO_BYTES
  ) {
    const datum = scriptDecodeStagingDatum(
      scriptBytes.subarray(offset, offset + SCRIPT_DECODE_STAGING_UTXO_BYTES),
    );
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
 * Builds and signs one Walk. Throws when the transaction cannot be built, for
 * instance when the Walk's fuel overruns the transaction budget.
 */
export const buildScriptDecodeWalk = async ({
  lucid,
  deployment,
  scriptHashHex,
  runUtxo,
  staging,
  step,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ScriptDecodeDeployment;
  readonly scriptHashHex: string;
  readonly runUtxo: UTxO;
  readonly staging: readonly UTxO[];
  readonly step: ScriptDecodeWalkStep;
}) => {
  const { policyId, spendingScriptAddress } = deployment.validator;
  const runUnit = `${policyId}${scriptDecodeRunTokenName(scriptHashHex)}`;
  const matches = ownTokenOutput(spendingScriptAddress, runUnit);
  const redeemer = ((ctx) =>
    scriptDecodeWalkRedeemer({
      staging: stagingIndices(ctx, staging),
      reveals: step.reveals,
      base: step.base,
      fuel: step.fuel,
      runOutput: requireOutputIndex(ctx.outputs, matches, "run"),
    })) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([runUtxo], redeemer)
    .readFrom([...staging, requireScriptReference(deployment)])
    .pay.ToContract(
      spendingScriptAddress,
      { kind: "inline", value: scriptDecodeRunDatum(step.after) },
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
