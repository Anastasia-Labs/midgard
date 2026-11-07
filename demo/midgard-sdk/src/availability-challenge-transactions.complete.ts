import {
  CML,
  type LucidEvolution,
  type ProtocolParameters,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import {
  type BuiltDaAvailabilityTransaction,
  type DaAvailabilityDeployment,
  type DaAvailabilityExpectedOutput,
  type DaAvailabilityTransactionAction,
  DaAvailabilityTransactionError,
  type DaAvailabilityTransactionResources,
  fail,
  refKey,
  sameAssets,
} from "./availability-challenge-transactions.at.js";
import {
  MAX_COLLATERAL_INPUTS,
  minAda,
  protocolParameters,
} from "./availability-challenge-transactions.plan-da-availability-timeout.js";
import { completeOptionsWithLocalEval } from "./tx-completion.js";
import {
  isPlainPositiveAdaOnlyUtxo,
  outputDatumCborMatches,
} from "./tx-output-utils.js";

/**
 * Picks at most three plain-ADA coins, largest first, whose total covers
 * `requiredLovelace` and leaves either nothing or at least
 * `minimumReturnLovelace` as collateral return.
 */
export const selectDaAvailabilityCollateral = (input: {
  readonly candidates: readonly UTxO[];
  readonly requiredLovelace: bigint;
  readonly minimumReturnLovelace: bigint;
}): readonly UTxO[] => {
  const sorted = [...input.candidates].sort((a, b) => {
    const x = a.assets.lovelace ?? 0n,
      y = b.assets.lovelace ?? 0n;
    if (x !== y) return x > y ? -1 : 1;
    // Ties break in code-unit order of the out-ref, independent of locale.
    const ka = refKey(a),
      kb = refKey(b);
    return ka < kb ? -1 : ka > kb ? 1 : 0;
  });
  const selected: UTxO[] = [];
  let total = 0n;
  for (const u of sorted) {
    if (selected.length === MAX_COLLATERAL_INPUTS) break;
    selected.push(u);
    total += u.assets.lovelace ?? 0n;
    const change = total - input.requiredLovelace;
    if (change === 0n || change >= input.minimumReturnLovelace) return selected;
  }
  return fail(
    `No ${MAX_COLLATERAL_INPUTS} collateral coins cover ${input.requiredLovelace} lovelace with a valid collateral return`,
    "collateral-insufficient",
  );
};

/**
 * The ledger's minimum fee for a completed, still unsigned transaction: CML's
 * `min_fee` over the body, redeemer budgets and reference scripts, plus one
 * vkey witness per distinct signing key and a small allowance for the
 * witness-set header and for coin-width changes when `c` is lowered. The
 * reference-script size is the provider's script CBOR, which is at least the
 * ledger's count, so the estimate errs high.
 */
export const daAvailabilityLedgerMinFee = (input: {
  readonly unsignedCbor: string;
  readonly protocolParameters: ProtocolParameters;
  readonly referenceScriptBytes: bigint;
  readonly vkeyWitnessCount: number;
}): bigint => {
  const p = input.protocolParameters;
  const tx = CML.Transaction.from_cbor_hex(input.unsignedCbor);
  const linear = CML.LinearFee.new(
    BigInt(p.minFeeA),
    BigInt(p.minFeeB),
    BigInt(p.minFeeRefScriptCostPerByte),
  );
  const mem = CML.SubCoin.new(BigInt(Math.round(p.priceMem * 1e8)), 100000000n);
  const step = CML.SubCoin.new(
    BigInt(Math.round(p.priceStep * 1e8)),
    100000000n,
  );
  const prices = CML.ExUnitPrices.new(mem, step);
  try {
    const base = CML.min_fee(tx, linear, prices, input.referenceScriptBytes);
    // A vkey witness is [bytes32, bytes64]: 101 bytes. The allowance covers the
    // witness-set key, the (tagged) set header and fee/coin width changes.
    const extraBytes = 101n * BigInt(input.vkeyWitnessCount) + 24n;
    return base + extraBytes * BigInt(p.minFeeA);
  } finally {
    prices.free();
    step.free();
    mem.free();
    linear.free();
    tx.free();
  }
};

export const alignResources = <P extends DaAvailabilityTransactionResources>(
  lucid: LucidEvolution,
  p: P,
): P => {
  const lowerSlot = lucid.unixTimeToSlot(Number(p.validFrom));
  const lower = BigInt(lucid.slotToUnixTime(lowerSlot));
  const validFrom =
    lower < p.validFrom ? BigInt(lucid.slotToUnixTime(lowerSlot + 1)) : lower;
  const validTo = BigInt(
    lucid.slotToUnixTime(lucid.unixTimeToSlot(Number(p.validTo))),
  );
  if (validTo <= validFrom)
    fail("Validity interval contains no complete ledger slot");
  return { ...p, validFrom, validTo };
};

export const complete = async (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityTransactionResources,
  tx: TxBuilder,
  meta: {
    action: DaAvailabilityTransactionAction;
    headerHash: string;
    challengeAssetName: string;
    inputs: readonly UTxO[];
    refs: readonly UTxO[];
    outputs: readonly DaAvailabilityExpectedOutput[];
    /** Timeout only: the slashed share of the fee, exempt from the cap. */
    timeoutFeePart?: bigint;
  },
): Promise<BuiltDaAvailabilityTransaction> => {
  Availability.assertCanonicalDaAvailabilityParameters(d.parameters);
  const cap =
    meta.action === "publish"
      ? d.parameters.max_publication_fee_lovelace
      : meta.action === "settle"
        ? d.parameters.max_settlement_fee_lovelace
        : meta.action === "open"
          ? d.parameters.max_open_fee_lovelace
          : meta.action === "close"
            ? d.parameters.max_close_fee_lovelace
            : d.parameters.max_timeout_fee_lovelace;
  if ((meta.action === "timeout") !== (meta.timeoutFeePart !== undefined))
    fail("Only a timeout carries a slashed fee part");
  // The timeout caps only the challenger's contribution c = fee - feePart; the
  // slashed penalty share is fee by protocol and outside every cap.
  const capped = p.feeLovelace - (meta.timeoutFeePart ?? 0n);
  if (
    p.feeLovelace <= 0n ||
    capped < 0n ||
    capped > cap ||
    p.validFrom < 0n ||
    p.validTo <= p.validFrom ||
    p.validTo - p.validFrom > 120_000n ||
    p.validTo > BigInt(Number.MAX_SAFE_INTEGER)
  )
    fail("Invalid fee or bounded validity interval");
  const walletAddress = await lucid.wallet().address();
  if (
    p.collateralInputs.length === 0 ||
    p.collateralInputs.some(
      (u) => !isPlainPositiveAdaOnlyUtxo(u) || u.address !== walletAddress,
    )
  )
    fail("Explicit plain-ADA wallet collateral is required");
  const spent = new Set(meta.inputs.map(refKey));
  const refs = new Set(meta.refs.map(refKey));
  if (
    spent.size !== meta.inputs.length ||
    p.collateralInputs.some(
      (u) => spent.has(refKey(u)) || refs.has(refKey(u)),
    ) ||
    meta.inputs.some((u) => refs.has(refKey(u)))
  )
    fail("Transaction resources overlap");
  const protocol = protocolParameters(lucid);
  const collateral =
    (p.feeLovelace * BigInt(protocol.collateralPercentage) + 99n) / 100n;
  const collateralInputs = selectDaAvailabilityCollateral({
    candidates: p.collateralInputs,
    requiredLovelace: collateral,
    minimumReturnLovelace: minAda(lucid, {
      address: walletAddress,
      assets: { lovelace: collateral },
    }),
  });
  const available = await lucid.utxosByOutRef([
    ...meta.inputs,
    ...meta.refs,
    ...collateralInputs,
  ]);
  const observed = new Map(available.map((u) => [refKey(u), u]));
  for (const u of [...meta.inputs, ...meta.refs, ...collateralInputs]) {
    const live = observed.get(refKey(u));
    if (
      !live ||
      live.address !== u.address ||
      !sameAssets(live.assets, u.assets) ||
      (u.datum == null
        ? live.datum != null
        : !outputDatumCborMatches(live, u.datum)) ||
      (live.scriptRef ? validatorToScriptHash(live.scriptRef) : null) !==
        (u.scriptRef ? validatorToScriptHash(u.scriptRef) : null)
    )
      fail(`Stale transaction resource ${refKey(u)}`);
  }
  for (const o of meta.outputs)
    if ((o.assets.lovelace ?? 0n) < minAda(lucid, o))
      fail("Protected output is below live ledger minimum ADA");
  let completed: TxSignBuilder;
  try {
    completed = await tx
      .setMinFee(p.feeLovelace)
      .validFrom(Number(p.validFrom))
      .validTo(Number(p.validTo))
      .complete({
        ...completeOptionsWithLocalEval({
          coinSelection: false,
          presetWalletInputs: collateralInputs,
        }),
        setCollateral: collateral,
      });
  } catch (cause) {
    if (cause instanceof DaAvailabilityTransactionError) throw cause;
    return fail(
      `Transaction does not complete at the exact fee ${p.feeLovelace}: ${cause instanceof Error ? cause.message : String(cause)}`,
      "completion-failed",
    );
  }
  const body = completed.toTransaction().body();
  if (
    body.fee() !== p.feeLovelace ||
    body.inputs().len() !== meta.inputs.length ||
    body.outputs().len() !== meta.outputs.length
  )
    fail(
      "Completed transaction changed protected fee, inputs, or outputs",
      "completion-failed",
    );
  for (let i = 0; i < body.inputs().len(); i++) {
    const u = body.inputs().get(i);
    if (!spent.has(`${u.transaction_id().to_hex()}#${u.index()}`))
      fail("Completed transaction selected an unreserved input");
  }
  const actualCollateral: UTxO[] = [];
  const collateralBody = body.collateral_inputs();
  if (!collateralBody || collateralBody.len() === 0)
    fail("Completed transaction lacks collateral");
  if (collateralBody!.len() > MAX_COLLATERAL_INPUTS)
    fail("Completed transaction exceeds the collateral input limit");
  for (let i = 0; i < collateralBody!.len(); i++) {
    const c = collateralBody!.get(i);
    const u = collateralInputs.find(
      (u) => refKey(u) === `${c.transaction_id().to_hex()}#${c.index()}`,
    );
    if (!u) fail("Completed transaction selected unreserved collateral");
    actualCollateral.push(u!);
  }
  const totalCollateral = body.total_collateral();
  if (totalCollateral !== undefined && totalCollateral < collateral)
    fail("Completed transaction collateral is below the ledger percentage");
  return {
    tx: completed,
    unsignedCbor: completed.toCBOR(),
    txId: completed.toHash(),
    action: meta.action,
    headerHash: meta.headerHash,
    challengeAssetName: meta.challengeAssetName,
    validityRange: { validFrom: p.validFrom, validTo: p.validTo },
    spentOutRefs: meta.inputs,
    referenceOutRefs: meta.refs,
    collateralOutRefs: actualCollateral,
    expectedOutputs: meta.outputs,
    feeLovelace: p.feeLovelace,
    ...(meta.timeoutFeePart === undefined
      ? {}
      : { timeoutFeePartLovelace: meta.timeoutFeePart }),
  };
};
