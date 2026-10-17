import { randomUUID } from "node:crypto";

import type {
  AvailabilityOperationIntent,
  AvailabilityOperationLease,
} from "@al-ft/midgard-core/availability-operation-journal";
import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  CML,
  coreToTxOutput,
  type LucidEvolution,
  paymentCredentialOf,
  selectUTxOs,
  sortUTxOs,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type DaAvailabilityOperationContext } from "./availability-challenge-operation.inspect-da-availability-signed-intent.js";
import {
  createDaAvailabilityReadScope,
  type DaAvailabilityReadScope,
} from "./availability-challenge-operation.read-scope.js";
import { fail, sameAssets } from "./availability-challenge-transactions.at.js";
import { daAvailabilityPlainCoinMinAda } from "./availability-challenge-transactions.da-availability-opening-input.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "./linked-list.js";
import { isPlainPositiveAdaOnlyUtxo } from "./tx-output-utils.js";

export type DaAvailabilityFundingPreparationInput = Readonly<{
  /**
   * The opening input: exactly `challenger_bond_lovelace +
   * challenge_record_lovelace + fee` of the Open it funds
   * (`daAvailabilityOpeningInputRequirement`).
   */
  outputLovelace: bigint;
  /** The preparation's own exact fee. */
  feeLovelace: bigint;
  /**
   * A second ADA-only output the challenge workflow posts as collateral
   * (`daAvailabilityWorkflowCollateralLovelace`). Omitted when the wallet
   * already holds plain-ADA collateral.
   */
  collateralLovelace?: bigint;
  /** A further ADA-only output funding the workflow's later step fees. */
  workingCapitalLovelace?: bigint;
  /**
   * The coins coin selection may spend, whatever they hold; every wallet
   * coin when omitted. Spent tokens return to the wallet as change.
   */
  walletInputs?: readonly UTxO[];
  validFrom: bigint;
  validTo: bigint;
}>;

const refOf = (u: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${u.txHash}#${u.outputIndex}`;

const tokensOf = (assets: Assets): Assets =>
  Object.fromEntries(
    Object.entries(assets).filter(([unit, n]) => unit !== "lovelace" && n > 0n),
  );

const addTokens = (into: Record<string, bigint>, assets: Assets): void => {
  for (const [unit, n] of Object.entries(tokensOf(assets)))
    into[unit] = (into[unit] ?? 0n) + n;
};

/**
 * Prepares the input an Open spends, from any wallet: one ADA-only,
 * no-datum, no-reference-script output of exactly `outputLovelace` at the
 * wallet address, then the optional ADA-only collateral and working-capital
 * outputs, then change. The Open takes the input only at the challenger's
 * enterprise key address, so a challenger prepares from its enterprise
 * wallet, as the CLI and the watcher do. Lucid's coin selection picks any
 * wallet coins, token-bearing ones included; one explicit change output
 * returns their tokens and spare lovelace, so the fee is exactly
 * `feeLovelace`. Every refusal is a `DaAvailabilityTransactionError` with a
 * reason.
 */
export const buildDaAvailabilityFundingPreparationTx = async (
  lucid: LucidEvolution,
  input: DaAvailabilityFundingPreparationInput,
  scope?: DaAvailabilityReadScope,
): Promise<TxSignBuilder> => {
  const read = <T>(run: () => Promise<T>) =>
    scope === undefined ? run() : scope.read(run);
  const reserved = [
    input.outputLovelace,
    ...(input.collateralLovelace === undefined
      ? []
      : [input.collateralLovelace]),
    ...(input.workingCapitalLovelace === undefined
      ? []
      : [input.workingCapitalLovelace]),
  ];
  if (
    reserved.some((lovelace) => lovelace <= 0n) ||
    reserved.slice(1).includes(input.outputLovelace) ||
    input.feeLovelace <= 0n ||
    input.validFrom < 0n ||
    input.validTo <= input.validFrom ||
    input.validTo - input.validFrom > 120_000n ||
    input.validTo > BigInt(Number.MAX_SAFE_INTEGER)
  )
    fail(
      "Availability funding preparation needs positive amounts, an opening amount no other prepared output repeats, a positive fee and a validity interval of at most 120 s",
      "preparation-invalid-request",
    );
  const walletAddress = await read(() => lucid.wallet().address());
  if (paymentCredentialOf(walletAddress).type !== "Key")
    fail(
      "Availability funding preparation needs a payment-key wallet",
      "preparation-wallet-not-key",
    );
  const coinsPerByte = lucid.config().protocolParameters?.coinsPerUtxoByte;
  if (!coinsPerByte)
    fail(
      "Availability funding preparation needs live protocol parameters",
      "protocol-parameters-unavailable",
    );
  for (const lovelace of reserved) {
    const minimum = daAvailabilityPlainCoinMinAda(
      coinsPerByte!,
      walletAddress,
      lovelace,
    );
    if (lovelace < minimum)
      fail(
        `Availability funding preparation output of ${lovelace.toString()} lovelace is below the live minimum ADA of ${minimum.toString()}`,
        "preparation-below-min-ada",
        { requiredLovelace: minimum, availableLovelace: lovelace },
      );
  }
  const candidates = [
    ...(input.walletInputs ?? (await read(() => lucid.wallet().getUtxos()))),
  ];
  if (candidates.length === 0)
    fail(
      "Availability funding preparation found no wallet coin to spend",
      "preparation-no-wallet-inputs",
    );
  const byRef = new Map(candidates.map((u) => [refOf(u), u]));
  if (
    byRef.size !== candidates.length ||
    candidates.some((u) => u.address !== walletAddress)
  )
    fail(
      "Availability funding preparation inputs must be distinct wallet coins",
      "preparation-invalid-request",
    );
  const lovelaceOf = (coins: readonly UTxO[]) =>
    coins.reduce((sum, u) => sum + (u.assets.lovelace ?? 0n), 0n);
  const tokensOfCoins = (coins: readonly UTxO[]) => {
    const tokens: Record<string, bigint> = {};
    for (const u of coins) addTokens(tokens, u.assets);
    return tokens;
  };
  // The change pays back every spent token and the spare lovelace. It is
  // needed unless the spend is exact and token-free, and then it must meet
  // its own min-ADA: the smallest such coin is a fixpoint, since its width
  // moves the floor.
  const changeFloor = (coins: readonly UTxO[], leftover: bigint): bigint => {
    const tokens = tokensOfCoins(coins);
    if (Object.keys(tokens).length === 0 && leftover === 0n) return 0n;
    let minimum = 0n;
    for (;;) {
      const next = calculateMinLovelaceFromUTxO(coinsPerByte!, {
        address: walletAddress,
        assets: { ...tokens, lovelace: minimum },
        txHash: "00".repeat(32),
        outputIndex: 0,
      });
      if (next <= minimum) return minimum;
      minimum = next;
    }
  };
  const prepared = reserved.reduce((sum, lovelace) => sum + lovelace, 0n);
  const spend = prepared + input.feeLovelace;
  const availableLovelace = lovelaceOf(candidates);
  const changeNeeded = changeFloor(candidates, availableLovelace - spend);
  const requiredLovelace = spend + changeNeeded;
  if (availableLovelace < requiredLovelace)
    fail(
      `Availability funding preparation needs ${requiredLovelace.toString()} lovelace (${reserved.map(String).join(" + ")} prepared + fee ${input.feeLovelace.toString()} + change minimum ADA ${changeNeeded.toString()}); the wallet coins hold ${availableLovelace.toString()}`,
      "preparation-insufficient-lovelace",
      { requiredLovelace, availableLovelace },
    );
  // Lucid's own coin selection, largest coin first, whatever the coins hold;
  // it widens the selection until the change meets its floor. Every floor
  // only grows with the coins added, so the check over all candidates above
  // is exactly when this succeeds.
  const ordered = sortUTxOs(candidates);
  let selected = selectUTxOs(ordered, { lovelace: spend }, true);
  for (;;) {
    const leftover = lovelaceOf(selected) - spend;
    const short = changeFloor(selected, leftover) - leftover;
    if (selected.length > 0 && short <= 0n) break;
    const more = selectUTxOs(
      ordered.filter((u) => !selected.includes(u)),
      { lovelace: short > 0n ? short : 1n },
      true,
    );
    if (more.length === 0)
      fail(
        `Availability funding preparation could not select coins for ${requiredLovelace.toString()} lovelace`,
        "preparation-insufficient-lovelace",
        { requiredLovelace, availableLovelace },
      );
    selected = [...selected, ...more];
  }
  const change = lovelaceOf(selected) - spend;
  const changeAssets: Assets = { ...tokensOfCoins(selected), lovelace: change };
  const changeCount = changeFloor(selected, change) === 0n ? 0 : 1;
  let tx = lucid.newTx().collectFrom(selected);
  for (const lovelace of reserved)
    tx = tx.pay.ToAddress(walletAddress, { lovelace });
  if (changeCount === 1) tx = tx.pay.ToAddress(walletAddress, changeAssets);
  let built: TxSignBuilder;
  try {
    built = await read(() =>
      tx
        .setMinFee(input.feeLovelace)
        .validFrom(Number(input.validFrom))
        .validTo(Number(input.validTo))
        .complete({ localUPLCEval: true, coinSelection: false }),
    );
  } catch (cause) {
    return fail(
      `Availability funding preparation does not complete at the exact fee ${input.feeLovelace.toString()}: ${cause instanceof Error ? cause.message : String(cause)}`,
      "completion-failed",
    );
  }
  const body = built.toTransaction().body();
  if (body.fee() !== input.feeLovelace)
    fail(
      `Availability funding preparation needs a fee of ${body.fee().toString()} lovelace, not the exact ${input.feeLovelace.toString()}`,
      "completion-failed",
    );
  const spent: UTxO[] = [];
  for (let i = 0; i < body.inputs().len(); i++) {
    const ref = body.inputs().get(i);
    const u = byRef.get(`${ref.transaction_id().to_hex()}#${ref.index()}`);
    if (u === undefined)
      fail(
        "Availability funding preparation selected a coin outside its wallet inputs",
        "preparation-layout-mismatch",
      );
    spent.push(u!);
  }
  const spentTokens: Record<string, bigint> = {};
  for (const u of spent) addTokens(spentTokens, u.assets);
  const changeTokens: Record<string, bigint> = {};
  const outputs = body.outputs();
  let layout =
    body.mint() === undefined &&
    body.certs() === undefined &&
    body.withdrawals() === undefined &&
    spent.length === selected.length &&
    outputs.len() === reserved.length + changeCount;
  for (let i = 0; layout && i < outputs.len(); i++) {
    const o = coreToTxOutput(outputs.get(i));
    if (o.address !== walletAddress) layout = false;
    else if (i < reserved.length)
      layout =
        isPlainPositiveAdaOnlyUtxo({ ...o, txHash: "", outputIndex: i }) &&
        o.assets.lovelace === reserved[i];
    else addTokens(changeTokens, o.assets);
  }
  if (!layout || !sameAssets(spentTokens, changeTokens))
    fail(
      `Availability funding preparation must pay ${reserved.map(String).join(", ")} lovelace as ADA-only, no-datum outputs to ${walletAddress} and return every spent token as change`,
      "preparation-layout-mismatch",
    );
  const live = await read(() => lucid.utxosByOutRef(spent));
  const observed = new Map(live.map((u) => [refOf(u), u]));
  for (const u of spent) {
    const now = observed.get(refOf(u));
    if (
      now === undefined ||
      now.address !== u.address ||
      !sameAssets(now.assets, u.assets)
    )
      fail(
        `Availability funding preparation input ${refOf(u)} is no longer unspent as observed`,
        "preparation-input-stale",
      );
  }
  scope?.assertCurrent();
  return built;
};

const assertContext = (context: DaAvailabilityOperationContext): void => {
  if (
    !/^[0-9a-f]{64}$/u.test(context.deploymentIdentity) ||
    !/^[0-9a-f]{56}$/u.test(context.actor) ||
    !/^[0-9a-f]{56}$/u.test(context.stateQueuePolicyId) ||
    !Number.isSafeInteger(context.minimumConfirmationDepth) ||
    context.minimumConfirmationDepth <= 0
  ) {
    throw new Error(
      "Invalid availability operation deployment/finality authority",
    );
  }
};

export const assertTerminalIntent = (
  context: DaAvailabilityOperationContext,
  intent: AvailabilityOperationIntent,
): void => {
  if (intent.action !== "timeout" && intent.action !== "remove") return;
  const mint = CML.Transaction.from_cbor_hex(intent.signedCbor).body().mint();
  const removesTarget =
    mint?.get(
      CML.ScriptHash.from_hex(context.stateQueuePolicyId),
      CML.AssetName.from_hex(
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX + intent.headerHash,
      ),
    ) === -1n;
  if (intent.completesWorkflow !== removesTarget) {
    throw new Error(
      "Availability terminal metadata does not match the signed target-header burn",
    );
  }
};

export const withLease = async <T>(
  context: DaAvailabilityOperationContext,
  run: (
    lease: AvailabilityOperationLease,
    assertCurrent: (scope?: DaAvailabilityReadScope) => Promise<void>,
    observationScope: () => DaAvailabilityReadScope,
  ) => Promise<T>,
): Promise<T> => {
  assertContext(context);
  const now = context.nowMs ?? Date.now;
  const lease = context.journal.acquire(
    context.actor,
    randomUUID(),
    now(),
    context.leaseDurationMs ?? 300_000,
  );
  const assertCurrent = async (
    scope?: DaAvailabilityReadScope,
  ): Promise<void> => {
    scope?.assertCurrent();
    context.journal.assertLease(lease, now());
    await context.assertActuationCurrent(scope);
    scope?.assertCurrent();
    context.journal.assertLease(lease, now());
  };
  let observed: DaAvailabilityReadScope | undefined;
  const observationScope = () =>
    (observed ??= createDaAvailabilityReadScope({
      attemptTimeoutMs:
        context.observationTimeoutMs ?? context.leaseDurationMs ?? 300_000,
      signal: context.observationSignal,
      nowMs: context.nowMs,
      monotonicMs: context.monotonicMs,
    }));
  try {
    // The caller performs this read inside its preparation/observation scope
    // before any journal mutation; read-only pending selection precedes it.
    return await run(lease, assertCurrent, observationScope);
  } finally {
    observed?.close();
    context.journal.release(lease);
  }
};
