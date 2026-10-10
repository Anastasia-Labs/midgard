import { type UTxO } from "@lucid-evolution/lucid";

import { DepositsDB, WithdrawalsDB } from "../database/index.js";
import {
  checkDeposits,
  checkWithdrawals,
} from "./state-reconciliation.check-deposits.js";
import {
  checkConfirmedRoot,
  checkNativeRoot,
  unencodableConfirmedReason,
  unrecomputableReason,
} from "./state-reconciliation.check-native-root.js";
import {
  attemptIsClean,
  attemptLedgerCache,
  checkPayouts,
  checkSettlements,
} from "./state-reconciliation.check-payouts.js";
import {
  checkStateQueueJournal,
  checkTailRoot,
} from "./state-reconciliation.check-state-queue-journal.js";
import {
  type DepositPayload,
  type LedgerPoint,
  type LedgerPointResult,
  type ReconciliationCheck,
  type ReconciliationReport,
  type WithdrawalPayload,
} from "./state-reconciliation.compares.js";
import {
  buildContext,
  type Context,
  finishCheck,
  LOCALLY_FINALIZED_ACTIVE_STATUSES,
  newAccumulator,
  plural,
  skipCheck,
  type StateReconciliationInput,
  toHex,
} from "./state-reconciliation.walk-merged-chain.js";

const checkLedgerCache = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.sql.confirmedRootError !== null) {
    return skipCheck("ledger-cache", unencodableConfirmedReason(ctx));
  }
  const rejected = ctx.sql.pendingTxs.filter((tx) => tx.delta === null);
  if (rejected.length > 0) {
    return skipCheck(
      "ledger-cache",
      `${plural(rejected.length, "pending transaction")} has no stored delta and cannot be decoded (first ${rejected[0]!.txId}: ${rejected[0]!.rejectDetail ?? "decode failed"}); the commit stage will reject it, so the expected cache is undefined until then`,
    );
  }
  const points: LedgerPoint[] = [];
  const tip = ctx.sql.finalizedTip;
  if (tip.kind === "materialized") points.push(tip.point);
  const active = ctx.sql.activeTip;
  const activeStatus =
    active?.kind === "materialized" && active.point.headerHash !== null
      ? ctx.journals.get(active.point.headerHash)?.status
      : undefined;
  if (
    active?.kind === "materialized" &&
    activeStatus !== undefined &&
    LOCALLY_FINALIZED_ACTIVE_STATUSES.has(activeStatus)
  ) {
    points.push(active.point);
  }
  if (points.length === 0) {
    if (tip.kind === "failed" && !tip.parentMissing) {
      const acc = newAccumulator();
      acc.failures.push(`committed tip not recomputable: ${tip.reason}`);
      return finishCheck("ledger-cache", acc, allowInFlight, "");
    }
    return skipCheck("ledger-cache", unrecomputableReason(ctx, tip));
  }
  const depositByOutref = new Map(
    ctx.sql.deposits
      .filter((row) => row.ledgerOutref !== null)
      .map((row) => [row.ledgerOutref!, row]),
  );
  const deltas = ctx.sql.pendingTxs.map((tx) => tx.delta!);
  const attempts = points.map((point) =>
    attemptLedgerCache(ctx, point, depositByOutref, deltas),
  );
  const clean = attempts.find(attemptIsClean);
  const acc = newAccumulator();
  if (clean === undefined) {
    const best = attempts.at(-1)!;
    const describe = (kind: string, list: readonly string[]) =>
      list.length === 0
        ? []
        : [`${plural(list.length, kind)} (first ${list[0]!})`];
    acc.failures.push(
      `mempool_ledger differs from the recomputed ledger at ${best.label} plus pending effects: ${[
        ...describe("missing outref", best.missing),
        ...describe("unexpected outref", best.unexpected),
        ...describe("mismatched output", best.mismatched),
        ...describe("pending spend of an absent outref", best.spentAbsent),
      ].join("; ")}`,
    );
  } else if (clean.label !== points[0]!.label) {
    acc.notes.push(
      `matched at ${clean.label} (locally finalized active journal)`,
    );
  }
  return finishCheck(
    "ledger-cache",
    acc,
    allowInFlight,
    `mempool_ledger (${plural(ctx.sql.mempoolLedger.length, "row")}) equals the ledger at ${clean?.label ?? "?"} plus ${plural(ctx.sql.pendingTxs.length, "pending transaction")}`,
  );
};

/**
 * An unmerged header still Unattested past end_time + da_attestation_timeout
 * is owed a timeout correction that has not landed. Before the deadline it is
 * a normal attestation in progress, noted and passed.
 */
const checkDaAttestation = (
  ctx: Context,
  nowMs: number,
  timeoutMs: number,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck(
      "da-attestation",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  for (const header of ctx.l1.unmerged) {
    const label = `header ${header.headerHash} (${header.outRef})`;
    if (header.daStatus === null || header.endTimeMs === null) {
      acc.notes.push(
        `${label}: DA status unknown, datum undecodable (state-queue-journal reports it)`,
      );
      continue;
    }
    if (header.daStatus !== "Unattested") continue;
    const deadlineMs = header.endTimeMs + timeoutMs;
    const deadline = `${new Date(deadlineMs).toISOString()} (${deadlineMs.toString()} ms)`;
    if (nowMs > deadlineMs) {
      acc.failures.push(
        `${label} is Unattested past its DA-attestation deadline ${deadline} by ${(nowMs - deadlineMs).toString()} ms; its timeout correction has not landed`,
      );
    } else {
      acc.notes.push(
        `${label} awaits DA attestation; deadline ${deadline} in ${(deadlineMs - nowMs).toString()} ms`,
      );
    }
  }
  return finishCheck(
    "da-attestation",
    acc,
    allowInFlight,
    `no unmerged header is Unattested past its DA-attestation deadline (timeout ${timeoutMs.toString()} ms, ${plural(ctx.l1.unmerged.length, "unmerged header")})`,
  );
};

// ---------------------------------------------------------------------------
// Evaluation entry point (pure)
// ---------------------------------------------------------------------------

const pointLabel = (result: LedgerPointResult | null): string =>
  result === null
    ? "none"
    : result.kind === "materialized"
      ? `${result.point.label} root=${result.point.root}`
      : `${result.label} (not recomputable: ${result.reason})`;

export const evaluateStateReconciliation = (
  input: StateReconciliationInput,
): ReconciliationReport => {
  const ctx = buildContext(input);
  const allow = input.allowInFlight;
  const checks: ReconciliationCheck[] = [
    checkConfirmedRoot(ctx, allow),
    checkNativeRoot(ctx, input.native, allow),
    checkStateQueueJournal(ctx, allow),
    checkTailRoot(ctx, input.native, allow),
    checkDeposits(ctx, allow),
    checkWithdrawals(ctx, allow),
    checkPayouts(ctx, allow),
    checkSettlements(ctx, allow),
    checkLedgerCache(ctx, allow),
    checkDaAttestation(ctx, input.nowMs, input.daAttestationTimeoutMs, allow),
  ];
  const summary = {
    pass: checks.filter((c) => c.status === "PASS").length,
    fail: checks.filter((c) => c.status === "FAIL").length,
    skipped: checks.filter((c) => c.status === "SKIPPED").length,
  };
  const ok = summary.fail === 0;
  return {
    ok,
    exitCode: ok ? 0 : 1,
    allowInFlight: allow,
    snapshot: {
      attempts: input.attempts ?? 1,
      l1:
        input.l1.kind === "observed"
          ? `observed (confirmed header ${input.l1.view.confirmed.headerHash}, ${plural(input.l1.view.unmerged.length, "unmerged header")})`
          : `unavailable: ${input.l1.reason}`,
      nativeRoot:
        input.native.kind === "observed"
          ? `${input.native.root} (${input.native.source})`
          : `${input.native.kind}: ${input.native.reason}`,
      sqlConfirmedRoot: input.sql.confirmedRoot,
      finalizedTip: pointLabel(input.sql.finalizedTip),
      activeJournal: pointLabel(input.sql.activeTip),
    },
    summary,
    checks,
  };
};

export const formatStateReconciliationReport = (
  report: ReconciliationReport,
): string => {
  const lines: string[] = [
    "Midgard state reconciliation (read-only)",
    `  snapshot attempts : ${report.snapshot.attempts.toString()}`,
    `  L1                : ${report.snapshot.l1}`,
    `  native root       : ${report.snapshot.nativeRoot}`,
    `  SQL confirmed root: ${report.snapshot.sqlConfirmedRoot}`,
    `  committed tip     : ${report.snapshot.finalizedTip}`,
    `  active journal    : ${report.snapshot.activeJournal}`,
    "",
  ];
  for (const check of report.checks) {
    lines.push(`[${check.status}] ${check.id}: ${check.reason}`);
    lines.push(`         compares: ${check.compares}`);
    for (const failure of check.failures)
      lines.push(`         FAIL: ${failure}`);
    for (const item of check.inFlight)
      lines.push(`         IN-FLIGHT: ${item}`);
    for (const note of check.notes) lines.push(`         note: ${note}`);
  }
  lines.push("");
  lines.push(
    `Result: ${report.summary.pass.toString()} PASS, ${report.summary.fail.toString()} FAIL, ${report.summary.skipped.toString()} SKIPPED -> ${report.ok ? "consistent" : "INCONSISTENT"} (exit ${report.exitCode.toString()})`,
  );
  return lines.join("\n");
};

// ---------------------------------------------------------------------------
// L1 collection
// ---------------------------------------------------------------------------

export const outRefOf = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const policyTokens = (utxo: UTxO, policyId: string) =>
  Object.entries(utxo.assets)
    .filter(([unit]) => unit !== "lovelace" && unit.startsWith(policyId))
    .map(([unit, quantity]) => ({
      assetName: unit.slice(policyId.length),
      quantity: quantity.toString(),
    }));

export const depositPayloadOf = (entry: DepositsDB.Entry): DepositPayload => ({
  eventId: toHex(entry[DepositsDB.Columns.ID]),
  info: toHex(entry[DepositsDB.Columns.INFO]),
  inclusionTimeMs: new Date(entry[DepositsDB.Columns.INCLUSION_TIME]).getTime(),
  ledgerTxId: toHex(entry[DepositsDB.Columns.LEDGER_TX_ID]),
  ledgerOutput: toHex(entry[DepositsDB.Columns.LEDGER_OUTPUT]),
  ledgerAddress: String(entry[DepositsDB.Columns.LEDGER_ADDRESS]),
});

export const withdrawalPayloadOf = (
  entry: WithdrawalsDB.Entry,
): WithdrawalPayload => ({
  eventId: toHex(entry[WithdrawalsDB.Columns.ID]),
  rawEventInfo: toHex(entry[WithdrawalsDB.Columns.RAW_EVENT_INFO]),
  inclusionTimeMs: new Date(
    entry[WithdrawalsDB.Columns.INCLUSION_TIME],
  ).getTime(),
  l1TxHash: toHex(entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]),
  l1OutputIndex: Number(
    entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX],
  ),
  assetName: toHex(entry[WithdrawalsDB.Columns.ASSET_NAME]),
  l2Outref: toHex(entry[WithdrawalsDB.Columns.L2_OUTREF]),
  l2Owner: toHex(entry[WithdrawalsDB.Columns.L2_OWNER]),
  l2Value: toHex(entry[WithdrawalsDB.Columns.L2_VALUE]),
  l1Address: toHex(entry[WithdrawalsDB.Columns.L1_ADDRESS]),
  l1Datum: toHex(entry[WithdrawalsDB.Columns.L1_DATUM]),
  refundAddress: toHex(entry[WithdrawalsDB.Columns.REFUND_ADDRESS]),
  refundDatum: toHex(entry[WithdrawalsDB.Columns.REFUND_DATUM]),
});
