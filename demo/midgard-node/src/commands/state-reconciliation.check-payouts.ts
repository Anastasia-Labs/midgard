import * as SDK from "@al-ft/midgard-sdk";
import { assetsEqual, valueToAssets } from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import { DepositsDB, WithdrawalsDB } from "../database/index.js";
import { canonicalDataCbor } from "./state-reconciliation.check-deposits.js";
import {
  type LedgerPoint,
  type PendingTxDelta,
  type ReconciliationCheck,
  type SqlDepositRow,
  type SqlWithdrawalRow,
} from "./state-reconciliation.compares.js";
import {
  assetsLabel,
  type Context,
  describeError,
  finishCheck,
  isMerged,
  newAccumulator,
  plural,
  rootsDiff,
  skipCheck,
} from "./state-reconciliation.walk-merged-chain.js";

export const checkPayouts = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck("payouts", `L1 unavailable: ${ctx.l1UnavailableReason}`);
  }
  const acc = newAccumulator();
  const byAssetName = new Map<string, SqlWithdrawalRow[]>();
  for (const row of ctx.sql.withdrawals) {
    const list = byAssetName.get(row.payload.assetName) ?? [];
    list.push(row);
    byAssetName.set(row.payload.assetName, list);
  }
  const seen = new Map<string, string>();
  for (const payout of ctx.l1.payouts) {
    const label = `payout ${payout.outRef}`;
    if (payout.tokens.length !== 1 || payout.tokens[0]!.quantity !== "1") {
      acc.failures.push(
        `${label}: expected exactly one payout token, found ${payout.tokens.map((t) => `${t.assetName}x${t.quantity}`).join(",") || "none"}`,
      );
      continue;
    }
    const assetName = payout.tokens[0]!.assetName;
    const duplicate = seen.get(assetName);
    if (duplicate !== undefined) {
      acc.failures.push(
        `${label}: payout token ${assetName} also held by ${duplicate}`,
      );
    }
    seen.set(assetName, payout.outRef);
    const rows = byAssetName.get(assetName) ?? [];
    if (rows.length !== 1) {
      acc.failures.push(
        `${label}: ${rows.length === 0 ? "no SQL withdrawal" : `${rows.length.toString()} SQL withdrawals`} carry asset name ${assetName}`,
      );
      continue;
    }
    const row = rows[0]!;
    const rowLabel = `${label} (withdrawal ${row.payload.eventId})`;
    if (row.status !== WithdrawalsDB.Status.Finalized) {
      acc.failures.push(
        `${rowLabel}: SQL status is ${row.status}, expected finalized`,
      );
    }
    if (row.validity !== WithdrawalsDB.Validity.WithdrawalIsValid) {
      acc.failures.push(
        `${rowLabel}: SQL validity is ${row.validity ?? "<unclassified>"}, expected WithdrawalIsValid`,
      );
    }
    if (
      row.projectedHeaderHash === null ||
      !isMerged(ctx, row.projectedHeaderHash)
    ) {
      acc.failures.push(
        `${rowLabel}: withdrawal header ${row.projectedHeaderHash ?? "<none>"} is not merged`,
      );
    }
    if (payout.decodeError !== null || payout.l2Value === null) {
      acc.failures.push(
        `${rowLabel}: payout datum undecodable: ${payout.decodeError ?? "missing"}`,
      );
      continue;
    }
    try {
      const sqlAssets = valueToAssets(
        LucidData.from(row.payload.l2Value, SDK.Value) as SDK.Value,
      );
      if (!assetsEqual(sqlAssets, payout.l2Value)) {
        acc.failures.push(
          `${rowLabel}: payout l2_value {${assetsLabel(payout.l2Value)}} != SQL l2_value {${assetsLabel(sqlAssets)}}`,
        );
      }
      if (
        canonicalDataCbor(row.payload.l1Address, SDK.AddressData) !==
        payout.l1AddressCbor
      ) {
        acc.failures.push(`${rowLabel}: payout l1_address differs from SQL`);
      }
      if (
        canonicalDataCbor(row.payload.l1Datum, SDK.CardanoDatum) !==
        payout.l1DatumCbor
      ) {
        acc.failures.push(`${rowLabel}: payout l1_datum differs from SQL`);
      }
    } catch (error) {
      acc.failures.push(
        `${rowLabel}: SQL payout fields undecodable: ${describeError(error)}`,
      );
    }
  }
  return finishCheck(
    "payouts",
    acc,
    allowInFlight,
    `${plural(ctx.l1.payouts.length, "L1 payout")} match finalized valid SQL withdrawals with equal l2_value, l1_address and l1_datum`,
  );
};

export const checkSettlements = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null || ctx.merged === null) {
    return skipCheck(
      "settlements",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  const merged = ctx.merged;
  const seen = new Map<string, string>();
  let compared = 0;
  for (const settlement of ctx.l1.settlements) {
    const label = `settlement ${settlement.outRef}`;
    if (
      settlement.tokens.length !== 1 ||
      settlement.tokens[0]!.quantity !== "1"
    ) {
      acc.failures.push(
        `${label}: expected exactly one settlement token, found ${settlement.tokens.map((t) => `${t.assetName}x${t.quantity}`).join(",") || "none"}`,
      );
      continue;
    }
    const headerHash = settlement.tokens[0]!.assetName;
    const duplicate = seen.get(headerHash);
    if (duplicate !== undefined) {
      acc.failures.push(
        `${label}: settlement for header ${headerHash} also at ${duplicate}`,
      );
    }
    seen.set(headerHash, settlement.outRef);
    const headerLabel = `${label} (header ${headerHash})`;
    const journal = ctx.journals.get(headerHash);
    if (!merged.headers.has(headerHash)) {
      if (ctx.onChainUnmerged.has(headerHash)) {
        acc.failures.push(
          `${headerLabel}: header is still unmerged on the L1 queue`,
        );
        continue;
      }
      if (merged.complete) {
        acc.failures.push(
          `${headerLabel}: header is not on the merged chain (merged walk: ${merged.stopReason})`,
        );
        continue;
      }
      if (journal === undefined) {
        acc.notes.push(
          `${headerLabel}: header predates locally known history (${merged.stopReason}); roots not comparable`,
        );
        continue;
      }
      acc.notes.push(
        `${headerLabel}: header lies beyond the locally known merged chain (${merged.stopReason}); roots still compared`,
      );
    }
    if (settlement.roots === null) {
      acc.failures.push(
        `${headerLabel}: settlement datum undecodable: ${settlement.decodeError ?? "missing"}`,
      );
      continue;
    }
    if (journal !== undefined) {
      compared += 1;
      acc.failures.push(
        ...rootsDiff(headerLabel, settlement.roots, journal.expected),
      );
    } else {
      acc.notes.push(
        `${headerLabel}: merged header without a local journal; roots not comparable`,
      );
    }
  }
  return finishCheck(
    "settlements",
    acc,
    allowInFlight,
    `${plural(ctx.l1.settlements.length, "L1 settlement")} belong to merged headers; ${plural(compared, "settlement datum", "settlement datums")} equal the local expected event roots`,
  );
};

type LedgerCacheAttempt = {
  readonly label: string;
  readonly missing: readonly string[];
  readonly unexpected: readonly string[];
  readonly mismatched: readonly string[];
  readonly spentAbsent: readonly string[];
};

export const attemptLedgerCache = (
  ctx: Context,
  point: LedgerPoint,
  depositByOutref: ReadonlyMap<string, SqlDepositRow>,
  deltas: readonly NonNullable<PendingTxDelta["delta"]>[],
): LedgerCacheAttempt => {
  const chain = new Set(point.chainHeaderHashes);
  const expected = new Map(point.entries);
  const cache = new Map(ctx.sql.mempoolLedger.map((row) => [row.outref, row]));
  // Deposits projected into the cache but not yet part of this ledger point.
  for (const row of ctx.sql.mempoolLedger) {
    if (row.sourceEventId === null || expected.has(row.outref)) continue;
    const deposit = depositByOutref.get(row.outref);
    if (deposit === undefined || deposit.payload.eventId !== row.sourceEventId)
      continue;
    if (deposit.status !== DepositsDB.Status.Projected) continue;
    if (deposit.payload.ledgerOutput !== row.output) continue;
    const header = deposit.projectedHeaderHash;
    const beyondPoint =
      header === null ||
      (!chain.has(header) &&
        (ctx.activeHeaders.has(header) ||
          (ctx.onChainUnmerged.has(header) && !isMerged(ctx, header))));
    if (beyondPoint) expected.set(row.outref, row.output);
  }
  // Projection writes a deposit into the cache and only a pending spend takes
  // it out, so a projected deposit no block holds yet is expected there even
  // when the cache lost it.
  for (const [outref, deposit] of depositByOutref) {
    if (
      deposit.status === DepositsDB.Status.Projected &&
      deposit.projectedHeaderHash === null &&
      !cache.has(outref)
    ) {
      expected.set(outref, deposit.payload.ledgerOutput);
    }
  }
  for (const delta of deltas) {
    for (const produced of delta.produced)
      expected.set(produced.outref, produced.output);
  }
  const spentAbsent: string[] = [];
  for (const delta of deltas) {
    for (const spent of delta.spent) {
      if (!expected.delete(spent) && !depositByOutref.has(spent)) {
        spentAbsent.push(spent);
      }
    }
  }
  const missing: string[] = [];
  const mismatched: string[] = [];
  for (const [outref, output] of expected) {
    const cached = cache.get(outref);
    if (cached === undefined) missing.push(outref);
    else if (cached.output !== output) mismatched.push(outref);
  }
  const unexpected = [...cache.keys()].filter(
    (outref) => !expected.has(outref),
  );
  return { label: point.label, missing, unexpected, mismatched, spentAbsent };
};

export const attemptIsClean = (attempt: LedgerCacheAttempt): boolean =>
  attempt.missing.length === 0 &&
  attempt.unexpected.length === 0 &&
  attempt.mismatched.length === 0 &&
  attempt.spentAbsent.length === 0;
