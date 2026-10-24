import {
  type LedgerPointResult,
  type NativeRootObservation,
  type ReconciliationCheck,
  type SqlStateSnapshot,
} from "./state-reconciliation.compares.js";
import {
  type Context,
  finishCheck,
  newAccumulator,
  plural,
  short,
  skipCheck,
} from "./state-reconciliation.walk-merged-chain.js";

// ---------------------------------------------------------------------------
// Checks
// ---------------------------------------------------------------------------

export const checkConfirmedRoot = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck(
      "confirmed-root",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  const l1Root = ctx.l1.confirmed.utxoRoot;
  const sqlRoot = ctx.sql.confirmedRoot;
  if (ctx.sql.confirmedRootError !== null) {
    acc.failures.push(
      `SQL confirmed_ledger (${plural(ctx.sql.confirmedEntryCount, "entry", "entries")}) cannot be encoded into the ledger MPF: ${ctx.sql.confirmedRootError}`,
    );
  } else if (l1Root !== sqlRoot) {
    // Lag diagnosis: SQL still at the pre-merge root of a header L1 has
    // already merged means the merge's SQL application has not run yet.
    const laggingJournal = [...(ctx.merged?.headers ?? [])]
      .map((hash) => ctx.journals.get(hash))
      .find((journal) => journal?.baseUtxosRoot === sqlRoot);
    if (laggingJournal !== undefined) {
      acc.inFlight.push(
        `SQL confirmed ledger root ${short(sqlRoot)} is the pre-merge root of header ${laggingJournal.headerHash}, which L1 has already merged (confirmed root ${short(l1Root)}); the merge has not been applied to SQL yet`,
      );
    } else {
      acc.failures.push(
        `L1 confirmed utxoRoot ${l1Root} != SQL confirmed_ledger root ${sqlRoot} (${plural(ctx.sql.confirmedEntryCount, "entry", "entries")})`,
      );
    }
  }
  return finishCheck(
    "confirmed-root",
    acc,
    allowInFlight,
    `L1 confirmed utxoRoot equals the SQL confirmed_ledger root ${sqlRoot} (${plural(ctx.sql.confirmedEntryCount, "entry", "entries")}, confirmed header ${ctx.l1.confirmed.headerHash})`,
  );
};

/**
 * Why no ledger point could be recomputed. A delta chain that stops at a
 * missing parent is a justified skip when the parent is history this node did
 * not produce (a foreign header); when SQL's confirmed ledger also disagrees
 * with L1 the chain stopped because the confirmed base is wrong, which the
 * confirmed-root check reports as the failure.
 */
export const unrecomputableReason = (
  ctx: Context,
  tip: LedgerPointResult,
): string => {
  const base = `no ledger point could be recomputed: ${tip.kind === "failed" ? tip.reason : "no candidate"}`;
  if (ctx.sqlMergeLag) {
    return `${base}; the SQL confirmed ledger root differs from L1, so the journal chain has no valid base (reported by confirmed-root)`;
  }
  return `${base}; the chain reaches history not produced by this node`;
};

export const unencodableConfirmedReason = (ctx: Context): string =>
  `the SQL confirmed ledger cannot be encoded, so no ledger point can be recomputed (reported by confirmed-root): ${ctx.sql.confirmedRootError ?? ""}`;

type NativeCandidate = { readonly label: string; readonly root: string };

const materializedCandidates = (sql: SqlStateSnapshot): NativeCandidate[] =>
  [sql.finalizedTip, sql.activeTip]
    .filter(
      (r): r is Extract<LedgerPointResult, { kind: "materialized" }> =>
        r?.kind === "materialized",
    )
    .map((r) => ({ label: r.point.label, root: r.point.root }));

export const checkNativeRoot = (
  ctx: Context,
  native: NativeRootObservation,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (native.kind === "unavailable") {
    return skipCheck("native-root", native.reason);
  }
  if (native.kind === "unhealthy") {
    const acc = newAccumulator();
    acc.failures.push(native.reason);
    return finishCheck("native-root", acc, allowInFlight, "");
  }
  if (ctx.sql.confirmedRootError !== null) {
    return skipCheck("native-root", unencodableConfirmedReason(ctx));
  }
  const tip = ctx.sql.finalizedTip;
  const candidates = materializedCandidates(ctx.sql);
  const acc = newAccumulator();
  if (tip.kind === "failed" && !tip.parentMissing) {
    acc.failures.push(
      `SQL journal delta chain for ${tip.label} does not reproduce its expected root: ${tip.reason}`,
    );
  }
  if (
    ctx.sql.activeTip?.kind === "failed" &&
    !ctx.sql.activeTip.parentMissing
  ) {
    acc.failures.push(
      `SQL journal delta chain for ${ctx.sql.activeTip.label} does not reproduce its expected root: ${ctx.sql.activeTip.reason}`,
    );
  }
  if (candidates.length === 0) {
    if (acc.failures.length > 0) {
      return finishCheck("native-root", acc, allowInFlight, "");
    }
    return skipCheck("native-root", unrecomputableReason(ctx, tip));
  }
  const matched = candidates.find((c) => c.root === native.root);
  if (matched === undefined) {
    acc.failures.push(
      `native root ${native.root} (${native.source}) matches no recomputed ledger point: ${candidates.map((c) => `${c.label}=${c.root}`).join("; ")}`,
    );
  } else if (tip.kind === "materialized" && matched.root !== tip.point.root) {
    acc.notes.push(
      `native root is one block ahead of the committed tip, at ${matched.label} (promotion follows block submission)`,
    );
  }
  if (tip.kind === "failed" && tip.parentMissing) {
    acc.notes.push(`committed tip not recomputable: ${tip.reason}`);
  }
  return finishCheck(
    "native-root",
    acc,
    allowInFlight,
    `native root ${native.root} (${native.source}) equals the recomputed root at ${matched?.label ?? "?"}`,
  );
};
