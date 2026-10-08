import {
  type NativeRootObservation,
  type ReconciliationCheck,
} from "./state-reconciliation.compares.js";
import {
  ACTIVE_JOURNAL_STATUSES,
  type Context,
  finishCheck,
  isMerged,
  JOURNAL_STATUS,
  newAccumulator,
  plural,
  rootsDiff,
  short,
  skipCheck,
} from "./state-reconciliation.walk-merged-chain.js";

export const checkStateQueueJournal = (
  ctx: Context,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null || ctx.merged === null) {
    return skipCheck(
      "state-queue-journal",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  const acc = newAccumulator();
  const merged = ctx.merged;
  let previousHeaderHash = ctx.l1.confirmed.headerHash;
  for (const header of ctx.l1.unmerged) {
    const label = `L1 header ${header.headerHash}`;
    if (header.decodeError !== null) {
      acc.failures.push(
        `${label}: datum header undecodable: ${header.decodeError}`,
      );
      previousHeaderHash = header.headerHash;
      continue;
    }
    if (header.recomputedHeaderHash !== header.headerHash) {
      acc.failures.push(
        `${label}: recomputed header hash ${header.recomputedHeaderHash ?? "<none>"} differs from its node key`,
      );
    }
    if (
      header.prevHeaderHash !== null &&
      header.prevHeaderHash !== previousHeaderHash
    ) {
      acc.failures.push(
        `${label}: prevHeaderHash ${header.prevHeaderHash} does not link to the preceding queue node ${previousHeaderHash}`,
      );
    }
    previousHeaderHash = header.headerHash;
    const journal = ctx.journals.get(header.headerHash);
    if (journal !== undefined) {
      if (header.roots !== null) {
        acc.failures.push(...rootsDiff(label, header.roots, journal.expected));
      }
      if (
        header.prevHeaderHash !== null &&
        journal.baseTailHeaderHash !== header.prevHeaderHash
      ) {
        acc.failures.push(
          `${label}: journal base tail ${journal.baseTailHeaderHash} differs from the header's prevHeaderHash ${header.prevHeaderHash}`,
        );
      }
      if (journal.status === JOURNAL_STATUS.LocallyApplied) continue;
      if (journal.status === JOURNAL_STATUS.Abandoned) {
        acc.failures.push(
          `${label}: its journal is marked abandoned (correction digest ${journal.correctionTransitionDigest ?? "<none>"}) but the header is still on L1`,
        );
        continue;
      }
      acc.notes.push(
        `${label}: journal is ${journal.status}, roots match, awaiting L1 stability`,
      );
      continue;
    }
    acc.failures.push(`${label}: no journal exists for this on-chain header`);
  }

  for (const hash of merged.abandonedOnChain) {
    acc.failures.push(
      `header ${hash} is on the merged L1 chain but its journal is marked abandoned`,
    );
  }

  for (const journal of ctx.sql.journals) {
    const onChain = ctx.onChainUnmerged.has(journal.headerHash);
    const isMergedHeader = merged.headers.has(journal.headerHash);
    if (
      journal.status === JOURNAL_STATUS.LocallyApplied &&
      !onChain &&
      !isMergedHeader
    ) {
      if (ctx.settlementHeaders.has(journal.headerHash)) {
        acc.notes.push(
          `finalized journal ${journal.headerHash} is merged per its settlement (merged walk: ${merged.stopReason})`,
        );
      } else if (
        !merged.complete &&
        journal.endTimeMs !== null &&
        journal.endTimeMs <= ctx.l1.confirmed.endTimeMs
      ) {
        acc.notes.push(
          `finalized journal ${journal.headerHash} lies beyond the locally known merged chain (${merged.stopReason}) and ends at or before the confirmed state; treated as merged`,
        );
      } else {
        acc.failures.push(
          `finalized journal ${journal.headerHash} is neither on the L1 queue nor merged (merged walk: ${merged.stopReason})`,
        );
      }
    }
    if (ACTIVE_JOURNAL_STATUSES.has(journal.status) && !onChain) {
      if (isMergedHeader) {
        acc.failures.push(
          `journal ${journal.headerHash} is still ${journal.status} but its header is already merged on L1`,
        );
      } else {
        acc.notes.push(
          `active journal ${journal.headerHash} (${journal.status}) is not on L1 yet${journal.submittedTxHash === null ? " (not submitted)" : ` (submitted in ${journal.submittedTxHash})`}`,
        );
      }
    }
  }
  if (ctx.sql.activeHeaderHashes.length > 1) {
    acc.failures.push(
      `${ctx.sql.activeHeaderHashes.length.toString()} journals are active at once: ${ctx.sql.activeHeaderHashes.join(", ")}`,
    );
  }

  for (const removal of ctx.sql.queueRemovals) {
    const label = `header ${removal.headerHash} removed by landed tx ${removal.transactionHash}`;
    if (ctx.onChainUnmerged.has(removal.headerHash)) {
      acc.failures.push(`${label} is still on the L1 queue`);
    }
    const journal = ctx.journals.get(removal.headerHash);
    if (journal === undefined) {
      acc.notes.push(`${label}: no local journal (not produced by this node)`);
    } else if (journal.status !== JOURNAL_STATUS.Abandoned) {
      acc.inFlight.push(
        `${label}: journal is still ${journal.status}; the landed-block rebase has not disposed of it yet`,
      );
    }
  }

  for (const hash of ctx.sql.blockHeaderHashes) {
    if (ctx.onChainUnmerged.has(hash) || ctx.activeHeaders.has(hash)) continue;
    const journal = ctx.journals.get(hash);
    if (merged.headers.has(hash) && ctx.sqlMergeLag) {
      acc.inFlight.push(
        `blocks table still references merged header ${hash}; SQL has not applied the latest L1 merge`,
      );
      continue;
    }
    acc.failures.push(
      `blocks table still references header ${hash}, which is neither on the L1 queue nor the active journal (journal status ${journal?.status ?? "<none>"}${merged.headers.has(hash) ? ", merged" : ""})`,
    );
  }

  return finishCheck(
    "state-queue-journal",
    acc,
    allowInFlight,
    `${plural(ctx.l1.unmerged.length, "unmerged L1 header")} and ${plural(ctx.sql.journals.length, "journal")} agree (merged walk: ${merged.stopReason}, ${plural(merged.headers.size, "merged header")})`,
  );
};

export const checkTailRoot = (
  ctx: Context,
  native: NativeRootObservation,
  allowInFlight: boolean,
): ReconciliationCheck => {
  if (ctx.l1 === null) {
    return skipCheck(
      "state-queue-tail-root",
      `L1 unavailable: ${ctx.l1UnavailableReason}`,
    );
  }
  if (native.kind === "unavailable") {
    return skipCheck("state-queue-tail-root", native.reason);
  }
  if (native.kind === "unhealthy") {
    return skipCheck(
      "state-queue-tail-root",
      `no native root to compare (reported by native-root): ${native.reason}`,
    );
  }
  const tail = ctx.l1.unmerged.at(-1);
  const tailRoot =
    tail === undefined
      ? ctx.l1.confirmed.utxoRoot
      : (tail.roots?.utxos ?? null);
  const tailLabel =
    tail === undefined
      ? `confirmed state ${ctx.l1.confirmed.headerHash}`
      : `tail header ${tail.headerHash}`;
  const acc = newAccumulator();
  if (tailRoot === null) {
    acc.failures.push(`${tailLabel} datum is undecodable`);
  } else if (tailRoot !== native.root) {
    const active = ctx.sql.activeTip;
    if (
      active?.kind === "materialized" &&
      active.point.root === native.root &&
      active.point.headerHash !== null &&
      !ctx.onChainUnmerged.has(active.point.headerHash)
    ) {
      acc.inFlight.push(
        `native root is at ${active.point.label}, whose header is not on L1 yet (${tailLabel} root ${short(tailRoot)})`,
      );
    } else {
      acc.failures.push(
        `${tailLabel} utxosRoot ${tailRoot} != native root ${native.root} (${native.source})`,
      );
    }
  }
  return finishCheck(
    "state-queue-tail-root",
    acc,
    allowInFlight,
    `${tailLabel} utxosRoot equals the native root ${native.root} (${native.source})`,
  );
};

type HeaderPlacement =
  | "none"
  | "on-chain"
  | "merged"
  | "active-pending"
  | "unknown";

export const placeHeader = (
  ctx: Context,
  headerHash: string | null,
): HeaderPlacement => {
  if (headerHash === null) return "none";
  if (ctx.onChainUnmerged.has(headerHash)) return "on-chain";
  if (isMerged(ctx, headerHash)) return "merged";
  if (ctx.activeHeaders.has(headerHash)) return "active-pending";
  return "unknown";
};

export const describeHeaderPlacement = (
  ctx: Context,
  headerHash: string,
): string => {
  const journal = ctx.journals.get(headerHash);
  return `header ${headerHash} is not on the L1 queue, not merged and not the active journal (journal status ${journal?.status ?? "<none>"}; merged walk ${ctx.merged?.stopReason ?? "n/a"})`;
};

export const payloadDiff = <P extends Record<string, unknown>>(
  l1: P,
  local: P,
): string[] => Object.keys(l1).filter((key) => l1[key] !== local[key]);

export const missingOrderIsImmature = (
  ctx: Context,
  inclusionTimeMs: number | undefined,
): boolean =>
  inclusionTimeMs !== undefined &&
  ctx.latestCommittedEndTimeMs !== null &&
  inclusionTimeMs > ctx.latestCommittedEndTimeMs;
