import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import { withLease } from "./availability-challenge-operation.build-da-availability-funding-preparation-tx.js";
import {
  type DaAvailabilityForeignSpend,
  type DaAvailabilityOperationContext,
  type DaAvailabilityOperationResult,
  inspectDaAvailabilitySignedIntent,
} from "./availability-challenge-operation.inspect-da-availability-signed-intent.js";
import type { DaAvailabilityReadScope } from "./availability-challenge-operation.read-scope.js";
import {
  authenticates,
  reconcile,
  retireIncluded,
} from "./availability-challenge-operation.reconcile.js";

export const reconcileDaAvailabilityOperations = (
  context: DaAvailabilityOperationContext,
): Promise<readonly DaAvailabilityOperationResult[]> =>
  withLease(context, async (lease, assertCurrent, observationScope) => {
    const results: DaAvailabilityOperationResult[] = [];
    const anchors = context.journal.finalizedAnchors(context.actor);
    const records = [
      ...context.journal.pending(context.deploymentIdentity, context.actor),
      ...context.journal.unfinalized(context.deploymentIdentity, context.actor),
      ...anchors.filter(
        ({ intent }) =>
          intent.deploymentIdentity === context.deploymentIdentity,
      ),
    ];
    const byTransaction = new Map(
      records.map((record) => [record.intent.txHash, record]),
    );
    // A confirmed anchor can now be rewound, and its confirmed ancestors may
    // have left the chain with it, so they join the audit. While the anchor
    // stays confirmed it covers them below and they are not observed.
    const parentOf = (ref: string): (typeof records)[number] | undefined => {
      const txHash = ref.split("#")[0]!;
      const known = byTransaction.get(txHash);
      if (known) return known;
      const found = context.journal.findTransaction(txHash);
      if (
        found?.state !== "confirmed" ||
        found.intent.deploymentIdentity !== context.deploymentIdentity ||
        found.intent.actor !== context.actor
      )
        return undefined;
      byTransaction.set(txHash, found);
      return found;
    };
    const ordered: typeof records = [];
    const visiting = new Set<string>();
    const visited = new Set<string>();
    const visit = (record: (typeof records)[number]): void => {
      if (visited.has(record.intent.id)) return;
      if (visiting.has(record.intent.id))
        throw new Error(
          "Availability journal contains cyclic transaction dependencies",
        );
      visiting.add(record.intent.id);
      for (const ref of record.intent.spentOutRefs) {
        const parent = parentOf(ref);
        if (parent) visit(parent);
      }
      visiting.delete(record.intent.id);
      visited.add(record.intent.id);
      ordered.push(record);
    };
    records.forEach(visit);
    const covered = new Set<string>();
    for (const record of [...ordered].reverse()) {
      if (covered.has(record.intent.id)) continue;
      const outcome = await reconcile(
        context,
        lease,
        record.intent,
        assertCurrent,
        observationScope(),
      );
      results.push(outcome);
      if (outcome.status !== "included" && outcome.status !== "confirmed")
        continue;
      const ancestors = [...record.intent.spentOutRefs];
      while (ancestors.length > 0) {
        const ref = ancestors.pop()!;
        const parent = byTransaction.get(ref.split("#")[0]!);
        if (!parent || covered.has(parent.intent.id)) continue;
        // An unfinalized child proves ancestry, but does not replace the audit
        // of a previously finalized parent after a possible deep rollback.
        if (parent.state === "confirmed" && outcome.status !== "confirmed")
          continue;
        covered.add(parent.intent.id);
        if (parent.state !== "confirmed") {
          const verified = inspectDaAvailabilitySignedIntent(parent.intent);
          if (JSON.stringify(verified) !== JSON.stringify(parent.intent))
            throw new Error("Corrupt availability ancestor intent");
          context.journal.transition(
            lease,
            parent.intent.id,
            outcome.status,
            `ancestor-of:${record.intent.txHash}`,
            null,
            (context.nowMs ?? Date.now)(),
          );
        }
        ancestors.push(...parent.intent.spentOutRefs);
      }
    }
    // A rolled-back child may be visited before its ancestor's expiry is proved.
    // Revisit only those children, now in dependency order, so an arbitrarily
    // long expired chain releases in one recovery pass rather than one per tick.
    // A child still recorded as confirmed is revisited too: an expired parent
    // contradicts it, and only that contradiction can rewind it.
    for (const record of ordered) {
      const index = results.findIndex(
        (result) =>
          result.txHash === record.intent.txHash && result.status === "waiting",
      );
      if (index < 0) continue;
      if (
        !record.intent.spentOutRefs.some(
          (ref) =>
            context.journal.findTransaction(ref.split("#")[0]!)?.state ===
            "expired",
        )
      )
        continue;
      results[index] = await reconcile(
        context,
        lease,
        record.intent,
        assertCurrent,
        observationScope(),
      );
    }
    const readBoundary = context.readBoundary;
    if (readBoundary !== undefined) {
      const scope = observationScope();
      const boundary = await scope.read(async () => {
        await assertCurrent(scope);
        const result = await readBoundary(scope);
        scope.assertCurrent();
        await assertCurrent(scope);
        return result;
      });
      context.journal.pruneExpired(
        lease,
        boundary.blockNo,
        DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
        (context.nowMs ?? Date.now)(),
      );
    }
    // A redeploy leaves earlier deployments' confirmed intents behind. This
    // deployment never reconciles, rewinds or rebroadcasts them, but they
    // retire under the same rules: authenticated inclusion frees their inputs
    // past validity and prunes them past recovery depth. Other evidence
    // changes nothing and is read again on the next pass.
    for (const { intent } of anchors) {
      if (intent.deploymentIdentity === context.deploymentIdentity) continue;
      const scope = observationScope();
      const observed = await scope.read(async () => {
        await assertCurrent(scope);
        const result = await context.observe(intent, scope);
        scope.assertCurrent();
        await assertCurrent(scope);
        return result;
      });
      if (
        observed.status === "included" &&
        authenticates(intent, observed) &&
        observed.confirmationDepth >= context.minimumConfirmationDepth
      )
        retireIncluded(context, lease, intent, observed);
    }
    return results;
  });

export type DaAvailabilityCanonicalBoundary = Readonly<{
  pointId: string;
  slot: number;
  blockNo?: number;
}>;

/**
 * Positive evidence that `outRef` was consumed by the canonical transaction
 * `transactionId`, read from its raw bytes: they hash to that id, the
 * transaction is phase-2 valid (it spent its inputs, not its collateral), and
 * its inputs list the outRef. A Kupo `spent_at` claim is trusted only through
 * this check.
 */
export const transactionConsumesOutRef = ({
  transactionCbor,
  transactionId,
  outRef,
}: {
  readonly transactionCbor: string;
  readonly transactionId: string;
  readonly outRef: string;
}): boolean => {
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  } catch {
    return false;
  }
  const body = transaction.body();
  if (
    CML.hash_transaction(body).to_hex() !== transactionId ||
    !transaction.is_valid()
  )
    return false;
  const inputs = body.inputs();
  return Array.from({ length: inputs.len() }, (_, index) =>
    inputs.get(index),
  ).some(
    (entry) =>
      `${entry.transaction_id().to_hex()}#${entry.index().toString()}` ===
      outRef,
  );
};

/**
 * Positive evidence that `outRef` was consumed as collateral by the
 * canonical transaction `transactionId`, read from its raw bytes: they hash
 * to that id, the transaction failed phase 2 (it spent its collateral, not
 * its inputs), and its collateral inputs list the outRef.
 */
export const transactionConsumesCollateral = ({
  transactionCbor,
  transactionId,
  outRef,
}: {
  readonly transactionCbor: string;
  readonly transactionId: string;
  readonly outRef: string;
}): boolean => {
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  } catch {
    return false;
  }
  const body = transaction.body();
  const collateral = body.collateral_inputs();
  if (
    CML.hash_transaction(body).to_hex() !== transactionId ||
    transaction.is_valid() ||
    collateral === undefined
  )
    return false;
  return Array.from({ length: collateral.len() }, (_, index) =>
    collateral.get(index),
  ).some(
    (entry) =>
      `${entry.transaction_id().to_hex()}#${entry.index().toString()}` ===
      outRef,
  );
};

/** A chain point as the foreign-spend readers name it. */
export type DaAvailabilityChainPoint = Readonly<{
  slot: number;
  blockHash: string;
}>;

/**
 * The L1 readers the verified foreign-spend check runs on. Each caller
 * injects its own Kupo and Ogmios transport; none has a default.
 */
export type DaAvailabilityForeignSpendReaders = Readonly<{
  /** The canonical boundary, with the block height of its point. */
  readBoundary: (
    scope?: DaAvailabilityReadScope,
  ) => Promise<Readonly<{ pointId: string; blockNo: number }>>;
  /** Kupo's exact-match `spent_at`, or undefined when it reports no spend. */
  fetchSpend: (
    outRef: Readonly<{ txHash: string; outputIndex: number }>,
    scope?: DaAvailabilityReadScope,
  ) => Promise<
    | Readonly<{ transactionId: string; point: DaAvailabilityChainPoint }>
    | undefined
  >;
  /** A Kupo checkpoint strictly before `slot`, to intersect chain-sync at. */
  fetchAncestor: (
    slot: number,
    scope?: DaAvailabilityReadScope,
  ) => Promise<DaAvailabilityChainPoint>;
  /**
   * The transaction `txHash`, read by chain-sync from `ancestor` forward to
   * the exact block `point`, with that block's height and, when Ogmios serves
   * it, the raw transaction. Undefined when the block does not carry it.
   */
  readTransaction: (
    input: Readonly<{
      ancestor: DaAvailabilityChainPoint;
      point: DaAvailabilityChainPoint;
      txHash: string;
    }>,
    scope?: DaAvailabilityReadScope,
  ) => Promise<
    | Readonly<{
        txHash: string;
        point: DaAvailabilityChainPoint & Readonly<{ blockNo: number }>;
        cbor?: string;
      }>
    | undefined
  >;
}>;

/** A verified spend, with the consuming transaction's checked bytes. */
export type DaAvailabilityVerifiedForeignSpend = DaAvailabilityForeignSpend &
  Readonly<{
    /** The raw transaction {@link transactionConsumesOutRef} accepted. */
    spendingTransactionCbor: string;
  }>;

/**
 * The canonical spend of `outRef`, verified from the consuming transaction's
 * own bytes through {@link transactionConsumesOutRef} (or, for `consumes:
 * "collateral"`, {@link transactionConsumesCollateral}). Kupo's `spent_at`
 * alone is never trusted, so a spend that fails verification reads as none.
 * The whole read sits inside one canonical boundary; a moved boundary, a
 * spend above it, or a spend Ogmios serves without its raw bytes throws.
 */
export const resolveDaAvailabilityForeignSpend = async (
  input: DaAvailabilityForeignSpendReaders &
    Readonly<{
      outRef: string;
      scope?: DaAvailabilityReadScope;
      /** What the spend must consume `outRef` as: an input (the default) or collateral. */
      consumes?: "inputs" | "collateral";
    }>,
): Promise<DaAvailabilityVerifiedForeignSpend | undefined> => {
  const [txHash, outputIndex] = input.outRef.split("#");
  const scope = input.scope;
  const read = <T>(run: () => Promise<T>) =>
    scope === undefined ? run() : scope.read(run);
  const before = await read(() => input.readBoundary(scope));
  const spend = await read(() =>
    input.fetchSpend(
      {
        txHash: txHash!,
        outputIndex: Number(outputIndex),
      },
      scope,
    ),
  );
  if (spend === undefined) return undefined;
  const ancestor = await read(() =>
    input.fetchAncestor(spend.point.slot, scope),
  );
  const transaction = await read(() =>
    input.readTransaction(
      {
        ancestor,
        point: spend.point,
        txHash: spend.transactionId,
      },
      scope,
    ),
  );
  const after = await read(() => input.readBoundary(scope));
  if (before.pointId !== after.pointId)
    throw new Error(
      "Availability input spend changed during its canonical read",
    );
  if (transaction !== undefined && transaction.point.blockNo > after.blockNo)
    throw new Error(
      "Availability input spend lies above the canonical boundary",
    );
  if (transaction === undefined || transaction.txHash !== spend.transactionId)
    return undefined;
  if (transaction.cbor === undefined)
    throw new Error(
      "Ogmios must run with --include-transaction-cbor to verify a rival spend",
    );
  if (
    !(
      input.consumes === "collateral"
        ? transactionConsumesCollateral
        : transactionConsumesOutRef
    )({
      transactionCbor: transaction.cbor,
      transactionId: spend.transactionId,
      outRef: input.outRef,
    })
  )
    return undefined;
  return {
    outRef: input.outRef,
    spendingTxHash: spend.transactionId,
    spendPoint: `${transaction.point.slot}:${transaction.point.blockHash}`,
    confirmationDepth: after.blockNo - transaction.point.blockNo,
    spendingTransactionCbor: transaction.cbor,
  };
};

/** Why a stranded challenge workflow row may be released (P20). */
export type DaAvailabilityWorkflowRelease = Readonly<{
  /**
   * `header-node-burned`: a transaction burned the header's queue node.
   * `challenge-closed`: a transaction spent the challenge record and minted
   * nothing under the queue policy, which is a Close by anyone.
   */
  reason: "header-node-burned" | "challenge-closed";
  txHash: string;
  spendPoint: string;
  confirmationDepth: number;
}>;

/** The most node-chain hops one release check walks. */
export const DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS = 256;

/**
 * The header's node chain is longer than one release check walks. The row is
 * kept and the check runs again on the next reconciliation.
 */
export class DaAvailabilityWorkflowReleaseHopCapError extends Error {
  constructor(headerHash: string) {
    super(
      `Availability workflow release walk for header ${headerHash} reached ${DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS.toString()} hops without a terminal transaction`,
    );
    this.name = "DaAvailabilityWorkflowReleaseHopCapError";
  }
}

export const transactionOutputs = (
  body: CML.TransactionBody,
): ReturnType<typeof coreToTxOutput>[] =>
  Array.from({ length: body.outputs().len() }, (_, index) =>
    coreToTxOutput(body.outputs().get(index)),
  );

export const transactionInputRefs = (body: CML.TransactionBody): string[] => {
  const inputs = body.inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const entry = inputs.get(index);
    return `${entry.transaction_id().to_hex()}#${entry.index().toString()}`;
  });
};
