import type { AvailabilityOperationRecord } from "@al-ft/midgard-core/availability-operation-journal";
import {
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type DaAvailabilityChallengeRecord,
  parseDaAvailabilityChallengeRecordCbor,
} from "./availability-challenge.js";
import {
  type DaAvailabilityForeignSpend,
  type DaAvailabilityOperationContext,
  type DaAvailabilityOperationObservation,
} from "./availability-challenge-operation.inspect-da-availability-signed-intent.js";
import {
  DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS,
  type DaAvailabilityCanonicalBoundary,
  type DaAvailabilityForeignSpendReaders,
  type DaAvailabilityWorkflowRelease,
  DaAvailabilityWorkflowReleaseHopCapError,
  resolveDaAvailabilityForeignSpend,
  transactionInputRefs,
  transactionOutputs,
} from "./availability-challenge-operation.reconcile-da-availability-operations.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "./linked-list.js";

/** Indices of the outputs holding any quantity of `unit`. */
const outputsCarrying = (
  outputs: readonly ReturnType<typeof coreToTxOutput>[],
  unit: string,
): number[] =>
  outputs.flatMap((output, index) =>
    (output.assets[unit] ?? 0n) === 0n ? [] : [index],
  );

const mintOf = (
  mint: CML.Mint | undefined,
  policy: string,
  assetName: string,
): bigint | undefined =>
  mint?.get(CML.ScriptHash.from_hex(policy), CML.AssetName.from_hex(assetName));

const mintsUnderPolicy = (
  mint: CML.Mint | undefined,
  policy: string,
): boolean => {
  const assets = mint?.get_assets(CML.ScriptHash.from_hex(policy));
  return assets !== undefined && assets.len() > 0;
};

/**
 * Evidence that this actor's challenge workflow for `headerHash` ended in a
 * terminal step someone else landed, or undefined when there is none yet
 * (P20). From the confirmed Open's signed bytes it derives, by asset and never
 * by index, the queue policy (the one policy under which an Open output holds
 * the header's node NFT), that node output Q0 and the challenge record output
 * R0 (the output holding the challenge asset the Open minted, whose datum
 * decodes as the header's record). It then walks the header's node chain from
 * Q0, one verified spend per hop, until a transaction either burns the node
 * (`header-node-burned`) or spends R0 while minting nothing under the queue
 * policy (`challenge-closed`). Any other hop continues from the one output
 * holding the node NFT.
 *
 * Only positive, verified, finalized evidence counts: every hop's spend comes
 * from {@link resolveDaAvailabilityForeignSpend} and must be at least
 * `minimumConfirmationDepth` deep. A missing or ambiguous derivation, a
 * missing spend, a shallow spend or a failed verification returns undefined;
 * a reader error or a spend above the boundary throws. Consuming the record
 * alone never releases: a Timeout with a descendant spends R0 and burns the
 * descendant's node, while the header's node continues.
 */
export const resolveDaAvailabilityWorkflowRelease = async (
  readers: DaAvailabilityForeignSpendReaders,
  openIntent: Pick<AvailabilityOperationRecord, "intent" | "state">,
  headerHash: string,
  minimumConfirmationDepth: number,
): Promise<DaAvailabilityWorkflowRelease | undefined> => {
  if (
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth <= 0
  )
    throw new Error("Invalid availability workflow release finality depth");
  const { intent } = openIntent;
  if (
    openIntent.state !== "confirmed" ||
    intent.action !== "open" ||
    intent.headerHash !== headerHash
  )
    return undefined;
  let open: CML.Transaction;
  try {
    open = CML.Transaction.from_cbor_hex(intent.signedCbor);
  } catch {
    return undefined;
  }
  const openBody = open.body();
  if (CML.hash_transaction(openBody).to_hex() !== intent.txHash)
    return undefined;
  const nodeAssetName = STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
  const openOutputs = transactionOutputs(openBody);
  const nodeHolders = openOutputs.flatMap((output, index) =>
    Object.entries(output.assets).flatMap(([unit, quantity]) =>
      unit.length === 56 + nodeAssetName.length &&
      unit.slice(56) === nodeAssetName &&
      quantity !== 0n
        ? [{ policy: unit.slice(0, 56), index, quantity }]
        : [],
    ),
  );
  if (nodeHolders.length !== 1 || nodeHolders[0]!.quantity !== 1n)
    return undefined;
  const queuePolicy = nodeHolders[0]!.policy;
  const nodeUnit = queuePolicy + nodeAssetName;
  const openMint = openBody.mint();
  const records = openOutputs.flatMap((output, index) => {
    if (typeof output.datum !== "string") return [];
    let record: DaAvailabilityChallengeRecord;
    try {
      record = parseDaAvailabilityChallengeRecordCbor(output.datum);
    } catch {
      return [];
    }
    if (record.commitment.header_hash !== headerHash) return [];
    const minted = Object.entries(output.assets).filter(
      ([unit, quantity]) =>
        unit.length === 56 + record.challenge_asset_name.length &&
        unit.slice(56) === record.challenge_asset_name &&
        quantity === 1n &&
        mintOf(openMint, unit.slice(0, 56), record.challenge_asset_name) === 1n,
    );
    return minted.length === 1 ? [index] : [];
  });
  if (records.length !== 1) return undefined;
  const recordOutRef = `${intent.txHash}#${records[0]!.toString()}`;
  let anchor = `${intent.txHash}#${nodeHolders[0]!.index.toString()}`;
  for (let hop = 0; hop < DA_AVAILABILITY_WORKFLOW_RELEASE_MAX_HOPS; hop++) {
    const spend = await resolveDaAvailabilityForeignSpend({
      ...readers,
      outRef: anchor,
    });
    if (
      spend === undefined ||
      spend.confirmationDepth < minimumConfirmationDepth
    )
      return undefined;
    const body = CML.Transaction.from_cbor_hex(
      spend.spendingTransactionCbor,
    ).body();
    const mint = body.mint();
    const evidence = {
      txHash: spend.spendingTxHash,
      spendPoint: spend.spendPoint,
      confirmationDepth: spend.confirmationDepth,
    };
    if (mintOf(mint, queuePolicy, nodeAssetName) === -1n)
      return { reason: "header-node-burned", ...evidence };
    if (
      transactionInputRefs(body).includes(recordOutRef) &&
      !mintsUnderPolicy(mint, queuePolicy)
    )
      return { reason: "challenge-closed", ...evidence };
    const next = outputsCarrying(transactionOutputs(body), nodeUnit);
    if (next.length !== 1) return undefined;
    anchor = `${spend.spendingTxHash}#${next[0]!.toString()}`;
  }
  throw new DaAvailabilityWorkflowReleaseHopCapError(headerHash);
};

/**
 * Provider adapter for a configured local canonical source. Boundary reads must
 * prove that the UTxO index and node share the same chain point. Inclusion depth
 * counts blocks, never elapsed slots. Source failures produce no mutation.
 */
export const createDaAvailabilityOperationObserver =
  (
    input: Readonly<{
      lucid: LucidEvolution;
      readBoundary: () => Promise<DaAvailabilityCanonicalBoundary>;
      /** Needed for providers whose transaction status omits block depth. */
      resolveInclusion?: (
        output: UTxO,
      ) => Promise<
        Readonly<{ slot?: number; blockHash?: string; depth?: number }>
      >;
      /**
       * The verified canonical spend of a missing normal input, or undefined
       * when there is none or it fails verification. Without it the observer
       * reports no `foreignSpends`.
       */
      resolveForeignSpend?: (
        outRef: string,
      ) => Promise<Omit<DaAvailabilityForeignSpend, "outRef"> | undefined>;
    }>,
  ): DaAvailabilityOperationContext["observe"] =>
  async (intent) => {
    const before = await input.readBoundary();
    if (
      !before.pointId ||
      !Number.isSafeInteger(before.slot) ||
      before.slot < 0
    ) {
      throw new Error(
        "Availability observer requires an aligned canonical boundary",
      );
    }
    const status = await input.lucid.transactionStatus(intent.txHash);
    let observation: DaAvailabilityOperationObservation;
    if (status.txHash !== intent.txHash)
      throw new Error(
        "Availability provider returned a foreign transaction status",
      );
    if (status.status === "confirmed") {
      const confirmation = status.confirmation;
      let point = {
        slot: confirmation.slot,
        blockHash: confirmation.blockHash,
        depth:
          confirmation.confirmations === undefined
            ? undefined
            : confirmation.confirmations - 1,
      };
      if (point.depth === undefined && input.resolveInclusion) {
        const body = CML.Transaction.from_cbor_hex(intent.signedCbor).body();
        if (body.outputs().len() === 0)
          throw new Error("Availability operation has no outputs");
        point = {
          ...point,
          ...(await input.resolveInclusion({
            ...coreToTxOutput(body.outputs().get(0)),
            txHash: intent.txHash,
            outputIndex: 0,
          })),
        };
      }
      observation =
        confirmation.txHash === intent.txHash &&
        typeof point.blockHash === "string" &&
        /^[0-9a-f]{64}$/u.test(point.blockHash) &&
        point.slot !== undefined &&
        Number.isSafeInteger(point.slot) &&
        point.slot >= 0 &&
        point.slot <= before.slot &&
        point.depth !== undefined &&
        Number.isSafeInteger(point.depth) &&
        point.depth >= 0
          ? {
              status: "included",
              txHash: intent.txHash,
              inclusionPoint: `${point.slot}:${point.blockHash}`,
              confirmationDepth: point.depth,
            }
          : {
              status: "unknown",
              reason: "Canonical inclusion depth is not available",
            };
    } else if (status.status === "pending") {
      observation = {
        status: "unknown",
        reason: "Signed availability transaction is pending",
      };
    } else {
      const refs = [...intent.spentOutRefs, ...intent.collateralOutRefs];
      const available = await input.lucid.utxosByOutRef(
        refs.map((ref) => {
          const [txHash, outputIndex] = ref.split("#");
          return { txHash: txHash!, outputIndex: Number(outputIndex) };
        }),
      );
      const keys = new Set(
        available.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
      );
      const missingOutRefs = refs.filter((ref) => !keys.has(ref));
      const foreignSpends: DaAvailabilityForeignSpend[] = [];
      if (input.resolveForeignSpend)
        for (const ref of intent.spentOutRefs) {
          if (keys.has(ref)) continue;
          const spend = await input.resolveForeignSpend(ref);
          if (spend)
            foreignSpends.push({
              outRef: ref,
              spendingTxHash: spend.spendingTxHash,
              spendPoint: spend.spendPoint,
              confirmationDepth: spend.confirmationDepth,
            });
        }
      observation =
        missingOutRefs.length === 0
          ? { status: "unspent", currentSlot: before.slot }
          : {
              status: "inputs_missing",
              currentSlot: before.slot,
              missingOutRefs,
              ...(foreignSpends.length === 0 ? {} : { foreignSpends }),
            };
    }
    const after = await input.readBoundary();
    if (before.pointId !== after.pointId || before.slot !== after.slot) {
      return {
        status: "unknown",
        reason: "Canonical source changed during availability reconciliation",
      };
    }
    return observation;
  };

export type DaAvailabilityOperationBuild = Readonly<{
  tx: TxSignBuilder;
  /** Timeout only: `min(penalty, taken)`, the slashed share of the fee. */
  timeoutFeePartLovelace?: bigint;
}>;
