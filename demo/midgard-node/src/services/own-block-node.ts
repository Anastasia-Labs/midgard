/**
 * The state-queue node this node's own block created, read back from its
 * journal's retained signed commit. A merged node is on no queue to read
 * back, so local finalization of a revived block, and startup hydration of
 * an observed one, re-derive it here; local finalization binds it to the
 * journal by every header root.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { type LedgerSnapshotOutput } from "../l1-ledger-snapshot.js";

const C = Pending.Columns;

const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table: Pending.tableName, message, cause });

/** A queue output named by the header it commits and the header that header
 * links to (for the root, the confirmed header's predecessor). */
export type QueueNode = Readonly<{
  node: SDK.StateQueueUTxO;
  headerHash: string;
  prevHeaderHash: string;
}>;

type StateQueueContracts = Pick<SDK.MidgardValidators, "stateQueue">;

/** Authenticates one state-queue output and names it by the header it
 * commits and that header's predecessor (the root by its confirmed state). */
const authenticateNode = (
  output: LedgerSnapshotOutput,
  contracts: StateQueueContracts,
) =>
  Effect.gen(function* () {
    const { policyId, spendingScriptAddress } = contracts.stateQueue;
    if (
      output.address !== spendingScriptAddress ||
      output.hasReferenceScript ||
      output.datum === undefined ||
      output.datumHash !== undefined
    )
      return yield* Effect.fail(
        failure(
          `State-queue output ${output.txHash}#${output.outputIndex.toString()} is not an inline-datum queue node`,
        ),
      );
    const node = yield* SDK.utxoToStateQueueUTxO(
      {
        txHash: output.txHash,
        outputIndex: output.outputIndex,
        address: output.address,
        assets: { ...output.assets },
        datum: output.datum,
      },
      policyId,
    );
    let headerHash: string;
    let prevHeaderHash: string;
    if (node.assetName === SDK.STATE_QUEUE_ROOT_ASSET_NAME) {
      if (node.datum.key !== "Empty")
        return yield* Effect.fail(failure("State-queue root has a key"));
      const confirmed = (yield* SDK.getConfirmedStateFromStateQueueDatum(
        node.datum,
      )).data;
      headerHash = confirmed.headerHash;
      prevHeaderHash = confirmed.prevHeaderHash;
    } else {
      const header = yield* SDK.getHeaderFromStateQueueDatum(node.datum);
      headerHash = yield* SDK.hashBlockHeader(header);
      prevHeaderHash = header.prevHeaderHash;
      if (
        node.datum.key === "Empty" ||
        node.datum.key.Key.key !== headerHash ||
        node.assetName !==
          `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`
      )
        return yield* Effect.fail(
          failure(`State-queue node ${headerHash} is not keyed by its header`),
        );
    }
    return { node, headerHash, prevHeaderHash } satisfies QueueNode;
  });

/** The node a journal's signed commit created for its own block: that
 * commit's output carrying the block's node token, before any later
 * transaction continued or merged it. Bytes that do not hash to the
 * journal's intended transaction or create no such node fail closed. */
export const signedCommitNode = (
  record: Pending.Record,
  contracts: StateQueueContracts,
) =>
  Effect.gen(function* () {
    const header = record[C.HEADER_HASH].toString("hex");
    const intended = record[C.INTENDED_TX_HASH]?.toString("hex");
    const signed = record[C.SIGNED_TX_CBOR];
    const unit = `${contracts.stateQueue.policyId}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`;
    const outputs = yield* Effect.try({
      try: () => {
        if (intended === undefined || signed == null)
          throw new Error("the journal retains no signed commit");
        const tx = CML.Transaction.from_cbor_bytes(signed);
        const body = tx.body();
        try {
          if (CML.hash_transaction(body).to_hex() !== intended)
            throw new Error("its signed bytes are not its intended commit");
          const all = body.outputs();
          return Array.from({ length: all.len() }, (_, index) =>
            coreToTxOutput(all.get(index)),
          );
        } finally {
          body.free();
          tx.free();
        }
      },
      catch: (cause) =>
        failure(
          `Block ${header} landed, but its node cannot be read from its signed commit`,
          cause,
        ),
    });
    const outputIndex = outputs.findIndex(
      (output) => output.assets[unit] === 1n,
    );
    const output = outputs[outputIndex];
    if (output === undefined)
      return yield* Effect.fail(
        failure(`The signed commit of block ${header} creates no node for it`),
      );
    const created = yield* authenticateNode(
      {
        txHash: intended!,
        outputIndex,
        address: output.address,
        assets: { ...output.assets },
        ...(output.datum == null ? {} : { datum: output.datum }),
        ...(output.datumHash == null ? {} : { datumHash: output.datumHash }),
        hasReferenceScript: output.scriptRef != null,
      },
      contracts,
    );
    if (created.headerHash !== header)
      return yield* Effect.fail(
        failure(`The signed commit of block ${header} creates another node`),
      );
    return created;
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : failure(
            `The node the retained signed commit of block ${record[C.HEADER_HASH].toString("hex")} creates does not authenticate`,
            cause,
          ),
    ),
  );
