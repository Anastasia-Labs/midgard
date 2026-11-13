/**
 * The stack drill's view of a state queue whose blocks have merged into its
 * root. Our node merges any attested block once it matures, whoever's shift
 * it is, so on an idle stack the queue can hold only its root, and a block
 * the drill read as the tail can merge before the drill commits after it.
 *
 * A commit on an empty queue spends the root as its anchor, with no
 * confirmed-state or head reference inputs
 * (onchain/aiken/validators/state-queue.ak:1157-1191). The new header must
 * continue the root's confirmed state: its previous header hash, previous
 * UTxO root, start time and protocol version follow the root's
 * (state-queue.ak:124-138). A merge sets the root to the merged block's
 * header hash, UTxO root and end time (state-queue.ak:401-409), so a block
 * built on a block that has since merged commits unchanged on the root.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToUtxo, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import type { PublishedHeaderConsumption } from "midgard-watcher/tests/support/published-da-target-consumption";
import { appendPointAfter } from "midgard-watcher/tests/support/published-deposit-trace.queue-tail";

import { readKupoUnitConsumptions } from "./kupo-consumption.js";

type StateQueueContract = Readonly<{
  spendingScriptAddress: string;
  policyId: string;
}>;

type ReadConfirmedTransaction = (
  txHash: string,
) => Promise<{ readonly cbor: string }>;

/** What a merge leaves in the root of the block it merged. */
export type QueueRootState = Pick<
  SDK.ConfirmedState,
  "headerHash" | "prevHeaderHash" | "utxoRoot" | "endTime"
>;

/** The queue as one read sees it: its root's state and its blocks, in order. */
export type StateQueueRead<Entry> = Readonly<{
  root: QueueRootState;
  blocks: readonly Entry[];
}>;

const nodeUnitOf = (policyId: string, headerHash: string) =>
  toUnit(policyId, SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash);

const rootUnitOf = (policyId: string) =>
  toUnit(policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME);

const outRefOf = (ref: { txHash: string; outputIndex: number }) =>
  `${ref.txHash}#${ref.outputIndex.toString()}`;

/** A transaction's outputs, each at its own reference. */
const transactionOutputs = (transaction: CML.Transaction) => {
  const txHash = CML.hash_transaction(transaction.body());
  const outputs = transaction.body().outputs();
  return Array.from({ length: outputs.len() }, (_, index) =>
    coreToUtxo(
      CML.TransactionUnspentOutput.new(
        CML.TransactionInput.new(txHash, BigInt(index)),
        outputs.get(index),
      ),
    ),
  );
};

/**
 * The queue's blocks for the tail waits, with the root standing in as the
 * settled tail once every block has merged. The root holds the last merged
 * block, which counts as attested and whose time is the root's end time. A
 * block that left the queue since an earlier read and that the root now
 * holds, or names as its parent, merged rather than being removed: reading
 * from it starts at the root. Only the block a wait returns is decoded, and
 * `mergedRoot` wraps the root without reading anything further.
 */
export const settledStateQueue = <
  Entry extends { headerHash: string },
  Node,
>(ports: {
  readQueue(): Promise<StateQueueRead<Entry>>;
  decodeBlock(entry: Entry): Promise<Node>;
  mergedRoot(
    root: QueueRootState,
    node: { headerHash: string; blockTime: bigint; attested: true },
  ): Node;
}) => {
  const rootNode = (root: QueueRootState) =>
    ports.mergedRoot(root, {
      headerHash: root.headerHash,
      blockTime: root.endTime,
      attested: true,
    });
  return {
    readTail: async () => {
      const { root, blocks } = await ports.readQueue();
      const tail = blocks.at(-1);
      return tail === undefined ? rootNode(root) : ports.decodeBlock(tail);
    },
    readFrom: async (headerHash: string) => {
      const { root, blocks } = await ports.readQueue();
      const index = blocks.findIndex(
        (block) => block.headerHash === headerHash,
      );
      if (index !== -1)
        return Promise.all(blocks.slice(index).map(ports.decodeBlock));
      if (headerHash !== root.headerHash && headerHash !== root.prevHeaderHash)
        return undefined;
      return [
        rootNode(root),
        ...(await Promise.all(blocks.map(ports.decodeBlock))),
      ];
    },
  };
};

/** Where a merge is looked up: Kupo's spends, and authenticated L1 bodies. */
type MergeLookup = Readonly<{
  kupoUrl: string;
  stateQueue: StateQueueContract;
  readConfirmedTransaction: ReadConfirmedTransaction;
  fetchImpl?: typeof fetch;
}>;

/** `txHash`'s transaction, which must be the exact valid included body. */
const authenticated = async (
  read: ReadConfirmedTransaction,
  txHash: string,
) => {
  const transaction = CML.Transaction.from_cbor_hex((await read(txHash)).cbor);
  if (
    !transaction.is_valid() ||
    CML.hash_transaction(transaction.body()).to_hex() !== txHash
  )
    throw new Error(
      `Transaction ${txHash} is not the exact valid included body`,
    );
  return transaction;
};

/**
 * Whether `spend` of a node output merged `headerHash`'s block: its
 * authenticated spender spends that output and leaves one root, holding the
 * block. A removal never does.
 */
const mergedBy = async (
  input: MergeLookup,
  headerHash: string,
  spend: PublishedHeaderConsumption,
) => {
  const { stateQueue } = input;
  const spender = await authenticated(
    input.readConfirmedTransaction,
    spend.spentByTxHash,
  );
  const inputs = spender.body().inputs();
  const spent = Array.from({ length: inputs.len() }, (_, index) => {
    const ref = inputs.get(index);
    return `${ref.transaction_id().to_hex()}#${ref.index().toString()}`;
  });
  if (!spent.includes(outRefOf(spend))) return false;
  const roots = transactionOutputs(spender).filter(
    ({ address, assets }) =>
      address === stateQueue.spendingScriptAddress &&
      assets[rootUnitOf(stateQueue.policyId)] === 1n,
  );
  if (roots.length !== 1) return false;
  const { data } = await Effect.runPromise(
    SDK.getConfirmedStateFromStateQueueDatum(
      await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(roots[0]!)),
    ),
  );
  return data.headerHash === headerHash;
};

/**
 * The spend Kupo indexed that merged `headerHash`'s block, if any. Kupo's
 * order within one slot is no guide to which spend came last, so every spend
 * is checked and none is chosen by its place in the list.
 */
const mergeSpend = async (input: MergeLookup, headerHash: string) => {
  const spends = await readKupoUnitConsumptions(
    input.kupoUrl,
    nodeUnitOf(input.stateQueue.policyId, headerHash),
    input.fetchImpl,
  );
  for (const spend of spends)
    if (await mergedBy(input, headerHash, spend)) return spend;
  return undefined;
};

/**
 * The node output the merge of `headerHash`'s block spent, read from its
 * authenticated transaction: it must hold the block's node, keyed by its
 * header hash, at the state queue's address.
 */
const mergedNodeOutput = async (input: MergeLookup, headerHash: string) => {
  const { stateQueue } = input;
  const spend = await mergeSpend(input, headerHash);
  if (spend === undefined)
    throw new Error(
      `Merged block ${headerHash} has no authenticated merge of its node`,
    );
  const output = transactionOutputs(
    await authenticated(input.readConfirmedTransaction, spend.txHash),
  ).find(({ outputIndex }) => outputIndex === spend.outputIndex);
  const holdsNoNode = () =>
    new Error(
      `Output ${outRefOf(spend)} does not hold merged block ${headerHash}'s node`,
    );
  if (
    output === undefined ||
    output.address !== stateQueue.spendingScriptAddress ||
    output.assets[nodeUnitOf(stateQueue.policyId, headerHash)] !== 1n
  )
    throw holdsNoNode();
  const view = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(output),
  );
  if (view.key === "Empty" || view.key.Key.key !== headerHash)
    throw holdsNoNode();
  return { output, view };
};

/**
 * The block the root holds, rebuilt from L1: the header in the node output
 * its merge spent, and the transaction that created that output. The header
 * must hash to the root's header hash and carry its UTxO root and end time. A
 * genesis root holds no block.
 */
export const rebuildMergedRootBlock = async (
  input: Omit<MergeLookup, "readConfirmedTransaction"> & {
    root: QueueRootState;
    readConfirmedTransaction?: ReadConfirmedTransaction;
  },
): Promise<{ header: SDK.Header; commitTxHash: string }> => {
  const { root, readConfirmedTransaction } = input;
  const { headerHash } = root;
  if (headerHash === SDK.GENESIS_HEADER_HASH)
    throw new Error(
      "The stack's state queue is at its genesis state: it holds no block to extend",
    );
  if (readConfirmedTransaction === undefined)
    throw new Error(
      `Every block of the stack's state queue merged, and this session reads no confirmed transactions to rebuild merged block ${headerHash}`,
    );
  const { output, view } = await mergedNodeOutput(
    { ...input, readConfirmedTransaction },
    headerHash,
  );
  const header = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(view),
  );
  const mismatch =
    (await Effect.runPromise(SDK.hashBlockHeader(header))) !== headerHash
      ? "header hash"
      : header.utxosRoot !== root.utxoRoot
        ? "UTxO root"
        : header.endTime !== root.endTime
          ? "end time"
          : undefined;
  if (mismatch !== undefined)
    throw new Error(
      `Merged block ${headerHash} rebuilt from ${outRefOf(output)} differs from the root's ${mismatch}`,
    );
  return { header, commitTxHash: output.txHash };
};

/**
 * Where staging commits a block after `block`: the block's node while it is
 * the exact tail with the same header, or the root once the block merged and
 * nothing follows it (and its header hashes to the root's). The queue's head
 * is absent on an empty queue: a commit on the root takes none.
 */
export const stagingAppendPoint = ({
  provider,
  stateQueue,
  readNode,
}: {
  provider: {
    getUtxosWithUnit(address: string, unit: string): Promise<UTxO[]>;
  };
  stateQueue: StateQueueContract;
  /** The live output holding a block's node, if any. */
  readNode(headerHash: string): Promise<UTxO | undefined>;
}) => {
  const readRoot = async () => {
    const roots = await provider.getUtxosWithUnit(
      stateQueue.spendingScriptAddress,
      rootUnitOf(stateQueue.policyId),
    );
    if (roots.length !== 1) throw new Error("Expected one state queue root");
    return roots[0]!;
  };
  return {
    tailOutput: async (block: { headerHash: string; header: SDK.Header }) => {
      const node = await readNode(block.headerHash);
      const point = await appendPointAfter(block.headerHash, {
        node,
        root: readRoot,
      });
      if (point === undefined) return undefined;
      if (point === node) {
        const header = await Effect.runPromise(
          SDK.getHeaderFromStateQueueDatum(
            await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(point)),
          ),
        );
        return SDK.encodeHeaderCbor(header).toString("hex") ===
          SDK.encodeHeaderCbor(block.header).toString("hex")
          ? point
          : undefined;
      }
      return (await Effect.runPromise(SDK.hashBlockHeader(block.header))) ===
        block.headerHash
        ? point
        : undefined;
    },
    queueHead: async () => {
      const root = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(await readRoot()),
      );
      if (root.next === "Empty") return undefined;
      const head = await readNode(root.next.Key.key);
      if (head === undefined)
        throw new Error(`Missing journey header ${root.next.Key.key}`);
      return head;
    },
  };
};

/**
 * The one output among `outputs` that ends the queue at `headerHash`: that
 * block's node, followed by nothing, or the root holding it once it merged.
 */
export const queueEndOutput = async (
  outputs: readonly UTxO[],
  expected: { stateQueue: StateQueueContract; headerHash: string },
) => {
  const { stateQueue, headerHash } = expected;
  const holding = (unit: string) =>
    outputs.filter(
      ({ address, assets }) =>
        address === stateQueue.spendingScriptAddress && assets[unit] === 1n,
    );
  const nodes = holding(nodeUnitOf(stateQueue.policyId, headerHash));
  const roots = holding(rootUnitOf(stateQueue.policyId));
  const candidates = nodes.length > 0 ? nodes : roots;
  const end =
    candidates.length === 1
      ? await appendPointAfter(headerHash, {
          node: nodes[0],
          root: async () => roots[0]!,
        })
      : undefined;
  if (end === undefined)
    throw new Error(`No output ends the state queue at ${headerHash}`);
  return end;
};

/**
 * Whether `headerHash`'s block left the queue by a merge: a spend Kupo
 * indexed of its node is an authenticated transaction that spends that
 * output and leaves the root holding the block. A removal never does.
 */
export const leftQueueByMerge = async (
  input: MergeLookup & { headerHash: string },
): Promise<boolean> =>
  (await mergeSpend(input, input.headerHash)) !== undefined;

/**
 * Whether `headerHash`'s block is on the queue as exactly one live node, or
 * left it by a merge. Our node merges a block once it matures, so a block
 * read long after its commit, or again on a resumed run, may have merged.
 */
export const onQueueOrMerged = async (
  input: MergeLookup & {
    headerHash: string;
    provider: {
      getUtxosWithUnit(address: string, unit: string): Promise<UTxO[]>;
    };
  },
): Promise<boolean> => {
  const live = await input.provider.getUtxosWithUnit(
    input.stateQueue.spendingScriptAddress,
    nodeUnitOf(input.stateQueue.policyId, input.headerHash),
  );
  return live.length === 0 ? leftQueueByMerge(input) : live.length === 1;
};

/**
 * The DA target of a block whose node has left the queue. A block merges only
 * once its node is Attested or Published (state-queue.ak:361-365), and the
 * merge leaves the root holding it; a child merging after it leaves the root
 * naming it as parent. Such a block reads as merged, with the transaction
 * whose output held its attested node, as while it was on the queue: the
 * output its authenticated merge spent. Any other block was removed, and
 * reads as absent so that its removal is authenticated.
 */
export const leftQueueDaTarget = async (
  input: Omit<MergeLookup, "readConfirmedTransaction"> & {
    headerHash: string;
    root: QueueRootState;
    readConfirmedTransaction?: ReadConfirmedTransaction;
  },
): Promise<{ kind: "absent" } | { kind: "merged"; txHash: string }> => {
  const { headerHash, root, readConfirmedTransaction } = input;
  if (headerHash !== root.headerHash && headerHash !== root.prevHeaderHash)
    return { kind: "absent" };
  if (readConfirmedTransaction === undefined)
    throw new Error(
      `DA target ${headerHash} merged, and this session reads no confirmed transactions to authenticate the output that held its attested node`,
    );
  const { output, view } = await mergedNodeOutput(
    { ...input, readConfirmedTransaction },
    headerHash,
  );
  const node = await Effect.runPromise(
    SDK.getStateQueueNodeFromStateQueueDatum(view),
  );
  if (!SDK.daAvailabilityStateQueueStatusPermitsMerge(node.da_attestation))
    throw new Error(
      `Merged DA target ${headerHash}'s node output ${outRefOf(output)} is not attested`,
    );
  if (
    (await Effect.runPromise(SDK.hashBlockHeader(node.header))) !== headerHash
  )
    throw new Error(
      `Merged DA target ${headerHash}'s node output ${outRefOf(output)} holds another block's header`,
    );
  return { kind: "merged", txHash: output.txHash };
};
