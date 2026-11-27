/**
 * A ledger as a worker thread reads it. The ledger LevelDB is open only on
 * the owner's thread, so a worker asks the owner for trie nodes by hash over
 * its worker port (`readNodes`); outputs come from the caller's reader.
 */
import {
  type Ledger,
  ledgerAt,
  type LedgerSource,
  type NodeHash,
  type StoredNode,
} from "@al-ft/midgard-fault-proofs/ledger-store";

import type { NativeMpfWorkerPortClient } from "./client.js";

/**
 * The read-only ledger with `root`, its nodes read from the owner through
 * `client` and its outputs through `outputs`. A node is asked for once per
 * ledger and its applies: a hash names its node's content.
 */
export const workerLedger = (
  client: Pick<NativeMpfWorkerPortClient, "readNodes">,
  root: string,
  outputs: LedgerSource["outputs"],
): Ledger => {
  const nodes = new Map<NodeHash, Promise<StoredNode | undefined>>();
  return ledgerAt(root, {
    node: (hash) => {
      let node = nodes.get(hash);
      if (node === undefined) {
        node = client.readNodes([hash]).then(([read]) => read);
        nodes.set(hash, node);
      }
      return node;
    },
    outputs,
  });
};
