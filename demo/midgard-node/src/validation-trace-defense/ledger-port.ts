/**
 * The defense material worker's read channel to the predecessor ledger. The
 * ledger is the node's store ledger at the disputed block's parent, held on
 * the main thread; the worker asks it for trie nodes by hash and outputs by
 * out-ref over a `MessagePort` of its own, so a build reads only what the
 * block's replay touches. The channel carries reads only: the worker gets
 * no database and no write capability.
 *
 * Reads from this node's own store are trusted: a served node is not hashed
 * again on the worker side.
 */
import type { MessagePort } from "node:worker_threads";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type {
  LedgerSource,
  NodeHash,
  OutRefHex,
  StoredNode,
} from "@al-ft/midgard-fault-proofs/ledger-store";

/** One read the worker asks for. */
type LedgerRead =
  | Readonly<{ kind: "nodes"; hashes: readonly NodeHash[] }>
  | Readonly<{ kind: "outputs"; outRefs: readonly OutRefHex[] }>;

/** A read as it crosses the channel, with the id its answer carries. */
export type LedgerReadRequest = LedgerRead & Readonly<{ id: number }>;

/** Its answer: the values in request order, `undefined` for one the ledger
 * does not hold, or why the read failed. */
export type LedgerReadResponse =
  | Readonly<{
      id: number;
      ok: true;
      value: readonly (StoredNode | Uint8Array | undefined)[];
    }>
  | Readonly<{ id: number; ok: false; error: string }>;

/** Answers every read that arrives on `port` from `source`, until the port
 * closes. A read `source` fails is answered with its error. */
export const serveLedgerReads = (
  port: MessagePort,
  source: LedgerSource,
): void => {
  port.on("message", (request: LedgerReadRequest) => {
    const read =
      request.kind === "nodes"
        ? Promise.all(request.hashes.map((hash) => source.node(hash)))
        : source.outputs(request.outRefs);
    read.then(
      (value) =>
        port.postMessage({
          id: request.id,
          ok: true,
          value,
        } satisfies LedgerReadResponse),
      (cause: unknown) =>
        port.postMessage({
          id: request.id,
          ok: false,
          error: formatUnknownError(cause),
        } satisfies LedgerReadResponse),
    );
  });
};

/** The worker side of the channel: `readNodes` for `workerLedger`, and the
 * ledger's outputs. */
export type LedgerReadClient = Readonly<{
  readNodes: (
    hashes: readonly NodeHash[],
  ) => Promise<(StoredNode | undefined)[]>;
  outputs: LedgerSource["outputs"];
  /** Closes the port; a read still waiting is refused. */
  close: () => void;
}>;

type Pending = Readonly<{
  resolve: (value: readonly (StoredNode | Uint8Array | undefined)[]) => void;
  reject: (error: Error) => void;
}>;

/** Reads served over `port` by `serveLedgerReads`. */
export const ledgerReadClient = (port: MessagePort): LedgerReadClient => {
  let next = 0;
  const pending = new Map<number, Pending>();
  const refuseAll = (error: Error) => {
    for (const waiting of pending.values()) waiting.reject(error);
    pending.clear();
  };
  port.on("message", (response: LedgerReadResponse) => {
    const waiting = pending.get(response.id);
    if (waiting === undefined) return;
    pending.delete(response.id);
    if (response.ok) waiting.resolve(response.value);
    else waiting.reject(new Error(response.error));
  });
  port.once("close", () =>
    refuseAll(new Error("the predecessor ledger's read channel closed")),
  );
  const ask = (request: LedgerRead) =>
    new Promise<readonly (StoredNode | Uint8Array | undefined)[]>(
      (resolve, reject) => {
        const id = ++next;
        pending.set(id, { resolve, reject });
        port.postMessage({ ...request, id } satisfies LedgerReadRequest);
      },
    );
  return {
    readNodes: async (hashes) =>
      (await ask({ kind: "nodes", hashes })) as (StoredNode | undefined)[],
    outputs: async (outRefs) =>
      // A Buffer crosses a port as a plain Uint8Array.
      (await ask({ kind: "outputs", outRefs })).map((output) => {
        if (output === undefined) return undefined;
        const view = output as Uint8Array;
        return Buffer.from(view.buffer, view.byteOffset, view.byteLength);
      }),
    close: () => {
      port.close();
      refuseAll(new Error("the predecessor ledger's read channel closed"));
    },
  };
};
