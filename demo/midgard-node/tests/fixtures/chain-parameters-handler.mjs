// A fake sidecar handler for a local node a process starts against: it
// answers the chain parameters (`system_start`, `era_history`,
// `protocol_params` from `options.protocolParameters`, hex) and the tip. Each
// handshake appends "hello" and each ledger query its name to
// `options.journal`. Every query reads `options.mode` (a file) first: while
// it is absent the query waits; "answer" answers, "unavailable" refuses as a
// node that is not up yet (`node_unavailable`, transient), and
// "slot-length-2000" answers an era history whose slots are 2 s long.
import { Buffer } from "node:buffer";
import { appendFileSync, readFileSync } from "node:fs";

import { encodeCbor } from "@al-ft/l1-node-transport/testing/fake-sidecar";

const TIP = { slot: 99n, hash: Buffer.alloc(32, 0xef), blockNo: 42n };

const modeOf = (path) => {
  try {
    return readFileSync(path, "utf8").trim();
  } catch {
    return undefined;
  }
};

const settledMode = async (path) => {
  for (;;) {
    const mode = modeOf(path);
    if (mode !== undefined) return mode;
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
};

export default (options) => ({
  hello: () => {
    appendFileSync(options.journal, "hello\n");
  },
  ledgerQuery: async (query) => {
    appendFileSync(options.journal, `${query.query}\n`);
    const mode = await settledMode(options.mode);
    if (mode === "unavailable")
      return { error: { code: "node_unavailable", message: "starting" } };
    switch (query.query) {
      case "system_start":
        return encodeCbor([2024n, 1n, 0n]);
      case "era_history":
        return encodeCbor([
          [
            [0n, 0n, 0n],
            null,
            [432_000n, mode === "slot-length-2000" ? 2_000n : 1_000n],
          ],
        ]);
      case "protocol_params":
        return Buffer.from(options.protocolParameters, "hex");
      case "chain_point":
        return encodeCbor([TIP.slot, TIP.hash]);
      case "chain_block_no":
        return encodeCbor([1n, TIP.blockNo]);
      default:
        return { error: { code: "unknown_query", message: query.query } };
    }
  },
});
