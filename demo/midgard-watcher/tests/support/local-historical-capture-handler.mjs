// The node behind the local historical capture tests: each exact-point query
// is one chain-sync stream that delivers the ordinary Conway fixture block.
// Queries are numbered across sidecar restarts by the "start" records in
// `options.logPath`, which also records each query's readiness, delivered
// event (in the watcher's event shape) and end.
import { appendFileSync, readFileSync } from "node:fs";

const SCHEMA_VERSION = "midgard-watcher-native-chain-sync-v1";

export default (options, controls) => {
  const records = () =>
    readFileSync(options.logPath, "utf8")
      .trim()
      .split("\n")
      .filter(Boolean)
      .map((line) => JSON.parse(line));
  const log = (kind, query, value) =>
    appendFileSync(
      options.logPath,
      `${JSON.stringify({ kind, query, ...(value === undefined ? {} : { value }) })}\n`,
    );
  const metadata = options.metadata;
  const rawHex = readFileSync(options.rawPath, "utf8").trim();
  const block = Buffer.from(rawHex, "hex");
  const open = new Set();
  // A sidecar that ends ends every query it served.
  process.once("exit", () => {
    for (const query of open) log("stop", query);
  });

  return {
    hello: ({ networkMagic }) =>
      networkMagic === 1
        ? undefined
        : {
            fatal: {
              code: "node_handshake_failed",
              message: "network magic differs",
              status: 69,
            },
          },
    openStream: async ({ points }, stream) => {
      const all = records();
      const starts = all.filter((record) => record.kind === "start");
      const query = starts.length + 1;
      const previous = starts.at(-1);
      log("start", query, {
        pid: process.pid,
        previousQueryOpen:
          previous !== undefined &&
          !all.some(
            (record) =>
              record.kind === "stop" && record.query === previous.query,
          ),
      });
      open.add(query);
      stream.onClose(() => {
        if (!open.delete(query)) return;
        log("stop", query);
      });
      const modes = options.modes;
      const mode = modes[(query - 1) % modes.length];
      if (mode === "no_ready") return await new Promise(() => {});
      const readyTip = {
        kind: "point",
        blockHash: "77".repeat(32),
        blockNo: (BigInt(metadata.blockNo) + 1n).toString(),
        slot: (BigInt(metadata.slot) + 1n).toString(),
      };
      log("ready", query, { currentTip: readyTip });
      if (mode !== "query_wait") {
        const offsets = options.tipOffsets;
        const offset = BigInt(offsets[(query - 1) % offsets.length]);
        const blockType =
          query % 2 === 0 && options.secondBlockType !== null
            ? options.secondBlockType
            : metadata.blockType;
        const tip = {
          kind: "point",
          blockHash: (query % 2 === 0 ? "99" : "88").repeat(32),
          blockNo: (BigInt(metadata.blockNo) + offset).toString(),
          slot: (BigInt(metadata.slot) + offset).toString(),
        };
        log("roll_forward", query, {
          schemaVersion: SCHEMA_VERSION,
          kind: "roll_forward",
          blockHash: metadata.blockHash,
          blockNo: metadata.blockNo,
          blockType,
          prevHash: metadata.prevHash,
          rawBlockCbor: rawHex,
          slot: metadata.slot,
          tip,
        });
        stream.rollForward({
          point: { slot: BigInt(metadata.slot), hash: metadata.blockHash },
          blockNo: BigInt(metadata.blockNo),
          blockType: Number(blockType),
          prevHash: metadata.prevHash,
          tip: {
            point: { slot: BigInt(tip.slot), hash: tip.blockHash },
            blockNo: BigInt(tip.blockNo),
          },
          block,
        });
      }
      if (mode === "query_exit") setTimeout(() => controls.exit(23), 100);
      return {
        intersection: points[0],
        tip: {
          point: { slot: BigInt(readyTip.slot), hash: readyTip.blockHash },
          blockNo: BigInt(readyTip.blockNo),
        },
      };
    },
  };
};
