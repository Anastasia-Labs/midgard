// A persistent exact-point helper for lifecycle tests. Every session answers
// from its own startup line, so concurrent sessions prove request-id routing;
// a target of "cc" bytes never rolls forward; the mode selects the helper
// failure under test. Like the real helper, it refuses an open beyond its
// live-session bound with service_session_limit.
import { appendFileSync } from "node:fs";
import { createHash } from "node:crypto";
import { createInterface } from "node:readline";

// maxServiceSessions in native-chain-sync/service.go.
const SESSION_LIMIT = 256;

export const serve = ({ mode, logPath }) => {
  const log = (kind, value = {}) =>
    appendFileSync(
      logPath,
      `${JSON.stringify({ kind, pid: process.pid, ...value })}\n`,
    );
  if (process.argv[2] !== "--exact-point-service") {
    log("not_service");
    process.exit(64);
  }
  log("start");
  const frame = (text) => process.stdout.write(text);
  const out = (id, value) => frame(`out ${id} ${JSON.stringify(value)}\n`);
  let opened = 0;
  const live = new Set();
  const reader = createInterface({ input: process.stdin, crlfDelay: Infinity });
  reader.on("line", (request) => {
    const [verb, id] = request.split(" ", 2);
    if (verb === "close") {
      log("close", { id });
      if (!live.has(id)) return;
      if (mode !== "ignore_close") {
        live.delete(id);
        frame(`end ${id} 0\n`);
      }
      // A frame for a session the helper has already ended.
      if (mode === "out_after_end") frame(`out ${id} {}\n`);
      return;
    }
    const line = request.slice(verb.length + id.length + 2);
    const startup = JSON.parse(line);
    opened += 1;
    log("open", { id, target: startup.operation.target.blockHash });
    if (live.size >= SESSION_LIMIT) {
      log("session_limit", { id });
      out(id, {
        code: "service_session_limit",
        kind: "error",
        schemaVersion: startup.schemaVersion,
      });
      return frame(`end ${id} 69\n`);
    }
    live.add(id);
    if (mode === "unknown_session") return frame(`out 999 {}\n`);
    if (mode === "malformed") return frame("malformed\n");
    if (mode === "crash_on_second_open" && opened === 2) process.exit(23);
    const target = startup.operation.target;
    const tip = {
      blockHash: "44".repeat(32),
      blockNo: String(BigInt(target.blockNo) + 2n),
      kind: "point",
      slot: String(BigInt(target.slot) + 2n),
    };
    frame(`err ${id} ${Buffer.from(`session ${id}\n`).toString("base64")}\n`);
    out(id, {
      authorityNodeId: startup.authorityNodeId,
      currentTip: tip,
      genesisIdentitySha256: startup.genesisIdentitySha256,
      kind: "ready",
      network: startup.network,
      networkMagic: startup.networkMagic,
      operation: startup.operation,
      schemaVersion: startup.schemaVersion,
      selectedIntersection: startup.intersection,
      socketPath: startup.socketPath,
      startupDigest: createHash("sha256").update(line, "utf8").digest("hex"),
    });
    if (target.blockHash === "cc".repeat(32)) return;
    out(id, {
      blockHash: target.blockHash,
      blockNo: target.blockNo,
      blockType: "6",
      kind: "roll_forward",
      prevHash: startup.intersection.blockHash,
      rawBlockCbor: "80",
      schemaVersion: startup.schemaVersion,
      slot: target.slot,
      tip,
    });
  });
  reader.on("close", () => {
    log("eof");
    process.exit(0);
  });
};
