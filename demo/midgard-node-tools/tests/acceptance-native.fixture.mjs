import { createHash } from "node:crypto";
import { createInterface } from "node:readline";
const canonical = (value) =>
  JSON.stringify(value, (_key, item) =>
    item && typeof item === "object" && !Array.isArray(item)
      ? Object.fromEntries(Object.entries(item).sort())
      : item,
  );
const tip = (hash, blockNo, slot) => ({
  kind: "point",
  blockHash: hash.repeat(32),
  blockNo: String(blockNo),
  slot: String(slot),
});
const reader = createInterface({ input: process.stdin });
const keep = setInterval(() => {}, 1000);
process.on("SIGTERM", () => {
  clearInterval(keep);
  process.exit(0);
});
reader.once("line", (line) => {
  const input = JSON.parse(line);
  const emit = (v) => process.stdout.write(canonical(v) + "\n");
  const mode = process.argv[2];
  emit({
    kind: "ready",
    schemaVersion: input.schemaVersion,
    authorityNodeId:
      mode === "wrong-authority" ? "wrong-node" : input.authorityNodeId,
    genesisIdentitySha256: input.genesisIdentitySha256,
    socketPath: input.socketPath,
    network: input.network,
    networkMagic: input.networkMagic,
    startupDigest: createHash("sha256").update(line).digest("hex"),
    operation: input.operation,
    selectedIntersection: input.intersection,
    currentTip: tip("bb", 11, 102),
  });
  const forward = (hash, prev, no, slot) =>
    emit({
      kind: "roll_forward",
      schemaVersion: input.schemaVersion,
      blockHash: hash.repeat(32),
      prevHash: prev.repeat(32),
      blockNo: String(no),
      slot: String(slot),
      blockType: "6",
      rawBlockCbor: "80",
      tip: tip(hash, no, slot),
    });
  if (mode !== "no-current") forward("bb", "aa", 11, 102);
  process.on("message", (message) => {
    if (message === "forward") forward("cc", "bb", 12, 103);
    if (message === "rollback")
      emit({
        kind: "roll_backward",
        schemaVersion: input.schemaVersion,
        point: { kind: "point", blockHash: "aa".repeat(32), slot: "101" },
        tip: tip("aa", 10, 101),
      });
    if (message === "exit") process.exit(23);
    if (message === "error")
      emit({
        kind: "error",
        schemaVersion: input.schemaVersion,
        code: "chain_sync_failed",
      });
  });
});
