import { createHash } from "node:crypto";
import { createInterface } from "node:readline";

const mode = process.argv[2] ?? "honest";
const reader = createInterface({ input: process.stdin, crlfDelay: Infinity });
const [line] = await new Promise((resolve) =>
  reader.once("line", (value) => resolve([value])),
);
const startup = JSON.parse(line);
const digest = createHash("sha256").update(line, "utf8").digest("hex");
const tip = {
  blockHash: "44".repeat(32),
  blockNo: "12",
  kind: "point",
  slot: "103",
};
const emit = (value) => process.stdout.write(`${JSON.stringify(value)}\n`);

if (mode === "retry_intersection" && startup.intersection.kind === "point") {
  emit({
    code: "intersection_failed",
    kind: "error",
    schemaVersion: startup.schemaVersion,
  });
  process.exit(69);
}

if (mode === "no_ready") {
  setInterval(() => undefined, 1000);
  await new Promise(() => {});
}

emit({
  authorityNodeId:
    mode === "forged_ready" ? "substituted-node" : startup.authorityNodeId,
  currentTip: tip,
  genesisIdentitySha256: startup.genesisIdentitySha256,
  kind: "ready",
  network: startup.network,
  networkMagic: startup.networkMagic,
  ...(mode === "missing_operation"
    ? {}
    : {
        operation:
          mode === "wrong_operation" ? { kind: "stream" } : startup.operation,
      }),
  schemaVersion: startup.schemaVersion,
  selectedIntersection: startup.intersection,
  socketPath: startup.socketPath,
  startupDigest: digest,
});

if (mode === "crash") process.exit(23);

const forward = {
  blockHash: "bb".repeat(32),
  blockNo: "10",
  blockType: "6",
  kind: "roll_forward",
  prevHash: "aa".repeat(32),
  rawBlockCbor: "80",
  schemaVersion: startup.schemaVersion,
  slot: "101",
  tip,
};

if (
  mode.startsWith("query_") ||
  mode === "wrong_operation" ||
  mode === "missing_operation"
) {
  if (mode === "query_ack")
    emit({
      kind: "roll_backward",
      point: startup.intersection,
      schemaVersion: startup.schemaVersion,
      tip,
    });
  if (mode !== "query_wait")
    emit({
      ...forward,
      ...(mode === "query_wrong_target" ? { blockNo: "11" } : {}),
      ...(mode === "query_old"
        ? { tip: { ...tip, blockNo: "5000", slot: "6000" } }
        : {}),
      ...(mode === "query_bad_tip" ? { tip: { ...tip, blockNo: "9" } } : {}),
    });
  if (mode === "query_exit") setTimeout(() => process.exit(23), 100);
  if (mode === "query_extra") setTimeout(() => emit(forward), 100);
} else if (mode === "retry_intersection") {
  // Successful Origin admission remains idle at the fixture tip.
} else if (mode === "reordered") {
  emit({ ...forward, prevHash: "cc".repeat(32) });
} else if (mode === "first_slot_regression") {
  emit({ ...forward, slot: "99" });
} else {
  emit(forward);
  emit({
    kind: "roll_backward",
    point: {
      blockHash:
        mode === "unknown_rollback" ? "dd".repeat(32) : "aa".repeat(32),
      kind: "point",
      slot: "100",
    },
    schemaVersion: startup.schemaVersion,
    tip,
  });
}

const keepAlive = setInterval(() => undefined, 1_000);
const stop = () => {
  clearInterval(keepAlive);
  process.exit(0);
};
process.once("SIGINT", stop);
process.once("SIGTERM", stop);
