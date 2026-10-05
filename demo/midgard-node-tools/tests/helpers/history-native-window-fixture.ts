import { createHash, randomUUID } from "node:crypto";
import {
  mkdirSync,
  mkdtempSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";

import {
  encodeCborArrayRaw,
  encodeCborBytes,
} from "@al-ft/midgard-core/codec/cbor";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { parseWatcherConfig } from "midgard-watcher";
import {
  startWatcherNativeChainSync,
  WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncRollForward,
  type WatcherNativeChainSyncRuntime,
} from "midgard-watcher/native-chain-sync";
import { config as baseConfig } from "midgard-watcher/tests/l1/native-chain-sync.config";

import type { HistoryWindowPoint } from "../../src/devnet-stack/history-native-window-proof.js";

/**
 * Replaces the synthetic native's control file by rename. The native re-reads
 * it every 10 ms from its own process; a truncate-then-write lets that read see
 * an empty file, whose JSON.parse failure kills the native and ends the stream
 * under test.
 */
export const writeSyntheticControls = (
  controlPath: string,
  controls: readonly WatcherNativeChainSyncEvent[],
) => {
  const staged = `${controlPath}.${randomUUID()}.tmp`;
  writeFileSync(staged, JSON.stringify(controls));
  renameSync(staged, controlPath);
};

/** Dummy header signatures; real raw CBOR/native admission, not Cardano consensus. */
const emptyBlock = (
  n: number,
  parent: HistoryWindowPoint | undefined,
): WatcherNativeChainSyncRollForward => {
  const bodyParts = [
    Buffer.from("80", "hex"),
    Buffer.from("80", "hex"),
    Buffer.from("a0", "hex"),
    Buffer.from("80", "hex"),
  ];
  const bodyHash = computeHash32(Buffer.concat(bodyParts.map(computeHash32)));
  const body = CML.HeaderBody.new(
    BigInt(n),
    BigInt(n + 1),
    parent === undefined
      ? undefined
      : CML.BlockHeaderHash.from_hex(parent.blockHash),
    CML.PublicKey.from_bytes(new Uint8Array(32).fill(1)),
    CML.VRFVkey.from_raw_bytes(new Uint8Array(32).fill(2)),
    CML.VRFCert.new(new Uint8Array(64).fill(3), new Uint8Array(80).fill(4)),
    BigInt(bodyParts.reduce((s, b) => s + b.length, 0)),
    CML.BlockBodyHash.from_raw_bytes(bodyHash),
    CML.OperationalCert.new(
      CML.KESVkey.from_raw_bytes(new Uint8Array(32).fill(5)),
      0n,
      0n,
      CML.Ed25519Signature.from_raw_bytes(new Uint8Array(64).fill(6)),
    ),
    CML.ProtocolVersion.new(9n, 0n),
  );
  const header = CML.Header.new(
    body,
    CML.KESSignature.from_cbor_bytes(
      encodeCborBytes(new Uint8Array(448).fill(7)),
    ),
  );
  try {
    const blockHash = computeHash32(header.to_cbor_bytes()).toString("hex");
    return {
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      kind: "roll_forward",
      blockType: "7",
      blockHash,
      blockNo: String(n),
      slot: String(n + 1),
      prevHash: parent?.blockHash ?? "",
      rawBlockCbor: encodeCborArrayRaw([
        header.to_cbor_bytes(),
        ...bodyParts,
      ]).toString("hex"),
      tip: {
        kind: "point",
        blockHash,
        blockNo: String(n),
        slot: String(n + 1),
      },
    };
  } finally {
    header.free();
    body.free();
  }
};
export const windowFixture = (last: number) => {
  const root = mkdtempSync("/var/tmp/codex-rel-history-window-");
  const directories: readonly [string, string] = [
    join(root, "a"),
    join(root, "b"),
  ];
  for (const d of directories)
    mkdirSync(join(d, "canonical"), { recursive: true });
  const events: WatcherNativeChainSyncRollForward[] = [];
  const points: HistoryWindowPoint[] = [];
  const appendBlock = (persist = true) => {
    const n = events.length;
    const event = emptyBlock(n, points[n - 1]);
    const point = {
      blockHash: event.blockHash,
      blockNo: event.blockNo,
      slot: event.slot,
      pointId: computeFraudProofRawL1PointId(event),
    };
    events.push(event);
    points.push(point);
    if (persist)
      for (const d of directories)
        writeFileSync(
          join(d, "canonical", `${n}.json`),
          JSON.stringify({ point, prevHash: event.prevHash }),
        );
    return event;
  };
  for (let n = 0; n <= last; n++) appendBlock();
  const publish = () => {
    const generation = JSON.stringify(randomUUID());
    for (const d of directories)
      writeFileSync(join(d, "canonical-ready"), generation);
  };
  publish();
  const eventsPath = join(root, "synthetic-events.json");
  const controlPath = join(root, "synthetic-control.json");
  const startsPath = join(root, "synthetic-starts.jsonl");
  writeFileSync(eventsPath, JSON.stringify(events));
  writeFileSync(controlPath, "[]");
  const genesisPath = join(root, "synthetic-genesis.json");
  const nodePath = join(root, "synthetic-node.json");
  const genesisBytes = JSON.stringify({ networkMagic: 1 });
  writeFileSync(genesisPath, genesisBytes);
  writeFileSync(nodePath, JSON.stringify({ ShelleyGenesisFile: genesisPath }));
  const base = baseConfig();
  if (base.l1.source.sourceMode !== "local_node")
    throw Error("local synthetic config required");
  const watcherConfig = {
    ...base,
    da: {
      ...base.da,
      peers: [
        {
          identity: "synthetic-da-a",
          multiaddr:
            "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
        },
      ],
    },
    l1: {
      ...base.l1,
      finality: {
        ...base.l1.finality,
        rollback: {
          beforeFinality: base.l1.finality.rollback.beforeFinality,
          afterFinality: base.l1.finality.rollback.afterFinality,
          maxDepth: base.l1.finality.rollback.maxDepth,
        },
      },
      source: {
        ...base.l1.source,
        chainSync: {
          ...base.l1.source.chainSync,
          nodeConfigPath: nodePath,
          genesisConfigPath: genesisPath,
          genesisIdentitySha256: createHash("sha256")
            .update(genesisBytes)
            .digest("hex"),
        },
      },
    },
  };
  const stallPath = join(root, "synthetic-stall");
  const binaryPath = join(root, "synthetic-native.mjs");
  writeFileSync(
    binaryPath,
    `#!/usr/bin/env node
import {readFileSync,appendFileSync,existsSync} from "node:fs";import {createHash} from "node:crypto";import {createInterface} from "node:readline";
const canon=v=>v===null||typeof v!=="object"?JSON.stringify(v):Array.isArray(v)?"["+v.map(canon).join(",")+"]":"{"+Object.keys(v).sort().map(k=>JSON.stringify(k)+":"+canon(v[k])).join(",")+"}";
const reader=createInterface({input:process.stdin});const line=await new Promise(r=>reader.once("line",r));const s=JSON.parse(line);const rows=JSON.parse(readFileSync(${JSON.stringify(eventsPath)},"utf8"));const tip=rows[rows.length-1].tip;
appendFileSync(${JSON.stringify(startsPath)},JSON.stringify(s.intersection)+"\\n");
const emit=v=>new Promise(r=>process.stdout.write(canon(v)+"\\n",r));
const stop=()=>process.exit(0);process.once("SIGTERM",stop);process.once("SIGINT",stop);
await emit({schemaVersion:s.schemaVersion,kind:"ready",authorityNodeId:s.authorityNodeId,currentTip:tip,genesisIdentitySha256:s.genesisIdentitySha256,network:s.network,networkMagic:s.networkMagic,operation:s.operation,selectedIntersection:s.intersection,socketPath:s.socketPath,startupDigest:createHash("sha256").update(line).digest("hex")});
await emit({schemaVersion:s.schemaVersion,kind:"roll_backward",point:s.intersection,tip});
const index=s.intersection.kind==="origin"?-1:rows.findIndex(e=>e.blockHash===s.intersection.blockHash&&e.slot===s.intersection.slot);if(s.intersection.kind!=="origin"&&index<0)process.exit(69);
if(existsSync(${JSON.stringify(stallPath)}))await new Promise(()=>{});
for(let n=index+1;n<rows.length;n++)await emit(rows[n]);
let seen=0;setInterval(async()=>{const controls=JSON.parse(readFileSync(${JSON.stringify(controlPath)},"utf8"));while(seen<controls.length)await emit(controls[seen++]);},10);
`,
    { mode: 0o700 },
  );
  const runtimes: WatcherNativeChainSyncRuntime[] = [];
  const startMain = async (
    onEvent: (event: WatcherNativeChainSyncEvent) => Promise<void>,
    origin = false,
  ) => {
    const target = points[last];
    if (target === undefined) throw Error("synthetic target absent");
    const runtime = await startWatcherNativeChainSync({
      binaryPath,
      watcherConfig: parseWatcherConfig(watcherConfig),
      intersection: origin
        ? { kind: "origin" }
        : {
            kind: "point",
            blockHash: target.blockHash,
            slot: target.slot,
          },
      startupTimeoutMs: 10000,
      onEvent,
    });
    runtimes.push(runtime);
    void runtime.done.catch(() => undefined);
    return runtime;
  };
  const retain = (event: WatcherNativeChainSyncRollForward) => {
    const p = {
      blockHash: event.blockHash,
      blockNo: event.blockNo,
      slot: event.slot,
      pointId: computeFraudProofRawL1PointId(event),
    };
    for (const d of directories)
      writeFileSync(
        join(d, "canonical", `${event.blockNo}.json`),
        JSON.stringify({ point: p, prevHash: event.prevHash }),
      );
    publish();
  };
  const controls: WatcherNativeChainSyncEvent[] = [];
  const send = (event: WatcherNativeChainSyncEvent) => {
    controls.push(event);
    writeSyntheticControls(controlPath, controls);
  };
  return {
    root,
    directories,
    watcherConfig,
    binaryPath,
    points,
    events,
    startsPath,
    eventsPath,
    stall: () => writeFileSync(stallPath, "synthetic stall"),
    publish,
    appendBlock,
    retain,
    send,
    startMain,
    close: async () => {
      try {
        await Promise.all(runtimes.map((r) => r.close()));
      } finally {
        rmSync(root, { recursive: true, force: true });
      }
    },
  };
};
