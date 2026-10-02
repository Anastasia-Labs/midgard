import {
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  rmSync,
} from "node:fs";
import { join } from "node:path";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import type {
  WatcherNativeBlockAdmission,
  WatcherNativeChainSyncEvent,
} from "midgard-watcher";

import { createJourneyNativeScriptArchive } from "../../devnet/watcher-journeys/history-archives.js";
import { writeDurableFile } from "./durable.js";

export type BlockPoint = {
  readonly blockHash: string;
  readonly blockNo: string;
  readonly slot: string;
};

type RollbackPoint = Extract<
  WatcherNativeChainSyncEvent,
  { kind: "roll_backward" }
>["point"];

/** How many retained blocks a restart offers the node as intersections. */
const MAX_RESUME_POINTS = 32;

/** Whether `point` lies on the chain past a rollback to `rollback`. */
export const after = (point: BlockPoint, rollback: RollbackPoint) =>
  rollback.kind === "origin" ||
  BigInt(point.slot) > BigInt(rollback.slot) ||
  (point.slot === rollback.slot && point.blockHash !== rollback.blockHash);

const canonicalBlockNos = (directory: string): bigint[] => {
  const canonical = join(directory, "canonical");
  if (!existsSync(canonical)) return [];
  return readdirSync(canonical)
    .filter((name) => /^[0-9]+\.json$/u.test(name))
    .map((name) => BigInt(name.slice(0, -".json".length)));
};

/** Block `blockNo` when every provider retains it with the same bytes. */
const retainedBlock = (directories: readonly string[], blockNo: bigint) => {
  const bytes = directories.map((directory) => {
    try {
      return readFileSync(
        join(directory, "canonical", `${blockNo}.json`),
        "utf8",
      );
    } catch {
      return undefined;
    }
  });
  if (bytes[0] === undefined || bytes.some((each) => each !== bytes[0]))
    return undefined;
  try {
    const { point, prevHash } = JSON.parse(bytes[0]) as {
      point: BlockPoint;
      prevHash: string | null;
    };
    return { point, prevHash };
  } catch {
    return undefined;
  }
};

/**
 * Every archive update removes both providers' ready markers first and writes
 * a fresh generation last, so matching markers mean no update was cut short.
 */
const archiveSettled = (directories: readonly string[]) => {
  const markers = directories.map((directory) => {
    try {
      return readFileSync(join(directory, "canonical-ready"), "utf8");
    } catch {
      return undefined;
    }
  });
  return (
    markers[0] !== undefined && markers.every((marker) => marker === markers[0])
  );
};

/**
 * The retained blocks, newest first, a restart may resume the chain from:
 * the newest few, then exponentially older ones, so a node that lost the
 * newest (a rollback while the recorder was down) still finds one.
 *
 * In a settled archive every retained block is complete and they form one
 * chain. After an interrupted update only the linked run from the oldest
 * block counts, less its newest block, which that update may have left
 * half-written; the node re-delivers it.
 */
export const resumePoints = (directories: readonly string[]): BlockPoint[] => {
  const [first, ...rest] = directories.map(
    (directory) => new Set(canonicalBlockNos(directory)),
  );
  const common = [...(first ?? [])]
    .filter((blockNo) => rest.every((set) => set.has(blockNo)))
    .sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));
  let chain: bigint[];
  if (archiveSettled(directories)) chain = common.reverse();
  else {
    const linked: bigint[] = [];
    let previous: { blockNo: bigint; blockHash: string } | undefined;
    for (const blockNo of common) {
      const block = retainedBlock(directories, blockNo);
      if (
        block === undefined ||
        (previous !== undefined &&
          (blockNo !== previous.blockNo + 1n ||
            block.prevHash !== previous.blockHash))
      )
        break;
      linked.push(blockNo);
      previous = { blockNo, blockHash: block.point.blockHash };
    }
    chain = linked.slice(0, -1).reverse();
  }
  const points: BlockPoint[] = [];
  for (
    let index = 0;
    index < chain.length && points.length < MAX_RESUME_POINTS;

  ) {
    const block = retainedBlock(directories, chain[index]!);
    if (block !== undefined) points.push(block.point);
    index = index < 2 ? index + 1 : index * 2;
  }
  return points;
};

/** Header hashes whose state-queue node this transaction mints: its commits. */
export const committedHeaderHashes = (
  transactionCbor: string,
  stateQueuePolicyId: string,
): string[] => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const mint = body.mint();
  const policy = CML.ScriptHash.from_hex(stateQueuePolicyId);
  const assets = mint?.get_assets(policy);
  const hashes: string[] = [];
  try {
    if (assets === undefined) return hashes;
    const names = assets.keys();
    for (let index = 0; index < names.len(); index += 1) {
      const name = names.get(index);
      const hex = Buffer.from(name.to_raw_bytes()).toString("hex");
      const quantity = assets.get(name);
      name.free();
      const suffix = hex.slice(STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length);
      if (
        hex.startsWith(STATE_QUEUE_NODE_ASSET_NAME_PREFIX) &&
        /^[0-9a-f]{56}$/u.test(suffix) &&
        quantity !== undefined &&
        quantity > 0n
      )
        hashes.push(suffix);
    }
    names.free();
    return hashes;
  } finally {
    assets?.free();
    policy.free();
    mint?.free();
    body.free();
    transaction.free();
  }
};

/**
 * Where each state-queue header was committed on the followed chain, one file
 * per header, so a restarted recorder that resumes past a commit still knows
 * it. An entry is written before its block is retained, dropped when a
 * rollback passes it, and forgotten once the header's payload is archived.
 */
export const createCommitIndex = (directory: string) => {
  const path = (headerHash: string) => join(directory, `${headerHash}.json`);
  return {
    exists: () => existsSync(directory),
    create: () => mkdirSync(directory, { recursive: true, mode: 0o700 }),
    record: (headerHash: string, point: BlockPoint) =>
      writeDurableFile(path(headerHash), JSON.stringify(point)),
    get: (headerHash: string): BlockPoint | undefined => {
      try {
        return JSON.parse(readFileSync(path(headerHash), "utf8")) as BlockPoint;
      } catch {
        return undefined;
      }
    },
    forget: (headerHash: string) => rmSync(path(headerHash), { force: true }),
    rollback: (point: RollbackPoint) => {
      if (!existsSync(directory)) return;
      for (const name of readdirSync(directory)) {
        if (!/^[0-9a-f]{56}\.json$/u.test(name)) continue;
        const file = join(directory, name);
        let entry: BlockPoint | undefined;
        try {
          entry = JSON.parse(readFileSync(file, "utf8")) as BlockPoint;
        } catch {
          // Torn by a crash: its block was never retained.
        }
        if (entry === undefined || after(entry, point))
          rmSync(file, { force: true });
      }
    },
  };
};

/**
 * The chain-following half of the history recorder: resumes from what the
 * providers already retain, keeps their canonical chain and native-script
 * records in step with the node, and indexes state-queue commits.
 *
 * Chain-sync opens every session with a rollback to the intersection it
 * selected. That acknowledgement removes only what lies above the
 * intersection; the retained chain below it stays served throughout.
 */
export const createHistoryChainFollower = (input: {
  readonly directories: readonly string[];
  readonly commitsDirectory: string;
  readonly stateQueuePolicyId: string;
  readonly admit: (
    event: Extract<WatcherNativeChainSyncEvent, { kind: "roll_forward" }>,
  ) => WatcherNativeBlockAdmission;
}) => {
  const { directories } = input;
  const archive = createJourneyNativeScriptArchive(directories);
  const commits = createCommitIndex(input.commitsDirectory);
  // An archive retained before commits were indexed is rebuilt once from origin.
  const resume = commits.exists() ? resumePoints(directories) : [];
  let acknowledged = false;
  let latestBlockNo: bigint | undefined;

  const alreadyCanonical = (blockNo: string, bytes: string) =>
    directories.every((directory) => {
      const path = join(directory, "canonical", `${blockNo}.json`);
      return existsSync(path) && readFileSync(path, "utf8") === bytes;
    });

  const onEvent = async (event: WatcherNativeChainSyncEvent) => {
    if (event.kind === "roll_backward") {
      const opening = !acknowledged;
      acknowledged = true;
      const { point } = event;
      commits.rollback(point);
      if (opening) {
        const at =
          point.kind === "origin"
            ? -1n
            : resume
                .filter((each) => each.blockHash === point.blockHash)
                .map((each) => BigInt(each.blockNo))[0];
        latestBlockNo = at === undefined || at < 0n ? undefined : at;
        const nothingAbove =
          at !== undefined &&
          archiveSettled(directories) &&
          directories.every((directory) =>
            canonicalBlockNos(directory).every((blockNo) => blockNo <= at),
          );
        if (!nothingAbove) await archive.rollbackNativeBlocks(point);
        commits.create();
        return;
      }
      latestBlockNo = undefined;
      await archive.rollbackNativeBlocks(point);
      return;
    }
    acknowledged = true;
    const block = input.admit(event);
    const point = {
      blockHash: block.blockHash,
      blockNo: block.blockNo,
      slot: block.slot,
    };
    for (const transaction of block.transactionCbors)
      for (const headerHash of committedHeaderHashes(
        transaction,
        input.stateQueuePolicyId,
      ))
        commits.record(headerHash, point);
    // A block re-delivered after an interrupted update is complete once its
    // canonical record matches, unless it carries transactions, whose
    // native-script records are always rewritten (they are idempotent).
    const canonical = JSON.stringify({
      point: { ...point, pointId: computeFraudProofRawL1PointId(point) },
      prevHash: block.prevHash,
    });
    if (
      block.transactionIds.length > 0 ||
      !alreadyCanonical(block.blockNo, canonical)
    )
      await archive.retainNativeBlock(block);
    latestBlockNo = BigInt(block.blockNo);
  };

  return {
    /** Newest retained blocks first; origin only as the last fallback. */
    intersectionCandidates: [
      ...resume.map(
        (point): RollbackPoint => ({
          kind: "point",
          blockHash: point.blockHash,
          slot: point.slot,
        }),
      ),
      { kind: "origin" } as const,
    ],
    onEvent,
    latestBlockNo: () => latestBlockNo,
    commits,
  };
};
