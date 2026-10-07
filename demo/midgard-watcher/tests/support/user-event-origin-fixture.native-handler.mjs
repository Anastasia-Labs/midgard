// The node behind the synthetic user-event origin fixture's fake node
// transport. It serves the fixture's block registry, reading the control files
// the fixture rewrites atomically:
//
// - A stream opened with a single credit is an exact-point query: it delivers
//   the successor of its intersection on the node's chain and logs the query.
//   The node's chain is the branch through the newest registered block.
// - Any other stream follows the registry from its intersection up to the tip.
// - Control commands roll every open stream back, or end the node.
// - Once a canonical branch is selected, points off it are not found.
import { createHash } from "node:crypto";
import {
  appendFileSync,
  readFileSync,
  renameSync,
  statSync,
  writeFileSync,
} from "node:fs";

// The producer replaces these files atomically. Poll metadata every tick, but
// parse growing block registries only when their file identity changes.
const cachedJson = (path) => {
  let identity;
  let value;
  return () => {
    const stat = statSync(path, { bigint: true });
    const next = [
      stat.dev,
      stat.ino,
      stat.size,
      stat.mtimeNs,
      stat.ctimeNs,
    ].join(":");
    if (next !== identity) {
      value = JSON.parse(readFileSync(path, "utf8"));
      identity = next;
    }
    return value;
  };
};

const toTip = (tip) => ({
  point: { slot: BigInt(tip.slot), hash: tip.blockHash },
  blockNo: BigInt(tip.blockNo),
});

export default (options) => {
  const readControl = cachedJson(options.controlPath);
  const readBlocks = cachedJson(options.registryPath);
  const baseDepth = BigInt(options.nativeTipBaseDepth);
  const baseSlots = BigInt(Math.max(600, options.nativeTipBaseDepth));
  const registered = (point) =>
    point === "origin" ||
    readBlocks().some(
      (block) =>
        block.point.blockHash === point.hash ||
        block.parentPoint.blockHash === point.hash,
    );
  const childOf = (blocks, point) =>
    point === "origin"
      ? blocks[0]
      : blocks.find(
          (block) =>
            block.parentPoint.blockHash === point.hash &&
            BigInt(block.parentPoint.slot) === BigInt(point.slot),
        );
  // The successor on the node's chain: the branch through the newest block.
  const successorOf = (point) => {
    const blocks = readBlocks();
    const byHash = new Map(
      blocks.map((block) => [block.point.blockHash, block]),
    );
    for (let cursor = blocks.at(-1); cursor !== undefined; ) {
      const parent = cursor.parentPoint;
      if (parent.blockHash === point.hash) return cursor;
      cursor = byHash.get(parent.blockHash);
    }
    return blocks.findLast(
      (block) => block.parentPoint.blockHash === point.hash,
    );
  };
  const exactTip = (target) => {
    const query = Number(readFileSync(options.counterPath, "utf8")) + 1;
    writeFileSync(options.counterPath, String(query));
    const tip = {
      kind: "point",
      blockHash: createHash("sha256")
        .update(`synthetic-tip-${query}`)
        .digest("hex"),
      blockNo: String(BigInt(target.point.blockNo) + baseDepth + BigInt(query)),
      slot: String(BigInt(target.point.slot) + baseSlots + BigInt(query)),
    };
    const next = `${options.tipPath}.next${process.pid}`;
    writeFileSync(next, JSON.stringify(tip));
    renameSync(next, options.tipPath);
    return tip;
  };
  const forwardOf = (block, tip) => ({
    point: { slot: BigInt(block.point.slot), hash: block.point.blockHash },
    blockNo: BigInt(block.point.blockNo),
    blockType: 7,
    prevHash: block.parentPoint.blockHash,
    tip: toTip(tip),
    block: Buffer.from(block.nativeBlock.rawBlockCbor, "hex"),
  });
  const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

  return {
    openStream: async ({ points, window }, stream) => {
      const exact = window === 1;
      let closed = false;
      stream.onClose(() => {
        closed = true;
      });
      // A held exact-point query waits before it intersects.
      while (exact && readControl().exactQueriesHeld === true) {
        if (readControl().closed || closed) return await new Promise(() => {});
        await sleep(10);
      }
      const initial = readControl();
      const fallbackTip = { kind: "point", ...initial.tip };
      const intersection = initial.canonicalBranchSelected
        ? points.find(registered)
        : exact
          ? points[0]
          : (points.find(registered) ?? points[0]);
      const target =
        exact && intersection !== undefined && intersection !== "origin"
          ? successorOf(intersection)
          : undefined;
      if (intersection === undefined || (exact && target === undefined))
        return { notFound: toTip(fallbackTip) };
      let fixedTip;
      if (initial.mode === "query_counter")
        fixedTip = exact
          ? exactTip(target)
          : (() => {
              try {
                return JSON.parse(readFileSync(options.tipPath, "utf8"));
              } catch {
                return fallbackTip;
              }
            })();
      const tipAt = (control) =>
        control.mode === "controlled"
          ? { kind: "point", ...control.tip }
          : fixedTip;
      const tip = tipAt(initial);
      if (exact) {
        appendFileSync(
          options.queryLogPath,
          `${JSON.stringify({
            target: {
              blockHash: target.point.blockHash,
              blockNo: target.point.blockNo,
              slot: target.point.slot,
            },
            tip: {
              blockHash: tip.blockHash,
              blockNo: tip.blockNo,
              slot: tip.slot,
            },
          })}\n`,
        );
        stream.rollForward(forwardOf(target, tip));
      }
      let position = intersection;
      let commandCursor = initial.commands.length;
      const timer = setInterval(() => {
        if (closed) return clearInterval(timer);
        const control = readControl();
        if (control.closed) {
          clearInterval(timer);
          return stream.fail("node_connection_lost", "the fixture closed");
        }
        if (exact && !control.canonicalBranchSelected) return;
        const now = tipAt(control);
        while (commandCursor < control.commands.length) {
          const command = control.commands[commandCursor++];
          if (command.kind === "exit") {
            process.stderr.write("Synthetic native stream exit requested\n");
            process.exit(command.exitCode);
          }
          position =
            command.point === "origin"
              ? "origin"
              : {
                  slot: BigInt(command.point.slot),
                  hash: command.point.blockHash,
                };
          stream.rollBackward({ point: position, tip: toTip(now) });
        }
        if (exact) return;
        const next = childOf(readBlocks(), position);
        if (
          next === undefined ||
          BigInt(next.point.blockNo) > BigInt(now.blockNo) ||
          BigInt(next.point.slot) > BigInt(now.slot)
        )
          return;
        stream.rollForward(forwardOf(next, now));
        position = {
          slot: BigInt(next.point.slot),
          hash: next.point.blockHash,
        };
      }, 10);
      return { intersection, tip: toTip(tip) };
    },
  };
};
