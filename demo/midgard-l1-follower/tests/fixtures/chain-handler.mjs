// A fake sidecar handler serving one fixed chain. Options: `base` (the
// point the first block extends: "origin" or {slot, hash}) and `blocks`
// ([{slot, hash, blockNo, prevHash, blockType?, block (hex)}], in order).
// FindIntersect matches the base or any block; the stream then rolls
// forward every later block, all reporting the last block as the tip.
// With `failingOpens` (n) and `servedBeforeFailing` (k), the first open
// serves k blocks and then fails, the next n opens fail at once, and later
// opens serve normally.
import { samePoint } from "@al-ft/l1-node-transport/testing/fake-sidecar";

export default (options) => {
  const blocks = options.blocks.map((block) => ({
    point: { slot: block.slot, hash: block.hash },
    blockNo: block.blockNo,
    prevHash: block.prevHash,
    blockType: block.blockType ?? 7,
    block: Buffer.from(block.block, "hex"),
  }));
  const last = blocks.at(-1);
  const tip =
    last === undefined
      ? { point: options.base, blockNo: 0 }
      : { point: last.point, blockNo: last.blockNo };
  const known = [options.base, ...blocks.map((block) => block.point)];
  let opens = 0;
  return {
    hello: () => ({ nodeToClientVersion: 32784 }),
    openStream: ({ points }, stream) => {
      const at = points.find((point) =>
        known.some((entry) => samePoint(entry, point)),
      );
      if (at === undefined) return { notFound: tip };
      const from = known.findIndex((entry) => samePoint(entry, at));
      const open = opens;
      opens += 1;
      const failing =
        options.failingOpens !== undefined && open <= options.failingOpens;
      const served = blocks.slice(from);
      for (const block of failing
        ? served.slice(0, open === 0 ? options.servedBeforeFailing : 0)
        : served)
        stream.rollForward({ ...block, tip });
      if (failing) stream.fail("node_connection_lost", "fake stream fault");
      return { intersection: at, tip };
    },
  };
};
