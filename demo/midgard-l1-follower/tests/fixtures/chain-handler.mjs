// A fake sidecar handler serving one fixed chain. Options: `base` (the
// point the first block extends: "origin" or {slot, hash}) and `blocks`
// ([{slot, hash, blockNo, prevHash, blockType?, block (hex)}], in order).
// FindIntersect matches the base or any block; the stream then rolls
// forward every later block, all reporting the last block as the tip.
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
  return {
    hello: () => ({ nodeToClientVersion: 32784 }),
    openStream: ({ points }, stream) => {
      const at = points.find((point) =>
        known.some((entry) => samePoint(entry, point)),
      );
      if (at === undefined) return { notFound: tip };
      const from = known.findIndex((entry) => samePoint(entry, at));
      for (const block of blocks.slice(from))
        stream.rollForward({ ...block, tip });
      return { intersection: at, tip };
    },
  };
};
