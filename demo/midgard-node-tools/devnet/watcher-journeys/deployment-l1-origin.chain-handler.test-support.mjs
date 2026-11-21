// A fake sidecar handler serving one fixed chain to every chain-sync open,
// as a node does: an open from genesis delivers every block, an open from a
// block on the chain delivers the blocks after it, and an open from any
// other point finds no intersection. Option `blocks`: the chain in order,
// each {slot, hash, block (hex), blockType}.
import { Buffer } from "node:buffer";

export default (options) => {
  const blocks = options.blocks;
  const last = blocks.at(-1);
  const tip =
    last === undefined
      ? { point: "origin", blockNo: 0 }
      : { point: { slot: last.slot, hash: last.hash }, blockNo: blocks.length };
  return {
    hello: () => ({ nodeToClientVersion: 32784 }),
    openStream: ({ points }, stream) => {
      const at = points[0];
      const start =
        at === "origin"
          ? 0
          : blocks.findIndex(
              (block) =>
                String(block.slot) === String(at.slot) &&
                block.hash === at.hash,
            ) + 1;
      if (start === 0 && at !== "origin") return { notFound: tip };
      for (let index = start; index < blocks.length; index += 1) {
        const { slot, hash, block, blockType } = blocks[index];
        stream.rollForward({
          point: { slot, hash },
          blockNo: index + 1,
          prevHash: index === 0 ? null : blocks[index - 1].hash,
          blockType,
          block: new Uint8Array(Buffer.from(block, "hex")),
          tip,
        });
      }
      return { intersection: at, tip };
    },
  };
};
