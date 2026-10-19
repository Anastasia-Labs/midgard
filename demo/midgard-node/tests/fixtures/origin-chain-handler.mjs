// A fake sidecar handler for the nonce command's L1 origin reads. Options:
// `tip` ("origin" or {slot, hash, blockNo}: the tip every open reports) and
// `settles` (one script per chain-sync open from a block, the last one
// repeating): {notFound: true}, or {events: [...]} where each event is
// {forward: {slot, hash}} or {backward: "origin" | {slot, hash}}; a forward
// may carry the block's bytes (`block`, hex) and `blockType`, by default the
// placeholder bytes 0x80 and type 7. An open from origin (the tip read) serves no
// events, or the `genesis` script ({events: [...]}) when one is given. With
// `journal` (a path), each open from a block appends "open <slot>.<hash>".
import { Buffer } from "node:buffer";
import { appendFileSync } from "node:fs";

export default (options) => {
  const tip =
    options.tip === "origin"
      ? { point: "origin", blockNo: 0 }
      : {
          point: { slot: options.tip.slot, hash: options.tip.hash },
          blockNo: options.tip.blockNo,
        };
  let settles = 0;
  const serve = (events, stream, prevHash) => {
    let blockNo = 1;
    for (const event of events) {
      if (event.forward !== undefined) {
        const { block, blockType, ...point } = event.forward;
        stream.rollForward({
          point,
          blockNo,
          prevHash,
          blockType: blockType ?? 7,
          block:
            block === undefined
              ? new Uint8Array([0x80])
              : new Uint8Array(Buffer.from(block, "hex")),
          tip,
        });
        prevHash = point.hash;
        blockNo += 1;
      } else stream.rollBackward({ point: event.backward, tip });
    }
  };
  return {
    hello: () => ({ nodeToClientVersion: 32784 }),
    openStream: ({ points }, stream) => {
      const at = points[0];
      if (at === "origin") {
        if (options.genesis !== undefined)
          serve(options.genesis.events, stream, undefined);
        return { intersection: "origin", tip };
      }
      if (options.journal !== undefined)
        appendFileSync(options.journal, `open ${at.slot}.${at.hash}\n`);
      const script =
        options.settles[Math.min(settles, options.settles.length - 1)];
      settles += 1;
      if (script.notFound === true) return { notFound: tip };
      serve(script.events, stream, at.hash);
      return { intersection: at, tip };
    },
  };
};
