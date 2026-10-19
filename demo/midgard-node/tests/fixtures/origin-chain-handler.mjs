// A fake sidecar handler for the nonce command's L1 origin reads. Options:
// `tip` ("origin" or {slot, hash, blockNo}: the tip every open reports) and
// `settles` (one script per chain-sync open from a block, the last one
// repeating): {notFound: true}, or {events: [...]} where each event is
// {forward: {slot, hash}} or {backward: "origin" | {slot, hash}}. An open
// from origin (the tip read) serves no events. With `journal` (a path), each
// open from a block appends "open <slot>.<hash>".
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
  return {
    hello: () => ({ nodeToClientVersion: 32784 }),
    openStream: ({ points }, stream) => {
      const at = points[0];
      if (at === "origin") return { intersection: "origin", tip };
      if (options.journal !== undefined)
        appendFileSync(options.journal, `open ${at.slot}.${at.hash}\n`);
      const script =
        options.settles[Math.min(settles, options.settles.length - 1)];
      settles += 1;
      if (script.notFound === true) return { notFound: tip };
      let prevHash = at.hash;
      let blockNo = 1;
      for (const event of script.events) {
        if (event.forward !== undefined) {
          stream.rollForward({
            point: event.forward,
            blockNo,
            prevHash,
            blockType: 7,
            block: new Uint8Array([0x80]),
            tip,
          });
          prevHash = event.forward.hash;
          blockNo += 1;
        } else stream.rollBackward({ point: event.backward, tip });
      }
      return { intersection: at, tip };
    },
  };
};
