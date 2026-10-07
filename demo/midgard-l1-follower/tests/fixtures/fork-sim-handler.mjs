// The fake sidecar handler of the fork simulator's transport test: it serves
// a scenario's events (options.events, from buildForkSteps) from the store's
// origin, exactly as the simulator emitted them.
import { samePoint } from "@al-ft/l1-node-transport/testing/fake-sidecar";

const point = (p) => (p === "origin" ? p : { slot: p.slot, hash: p.hash });

export default (options) => ({
  hello: () => ({ nodeToClientVersion: 32784 }),
  openStream: ({ points }, stream) => {
    const origin = point(options.origin);
    const last = options.events[options.events.length - 1];
    const tip = last === undefined ? { point: origin, blockNo: 0 } : last.tip;
    if (!points.some((p) => samePoint(p, origin))) return { notFound: tip };
    for (const event of options.events)
      if (event.kind === "roll_forward")
        stream.rollForward({
          point: event.point,
          blockNo: event.blockNo,
          blockType: event.blockType,
          prevHash: event.prevHash,
          tip: event.tip,
          block: Uint8Array.from(Buffer.from(event.block, "hex")),
        });
      else stream.rollBackward({ point: event.point, tip: event.tip });
    return { intersection: origin, tip };
  },
});
