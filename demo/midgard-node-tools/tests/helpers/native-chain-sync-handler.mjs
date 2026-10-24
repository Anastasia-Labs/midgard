// The node behind the watcher's fake node transport. Each chain-sync open
// takes the next scripted step (`options.steps`, then `options.mode`, then
// "honest"); a "hello:<code>" step refuses the next session's handshake
// instead. Taken steps and closed streams are appended to `options.journal`
// so a test can see what the node was asked. SIGUSR1 fails every open
// stream, as a lost node connection would.
import { appendFileSync, readFileSync, writeFileSync } from "node:fs";

const hash = (byte) => byte.repeat(32);
const tip = { point: { slot: 103, hash: hash("44") }, blockNo: 12 };
const forward = {
  point: { slot: 101, hash: hash("bb") },
  blockNo: 10,
  blockType: 6,
  prevHash: hash("aa"),
  tip,
  block: Uint8Array.from([0x80]),
};

export default (options, controls) => {
  const steps = options.steps ?? [];
  const journal = (line) => {
    if (options.journal !== undefined)
      appendFileSync(options.journal, `${line}\n`);
  };
  const taken = () => {
    if (options.journal === undefined) return 0;
    try {
      return readFileSync(options.journal, "utf8")
        .split("\n")
        .filter((line) => line.startsWith("step ")).length;
    } catch {
      return 0;
    }
  };
  let localTaken = 0;
  const peek = () =>
    steps[options.journal === undefined ? localTaken : taken()] ??
    options.mode ??
    "honest";
  const take = () => {
    const step = peek();
    localTaken += 1;
    journal(`step ${step}`);
    return step;
  };
  if (options.pidFile !== undefined)
    writeFileSync(options.pidFile, String(process.pid));
  const later = (ms, action) => setTimeout(action, ms);
  const open = new Set();
  process.on("SIGUSR1", () => {
    for (const stream of open)
      stream.fail("node_connection_lost", "fake fault");
    open.clear();
  });

  return {
    hello: ({ networkMagic }) => {
      if (networkMagic !== (options.magic ?? 1))
        return {
          fatal: {
            code: "node_handshake_failed",
            message: "network magic differs",
            status: 69,
          },
        };
      const step = peek();
      if (!step.startsWith("hello:")) return undefined;
      take();
      return {
        fatal: {
          code: step.slice("hello:".length),
          message: "the node did not answer",
          status: 69,
        },
      };
    },
    openStream: async ({ points }, stream) => {
      const step = take();
      open.add(stream);
      stream.onClose(() => {
        open.delete(stream);
        journal("closed");
      });
      if (step.startsWith("fail:"))
        return { error: { code: step.slice("fail:".length) } };
      if (step === "no_ready") return await new Promise(() => {});
      if (step === "retry_intersection") {
        // Only the Origin is on this node's chain; it stays idle there.
        return points.includes("origin")
          ? { intersection: "origin", tip }
          : { notFound: tip };
      }
      const intersection = points[0];
      if (intersection === "origin") return { intersection, tip };
      if (step.startsWith("query_")) {
        if (step !== "query_wait")
          stream.rollForward({
            ...forward,
            ...(step === "query_wrong_target" ? { blockNo: 11 } : {}),
            ...(step === "query_old"
              ? {
                  tip: {
                    point: { slot: 6000, hash: hash("44") },
                    blockNo: 5000,
                  },
                }
              : {}),
            ...(step === "query_bad_tip"
              ? { tip: { ...tip, blockNo: 9 } }
              : {}),
          });
        if (step === "query_exit") later(100, () => controls.exit(23));
        if (step === "query_extra")
          later(50, () =>
            stream.rollForward({
              ...forward,
              point: { slot: 102, hash: hash("cc") },
              prevHash: hash("bb"),
              blockNo: 11,
            }),
          );
        if (step === "query_fail")
          later(100, () => stream.fail("node_connection_lost", "fake fault"));
        return { intersection, tip };
      }
      switch (step) {
        case "runtime_failure":
          later(25, () =>
            stream.fail(
              "node_connection_lost",
              "actual underlying socket failure",
            ),
          );
          break;
        case "crash":
          later(50, () => controls.exit(23));
          break;
        case "reordered":
          stream.rollForward({ ...forward, prevHash: hash("cc") });
          break;
        case "first_slot_regression":
          stream.rollForward({
            ...forward,
            point: { ...forward.point, slot: 99 },
          });
          break;
        default: {
          stream.rollForward(forward);
          const below = step === "below_intersection";
          stream.rollBackward({
            point: {
              slot: below ? 90 : 100,
              hash: ["unknown_rollback", "below_intersection"].includes(step)
                ? hash("dd")
                : hash("aa"),
            },
            tip,
          });
          if (below)
            stream.rollForward({
              ...forward,
              point: { slot: 91, hash: hash("ee") },
              prevHash: hash("dd"),
              blockNo: 9,
            });
        }
      }
      return { intersection, tip };
    },
  };
};
