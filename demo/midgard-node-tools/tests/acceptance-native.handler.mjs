// The node behind the acceptance native boundary tests' fake node transport.
// Every chain-sync stream intersects at its first point and, unless the mode is
// "no-current", delivers the current block at the node tip. The test appends
// commands to `options.commands` (forward, rollback, exit, error); the node
// applies each new one to every open stream. Opened and ended streams are
// appended to `options.journal`; a stream ends when the client closes it, when
// the node fails it, or when the sidecar exits.
import { appendFileSync, readFileSync } from "node:fs";

const hash = (byte) => byte.repeat(32);
const tip = (byte, blockNo, slot) => ({
  point: { slot: BigInt(slot), hash: hash(byte) },
  blockNo: BigInt(blockNo),
});
const forward = (byte, prev, blockNo, slot) => ({
  point: { slot: BigInt(slot), hash: hash(byte) },
  blockNo: BigInt(blockNo),
  blockType: 6,
  prevHash: hash(prev),
  tip: tip(byte, blockNo, slot),
  block: Uint8Array.from([0x80]),
});

export default (options) => {
  const journal = (line) => appendFileSync(options.journal, `${line}\n`);
  const open = new Set();
  const end = (stream) => {
    if (open.delete(stream)) journal("closed");
  };
  process.once("exit", () => {
    for (const stream of [...open]) end(stream);
  });
  let applied = 0;
  setInterval(() => {
    let commands;
    try {
      commands = readFileSync(options.commands, "utf8")
        .split("\n")
        .filter(Boolean);
    } catch {
      return;
    }
    while (applied < commands.length) {
      const command = commands[applied++];
      if (command === "exit") process.exit(23);
      for (const stream of [...open]) {
        if (command === "forward")
          stream.rollForward(forward("cc", "bb", 12, 103));
        if (command === "rollback")
          stream.rollBackward({
            point: { slot: 101n, hash: hash("aa") },
            tip: tip("aa", 10, 101),
          });
        if (command === "error") {
          stream.fail("chain_sync_failed", "the node failed the stream");
          end(stream);
        }
      }
    }
  }, 10);

  return {
    openStream: ({ points }, stream) => {
      open.add(stream);
      journal("opened");
      stream.onClose(() => end(stream));
      if (options.mode !== "no-current")
        stream.rollForward(forward("bb", "aa", 11, 102));
      return { intersection: points[0], tip: tip("bb", 11, 102) };
    },
  };
};
