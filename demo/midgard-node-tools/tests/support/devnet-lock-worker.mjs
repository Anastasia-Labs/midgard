import fs from "node:fs";
import { syncBuiltinESMExports } from "node:module";

const [path, markers, name, pauseRead] = process.argv.slice(2);
const mark = (state) => fs.writeFileSync(`${markers}/${name}-${state}`, "1");
if (pauseRead === "pause") {
  const read = fs.readFileSync;
  let first = true;
  fs.readFileSync = (...args) => {
    const bytes = read(...args);
    if (first && args[0] === path) {
      first = false;
      mark("read");
      process.kill(process.pid, "SIGSTOP");
    }
    return bytes;
  };
  syncBuiltinESMExports();
}
const { acquireLock } = await import("../../src/devnet-stack/lock.ts");
try {
  const release = acquireLock(path);
  mark("owned");
  let releases = 0;
  process.on("message", () => {
    release();
    mark("released");
    mark(`released-${++releases}`);
  });
  setInterval(() => {}, 1_000);
} catch (error) {
  fs.writeFileSync(`${markers}/${name}-refused`, String(error));
}
