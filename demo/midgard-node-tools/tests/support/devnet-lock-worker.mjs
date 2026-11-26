import fs from "node:fs";

const [path, markers, name] = process.argv.slice(2);
const mark = (state) => fs.writeFileSync(`${markers}/${name}-${state}`, "1");
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
