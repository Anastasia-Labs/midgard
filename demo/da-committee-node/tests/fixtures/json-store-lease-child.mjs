import fs from "node:fs/promises";
import { syncBuiltinESMExports } from "node:module";

const [bundle, storePath, role] = process.argv.slice(2);
const send = (event) => process.send(event);
const message = () =>
  new Promise((resolve) => process.once("message", resolve));
let armed = false;
const open = fs.open;
fs.open = async (...args) => {
  const handle = await open(...args);
  const path = String(args[0]);
  if (
    path.startsWith(`${storePath}.`) &&
    path.endsWith(".tmp") &&
    !path.endsWith(".metadata.tmp")
  ) {
    const writeFile = handle.writeFile.bind(handle);
    handle.writeFile = async (...writeArgs) => {
      const result = await writeFile(...writeArgs);
      if (armed) {
        armed = false;
        send({ event: "write-paused" });
        await message();
      }
      return result;
    };
  }
  return handle;
};
syncBuiltinESMExports();
const { JsonFileCommitteeStore } = await import(bundle);
let store;
try {
  store = await JsonFileCommitteeStore.open(storePath, {
    renewMs: 1_000_000_000,
    ...(role === "contender" ? { now: () => Date.now() + 120_000 } : {}),
  });
} catch (error) {
  send({ event: "refused", message: error.message });
  process.disconnect();
  process.exit(0);
}
send({ event: "opened" });
await message();
armed = role === "paused";
await store.savePeerHealth({
  peerId: role === "paused" ? "old-peer" : "new-peer",
  consecutiveFailures: 0,
  updatedAt: new Date().toISOString(),
});
send({
  event: "saved",
  peers: (await store.listPeerHealth()).map((peer) => peer.peerId),
});
await message();
await store.close();
send({ event: "closed" });
process.disconnect();
