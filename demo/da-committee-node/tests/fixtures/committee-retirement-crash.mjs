import fs from "node:fs";
import { syncBuiltinESMExports } from "node:module";

import { JsonFileCommitteeStore } from "da-committee-node/store";

const [filePath, boundary] = process.argv.slice(2);
if (!filePath || !["before-rename", "after-rename"].includes(boundary))
  throw new Error("Invalid isolated retirement crash fixture arguments");
const store = await JsonFileCommitteeStore.open(filePath);
const rename = fs.promises.rename;
fs.promises.rename = async (from, to) => {
  if (to === filePath && boundary === "before-rename")
    process.kill(process.pid, "SIGKILL");
  await rename(from, to);
  if (to === filePath && boundary === "after-rename")
    process.kill(process.pid, "SIGKILL");
};
syncBuiltinESMExports();
await store.recordRetirementBreach("controlled native rollback", {
  slot: 1,
  blockHash: "aa".repeat(32),
});
await store.close();
throw new Error("Controlled physical rename boundary was not reached");
