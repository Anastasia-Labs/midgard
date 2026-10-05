/** Loaded explicitly by synthetic CLI process tests; never a production hook. */
import fs from "node:fs/promises";
import { syncBuiltinESMExports } from "node:module";

const fault = process.env.MIDGARD_TEST_AUTHORITY_FAULT_MODE;
if (fault === "output") {
  process.stdout.write = () => {
    throw new Error("synthetic initialization output acknowledgement loss");
  };
} else if (fault === "sync") {
  const open = fs.open;
  fs.open = async (...args) => {
    const handle = await open(...args);
    if (args[0].toString() === process.env.MIDGARD_TEST_AUTHORITY_DIRECTORY)
      handle.sync = async () => {
        throw new Error("synthetic selected namespace sync refusal");
      };
    return handle;
  };
  syncBuiltinESMExports();
} else if (fault === "before" || fault === "after") {
  const link = fs.link;
  fs.link = async (from, to) => {
    if (to.toString().endsWith("/authority-backend.json")) {
      if (fault === "after") await link(from, to);
      process.kill(process.pid, "SIGKILL");
    }
    await link(from, to);
  };
  syncBuiltinESMExports();
}
