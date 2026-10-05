import fs from "node:fs/promises";
import { syncBuiltinESMExports } from "node:module";

import {
  repairLegacyWatcherTrustedHeadAuthorityFinalRecord,
  type WatcherLegacyAuthorityRepairInput,
} from "../../src/runtime/trusted-head-authority.legacy-repair.js";

type Request = {
  input: Omit<WatcherLegacyAuthorityRepairInput, "recordAuthenticationKey"> & {
    recordAuthenticationKey: number[];
  };
  phase: string;
};
const run = async (request: Request): Promise<void> => {
  const link = fs.link,
    unlink = fs.unlink;
  const kill = () => process.kill(process.pid, "SIGKILL");
  fs.link = async (from, to) => {
    const name = to.toString().split("/").at(-1);
    const stage =
      name === "removed-record.bin"
        ? "raw"
        : name === "intent.json"
          ? "intent"
          : name === "completed.json"
            ? "complete"
            : null;
    if (request.phase === stage + "-before") kill();
    await link(from, to);
    if (request.phase === stage + "-after") kill();
  };
  fs.unlink = async (path) => {
    const final = path
      .toString()
      .endsWith("/" + request.input.expectedTornRecordName);
    if (final && request.phase === "remove-before") kill();
    await unlink(path);
    if (final && request.phase === "remove-after") kill();
  };
  syncBuiltinESMExports();
  try {
    await repairLegacyWatcherTrustedHeadAuthorityFinalRecord({
      ...request.input,
      recordAuthenticationKey: Uint8Array.from(
        request.input.recordAuthenticationKey,
      ),
    });
    process.send?.({ kind: "unexpected-success" }, () => process.disconnect());
  } catch (error) {
    process.send?.({ kind: "error", message: String(error) }, () =>
      process.disconnect(),
    );
  }
};
process.once("message", (request: Request) => {
  void run(request);
});
