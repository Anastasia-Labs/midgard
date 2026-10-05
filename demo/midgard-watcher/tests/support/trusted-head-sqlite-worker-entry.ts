/** Synthetic process fault fixture: all persistent state is supplied by the test. */
import fs from "node:fs/promises";
import { syncBuiltinESMExports } from "node:module";
import { DatabaseSync } from "node:sqlite";

import {
  importLegacyAuthorityStore,
  initializeSelectedAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
} from "../../src/runtime/trusted-head-authority.js";

type Input = Parameters<typeof initializeSelectedAuthorityStore>[0];
type Request = Readonly<{
  input: Omit<Input, "recordAuthenticationKey"> & {
    recordAuthenticationKey: number[];
    legacyDirectory?: string;
  };
  operation: "initialize" | "import" | "cas";
  expectedTrustedHead?: unknown;
  nextTrustedHead?: unknown;
  fault?: "before" | "after";
  selectorFault?: "before" | "after";
  pauseSelector?: boolean;
}>;

const run = async (request: Request): Promise<void> => {
  const input = {
    ...request.input,
    recordAuthenticationKey: Uint8Array.from(
      request.input.recordAuthenticationKey,
    ),
  };
  let store:
    | Awaited<ReturnType<typeof openWatcherTrustedHeadAuthorityStore>>
    | undefined;
  try {
    if (request.operation === "cas")
      store = await openWatcherTrustedHeadAuthorityStore(input);
    // Install only after ordinary open's read transaction has finished.
    const original = DatabaseSync.prototype.exec;
    DatabaseSync.prototype.exec = function (sql: string): void {
      const matches =
        request.operation === "initialize"
          ? sql.startsWith("COMMIT;")
          : sql === "COMMIT";
      if (matches && request.fault !== undefined) {
        if (request.fault === "after") original.call(this, sql);
        process.kill(process.pid, "SIGKILL");
      }
      original.call(this, sql);
    };
    if (request.selectorFault !== undefined || request.pauseSelector === true) {
      const link = fs.link;
      fs.link = async (from, to) => {
        if (to.toString().endsWith("/authority-backend.json")) {
          if (request.pauseSelector === true) {
            process.send?.({ kind: "prepared" });
            await new Promise<void>((resolve) =>
              process.once("message", () => resolve()),
            );
          }
          if (request.selectorFault !== undefined) {
            if (request.selectorFault === "after") await link(from, to);
            process.kill(process.pid, "SIGKILL");
          }
        }
        await link(from, to);
      };
      syncBuiltinESMExports();
    }
    process.send?.({ kind: "ready" });
    await new Promise<void>((resolve) =>
      process.once("message", () => resolve()),
    );
    const result =
      request.operation === "initialize"
        ? await initializeSelectedAuthorityStore(input)
        : request.operation === "import"
          ? await importLegacyAuthorityStore({
              ...input,
              legacyDirectory: input.legacyDirectory!,
            })
          : await store!.compareAndSwap({
              expectedTrustedHead: request.expectedTrustedHead ?? null,
              nextTrustedHead: request.nextTrustedHead,
            });
    store?.close();
    process.send?.({ kind: "result", result }, () => process.disconnect());
  } catch (error) {
    store?.close();
    process.send?.({ kind: "error", message: String(error) }, () =>
      process.disconnect(),
    );
  }
};
process.once("message", (request: Request) => {
  void run(request);
});
