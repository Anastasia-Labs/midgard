import { startWatcherTrustedHeadAuthorityProcess } from "../../src/runtime/trusted-head-runtime.js";

type Input = Parameters<typeof startWatcherTrustedHeadAuthorityProcess>[0];
let runtime:
  | Awaited<ReturnType<typeof startWatcherTrustedHeadAuthorityProcess>>
  | undefined;
let starting = false;
let closing = false;

const close = async (): Promise<void> => {
  if (closing) return;
  closing = true;
  await runtime?.close();
  if (process.connected) process.disconnect?.();
};

process.on("disconnect", () => {
  void close().catch(() => process.exit(1));
});
process.on(
  "message",
  (message: { kind: "start"; input: Input } | { kind: "close" }) => {
    if (message.kind === "close") {
      void close().catch(() => process.exit(1));
      return;
    }
    if (starting || closing) return;
    starting = true;
    void startWatcherTrustedHeadAuthorityProcess(message.input)
      .then(async (started) => {
        runtime = started;
        if (closing) {
          await runtime.close();
          return;
        }
        process.send?.({
          kind: "ready",
          endpoint: runtime.server.endpoint,
          processId: process.pid,
        });
      })
      .catch(() => {
        // Config and secret values stay inside the child, including on failure.
        process.send?.({ kind: "error" }, () => process.disconnect?.());
        process.exitCode = 1;
      });
  },
);
