import { isAbsolute, resolve } from "node:path";

import { admitHistoryChild } from "./history-child-admission.js";
import { waitHistoryChildLifetime } from "./history-child-lifetime.js";
import { startHistoryChildLifecycle } from "./history-child-startup.js";
import {
  historyCommandExitCode,
  HistoryConfigurationRefusal,
} from "./history-configuration-refusal.js";
import { answerHistoryProviderReadiness } from "./history-provider-readiness.js";
import { readHistoryChildContext } from "./history-role-context.js";
import { makeLayout, readRunEnv } from "./layout.js";
import { readServiceRecoveryContext } from "./service-recovery-context.js";
import { DEFAULT_POLICY } from "./supervisor.js";
import {
  HISTORY_ROLES,
  startHistoryArchive,
  startHistoryTunnel,
} from "./watcher-history.js";
import { runHistoryRecorder } from "./watcher-history-recorder.js";

/** Only the three supervisor-owned history commands use EX_CONFIG/EX_SOFTWARE. */
export const runHistoryRoleCommand = async (options: {
  readonly runDir: string;
  readonly role: "archive" | "tunnel" | "recorder";
  readonly provider?: string;
}): Promise<void> => {
  let lifecycle: ReturnType<typeof startHistoryChildLifecycle> | undefined;
  try {
    if (!isAbsolute(options.runDir))
      throw new HistoryConfigurationRefusal("--run-dir must be absolute");
    const layout = makeLayout(resolve(options.runDir));
    const run = readRunEnv(layout);
    if (options.role === "recorder") {
      const context = readHistoryChildContext("history-recorder", run);
      lifecycle = startHistoryChildLifecycle(context.actor);
      const child = await admitHistoryChild(
        "history-recorder",
        layout,
        run,
        lifecycle.signal,
        context,
      );
      if (child === undefined || lifecycle.signal.aborted) return;
      await runHistoryRecorder(
        readServiceRecoveryContext(layout),
        child,
        lifecycle,
      );
      return;
    }
    const provider =
      options.role === "archive"
        ? HISTORY_ROLES.find((role) => role === options.provider)
        : undefined;
    if (options.role === "archive" && provider === undefined)
      throw new HistoryConfigurationRefusal(
        `--provider must be one of ${HISTORY_ROLES.join(", ")}`,
      );
    const role =
      options.role === "tunnel"
        ? "history-tunnel"
        : provider === "a"
          ? "history-archive-a"
          : "history-archive-b";
    const context = readHistoryChildContext(role, run);
    lifecycle = startHistoryChildLifecycle(context.actor);
    const child = await admitHistoryChild(
      role,
      layout,
      run,
      lifecycle.signal,
      context,
    );
    if (child === undefined || lifecycle.signal.aborted) return;
    const binding =
      provider === "a"
        ? child.admission.providers[0]?.binding
        : child.admission.providers[1]?.binding;
    if (provider !== undefined && binding === undefined)
      throw new HistoryConfigurationRefusal(
        "history archive recorded provider binding is absent",
      );
    const listening =
      provider === undefined
        ? await startHistoryTunnel(run)
        : await startHistoryArchive(
            layout,
            run,
            provider,
            binding === undefined
              ? undefined
              : {
                  binding,
                  maximumBudgetMs: DEFAULT_POLICY.probeTimeoutMs,
                },
          );
    let stopReadiness: (() => void) | undefined;
    try {
      if (!lifecycle.signal.aborted) {
        stopReadiness = answerHistoryProviderReadiness(child, run, lifecycle);
        await waitHistoryChildLifetime(child, lifecycle.signal);
      }
    } finally {
      stopReadiness?.();
      await listening.close();
    }
  } catch (error) {
    if (lifecycle?.signal.aborted === true && error === lifecycle.signal.reason)
      return; // The native owner already joined its helper before cancellation.
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = historyCommandExitCode(error);
  } finally {
    try {
      await lifecycle?.close();
    } catch (error) {
      console.error(error instanceof Error ? error.message : String(error));
      process.exitCode = historyCommandExitCode(error);
    }
  }
};
