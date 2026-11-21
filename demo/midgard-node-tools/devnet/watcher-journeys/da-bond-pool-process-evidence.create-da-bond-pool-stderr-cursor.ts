import {
  type DaBondPoolStderrEvent,
  isDaBondPoolEvent,
  NEWLINE,
} from "./da-bond-pool-process-evidence.check-da-bond-cli-submit-evidence.js";
import { isRecord } from "./da-bond-pool-process-evidence.command-mismatches.js";

/**
 * Collects the pool-monitor events one node process writes to stderr (P27).
 * The cursor belongs to one pid: each `take` gets that process's whole stderr
 * capture so far, and returns the JSON event lines whose `event` `matches`
 * (by default the pool-monitor transition events) between the byte offset it stopped
 * at last time and the capture's last newline, each tied to the pid. A
 * partial last line waits for its newline; every other line (logs, other
 * events) is skipped. A restarted node gets a new cursor, so an event can
 * never be carried across the restart.
 */
export const createDaBondPoolStderrCursor = (
  pid: number,
  matches: (event: string) => boolean = isDaBondPoolEvent,
) => {
  let consumed = 0;
  return {
    pid,
    /** The byte offset read up to. */
    offset: () => consumed,
    take: (captured: Uint8Array): readonly DaBondPoolStderrEvent[] => {
      if (captured.length < consumed)
        throw new Error(`the stderr capture of pid ${pid} shrank`);
      const end = captured.lastIndexOf(NEWLINE) + 1;
      if (end <= consumed) return [];
      const text = Buffer.from(captured.subarray(consumed, end)).toString(
        "utf8",
      );
      consumed = end;
      const events: DaBondPoolStderrEvent[] = [];
      for (const line of text.split("\n")) {
        const trimmed = line.trim();
        if (!trimmed.startsWith("{")) continue;
        let value: unknown;
        try {
          value = JSON.parse(trimmed);
        } catch {
          continue;
        }
        if (
          isRecord(value) &&
          typeof value.event === "string" &&
          matches(value.event)
        )
          events.push({ pid, event: value.event, line: trimmed });
      }
      return events;
    },
  };
};
