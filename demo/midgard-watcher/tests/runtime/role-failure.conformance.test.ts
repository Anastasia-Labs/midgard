/**
 * The watcher's adapters agree with the shared failure classification
 * (`classifyRoleFailure`, `@al-ft/midgard-l1-follower`) on every
 * conformance row:
 * - `watcherStartupFailureExits` exits on a `transient` or `restart`
 *   failure and holds on any other, except an `unknown` failure carrying
 *   an errno (`ERRNO_EXITS`: a file not written yet, an I/O error), which
 *   it still exits on until the owner classes those errnos;
 * - `isTransientJournalFailure` retries a `transient` or `contention`
 *   journal failure on its timer and leaves any other to an operator.
 */
import { ROLE_FAILURE_ROWS } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import { isTransientJournalFailure } from "../../src/fault-proofs/watcher-journal-database.opener.js";
import { watcherStartupFailureExits } from "../../src/runtime/watcher-runtime.startup-hold.js";

/** `unknown` rows whose errno the watcher still exits on. */
const ERRNO_EXITS: ReadonlySet<string> = new Set([
  "a file not written yet (ENOENT)",
  "an I/O error (EIO)",
]);

const rows = ROLE_FAILURE_ROWS.map((row) => ({
  ...row,
  exits:
    row.expected === "transient" ||
    row.expected === "restart" ||
    ERRNO_EXITS.has(row.name),
  retried: row.expected === "transient" || row.expected === "contention",
}));

describe("the watcher agrees with the shared failure classification", () => {
  it.each(rows)(
    "watcherStartupFailureExits on $name ($expected)",
    ({ error, exits }) => {
      expect(watcherStartupFailureExits(error())).toBe(exits);
      expect(
        watcherStartupFailureExits(new Error("startup", { cause: error() })),
      ).toBe(exits);
    },
  );

  it.each(rows)(
    "isTransientJournalFailure on $name ($expected)",
    ({ error, retried }) => {
      expect(isTransientJournalFailure(error())).toBe(retried);
    },
  );
});
