import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { afterAll, describe, expect, it } from "vitest";

import { journey } from "./submit-init-emulator-event-history-list.journey.js";
import {
  blueprintBytes,
  candidateBounds,
  exploratoryBounds,
  records,
} from "./submit-init-emulator-event-history-list.setup.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

afterAll(() => {
  expect(readFileSync(realBlueprintPath).equals(blueprintBytes)).toBe(true);
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "list-emulator.json"),
    JSON.stringify(
      {
        scope:
          "Applied initialization, admission, pointer continuation, retirement and reclamation; fixture hub/finality/settlement/payout mint authorities; provisional recipe recorded per scenario; exploratory bounds explicitly marked in test names; not production queue or payout-spend acceptance",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ) + "\n",
  );
});

// The external-payload diagnostics settle 1024-node and 14000-byte payloads
// through the emulator and take several seconds each.
describe("applied authenticated event lists", { timeout: 60_000 }, () => {
  it.each(["Deposit", "Withdrawal"] as const)(
    "admits inline %s through permissionless filler promotion",
    async (kind) => journey(kind, false),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "admits externally prepublished %s through permissionless filler promotion",
    async (kind) => journey(kind, true),
  );
  it.each([false, true])(
    "settles and unlinks deposit (external=%s)",
    async (external) => journey("Deposit", external, "settle"),
  );
  it.each([false, true])(
    "initializes payout and unlinks withdrawal (external=%s)",
    async (external) => journey("Withdrawal", external, "settle"),
  );
  it.each([false, true])(
    "refunds and unlinks invalid withdrawal (external=%s)",
    async (external) => journey("Withdrawal", external, "refund"),
  );

  it.each(["Deposit", "Withdrawal"] as const)(
    "settles exact inline boundary for %s",
    async (kind) => journey(kind, false, "settle", "inline-boundary"),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "diagnostic singleton settles 14000-byte external datum for %s",
    async (kind) =>
      journey(
        kind,
        true,
        "settle",
        "ab".repeat(14000),
        false,
        0,
        exploratoryBounds,
      ),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "diagnostic singleton settles exact 1024-node external payload for %s",
    async (kind) =>
      journey(
        kind,
        true,
        "settle",
        "node-boundary",
        false,
        0,
        exploratoryBounds,
      ),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "rejects and reclaims over-budget external payload for %s",
    async (kind) =>
      journey(
        kind,
        true,
        "settle",
        Array.from({ length: 14000 }, () => 0n),
        true,
      ),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "settles 64-level proof with exact inline payload for %s",
    async (kind) =>
      journey(kind, false, "settle", "inline-boundary", false, 64),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "settles 64-level proof with external bytes for %s",
    async (kind) => journey(kind, true, "settle", "ab".repeat(4000), false, 64),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "settles 64-level proof with exact node boundary for %s",
    async (kind) => journey(kind, true, "settle", "node-boundary", false, 64),
  );

  it.each(["Deposit", "Withdrawal"] as const)(
    "settles 64-level proof at combined byte and node bounds for %s",
    async (kind) =>
      journey(kind, true, "settle", "combined-boundary", false, 64),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "rejects and reclaims oversized bytes under candidate bounds for %s",
    async (kind) => journey(kind, true, "settle", "ab".repeat(14000), true),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "settles maximum predecessor and 64-level proof at combined bounds for %s",
    async (kind) =>
      journey(
        kind,
        true,
        "settle",
        "combined-boundary",
        false,
        64,
        candidateBounds,
        true,
      ),
  );
  it.each([
    { kind: "Deposit", mode: "settle" },
    { kind: "Withdrawal", mode: "settle" },
    { kind: "Withdrawal", mode: "refund" },
  ] as const)(
    "CP3 combined maximum $kind $mode preserves nine wide assets through retirement and reclaim",
    async ({ kind, mode }) =>
      journey(
        kind,
        true,
        mode,
        "combined-boundary",
        false,
        64,
        candidateBounds,
        true,
        undefined,
        undefined,
        undefined,
        true,
      ),
  );
  it.each(["Deposit", "Withdrawal"] as const)(
    "refuses a %s node funded only for its admission output",
    async (kind) =>
      journey(
        kind,
        false,
        undefined,
        undefined,
        true,
        0,
        candidateBounds,
        false,
        "node",
      ),
  );
  it("refuses a deposit whose structural ADA leaves an unspendable reserve Value", async () =>
    journey(
      "Deposit",
      false,
      undefined,
      undefined,
      true,
      0,
      candidateBounds,
      false,
      "deposit-reserve",
    ));
  it("refuses locked withdrawal ADA above its target", async () =>
    journey(
      "Withdrawal",
      false,
      undefined,
      undefined,
      true,
      0,
      candidateBounds,
      false,
      "withdrawal-target",
    ));
  it("refuses insufficient future payout ADA and reclaims unused data", async () =>
    journey(
      "Withdrawal",
      true,
      "settle",
      undefined,
      true,
      0,
      candidateBounds,
      false,
      "withdrawal-payout",
    ));
  it("refuses insufficient future refund ADA and reclaims unused data", async () =>
    journey(
      "Withdrawal",
      true,
      "refund",
      "",
      true,
      0,
      candidateBounds,
      false,
      "withdrawal-refund",
    ));
});

// Bypass SDK preflight only to exercise the applied validator's independent
// authentication rules. Existing positive journeys use the public builders.
describe(
  "CP3 applied admission authentication refusals",
  { timeout: 60_000 },
  () => {
    for (const kind of ["Deposit", "Withdrawal"] as const) {
      for (const fault of [
        "hash-only",
        "same-transaction-publication",
        "wrong-retention-address",
        "mismatched-datum-hash",
        "backdated-inclusion",
        "short-key",
        "oversized-key",
      ] as const) {
        it(`${kind} refuses ${fault} without consuming its nonce, filler or retained data`, async () => {
          const external =
            fault === "hash-only" ||
            fault === "same-transaction-publication" ||
            fault === "wrong-retention-address" ||
            fault === "mismatched-datum-hash";
          await journey(
            kind,
            external,
            undefined,
            undefined,
            true,
            0,
            candidateBounds,
            false,
            undefined,
            fault,
          );
        });
      }
    }
  },
);

describe(
  "CP3 applied finalized-frontier retirement refusals",
  { timeout: 60_000 },
  () => {
    for (const kind of ["Deposit", "Withdrawal"] as const) {
      for (const fault of [
        "before-inclusion",
        "wrong-confirmed-token",
      ] as const) {
        it(`${kind} refuses retirement with ${fault} despite valid settlement membership`, async () => {
          await journey(
            kind,
            true,
            "settle",
            undefined,
            false,
            0,
            candidateBounds,
            false,
            undefined,
            undefined,
            fault,
          );
        });
      }
    }
  },
);
