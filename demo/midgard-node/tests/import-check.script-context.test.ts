/**
 * A foreign block's Plutus transactions apply by their committed verdicts,
 * so its import builds no script context for them. Building the block's
 * committed runs (a full replay of each transaction) builds one context per
 * run, which shows the count below sees every build.
 */
import { describe, expect, it, vi } from "vitest";

const contexts = vi.hoisted(() => ({ built: 0 }));
vi.mock("@al-ft/midgard-validation/script-context", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("@al-ft/midgard-validation/script-context")
    >();
  return {
    ...actual,
    buildPlutusV3ScriptContext: (
      ...args: Parameters<typeof actual.buildPlutusV3ScriptContext>
    ) => {
      contexts.built += 1;
      return actual.buildPlutusV3ScriptContext(...args);
    },
    buildMidgardScriptContext: (
      ...args: Parameters<typeof actual.buildMidgardScriptContext>
    ) => {
      contexts.built += 1;
      return actual.buildMidgardScriptContext(...args);
    },
  };
});

import {
  committedBlock,
  fundedBy,
  type LedgerEntry,
} from "./import-check.fixture.js";

describe("foreign import by committed verdict", () => {
  it("imports a block of Plutus transactions without building a script context", async () => {
    const parent: LedgerEntry[] = [
      fundedBy(0x41, "accept"),
      fundedBy(0x42, "reject"),
      fundedBy(0x43, "accept"),
    ];
    contexts.built = 0;
    const block = await committedBlock(
      parent,
      parent.map((entry, index) => ({
        input: entry.outRef,
        body: index === 1 ? ("reject" as const) : ("accept" as const),
        next: "accept" as const,
      })),
    );
    expect(contexts.built).toBe(block.runs.flat().length);
    expect(contexts.built).toBe(parent.length);

    contexts.built = 0;
    const result = await block.imported();
    if (result._tag === "Left") throw result.left;
    expect(result.right.root).toBe(block.payload.block_body.header.utxosRoot);
    expect(result.right.scriptChecks.checks).toHaveLength(parent.length);
    expect(contexts.built).toBe(0);
  });
});
