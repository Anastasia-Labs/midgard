import "./utils.js";

import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  REFERENCE_SCRIPT_COMMAND_NAMES,
  nodeRuntimeReferenceScriptTargets,
  referenceScriptTargetsByCommand,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "@/transactions/reference-scripts.js";

const REFERENCE_SCRIPT_ADDRESS = "addr_test1reference";

describe("node-runtime reference-script registry", () => {
  it("exposes node-runtime as the primary deployment command", () => {
    expect(REFERENCE_SCRIPT_COMMAND_NAMES[0]).toEqual("node-runtime");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("node-runtime");
  });

  it("contains the static scripts currently used by node runtime flows", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const names = targets.map(({ name }) => name);

    expect(new Set(names).size).toEqual(names.length);
    expect(names).toContain("hub-oracle minting");
    expect(names).toContain("scheduler spending");
    expect(names).toContain("scheduler minting");
    expect(names).toContain("state-queue spending");
    expect(names).toContain("state-queue minting");
    expect(names).toContain("registered-operators spending");
    expect(names).toContain("registered-operators minting");
    expect(names).toContain("active-operators spending");
    expect(names).toContain("active-operators minting");
    expect(names).toContain("retired-operators spending");
    expect(names).toContain("retired-operators minting");
    expect(names).toContain("fraud-proof-catalogue minting");
    expect(names).toContain("deposit minting");
    expect(names).toContain("settlement minting");
  });

  it("derives protocol-init as a strict subset of node-runtime", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const byCommand = referenceScriptTargetsByCommand(contracts);
    const runtimeNames = new Set(
      byCommand["node-runtime"].map(({ name }) => name),
    );

    for (const initTarget of byCommand["protocol-init"]) {
      expect(runtimeNames.has(initTarget.name)).toEqual(true);
    }
  });

  it("accepts a complete published node-runtime reference-script set", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const lucidWithReferences = {
      utxosAt: async () =>
        targets.map(
          (target, index): UTxO => ({
            txHash: index.toString(16).padStart(64, "0"),
            outputIndex: index,
            address: REFERENCE_SCRIPT_ADDRESS,
            assets: { lovelace: 4_000_000n },
            scriptRef: target.script,
          }),
        ),
    } as unknown as LucidEvolution;

    const resolved = await Effect.runPromise(
      verifyNodeRuntimeReferenceScriptsProgram(
        lucidWithReferences,
        REFERENCE_SCRIPT_ADDRESS,
        contracts,
      ),
    );

    expect(resolved.map(({ name }) => name)).toEqual(
      targets.map(({ name }) => name),
    );
  });

  it("fails startup verification with a complete missing-reference diagnostic", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return yield* AlwaysSucceedsContract;
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const emptyLucid = {
      utxosAt: async () => [] as UTxO[],
    } as unknown as LucidEvolution;

    await expect(
      Effect.runPromise(
        verifyNodeRuntimeReferenceScriptsProgram(
          emptyLucid,
          REFERENCE_SCRIPT_ADDRESS,
          contracts,
        ),
      ),
    ).rejects.toMatchObject({
      message: "Missing node-runtime reference scripts",
    });
  });
});
