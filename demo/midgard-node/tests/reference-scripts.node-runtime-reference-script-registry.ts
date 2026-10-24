import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import { referenceScriptAuthTokenName } from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  nodeRuntimeReferenceScriptTargets,
  REFERENCE_SCRIPT_COMMAND_NAMES,
  referenceScriptTargetsByCommand,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "../src/transactions/reference-scripts.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  REFERENCE_SCRIPT_ADDRESS,
  txHashFixture,
} from "./reference-scripts.mk-utxo.js";

describe("node-runtime reference-script registry", () => {
  it("publishes exactly every manifest role for the real contract set", async () => {
    const contracts = await loadRealMidgardContractsForTest({
      txHash: txHashFixture("0"),
      outputIndex: 0,
    });
    const targets = nodeRuntimeReferenceScriptTargets(contracts);

    expect(targets.map(({ name }) => name).sort()).toEqual(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).sort(),
    );
    expect(
      targets.find(({ name }) => name === "V1 field-preimage certificate")
        ?.script,
    ).toEqual(contracts.fieldPreimageCertificate.spendingScript);
    expect(
      targets.find(
        ({ name }) => name === "V1 field-preimage certificate minting",
      )?.script,
    ).toEqual(contracts.fieldPreimageCertificate.mintingScript);
  });

  it("exposes node-runtime as the primary deployment command", () => {
    expect(REFERENCE_SCRIPT_COMMAND_NAMES[0]).toEqual("node-runtime");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("node-runtime");
  });

  it("contains the static scripts currently used by node runtime flows", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const names = targets.map(({ name }) => name);

    expect(new Set(names).size).toEqual(names.length);
    expect(names).toContain("reference-script-auth minting");
    expect(names).toContain("hub-oracle minting");
    expect(names).toContain("da-params-governor spending");
    expect(names).toContain("da-params-governor minting");
    expect(names).toContain("da-attestation spending");
    expect(names).toContain("da-attestation minting");
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
    expect(names).toContain("deposit spending");
    expect(names).toContain("withdrawal minting");
    expect(names).toContain("withdrawal spending");
    expect(names).toContain("settlement minting");
    expect(names).toContain("membership proof withdrawal");
    expect(names).toContain("reserve spending");
    expect(names).toContain("reserve observer");
    expect(names).toContain("payout spending");
    expect(names).toContain("payout minting");
    expect(
      names.filter((name) => name.startsWith("V1 validation-trace ")).sort(),
    ).toEqual(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE)
        .filter((name) => name.startsWith("V1 validation-trace "))
        .sort(),
    );
    const registeredFraudProofNames = names.filter((name) =>
      name.startsWith("V1 fraud-proof "),
    );
    // Node-runtime publishes EVERY `V1 fraud-proof ` role the canonical
    // manifest declares. This is stated as the set rather than as a count
    // because it is a coverage requirement, not a pin to maintain: a manifest
    // is only valid when `validateReferenceScripts` finds a confirmed
    // reference script for every declared role, so a family the registry does
    // not enumerate is a deployment that cannot pass startup verification. A
    // count let that gap sit as a stale number; the set names it.
    expect([...registeredFraudProofNames].sort()).toEqual(
      Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE)
        .filter((role) => role.startsWith("V1 fraud-proof "))
        .sort(),
    );
    expect(registeredFraudProofNames).toContain(
      "V1 fraud-proof missing-signature step-04",
    );
  });

  it("derives protocol-init as a strict subset of node-runtime", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
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

  it("exposes reserve and payout script sets as explicit deployment commands", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const byCommand = referenceScriptTargetsByCommand(contracts);

    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("reserve");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("payout");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("withdrawal");
    expect(REFERENCE_SCRIPT_COMMAND_NAMES).toContain("phas-membership");
    expect(byCommand.deposit.map(({ name }) => name)).toEqual([
      "deposit history retention",
      "deposit history retirement",
      "deposit minting",
      "deposit spending",
    ]);
    expect(byCommand.withdrawal.map(({ name }) => name)).toEqual([
      "withdrawal history retention",
      "withdrawal history retirement",
      "withdrawal minting",
      "withdrawal spending",
    ]);
    expect(byCommand.reserve.map(({ name }) => name)).toEqual([
      "reserve spending",
      "reserve observer",
    ]);
    expect(byCommand["phas-membership"].map(({ name }) => name)).toEqual([
      "membership proof withdrawal",
    ]);
    expect(byCommand.payout.map(({ name }) => name)).toEqual([
      "payout spending",
      "payout minting",
    ]);
  });

  it("accepts a complete published node-runtime reference-script set", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const authPolicy = contracts.referenceScriptAuth;
    const lucidWithReferences = {
      utxosAt: async () =>
        targets.map(
          (target, index): UTxO => ({
            txHash: index.toString(16).padStart(64, "0"),
            outputIndex: index,
            address: REFERENCE_SCRIPT_ADDRESS,
            assets: {
              lovelace: 4_000_000n,
              [toUnit(
                authPolicy.policyId,
                referenceScriptAuthTokenName(target.name),
              )]: 1n,
            },
            scriptRef: target.script,
          }),
        ),
    } as unknown as LucidEvolution;

    const resolved = await Effect.runPromise(
      verifyNodeRuntimeReferenceScriptsProgram(
        lucidWithReferences,
        REFERENCE_SCRIPT_ADDRESS,
        contracts,
        authPolicy,
      ),
    );

    expect(resolved.map(({ name }) => name)).toEqual(
      targets.map(({ name }) => name),
    );
  });

  it("fails startup verification with a complete missing-reference diagnostic", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const emptyLucid = {
      utxosAt: async () => [] as UTxO[],
    } as unknown as LucidEvolution;

    const result = await Effect.runPromise(
      Effect.either(
        verifyNodeRuntimeReferenceScriptsProgram(
          emptyLucid,
          REFERENCE_SCRIPT_ADDRESS,
          contracts,
          contracts.referenceScriptAuth,
        ),
      ),
    );

    expect(result._tag).toEqual("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toEqual(
        "Missing node-runtime reference scripts",
      );
      expect(String(result.left.cause)).toContain("reserve spending");
      expect(String(result.left.cause)).toContain("payout minting");
    }
  });

  it("rejects a complete script-ref set without auth role tokens", async () => {
    const contracts = await Effect.runPromise(
      Effect.gen(function* () {
        return withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
          txHash: "00".repeat(32),
          outputIndex: 0,
        });
      }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const lucidWithBareReferences = {
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

    const result = await Effect.runPromise(
      Effect.either(
        verifyNodeRuntimeReferenceScriptsProgram(
          lucidWithBareReferences,
          REFERENCE_SCRIPT_ADDRESS,
          contracts,
          contracts.referenceScriptAuth,
        ),
      ),
    );

    expect(result._tag).toEqual("Left");
    if (result._tag === "Left") {
      expect(result.left.message).toEqual(
        "Missing node-runtime reference scripts",
      );
      expect(String(result.left.cause)).toContain("hub-oracle minting");
    }
  });
});
