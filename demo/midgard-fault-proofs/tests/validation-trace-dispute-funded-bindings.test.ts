/** The prover funding permit refuses a validation-trace dispute VerifySource
 * whose authority and outputs agree with each other but not with the
 * deployment's dispute terms: a bond above the deployment-bound bond, or the
 * bond or case paid to, and bound at, another dispute contract. The
 * workflow stalls with the refusal after Init and Open.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import type { CML } from "@lucid-evolution/lucid";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import {
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  confirmedStages,
  FUNDED,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  bondIndex,
  caseIndex,
  changeIndex,
  presentVerifySource,
  type VerifySourcePresentation,
  withAddress,
  withCoin,
} from "./support/validation-trace-dispute-tampered-verify-source.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

let scene: Scene;

const BINDING =
  "validation dispute funding authority differs from its exact VerifySource action or deployment terms";

/** Another proof-thread contract the deployment's roster governs. */
const anotherContract = () => scene.chain.source.spendingScriptAddress;

describe("validation-trace dispute VerifySource bound to other dispute terms", () => {
  // Each case disputes the scene's failing block from a fresh deployment,
  // its prover wallet split into a collateral output and plain funding.
  beforeEach(async () => {
    scene = await setUpScene();
  }, TIMEOUT);

  it.each([
    {
      name: "an authority and a bond one lovelace over the deployment-bound bond",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { bond: string; wallet: string },
        ) => {
          const bond = bondIndex(outputs, addresses.bond);
          const change = changeIndex(outputs, addresses.wallet);
          return outputs.map((output, index) =>
            index === bond
              ? withCoin(output, output.amount().coin() + 1n)
              : index === change
                ? withCoin(output, output.amount().coin() - 1n)
                : output,
          );
        },
        authority: (grant) => ({
          ...grant,
          bond: { ...grant.bond, lovelace: grant.bond.lovelace + 1n },
        }),
      }),
      refusal: `${BINDING} (bond)`,
    },
    {
      name: "an authority and a bond at another dispute contract",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { bond: string },
        ) => {
          const bond = bondIndex(outputs, addresses.bond);
          return outputs.map((output, index) =>
            index === bond ? withAddress(output, anotherContract()) : output,
          );
        },
        authority: (grant) => ({
          ...grant,
          bond: { ...grant.bond, address: anotherContract() },
        }),
      }),
      refusal: `${BINDING} (bond address)`,
    },
    {
      name: "an authority and a case at another dispute contract",
      present: (): VerifySourcePresentation => ({
        outputs: (
          outputs: CML.TransactionOutput[],
          addresses: { case: string },
        ) => {
          const at = caseIndex(outputs, addresses.case);
          return outputs.map((output, index) =>
            index === at ? withAddress(output, anotherContract()) : output,
          );
        },
        authority: (grant) => ({
          ...grant,
          caseOutput: { ...grant.caseOutput, address: anotherContract() },
        }),
      }),
      refusal: `${BINDING} (case address)`,
    },
  ])(
    "refuses VerifySource with $name",
    async ({ present, refusal }) => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      try {
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
          { wrap: presentVerifySource(scene, present()) },
        );
        expect(result.kind, history.join("\n")).toBe("stalled");
        if (result.kind !== "stalled") return;
        expect(result.reason).toContain("funding reservation failed for");
        expect(result.reason).toContain(refusal);
        expect(confirmedStages(result)).toStrictEqual(["init", "open"]);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
