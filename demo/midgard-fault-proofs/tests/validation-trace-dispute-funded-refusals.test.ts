/** The prover funding permit refuses a validation-trace dispute VerifySource
 * other than the one its builder evaluated, before it is submitted: a bond
 * above the deployment-bound bond, a second bond, the bond at another
 * contract or at a wallet outside the deployment, or the evaluated bytes
 * without the builder's authority. The workflow stalls with the refusal
 * after Init and Open.
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
  BOND_LOVELACE,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  bondIndex,
  changeIndex,
  presentVerifySource,
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
/** A wallet outside the deployment: the funder's. */
let funderAddress: string;

describe("validation-trace dispute VerifySource refused by the prover funding permit", () => {
  // Each case disputes the scene's failing block from a fresh deployment,
  // its prover wallet split into a collateral output and plain funding.
  beforeEach(async () => {
    scene = await setUpScene();
    funderAddress = await scene.h.funderLucid.wallet().address();
  }, TIMEOUT);

  it.each([
    {
      name: "a bond one lovelace over the deployment-bound bond",
      tamper: (
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
      refusal: `validation dispute bond output differs from the deployment-bound bond: actual=${(BOND_LOVELACE + 1n).toString()} bound=${BOND_LOVELACE.toString()}`,
    },
    {
      name: "a second bond output",
      tamper: (
        outputs: CML.TransactionOutput[],
        addresses: { bond: string; wallet: string },
      ) => {
        const bond = outputs[bondIndex(outputs, addresses.bond)]!;
        const change = changeIndex(outputs, addresses.wallet);
        return [
          ...outputs.map((output, index) =>
            index === change
              ? withCoin(output, output.amount().coin() - BOND_LOVELACE)
              : output,
          ),
          bond,
        ];
      },
      refusal:
        "validation dispute VerifySource must pay exactly one bond and one case output: bonds=2 cases=1",
    },
    {
      name: "the bond paid to another dispute contract",
      tamper: (
        outputs: CML.TransactionOutput[],
        addresses: { bond: string; wallet: string },
      ) => {
        const bond = bondIndex(outputs, addresses.bond);
        return outputs.map((output, index) =>
          index === bond
            ? withAddress(output, scene.chain.source.spendingScriptAddress)
            : output,
        );
      },
      refusal:
        "validation dispute VerifySource pays custody beyond its bond and case",
    },
    {
      name: "the bond paid to a wallet outside the deployment",
      tamper: (
        outputs: CML.TransactionOutput[],
        addresses: { bond: string; wallet: string },
      ) => {
        const bond = bondIndex(outputs, addresses.bond);
        return outputs.map((output, index) =>
          index === bond ? withAddress(output, funderAddress) : output,
        );
      },
      refusal: "funding output escapes the governed contract roster",
    },
    {
      name: "the evaluated bytes presented without the builder's authority",
      tamper: null,
      refusal: "funding transaction exceeds its governed custody allocation",
    },
  ])(
    "refuses VerifySource with $name",
    async ({ tamper, refusal }) => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      try {
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
          { wrap: presentVerifySource(scene, tamper) },
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
