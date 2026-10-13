import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import {
  expectSinglePhaseAAcceptance,
  expectSinglePhaseARejection,
  queuedNativeTx,
  runPhaseA,
} from "./phase-a.outref-projection.js";
import { makeNativeTx, makeRedeemersCbor } from "./validation-fixtures.js";

const CAP_MEMORY = BigInt(
  MIDGARD_CONSENSUS_PROFILE.limits.minSupportedTransactionExecutionMemoryUnits,
);
const CAP_STEPS = BigInt(
  MIDGARD_CONSENSUS_PROFILE.limits.minSupportedTransactionExecutionCpuUnits,
);

const txDeclaring = (exUnits: readonly (readonly [bigint, bigint])[]) =>
  makeNativeTx({
    redeemerTxWitsPreimageCbor: makeRedeemersCbor(
      exUnits.map((units, index) => ({
        tag: 0,
        index: BigInt(index),
        exUnits: units,
      })),
    ),
    scriptLanguages: ["PlutusV3"],
  });

describe("phase A per-tx ex-units cap", () => {
  it("pins the cap at Cardano's maxTxExUnits", () => {
    expect([CAP_MEMORY, CAP_STEPS]).toEqual([16_500_000n, 10_000_000_000n]);
  });

  it("keeps the cap equal to the L1 per-tx limits every deployment supports", () => {
    const { limits } = MIDGARD_CONSENSUS_PROFILE;
    expect([CAP_MEMORY, CAP_STEPS]).toEqual([
      BigInt(limits.minSupportedL1MaxTxMemoryUnits),
      BigInt(limits.minSupportedL1MaxTxCpuUnits),
    ]);
    // The public deployment profile binds no ex-units parameter of its own,
    // so the consensus profile is the single source. Adding one there must
    // reconcile it with this cap.
    expect(
      JSON.stringify(DEPLOYMENT_PROFILES["preprod-public"]).toLowerCase(),
    ).not.toMatch(/ex_?units|ex_?mem|ex_?steps|execution/);
  });

  it("admits a transaction whose declared sum is exactly the cap", async () => {
    await expectSinglePhaseAAcceptance(
      txDeclaring([
        [CAP_MEMORY - 1n, CAP_STEPS - 1n],
        [1n, 1n],
      ]),
    );
  });

  it.each([
    ["memory", [[CAP_MEMORY + 1n, CAP_STEPS]]],
    ["steps", [[CAP_MEMORY, CAP_STEPS + 1n]]],
    [
      "memory summed over redeemers",
      [
        [CAP_MEMORY, 1n],
        [1n, 1n],
      ],
    ],
    [
      "steps summed over redeemers",
      [
        [1n, CAP_STEPS],
        [1n, 1n],
      ],
    ],
  ] as const)(
    "refuses a declared sum one over the cap in %s",
    async (_label, exUnits) => {
      const rejection = await expectSinglePhaseARejection(
        txDeclaring(exUnits),
        RejectCodes.TxExUnits,
      );
      expect(rejection.consensusPhase).toBe("phaseAScriptPreconditions");
      expect(rejection.subject).toBeUndefined();
    },
  );

  it("does not apply the normal-transaction cap to a forced transaction", async () => {
    const fixture = txDeclaring([[CAP_MEMORY + 1n, CAP_STEPS + 1n]]);
    const result = await runPhaseA([
      {
        ...queuedNativeTx(fixture),
        sourceKind: "forced",
        txCbor: encodeMidgardForcedTxCanonical(
          materializeMidgardForcedTxFromCanonical(fixture.tx),
        ),
      },
    ]);
    expect(result.rejected).toEqual([]);
    expect(result.accepted).toHaveLength(1);
  });
});
