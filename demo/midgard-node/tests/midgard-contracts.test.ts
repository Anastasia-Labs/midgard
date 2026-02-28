import { describe, expect } from "vitest";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { mintingPolicyToId } from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  REAL_HUB_ORACLE_SCRIPT_TITLES,
  REAL_STATE_QUEUE_SCRIPT_TITLES,
  withRealStateQueueContracts,
} from "@/services/midgard-contracts.js";

describe("midgard contracts registry", () => {
  const oneShotOutRef = {
    txHash: "00".repeat(32),
    outputIndex: 0,
  } as const;

  it.effect("resolves real state_queue and hub_oracle scripts", () =>
    Effect.gen(function* () {
      const placeholderContracts = yield* AlwaysSucceedsContract;
      const resolved = yield* withRealStateQueueContracts(
        "Preprod",
        placeholderContracts,
        { ...oneShotOutRef },
      );

      expect(REAL_HUB_ORACLE_SCRIPT_TITLES.mint).toBe("hub_oracle.mint.mint");
      expect(REAL_STATE_QUEUE_SCRIPT_TITLES.spend).toBe(
        "state_queue.spend.spend",
      );
      expect(REAL_STATE_QUEUE_SCRIPT_TITLES.mint).toBe("state_queue.mint.mint");

      expect(resolved.hubOracle.mintingScriptCBOR).not.toEqual(
        placeholderContracts.hubOracle.mintingScriptCBOR,
      );
      expect(resolved.hubOracle.policyId).toEqual(
        mintingPolicyToId(resolved.hubOracle.mintingScript),
      );

      expect(resolved.stateQueue.spendingScriptCBOR).not.toEqual(
        placeholderContracts.stateQueue.spendingScriptCBOR,
      );
      expect(resolved.stateQueue.mintingScriptCBOR).not.toEqual(
        placeholderContracts.stateQueue.mintingScriptCBOR,
      );
      expect(resolved.stateQueue.policyId).toEqual(
        mintingPolicyToId(resolved.stateQueue.mintingScript),
      );

      expect(resolved.deposit.policyId).toEqual(
        placeholderContracts.deposit.policyId,
      );
      expect(resolved.scheduler.policyId).toEqual(
        placeholderContracts.scheduler.policyId,
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

  it.effect("rejects invalid one-shot hub-oracle outref configuration", () =>
    Effect.gen(function* () {
      const placeholderContracts = yield* AlwaysSucceedsContract;
      const result = yield* Effect.either(
        withRealStateQueueContracts("Preprod", placeholderContracts, {
          txHash: "zz",
          outputIndex: -1,
        }),
      );
      expect(result._tag).toEqual("Left");
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
});
