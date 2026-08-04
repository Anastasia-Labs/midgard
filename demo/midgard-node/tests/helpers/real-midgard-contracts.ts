import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "@/services/midgard-contracts.js";

export type TestOneShotOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

export const loadRealMidgardContractsForTest = (
  oneShotOutRef: TestOneShotOutRef,
  referenceScriptAuth?: SDK.MintingValidator,
): Promise<SDK.MidgardValidators> =>
  Effect.runPromise(
    Effect.gen(function* () {
      const placeholder = yield* AlwaysSucceedsContract;
      return yield* withRealStateQueueAndOperatorContracts(
        "Preprod",
        placeholder,
        oneShotOutRef,
        {
          referenceScriptAuth:
            referenceScriptAuth ?? placeholder.referenceScriptAuth,
        },
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
