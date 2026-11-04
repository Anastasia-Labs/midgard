import { Effect } from "effect";

import { Lucid } from "../../src/services/lucid.js";

/**
 * A Lucid service for tests that never reach the L1: an unexpected chain or
 * slot read dies, wallet switches do nothing, and `overrides` supplies what
 * the test does read.
 */
export const lucidServiceStub = (
  overrides: Partial<Parameters<typeof Lucid.make>[0]> = {},
): Lucid =>
  Lucid.make({
    api: {} as never,
    referenceScriptsApi: {} as never,
    operatorMainAddress: "",
    operatorMergeAddress: "",
    referenceScriptsWalletAddress: "",
    referenceScriptsAddress: "addr_test1referencescripts",
    submitSlotSnapshot: () => Effect.die("unexpected submit-slot lookup"),
    viewSlot: () => Effect.die("unexpected view-slot read"),
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
    switchToReferenceScriptWallet: Effect.void,
    ...overrides,
  });
