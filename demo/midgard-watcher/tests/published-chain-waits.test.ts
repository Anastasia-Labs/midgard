import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import {
  createPublishedWatcherBlockActor,
  type PublishedWatcherBlock,
  type PublishedWatcherDeployment,
} from "./support/published-block-actor.js";

vi.mock("midgard-node/transactions/register-active-operator", () => ({
  registerOperatorProgram: () => Effect.void,
  activateOperatorProgram: () => Effect.void,
}));

const fixture = async (schedulerStart: number) => {
  const operator = "ab".repeat(28);
  const address = credentialToAddress("Preprod", {
    type: "Key",
    hash: operator,
  });
  const activePolicy = "bc".repeat(28);
  const schedulerPolicy = "cd".repeat(28);
  const paramsPolicy = "de".repeat(28);
  const output: UTxO = {
    txHash: "ef".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 2_000_000n },
  };
  const outputs = new Map([
    [
      toUnit(
        activePolicy,
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operator,
      ),
      [output],
    ],
    [
      toUnit(schedulerPolicy, SDK.SCHEDULER_ASSET_NAME),
      [
        {
          ...output,
          datum: Data.to(
            {
              ActiveOperator: { operator, start_time: BigInt(schedulerStart) },
            },
            SDK.SchedulerDatum,
          ),
        },
      ],
    ],
    [toUnit(paramsPolicy, SDK.DA_PARAMS_ASSET_NAME), [output]],
  ]);
  const lucid = {
    wallet: () => ({ address: async () => address }),
    utxosAt: async () => [],
    overrideUTxOs: vi.fn(),
    utxosAtWithUnit: async (_address: string, unit: string) =>
      outputs.get(unit) ?? [],
  } as unknown as LucidEvolution;
  const delaySlots = vi.fn(async (_slots: number) => {});
  const awaitLedgerTime = vi.fn(async (_targetMs: number) => {});
  const readHeaderConsumptions = vi.fn(async (_unit: string) => []);
  const deployment = {
    publisherLucid: lucid,
    references: new Map(),
    chain: { now: () => 100_000, delaySlots, awaitLedgerTime },
    contracts: {
      stateQueue: { policyId: "aa".repeat(28), spendingScriptAddress: address },
      activeOperators: {
        policyId: activePolicy,
        spendingScriptAddress: address,
      },
      scheduler: { policyId: schedulerPolicy, spendingScriptAddress: address },
      daParamsGovernor: {
        policyId: paramsPolicy,
        spendingScriptAddress: address,
      },
      daAttestation: { policyId: "bb".repeat(28) },
    },
  } as unknown as PublishedWatcherDeployment;
  const actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid,
    daSignerConfig: {} as never,
    readHeaderConsumptions,
    readConfirmedTransaction: vi.fn(),
  });
  return { actor, delaySlots, awaitLedgerTime, readHeaderConsumptions };
};

it.each([50_000, 120_000])(
  "waits for the exact scheduler bound %s rather than the current wall-clock slot",
  async (schedulerStart) => {
    const f = await fixture(schedulerStart);
    await f.actor.onboardOperator();
    expect(f.awaitLedgerTime).toHaveBeenCalledExactlyOnceWith(
      schedulerStart + 1,
    );
    expect(f.delaySlots).not.toHaveBeenCalled();
  },
);

it("rechecks an indexing-lagged consumption after a polling delay without a ledger-time gate", async () => {
  const f = await fixture(50_000);
  const stop = new Error("second receipt lookup reached");
  f.readHeaderConsumptions
    .mockResolvedValueOnce([])
    .mockRejectedValueOnce(stop);
  const block = { headerHash: "cc".repeat(28) } as PublishedWatcherBlock;
  await expect(f.actor.attest(block)).rejects.toBe(stop);
  expect(f.delaySlots).toHaveBeenCalledExactlyOnceWith(1);
  expect(f.awaitLedgerTime).not.toHaveBeenCalled();
  expect(f.readHeaderConsumptions).toHaveBeenCalledTimes(2);
});
