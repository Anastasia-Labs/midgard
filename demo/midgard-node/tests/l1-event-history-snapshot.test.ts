import * as SDK from "@al-ft/midgard-sdk";
import { Data, datumToHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { decodeEventHistoryLedgerSnapshot } from "../src/l1-event-history-snapshot.js";
import type {
  AcquiredLedgerSnapshot,
  LedgerSnapshotOutput,
} from "../src/l1-ledger-snapshot.js";

const owner = "aa".repeat(28);
const auth = { PublicKeyCredential: [owner] as [string] };
const address = { paymentCredential: auth, stakeCredential: null };
const id = { transactionId: "bb".repeat(32), outputIndex: 0n };
const key = datumToHash(Data.to(id, SDK.OutputReference));
const token = "cc".repeat(28) + "abcd";
const deployments = {
  deposit: {
    policyId: "11".repeat(28),
    address: "deposit",
    retentionAddress: "deposit-data",
    inlineLimitBytes: 512n,
  },
  withdrawal: {
    policyId: "22".repeat(28),
    address: "withdrawal",
    retentionAddress: "withdrawal-data",
    inlineLimitBytes: 512n,
  },
};

const fixture = (kind: "deposit" | "withdrawal", external: boolean) => {
  const deployment = deployments[kind];
  const arbitraryData = external ? "ab".repeat(600) : "ab";
  const payload: SDK.EventHistoryPayload =
    kind === "deposit"
      ? {
          DepositPayload: {
            event: {
              id,
              info: {
                l2_address: address,
                l2_network_id: 0n,
                l2_datum: arbitraryData,
              },
            },
          },
        }
      : {
          WithdrawalPayload: {
            event: {
              id,
              info: {
                body: {
                  l2_outref: id,
                  l2_owner: owner,
                  l2_value: new Map([["", new Map([["", 9_000_000n]])]]),
                  l1_address: address,
                  l1_datum: "NoDatum",
                },
                signature: ["44".repeat(32), "55".repeat(64)],
                validity: "WithdrawalIsValid",
              },
            },
            refund_address: address,
            refund_datum: { InlineDatum: { data: arbitraryData } },
          },
        };
  const plan = SDK.prepareEventHistoryPayload(payload, auth, {
    inlineLimitBytes: 512n,
    maxPayloadBytes: 15000n,
    maxPayloadNodes: 1024n,
  });
  expect(plan.kind).toBe(external ? "External" : "Inline");
  const txHash = (kind === "deposit" ? "33" : "44").repeat(32);
  const structural = {
    address: deployment.address,
    hasReferenceScript: false,
    txHash,
  };
  const outputs: LedgerSnapshotOutput[] = [
    {
      ...structural,
      outputIndex: 0,
      assets: { lovelace: 3_000_000n, [deployment.policyId]: 1n },
      datum: Data.to(
        {
          position: "Root",
          next: key,
          protected_until: 0n,
          payload: "RootContent",
        },
        SDK.EventHistoryNode,
      ),
    },
    {
      ...structural,
      outputIndex: 1,
      assets: {
        lovelace: 7_000_000n,
        [token]: 9007199254740993n,
        [deployment.policyId + key]: 1n,
      },
      datum: Data.to(
        {
          position: { Key: [key] },
          next: null,
          protected_until: 0n,
          payload: {
            Order: {
              facts: {
                event_id: id,
                inclusion_time: 1000n,
                location: plan.location,
                structural_lovelace: 2_000_000n,
                structural_refund_key: owner,
              },
            },
          },
        },
        SDK.EventHistoryNode,
      ),
    },
  ];
  if (plan.kind === "External")
    outputs.push({
      txHash,
      outputIndex: 2,
      address: deployment.retentionAddress,
      hasReferenceScript: false,
      assets: { lovelace: 3_000_000n },
      datum: plan.datumCbor,
    });
  return outputs;
};

const ledger = (
  depositExternal = false,
  withdrawalExternal = false,
): AcquiredLedgerSnapshot => ({
  point: { slot: 100, id: "99".repeat(32) },
  addresses: Object.values(deployments).flatMap((deployment) => [
    deployment.address,
    deployment.retentionAddress,
  ]),
  outputs: [
    ...fixture("deposit", depositExternal),
    ...fixture("withdrawal", withdrawalExternal),
  ],
});
const decode = (snapshot: AcquiredLedgerSnapshot) =>
  Effect.runPromise(decodeEventHistoryLedgerSnapshot(snapshot, deployments));

describe("node acquired history adapter", () => {
  it.each([
    [false, false],
    [false, true],
    [true, false],
    [true, true],
  ])(
    "opens both complete lists with deposit external=%s, withdrawal external=%s",
    async (depositExternal, withdrawalExternal) => {
      const snapshot = ledger(depositExternal, withdrawalExternal);
      const result = await decode(snapshot);
      expect(result.ledger).toBe(snapshot);
      expect(result.deposits).toHaveLength(1);
      expect(result.withdrawals).toHaveLength(1);
      expect(result.deposits[0]!.event.id).toEqual(id);
      expect(result.withdrawals[0]!.event.id).toEqual(id);
      expect(result.deposits[0]!.originalAssets).toEqual({
        lovelace: 5_000_000n,
        [token]: 9007199254740993n,
      });
      expect(result.deposits[0]!.inclusionTime.getTime()).toBe(1000);
      expect(result.withdrawals[0]!.inclusionTime.getTime()).toBe(1000);
    },
  );

  it.each(["deposit", "withdrawal"] as const)(
    "refuses a missing %s root and publishes neither list",
    async (kind) => {
      const snapshot = ledger();
      await expect(
        decode({
          ...snapshot,
          outputs: snapshot.outputs.filter(
            (output) =>
              output.address !== deployments[kind].address ||
              output.outputIndex !== 0,
          ),
        }),
      ).rejects.toThrow();
    },
  );

  it.each(["deposit", "withdrawal"] as const)(
    "does not fetch missing %s retained data from another snapshot",
    async (kind) => {
      const snapshot = ledger(true, true);
      await expect(
        decode({
          ...snapshot,
          outputs: snapshot.outputs.filter(
            (output) => output.address !== deployments[kind].retentionAddress,
          ),
        }),
      ).rejects.toThrow();
    },
  );

  it("requires actual coverage even when an address contains no retained outputs", async () => {
    const snapshot = ledger();
    await expect(
      decode({
        ...snapshot,
        addresses: snapshot.addresses.filter(
          (address) => address !== deployments.withdrawal.retentionAddress,
        ),
      }),
    ).rejects.toThrow(/Invalid acquired history snapshot/);
  });

  it("ignores script-bearing donations but refuses an authenticated script-bearing node", async () => {
    const snapshot = ledger();
    const donation = {
      ...snapshot.outputs[0]!,
      outputIndex: 99,
      assets: { lovelace: 1_000_000n },
      hasReferenceScript: true,
    };
    expect(
      (await decode({ ...snapshot, outputs: [...snapshot.outputs, donation] }))
        .deposits,
    ).toHaveLength(1);
    await expect(
      decode({
        ...snapshot,
        outputs: snapshot.outputs.map((output, index) =>
          index === 0 ? { ...output, hasReferenceScript: true } : output,
        ),
      }),
    ).rejects.toThrow(/Invalid acquired history snapshot/);
  });
});
