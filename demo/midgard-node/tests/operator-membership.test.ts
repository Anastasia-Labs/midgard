import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Deferred, Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  classifyOperatorMembership,
  decodeOperatorMembership,
  OPERATOR_MEMBERSHIP_HALT_REASONS,
  operatorActivityIsRetained,
  publishOperatorMembership,
  relevantMembershipAsset,
} from "../src/fibers/operator-membership.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { Globals } from "../src/services/globals.js";
import { HaltSource } from "../src/services/liveness-halt.js";
import {
  activeNode,
  contracts,
  otherKey,
  ownKey,
  roots,
  runMembershipTick,
  snapshot,
} from "./helpers/operator-membership-tick.js";
import { provideDatabaseLayers } from "./utils.js";

describe("authenticated operator membership", () => {
  it("excludes unrelated donations before exact-reference acquisition", () => {
    expect(
      relevantMembershipAsset(
        "",
        SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
      ),
    ).toBe(false);
    expect(
      relevantMembershipAsset(
        "aa".repeat(28),
        SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
      ),
    ).toBe(false);
    expect(
      relevantMembershipAsset(
        SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
      ),
    ).toBe(true);
    expect(
      relevantMembershipAsset(
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + ownKey,
        SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
      ),
    ).toBe(true);
  });
  it("filters a post-head donation in the real membership tick before acquired-state lookup", async () => {
    const tick = await runMembershipTick([
      ...roots(ownKey),
      activeNode(ownKey),
    ]);
    expect(tick.acquired).toHaveLength(1);
    expect(tick.acquired[0]!.at).toEqual(tick.head);
    expect(tick.acquired[0]!.binding.genesisSha256).toBe(tick.genesisPin);
    expect(tick.acquired[0]!.outputReferences).toEqual(
      expect.arrayContaining(
        tick.outputs.map(({ txHash, outputIndex }) => ({
          txHash,
          outputIndex,
        })),
      ),
    );
    expect(tick.acquired[0]!.outputReferences).not.toContainEqual({
      txHash: tick.donation.txHash,
      outputIndex: tick.donation.outputIndex,
    });
    expect(tick.state).toBe("active");
    expect(tick.indexerCalls).toBe(4);
  });
  it("keeps a pending removal once its retained proof of past activity ages out", async () => {
    // No retained activity record, and the head shows the key absent: on its
    // own that is unknown, but it must not lift an authenticated removal.
    const tick = await runMembershipTick(roots(), {
      prior: "removal_pending",
    });
    expect(tick.state).toBe("removal_pending");
    expect(tick.halt).toBe(OPERATOR_MEMBERSHIP_HALT_REASONS.removal_pending);
  });
  it("keeps confirming a pending removal after its proof of past activity ages below the anchor", async () => {
    // The activity that raised the pending removal is now older than the
    // retained anchor: final, not orphaned. The pending state must still reach
    // the finalized-point scan, so a removed operator shuts down.
    const tick = await runMembershipTick(roots(), {
      prior: "removal_pending",
      agedActivity: true,
    });
    expect(tick.acquired.at(-1)!.at).toEqual({
      id: "3b".repeat(32),
      slot: 23,
    });
    expect(tick.state).toBe("removed");
    expect(tick.halt).toBe(OPERATOR_MEMBERSHIP_HALT_REASONS.removed);
  });
  it("confirms removal with the finalized-point scan outside the history producer section", async () => {
    const tick = await runMembershipTick(roots(), { finalizing: true });
    // History recovery drains producers, so a long scan inside one would
    // stall it for the scan's whole deadline.
    expect(tick.scansInsideProducer).toEqual([false]);
    expect(tick.acquired.at(-1)!.at).toEqual({
      id: "3b".repeat(32),
      slot: 23,
    });
    expect(tick.state).toBe("removed");
    expect(tick.halt).toBe(OPERATOR_MEMBERSHIP_HALT_REASONS.removed);
  });
  it("distinguishes own activity from complete authenticated absence", async () => {
    expect(
      await Effect.runPromise(
        decodeOperatorMembership(
          snapshot([...roots(ownKey), activeNode(ownKey)]),
          contracts,
          ownKey,
        ),
      ),
    ).toBe("active");
    expect(
      await Effect.runPromise(
        decodeOperatorMembership(
          snapshot([...roots(otherKey), activeNode(otherKey)]),
          contracts,
          ownKey,
        ),
      ),
    ).toBe("absent");
  });
  it.each([
    ["missing root", roots().slice(1)],
    ["missing linked successor", roots(ownKey)],
    ["cyclic list", [...roots(ownKey), activeNode(ownKey, ownKey)]],
    ["disconnected node", [...roots(), activeNode(ownKey)]],
    ["duplicated root", [...roots(), roots()[0]!]],
    [
      "bad NFT quantity",
      [
        {
          ...roots()[0]!,
          assets: {
            [contracts.activeOperators.policyId +
            SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME]: 2n,
          },
        },
        ...roots().slice(1),
      ],
    ],
  ])(
    "rejects %s rather than treating it as removal",
    async (_name, outputs) => {
      await expect(
        Effect.runPromise(
          decodeOperatorMembership(
            snapshot(outputs as LedgerSnapshotOutput[]),
            contracts,
            ownKey,
          ),
        ),
      ).rejects.toThrow();
    },
  );
  it("waits for first activation and requires prior activity for an absent key", () => {
    expect(classifyOperatorMembership("registered", false)).toBe(
      "awaiting_activation",
    );
    expect(classifyOperatorMembership("absent", false)).toBe("unknown");
    expect(classifyOperatorMembership("absent", true)).toBe("removed");
    expect(classifyOperatorMembership("retired", false)).toBe("removed");
    expect(classifyOperatorMembership("active", true)).toBe("active");
  });
  it("does not turn an orphan observation older than the retained anchor into removal", async () => {
    // The real ancestry reader rejects the aged point before querying SQL.
    const checkpoint = {
      anchor: { id: "44".repeat(32), slot: 200, height: 100 },
      head: { id: "55".repeat(32), slot: 300, height: 150 },
    } as Parameters<typeof operatorActivityIsRetained>[2];
    const binding = { digest: "66".repeat(32) } as Parameters<
      typeof operatorActivityIsRetained
    >[1];
    const prior = await Effect.runPromise(
      provideDatabaseLayers(
        operatorActivityIsRetained(
          {
            active_block_hash: Buffer.from("77".repeat(32), "hex"),
            active_block_slot: "100",
          },
          binding,
          checkpoint,
        ),
      ),
    );
    expect(prior).toBe(false);
    expect(classifyOperatorMembership("absent", prior)).toBe("unknown");
  });
  it("publishes sticky unready state before signaling shutdown", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* publishOperatorMembership("removed");
        expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe("removed");
        expect(yield* Deferred.isDone(globals.OPERATOR_REMOVAL_SHUTDOWN)).toBe(
          true,
        );
        yield* publishOperatorMembership("active");
        expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe("removed");
        expect(
          (yield* Ref.get(globals.LIVENESS_REASONS)).get(
            HaltSource.operatorMembership,
          ),
        ).toBe(OPERATOR_MEMBERSHIP_HALT_REASONS.removed);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
  it("keeps prior activity across process globals and scopes it by deployment and key", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const manifest = Buffer.from("a4".repeat(32), "hex");
          const key = Buffer.from(ownKey, "hex");
          yield* sql`DELETE FROM operator_membership_observations WHERE manifest_id = ${manifest}`;
          yield* sql`INSERT INTO operator_membership_observations VALUES (${manifest}, ${key}, ${Buffer.from("a5".repeat(32), "hex")}, 123, 45)`;
          yield* Effect.gen(function* () {
            const globals = yield* Globals;
            expect(yield* Ref.get(globals.OPERATOR_MEMBERSHIP)).toBe("unknown");
            const rows =
              yield* sql`SELECT active_block_height FROM operator_membership_observations WHERE manifest_id = ${manifest} AND operator_key = ${key}`;
            expect(rows).toHaveLength(1);
            const anotherDeployment =
              yield* sql`SELECT 1 FROM operator_membership_observations WHERE manifest_id = ${Buffer.from("a6".repeat(32), "hex")} AND operator_key = ${key}`;
            const anotherKey =
              yield* sql`SELECT 1 FROM operator_membership_observations WHERE manifest_id = ${manifest} AND operator_key = ${Buffer.from(otherKey, "hex")}`;
            expect(anotherDeployment).toHaveLength(0);
            expect(anotherKey).toHaveLength(0);
          }).pipe(Effect.provide(Globals.Default));
          yield* sql`DELETE FROM operator_membership_observations WHERE manifest_id = ${manifest}`;
        }),
      ),
    );
  });
});
