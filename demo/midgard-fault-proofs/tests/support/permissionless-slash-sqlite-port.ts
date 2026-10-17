import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  toUnit,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationStore,
} from "midgard-watcher";
import { expect } from "vitest";

import type { FraudSlashFundingAuthority } from "../../src/remove-fraudulent-block.js";
import {
  type WorkflowFundingReservationPort,
  type WorkflowFundingReservationSnapshot,
} from "../../src/workflow/funding-reservation-permit.js";
import type { bindCanonicalFixtureHeader } from "./submit-init-emulator-fixtures.deployment-context.js";

// Every operation delegates to the current connection of the same real store.
export const createPermissionlessSlashSqlitePort = async ({
  readStore,
  plan,
  callerLucid,
  contracts,
}: {
  readonly readStore: () => WatcherProverFundingReservationStore;
  readonly plan: WatcherProverFundingReservationPlan;
  readonly callerLucid: LucidEvolution;
  readonly contracts: ReadonlyMap<string, unknown>;
}) => {
  const { parseWatcherProverFundingReservationRecord } = await import(
    "midgard-watcher"
  );
  const { walletAddress, fundingPaymentKeyHash } = plan;
  const record = async () => {
    const rows = await readStore().readAll();
    expect(rows).toHaveLength(1);
    return parseWatcherProverFundingReservationRecord(rows[0]);
  };
  const snapshot = (
    row: Awaited<ReturnType<typeof record>>,
  ): WorkflowFundingReservationSnapshot => ({
    reservationId: row.reservationId,
    deploymentFingerprint: row.deploymentFingerprint,
    decisionDigest: row.decisionDigest,
    policyDigest: row.policyDigest,
    reservationBasisDigest: row.reservationBasisDigest,
    revision: row.revision,
    rollbackGeneration: "0",
    walletAddress,
    fundingPaymentKeyHash,
    state: row.state,
    activeInputs: row.activeInputs,
  });
  let resolveCalls = 0;
  const transitionDigest = async () => {
    const row = await record();
    const digest =
      row.pendingTransition?.transitionDigest ??
      row.lastConfirmedTransitionDigest;
    if (digest === null)
      throw new Error("Actual durable transition digest was lost");
    return digest;
  };
  const port: WorkflowFundingReservationPort = {
    load: async () => snapshot(await record()),
    readPendingTransition: () => readStore().readPendingTransition(plan),
    readPendingHandoff: () => readStore().readPendingHandoff(plan),
    readCompletionHandoff: () => readStore().readCompletionHandoff(plan),
    readAbandonmentHandoff: () => readStore().readAbandonmentHandoff(plan),
    resolveInputs: async (outRefs) => {
      resolveCalls += 1;
      return callerLucid.utxosByOutRef(
        outRefs.map((ref) => {
          const [txHash, index] = ref.split("#");
          return { txHash: txHash!, outputIndex: Number(index) };
        }),
      );
    },
    resolveConfirmedInput: ({ outRef }) =>
      readStore().readConfirmedInput({
        reservationId: plan.reservationId,
        outRef,
      }),
    resolveProtocolInputAuthority: async ({
      deploymentFingerprint,
      outRef,
      semanticRole,
    }) => {
      expect(deploymentFingerprint).toBe(plan.deploymentFingerprint);
      const [txHash, index] = outRef.split("#");
      const resolved = await callerLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(index) },
      ]);
      expect(resolved).toHaveLength(1);
      if (!contracts.has(resolved[0]!.address))
        throw new Error(
          "Resolved protocol input is outside the real deployed roster",
        );
      return {
        deploymentFingerprint,
        outRef,
        semanticRole,
        resolvedOutputCborHex: utxoToCore(resolved[0]!)
          .output()
          .to_canonical_cbor_hex(),
      };
    },
    prepare: async ({ expectedRevision, transition, handoff }) =>
      snapshot(
        await readStore().prepareTransition({
          plan,
          expectedRevision,
          ...transition,
          handoff,
        }),
      ),
    confirm: async (input) =>
      snapshot(
        await readStore().confirmTransition({
          plan,
          ...input,
          transitionDigest: await transitionDigest(),
        }),
      ),
    abandon: async ({ expectedRevision, handoff }) =>
      snapshot(
        await readStore().abandonPendingTransition({
          plan,
          expectedRevision,
          handoff,
          transitionDigest: await transitionDigest(),
        }),
      ),
    acknowledgeAbandonment: async (input) =>
      snapshot(await readStore().acknowledgeAbandonment({ plan, ...input })),
    markConflict: async (input) =>
      snapshot(await readStore().markConflict({ plan, ...input })),
    release: async (input) =>
      snapshot(await readStore().release({ plan, ...input })),
  };
  return { port, record, resolveCallCount: () => resolveCalls };
};

export const assertPermissionlessSlashRemovedHeader = async ({
  authority,
  manifest,
  removedHeader,
  callerLucid,
}: {
  readonly authority: FraudSlashFundingAuthority;
  readonly manifest: Awaited<
    ReturnType<typeof bindCanonicalFixtureHeader>
  >["manifest"];
  readonly removedHeader: SDK.Header;
  readonly callerLucid: LucidEvolution;
}) => {
  const [txHash, index] = authority.removedStateQueueOutRef.split("#");
  const removed = await callerLucid.utxosByOutRef([
    { txHash: txHash!, outputIndex: Number(index) },
  ]);
  expect(removed).toHaveLength(1);
  const policyId = manifest.contracts.stateQueueMint!.scriptHash;
  expect(removed[0]!.address).toBe(
    credentialToAddress(manifest.network, {
      type: "Script",
      hash: manifest.contracts.stateQueueSpend!.scriptHash,
    }),
  );
  const parsed = await Effect.runPromise(
    SDK.utxoToStateQueueUTxO(removed[0]!, policyId),
  );
  const actualHeader = await Effect.runPromise(
    SDK.getHeaderFromStateQueueDatum(parsed.datum),
  );
  expect(actualHeader).toEqual(removedHeader);
  const expectedHash = await Effect.runPromise(
    SDK.hashBlockHeader(removedHeader),
  );
  const actualHash = await Effect.runPromise(SDK.hashBlockHeader(actualHeader));
  expect(actualHash).toBe(expectedHash);
  expect(
    removed[0]!.assets[
      toUnit(policyId, SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + expectedHash)
    ],
  ).toBe(1n);
};
