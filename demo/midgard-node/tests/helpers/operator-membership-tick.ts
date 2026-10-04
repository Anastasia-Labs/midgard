import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";
import { vi } from "vitest";

import * as Journal from "../../src/database/eventHistoryJournal.js";
import {
  type OperatorMembershipState,
  operatorMembershipTick,
  publishOperatorMembership,
} from "../../src/fibers/operator-membership.js";
import * as HistorySource from "../../src/l1-event-history-source.js";
import type {
  AcquiredLedgerSnapshot,
  LedgerSnapshotOutput,
} from "../../src/l1-ledger-snapshot.js";
import type { EventHistoryOwner } from "../../src/services/event-history-owner.js";
import { Globals } from "../../src/services/globals.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../../src/services/index.js";
import { HaltSource } from "../../src/services/liveness-halt.js";
import { provideDatabaseLayers } from "../utils.js";
import { makeFinalizedDeploymentManifestFixture } from "./finalized-deployment-manifest.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

/** Directory fixtures and the real membership tick behind a seam, for
 * `operator-membership.test.ts`. */
export const ownKey = "ab".repeat(28);
export const otherKey = "cd".repeat(28);
export const contracts = {
  activeOperators: {
    spendingScriptAddress: "active",
    policyId: "01".repeat(28),
  },
  registeredOperators: {
    spendingScriptAddress: "registered",
    policyId: "02".repeat(28),
  },
  retiredOperators: {
    spendingScriptAddress: "retired",
    policyId: "03".repeat(28),
  },
} as Pick<
  SDK.OperatorDirectoryValidators,
  "activeOperators" | "registeredOperators" | "retiredOperators"
>;
const root = (
  name: keyof typeof contracts,
  asset: string,
  next: string | null,
): LedgerSnapshotOutput => ({
  address: contracts[name].spendingScriptAddress,
  txHash: "11".repeat(32),
  outputIndex: Object.keys(contracts).indexOf(name),
  assets: { lovelace: 2_000_000n, [contracts[name].policyId + asset]: 1n },
  hasReferenceScript: false,
  datum: SDK.encodeLinkedListNodeView({
    key: "Empty",
    next: next === null ? "Empty" : { Key: { key: next } },
    data: 0n,
  }),
});
export const roots = (active: string | null = null): LedgerSnapshotOutput[] => [
  root("activeOperators", SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME, active),
  root("registeredOperators", SDK.REGISTERED_OPERATORS_ROOT_ASSET_NAME, null),
  root("retiredOperators", SDK.RETIRED_OPERATORS_ROOT_ASSET_NAME, null),
];
export const activeNode = (
  key: string,
  next: string | null = null,
): LedgerSnapshotOutput => ({
  address: "active",
  txHash: "22".repeat(32),
  outputIndex: 0,
  assets: {
    lovelace: 2_000_000n,
    [contracts.activeOperators.policyId +
    SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX +
    key]: 1n,
  },
  hasReferenceScript: false,
  datum: SDK.encodeLinkedListNodeView({
    key: { Key: { key } },
    next: next === null ? "Empty" : { Key: { key: next } },
    data: Data.castTo(
      { bond_unlock_time: null, inactivity_strikes: 0n },
      SDK.ActiveOperatorDatum,
    ),
  }),
});
export const snapshot = (
  outputs: LedgerSnapshotOutput[],
): AcquiredLedgerSnapshot => ({
  point: { id: "33".repeat(32), slot: 123 },
  addresses: ["active", "registered", "retired"],
  outputs,
});

/** Runs the real tick against `directory` at a fixed authenticated head, with
 * an indexer that also offers an unrelated post-head hub-policy donation.
 * `finalizing` retains past activity and puts the absence point at the
 * finalized depth, so the tick goes on to the finalized-point scan.
 * `agedActivity` does the same, but the activity record has aged below the
 * retained anchor, so only the finalized point is still retained. */
export const runMembershipTick = async (
  directory: LedgerSnapshotOutput[],
  {
    prior,
    finalizing: retainedActivity = false,
    agedActivity = false,
  }: {
    prior?: OperatorMembershipState;
    finalizing?: boolean;
    agedActivity?: boolean;
  } = {},
) => {
  const finalizing = retainedActivity || agedActivity;
  const manifest = await makeFinalizedDeploymentManifestFixture();
  const deployed = await loadRealMidgardContractsForTest({
    txHash: "ab".repeat(32),
    outputIndex: 0,
  });
  const identity = ContractDeploymentIdentity.make({
    kind: "manifest",
    manifestId: manifest.manifestId,
    manifest,
    consensusProfile: manifest.consensusProfile,
  });
  const genesisPin = HistorySource.eventHistoryGenesisLosslessSha256({
    era: "shelley",
    startTime: "2026-01-01T00:00:00Z",
    slotLength: { milliseconds: 1000 },
    maxLovelaceSupply: 45_000_000_000_000_000n,
  });
  const binding = await Effect.runPromise(
    HistorySource.makeEventHistorySourceBinding({
      contracts: deployed,
      identity,
      network: "Preprod",
      expectedGenesisLosslessSha256: genesisPin,
    }),
  );
  const head = { id: "38".repeat(32), slot: 123, height: 100 };
  const finalizedPoint = {
    id: "3b".repeat(32),
    slot: 23,
    height: head.height - manifest.l1Finality.confirmationDepth,
  };
  const checkpoint = {
    anchor: finalizing ? finalizedPoint : head,
    head,
  } as Journal.Checkpoint;
  let producing = false;
  const scansInsideProducer: boolean[] = [];
  const directoryOutputs = directory.map((output) => {
    const name = (Object.keys(contracts) as (keyof typeof contracts)[]).find(
      (name) => contracts[name].spendingScriptAddress === output.address,
    )!;
    const asset = Object.keys(output.assets)
      .find((unit) => unit !== "lovelace")!
      .slice(56);
    return {
      ...output,
      address: deployed[name].spendingScriptAddress,
      assets: {
        lovelace: 2_000_000n,
        [deployed[name].policyId + asset]: 1n,
      },
    };
  });
  const hub: LedgerSnapshotOutput = {
    txHash: "39".repeat(32),
    outputIndex: 0,
    address: binding.hubAddress,
    assets: { lovelace: 2_000_000n, [binding.hubUnit]: 1n },
    datum: binding.hubDatumCbor,
    hasReferenceScript: false,
  };
  // A lagging/incorrect indexer supplies an unrelated hub-policy NFT at the
  // hub address after the pinned head. SDK policy authentication retains it;
  // the tick's protocol-asset filter must exclude it before exact acquisition.
  const donation: LedgerSnapshotOutput = {
    ...hub,
    txHash: "3a".repeat(32),
    assets: {
      lovelace: 2_000_000n,
      [deployed.hubOracle.policyId + "ff"]: 1n,
    },
  };
  const outputs = [hub, ...directoryOutputs];
  const indexerQuery = vi.fn(
    async (address: string) =>
      [...outputs, donation].filter(
        (output) => output.address === address,
      ) as UTxO[],
  );
  const acquired = vi
    .spyOn(HistorySource, "readBoundRecoveryLedgerSnapshot")
    .mockImplementation(async (options) => {
      if (options.outputReferences === undefined)
        scansInsideProducer.push(producing);
      if (
        options.outputReferences?.some(
          ({ txHash }) => txHash === donation.txHash,
        )
      )
        throw new Error("Post-head donation is absent from acquired state");
      return {
        bindingDigest: binding.digest,
        ledger: {
          point: head,
          addresses: [
            binding.hubAddress,
            ...Object.values(deployed).flatMap((value) =>
              typeof value === "object" &&
              value !== null &&
              "spendingScriptAddress" in value
                ? [value.spendingScriptAddress as string]
                : [],
            ),
          ],
          outputs,
        },
      };
    });
  const history = vi
    .spyOn(Journal, "loadCurrent")
    .mockReturnValue(Effect.succeed(checkpoint));
  const retained = vi
    .spyOn(Journal, "retains")
    .mockImplementation((_binding, _checkpoint, target) =>
      Effect.succeed(
        retainedActivity || (agedActivity && target.id === finalizedPoint.id),
      ),
    );
  try {
    const state = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const config = yield* NodeConfig;
          const globals = yield* Globals;
          yield* sql`DELETE FROM operator_membership_observations WHERE manifest_id = ${Buffer.from(manifest.manifestId, "hex")} AND operator_key = ${Buffer.from(ownKey, "hex")}`;
          // The existing owner transport tests exercise the generation authority;
          // this seam isolates candidate discovery through the real tick.
          const owner = {
            runProducer: (
              produce: Parameters<EventHistoryOwner["runProducer"]>[0],
            ) =>
              Effect.acquireUseRelease(
                Effect.sync(() => (producing = true)),
                () => produce({} as never, Effect.void, {} as never),
                () => Effect.sync(() => (producing = false)),
              ),
          } as EventHistoryOwner;
          yield* Ref.set(globals.EVENT_HISTORY_OWNER, owner);
          if (prior !== undefined) yield* publishOperatorMembership(prior);
          if (finalizing) {
            yield* sql`INSERT INTO operator_membership_observations VALUES (${Buffer.from(manifest.manifestId, "hex")}, ${Buffer.from(ownKey, "hex")}, ${Buffer.from("3c".repeat(32), "hex")}, 1, 1)`;
            yield* Ref.set(
              globals.OPERATOR_MEMBERSHIP_MISSING_HEIGHT,
              finalizedPoint.height,
            );
          }
          yield* operatorMembershipTick.pipe(
            Effect.provideService(NodeConfig, {
              ...config,
              NETWORK: "Preprod",
              L1_HISTORY_GENESIS_LOSSLESS_SHA256: genesisPin,
              L1_OGMIOS_KEY: "http://ogmios.invalid:1337",
              L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
            }),
            Effect.provideService(
              MidgardContracts,
              MidgardContracts.make({
                ...deployed,
                consensusProfile: manifest.consensusProfile,
              }),
            ),
            Effect.provideService(ContractDeploymentIdentity, identity),
            Effect.provideService(Lucid, {
              api: { utxosAtWithPolicy: indexerQuery },
              operatorMainAddress: credentialToAddress("Preprod", {
                type: "Key",
                hash: ownKey,
              }),
            } as unknown as Lucid),
          );
          const state = yield* Ref.get(globals.OPERATOR_MEMBERSHIP);
          const halt = (yield* Ref.get(globals.LIVENESS_REASONS)).get(
            HaltSource.operatorMembership,
          );
          yield* sql`DELETE FROM operator_membership_observations WHERE manifest_id = ${Buffer.from(manifest.manifestId, "hex")} AND operator_key = ${Buffer.from(ownKey, "hex")}`;
          return { state, halt };
        }).pipe(Effect.provide(Globals.Default)),
      ),
    );
    return {
      ...state,
      head,
      genesisPin,
      outputs,
      donation,
      acquired: acquired.mock.calls.map(([options]) => options),
      indexerCalls: indexerQuery.mock.calls.length,
      scansInsideProducer,
    };
  } finally {
    acquired.mockRestore();
    history.mockRestore();
    retained.mockRestore();
  }
};
