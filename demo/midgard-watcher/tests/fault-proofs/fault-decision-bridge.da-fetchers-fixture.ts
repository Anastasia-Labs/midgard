/**
 * The DA fetchers of the per-header alert suites: a retained-DA runtime whose
 * public DA peers fail every fetch, the watcher's replayed ledger over empty
 * blocks, and a source serving some of them.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { RetainedDaPayloadSource } from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import type { WatcherAlertCode } from "../../src/runtime/operations-observability.js";
import { createWatcherRetainedDaRuntime } from "../../src/storage/retained-da-runtime.create-watcher-retained-da-runtime-owner.js";
import { bindWatcherRetainedDaOperations } from "../../src/storage/retained-da-runtime.retained-da-request-permits.js";
import {
  admittedConfig,
  transportFactory,
} from "../storage/retained-da-runtime.fixtures.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.build-watcher-deployment-authority-fixture.js";
import type { operations } from "./fault-decision-bridge.released-fixture.js";

export const AUTHORITY = makeWatcherDeploymentAuthorityFixture();

export const active = (
  observability: ReturnType<typeof operations>,
  code: WatcherAlertCode,
) =>
  observability.api
    .status()
    .activeAlerts.filter((alert) => alert.code === code)
    .map(({ subjectDigest }) => subjectDigest);

export type Block = Awaited<ReturnType<typeof buildCanonicalBlockFixture>>;

export const emptyBlock = async (prevHeaderHash: string) =>
  await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [],
    prevHeaderHash,
  });

/** The watcher's replayed ledger, its checkpoint moving to `confirmed`. */
export const replayKeeper = (confirmed: () => Block) =>
  createWatcherReplayedLedger({
    follower: {
      epoch: () => 0,
      confirmed: async () => ({
        state: {
          ...SDK.makeGenesisConfirmedState(1000n),
          headerHash: confirmed().headerHash,
          utxoRoot: confirmed().header.utxosRoot,
        },
        current: () => true,
      }),
      materialFor: async (payload) => ({
        blockTimeMs: Number(payload.block_body.header.endTime),
        replayUserEvent: () => Effect.die("the block has no user event"),
        verifyForcedSource: () => Effect.die("the block has no forced order"),
      }),
      availabilityStatus: async () => "Unattested",
      checkpointDurable: () => undefined,
      close: () => undefined,
    },
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    availabilityAuthority: () => ({
      deploymentIdentity: "c7".repeat(28),
      responseGeometry: SDK.availabilityResponseGeometry({
        chunkByteLength: 4096,
        trancheByteLength: 64 * 1024,
        maxTrancheCount: 1024,
      }),
    }),
    checkpointPath: "unused",
    unsafeCheckpointStoreForTest: {
      read: async () => undefined,
      write: async () => undefined,
    },
  });

/** A source serving the payloads of `blocks` and no other. */
export const serving = (blocks: readonly Block[]): RetainedDaPayloadSource => ({
  sourceId: "serving",
  fetchPayloadByHeaderHash: async (headerHash) => {
    const block = blocks.find((block) => block.headerHash === headerHash);
    if (block === undefined)
      return { ok: false, sourceId: "serving", attempts: [] };
    return {
      ok: true,
      sourceId: "serving",
      sourcePeerId: "peer",
      payloadEnvelopeCbor: block.payloadEnvelopeCbor,
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "serving/peer",
        grade: "security",
      },
      attempts: [],
    };
  },
});

/** A bound diagnostics sink, and a retained-DA runtime every public DA peer
 * of which fails each fetch. */
export const failingFetches = async (
  observability: ReturnType<typeof operations>,
  mayStillFetch?: () => Iterable<string>,
) => {
  const binding = bindWatcherRetainedDaOperations({
    deploymentIdentity: AUTHORITY.result,
    sink: observability.sink,
  });
  const fake = transportFactory();
  fake.request.mockRejectedValue(new Error("public DA peer did not answer"));
  const runtime = await createWatcherRetainedDaRuntime({
    watcherConfig: admittedConfig(),
    deploymentIdentity: AUTHORITY.result,
    unsafeTransportFactoryForTest: fake.factory,
    ...(mayStillFetch === undefined ? {} : { mayStillFetch }),
  });
  return {
    binding,
    runtime,
    close: async () => {
      await runtime.close();
      binding.close();
    },
  };
};
