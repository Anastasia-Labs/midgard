/**
 * Zero L1 reads in the planner fibers' state-queue reads (plan §15 N2). Each
 * fiber tick's read of the queue runs against a seeded landed queue (P1)
 * with a Lucid client whose provider records every call, and with `fetch`
 * (Kupo, Ogmios HTTP) and `WebSocket` (Ogmios) recording every use. The
 * reads must answer from P1 alone: any provider, Kupo or Ogmios touch fails
 * the test, naming the call. The source-level half of the gate is the grep
 * in the N2 report; this is its runtime half.
 *
 * Covered ticks: block commitment (the preflight snapshot, the tail, the
 * base revalidation and both append-fence reads), block confirmation, merge
 * (candidate readiness and the queue gauge), DA attestation, the retention
 * sweep, and attestation-timeout correction up to its first non-queue step.
 * The L1 slot comes from a registered tip source, as the follower's covered
 * tip supplies it in the node.
 */
import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  credentialToAddress,
  Lucid as makeLucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Provider,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { attestationTimeoutCorrectionAction } from "../src/fibers/attestation-timeout-correction.attestation-timeout-correction-action.js";
import { fetchRetentionL1View } from "../src/fibers/retention-sweeper.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import type { IntentJournal } from "../src/services/intent-journal.js";
import { landedStateQueueSnapshot } from "../src/services/landed-state-queue.js";
import { fetchUnattestedHeaders } from "../src/transactions/da-attestation.fetch-unattested-headers.js";
import {
  fetchCanonicalMergeCandidateReadiness,
  getStateQueueLength,
} from "../src/transactions/state-queue/merge-to-confirmed-state.fetch-canonical-merge-candidate-readiness.js";
import {
  fetchExpectedStateQueueTailLocal,
  fetchLatestCommittedBlockLocal,
  resolveCommitAppendFenceEndTimeCapLocal,
  resolveCommitAppendFenceReferencesLocal,
} from "../src/workers/commit-block-header/state-queue.js";
import { fetchSortedCommittedStateQueueBlocks } from "../src/workers/utils/confirm-block-commitments.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import { attachTestL1Access } from "./helpers/l1-tip.js";
import { seedLandedStateQueue } from "./helpers/landed-state-queue.js";
import {
  nodeDatum,
  rootDatum,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";
import { provideDatabaseLayers } from "./utils.js";

const GENESIS = "00".repeat(28);
const POLICY = "71".repeat(28);
const stateQueue = {
  spendingScriptAddress: credentialToAddress("Preprod", {
    type: "Script",
    hash: "5a".repeat(28),
  }),
  policyId: POLICY,
};
const fetchConfig: SDK.StateQueueFetchConfig = {
  stateQueueAddress: stateQueue.spendingScriptAddress,
  stateQueuePolicyId: POLICY,
};
const contracts = { stateQueue } as unknown as SDK.MidgardValidators;
const TIP_SLOT = 100_000_000;

/** Every L1 touch the reads made, in order. */
const touches: string[] = [];
const touch = (what: string) => {
  touches.push(what);
  return Promise.reject(new Error(`L1 read in a fiber tick: ${what}`));
};

const PROVIDER_METHODS = [
  "getProtocolParameters",
  "getUtxos",
  "getUtxosWithUnit",
  "getUtxoByUnit",
  "getUtxosByOutRef",
  "getDelegation",
  "getDatum",
  "awaitTx",
  "submitTx",
  "evaluateTx",
] as const;

/** A Lucid provider that records, and refuses, every call. */
const spyProvider = (): Provider =>
  Object.fromEntries(
    PROVIDER_METHODS.map((method) => [
      method,
      (...args: unknown[]) =>
        touch(`provider.${method}(${JSON.stringify(args).slice(0, 80)})`),
    ]),
  ) as unknown as Provider;

const utxo = (nonce: number, assetName: string, datum: Buffer): UTxO => ({
  txHash: nonce.toString(16).padStart(64, "0"),
  outputIndex: 0,
  address: stateQueue.spendingScriptAddress,
  assets: { lovelace: 5_000_000n, [toUnit(POLICY, assetName)]: 1n },
  datum: datum.toString("hex"),
});

let lucid: LucidEvolution;

beforeEach(async () => {
  touches.length = 0;
  lucid = await makeLucid(spyProvider(), "Preprod", {
    presetProtocolParameters: PROTOCOL_PARAMETERS_DEFAULT,
  });
  // The follower's covered tip stands in for the node's slot source.
  attachTestL1Access(lucid, TIP_SLOT);
  vi.spyOn(globalThis, "fetch").mockImplementation((input) =>
    touch(`fetch(${String(input instanceof Request ? input.url : input)})`),
  );
  vi.stubGlobal(
    "WebSocket",
    class {
      constructor(url: string) {
        touches.push(`WebSocket(${url})`);
        throw new Error(`L1 read in a fiber tick: WebSocket(${url})`);
      }
    },
  );
  touches.length = 0;
});

afterEach(() => {
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

/** A healthy landed queue: the root and one unattested block that just ended. */
const seedQueue = () => {
  const endTime = BigInt(lucid.slotToUnixTime(TIP_SLOT) - 1_000);
  const header = {
    ...simHeader(1, GENESIS),
    endTime,
    startTime: endTime - 1_000n,
  };
  const headerHash = SDK.stateQueueHeaderHash(header);
  return Effect.as(
    seedLandedStateQueue(
      stateQueue,
      [
        utxo(
          1,
          SDK.STATE_QUEUE_ROOT_ASSET_NAME,
          rootDatum(GENESIS, headerHash),
        ),
        utxo(
          2,
          SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
          nodeDatum(header, "Unattested", null),
        ),
      ],
      10,
    ),
    headerHash,
  );
};

const run = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Globals
    | Lucid
    | MidgardContracts
    | ContractDeploymentIdentity
    | NodeConfig
    | SqlClient.SqlClient
    | IntentJournal
  >,
) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Effect.provide(Globals, Globals.Default);
      return yield* provideDatabaseLayers(
        withoutFollowerJournal(effect).pipe(
          Effect.provideService(Globals, globals),
          Effect.provideService(Lucid, { api: lucid } as never),
          Effect.provideService(MidgardContracts, contracts as never),
          // No manifest: attestation-timeout correction stops at its first
          // step past the queue read and the L1 slot.
          Effect.provideService(ContractDeploymentIdentity, {
            manifestId: undefined,
          } as never),
        ),
      );
    }),
  );

describe("the planner fibers' state-queue reads", () => {
  it("answer every tick's queue read from P1 with no Lucid provider, Kupo or Ogmios call", async () => {
    await run(
      Effect.gen(function* () {
        const headerHash = yield* seedQueue();
        touches.length = 0;

        // Block commitment.
        const snapshot = yield* landedStateQueueSnapshot(
          stateQueue,
          "commit_preflight",
        );
        expect(snapshot.tailCommitBase.headerHash).toBe(headerHash);
        const tail = yield* fetchLatestCommittedBlockLocal(fetchConfig);
        expect(yield* fetchExpectedStateQueueTailLocal(fetchConfig, tail)).toBe(
          tail,
        );
        expect(
          yield* resolveCommitAppendFenceReferencesLocal(
            lucid,
            fetchConfig,
            tail,
          ),
        ).toHaveProperty("confirmedStateRefInput");
        expect(
          yield* resolveCommitAppendFenceEndTimeCapLocal(lucid, fetchConfig),
        ).toBeTypeOf("number");

        // Block confirmation.
        expect(
          yield* fetchSortedCommittedStateQueueBlocks(stateQueue as never),
        ).toHaveLength(2);

        // Merge.
        const readiness = yield* fetchCanonicalMergeCandidateReadiness(
          lucid,
          fetchConfig,
          contracts,
        );
        expect(readiness.status).toBe("candidate");
        expect(yield* getStateQueueLength(fetchConfig)).toBe(1);

        // DA attestation.
        const unattested = yield* fetchUnattestedHeaders(contracts);
        expect(unattested.map((target) => target.headerHash)).toEqual([
          headerHash,
        ]);

        // Retention sweep.
        const view = yield* fetchRetentionL1View;
        expect(
          view.liveQueueHeaderHashes.map((h) => h.toString("hex")),
        ).toEqual([headerHash]);

        // Attestation-timeout correction, up to the manifest check.
        const correction = yield* Effect.either(
          attestationTimeoutCorrectionAction(),
        );
        expect(Either.isLeft(correction)).toBe(true);
        if (Either.isLeft(correction))
          expect(String(correction.left)).toContain(
            "requires the exact authenticated deployment manifest",
          );

        expect(touches).toEqual([]);
      }),
    );
  });

  it("would see a provider, Kupo or Ogmios touch (the spies record)", async () => {
    await expect(
      lucid.utxosAt(stateQueue.spendingScriptAddress),
    ).rejects.toThrow("L1 read in a fiber tick");
    await expect(fetch("http://kupo.invalid/matches")).rejects.toThrow(
      "L1 read in a fiber tick",
    );
    expect(() => new WebSocket("ws://ogmios.invalid")).toThrow(
      "L1 read in a fiber tick",
    );
    expect(touches).toHaveLength(3);
    expect(touches[0]).toMatch(/^provider\.getUtxos/);
  });
});
