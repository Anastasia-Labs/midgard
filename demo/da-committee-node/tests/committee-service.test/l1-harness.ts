import type { ChainSyncEvent } from "@al-ft/l1-node-transport";
import {
  type DepthParameters,
  type FactStore,
  followChain,
  type FollowStatus,
  openSqliteFactStore,
  type OutRef,
  projectionStoreOptions,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  type SimOutput,
  type SimTx,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, getAddressDetails } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { expect } from "vitest";

import { CommitteeService } from "../../src/committee-service.js";
import type { Header } from "../../src/domain.js";
import { committeeAvailabilityReads } from "../../src/l1/follower/availability-reads.js";
import { committeeTracked } from "../../src/l1/follower/committee-follower-config.js";
import { committeeL1Source } from "../../src/l1/follower/l1-follower.js";
import { committeeProjection } from "../../src/l1/follower/projection.js";
import { headerHashOf } from "../../src/l1/follower/queue-derivation.js";
import { loadDaSigner, validateDaSignerMembership } from "../../src/signer.js";
import { bytesToHex } from "../../src/utils/hex.js";
import {
  makePayloadFixture,
  minimalConfig,
  payloadSourceFromBytes,
  tempDir,
} from ".././helpers.js";
import { growingChainSync } from ".././helpers/scripted-chain-sync.js";
import {
  nodeDatum,
  queueOutput,
  rootDatum,
  SIM_QUEUE,
  SIM_SLOT_TIME,
} from ".././l1-follower/queue-sim.js";
import { failPayloadSource, openTestCommitteeStore } from "./fixtures.js";

/** cd and k of the harness chain: `minimalConfig`'s finality depth, k = 2·cd. */
export const PARAMETERS: DepthParameters = {
  confirmationDepth: 2,
  securityParameter: 4,
};
export const CD = PARAMETERS.confirmationDepth;
export const K = PARAMETERS.securityParameter;

const pause = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

export type Committed = Readonly<{ header: Header; headerHash: string }>;

const scriptAddress = (hash: string): string =>
  credentialToAddress("Preprod", { type: "Script", hash });
const addressBytes = (bech32: string): Buffer =>
  Buffer.from(getAddressDetails(bech32).address.hex, "hex");

/**
 * The real path end to end: a simulated chain served over chain-sync to the
 * follower loop, the committee projections in its SQLite store, the
 * committee's L1 source over that store and the loop's status, and a
 * signing committee member on a Postgres committee store.
 */
export const harness = async () => {
  const dir = await tempDir();
  const seed = `${"00".repeat(31)}47`;
  const signer = await loadDaSigner(`hex:${seed}`);
  const base = minimalConfig({
    manifestPath: `${dir}/manifest.json`,
    deploymentInfoPath: `${dir}/deployment.json`,
    signerSeed: seed,
    signerPublicKey: signer.publicKeyHex,
  });
  const config = {
    ...base,
    daParamsGovernorAddress: scriptAddress("b1".repeat(28)),
    daAttestationAddress: scriptAddress("b2".repeat(28)),
    correctionLockAddress: scriptAddress("b3".repeat(28)),
    daParams: {
      ...base.daParams,
      committeeSignersHash: bytesToHex(
        blake2b(Buffer.from(signer.publicKeyHex, "hex"), { dkLen: 32 }),
      ),
    },
  };

  const stores: FactStore[] = [];
  const options = projectionStoreOptions(
    [committeeProjection(SIM_QUEUE, committeeTracked(config))],
    {
      securityParameter: K,
      trackedSet: {
        addresses: new Set(),
        paymentCredentials: new Set(),
        policies: new Set(),
      },
    },
    "sqlite",
  );
  const facts = openSqliteFactStore({ ...options, path: ":memory:" });
  stores.push(facts);
  const chain = new SimChain(simUniverse(), SIM_ORIGIN, options.trackedSet);
  const events: ChainSyncEvent[] = [];
  const oneShot = chain.outsideInput();

  // Protocol init as the SDK builds it: it spends the one-shot, mints the
  // hub tokens, pays the hub oracle to the hub policy's script and the
  // correction lock to its address. The state queue's root comes next.
  const hubTokens = new Map([
    [
      config.hubOraclePolicyId,
      new Map([
        ["01", 1n],
        ["02", 1n],
      ]),
    ],
  ]);
  const correctionLock = (): SimOutput => ({
    address: addressBytes(config.correctionLockAddress),
    lovelace: 2_000_000n,
    assets: new Map([[config.hubOraclePolicyId, new Map([["02", 1n]])]]),
  });
  const init = chain.forward([
    {
      inputs: [oneShot],
      outputs: [
        {
          address: addressBytes(scriptAddress(config.hubOraclePolicyId)),
          lovelace: 2_000_000n,
          assets: new Map([[config.hubOraclePolicyId, new Map([["01", 1n]])]]),
        },
        correctionLock(),
      ],
      mint: hubTokens,
      nonce: chain.nonce(),
    },
    {
      inputs: [chain.outsideInput()],
      outputs: [
        queueOutput(
          SDK.STATE_QUEUE_ROOT_ASSET_NAME,
          rootDatum(SDK.GENESIS_HEADER_HASH, 0, null),
        ),
      ],
      nonce: chain.nonce(),
    },
  ]);
  events.push(init.event);
  let correctionLockOutRef: OutRef = {
    txHash: init.encoded.txHashes[0] as Buffer,
    index: 1,
  };
  const rootOutRef: OutRef = {
    txHash: init.encoded.txHashes[1] as Buffer,
    index: 0,
  };

  let status: FollowStatus | null = null;
  const abort = new AbortController();
  let loopOutcome: "running" | "returned" | "rejected" = "running";
  const loop = followChain({
    store: facts,
    transport: growingChainSync(events),
    origin: { origin: SIM_ORIGIN.point, hubOracleOneShot: oneShot },
    signal: abort.signal,
    backoffMs: { initial: 1, max: 4 },
    onStatus: (next) => {
      status = next;
    },
  }).then(
    (final) => {
      loopOutcome = "returned";
      return final;
    },
    (error: unknown) => {
      loopOutcome = "rejected";
      throw error;
    },
  );

  const payloads = new Map<string, Buffer>();
  const committeeStore = await openTestCommitteeStore();
  const l1 = committeeL1Source({
    store: facts,
    parameters: PARAMETERS,
    status: () => status,
    slotTime: async () => SIM_SLOT_TIME,
  });
  const service = new CommitteeService({
    config,
    store: committeeStore,
    l1,
    payloadSource: {
      fetchPayloadCandidates: async (headerHash) => {
        const payload = payloads.get(headerHash);
        return (
          payload === undefined
            ? failPayloadSource(`no payload for ${headerHash}`)
            : payloadSourceFromBytes(payload)
        ).fetchPayloadCandidates(headerHash);
      },
    },
    signer,
    signerValidation: validateDaSignerMembership({
      daParams: config.daParams,
      signer,
      signerIndex: 0,
    }),
    coordinator: { publishSignature: async () => "posted" },
  });
  expect(await facts.start()).toMatchObject({ kind: "ready" });
  await service.initialize();

  return {
    chain,
    service,
    availabilityReads: committeeAvailabilityReads({
      store: facts,
      readiness: () => l1.readiness(),
    }),
    l1,
    committeeStore,
    status: () => status,
    loopOutcome: () => loopOutcome,
    /** A committed header whose payload the member can fetch. */
    header: async (transactionCount: number): Promise<Committed> => {
      const fixture = await makePayloadFixture(transactionCount);
      payloads.set(fixture.headerHash, fixture.payloadCbor);
      expect(headerHashOf(fixture.header)).toBe(fixture.headerHash);
      return { header: fixture.header, headerHash: fixture.headerHash };
    },
    /** The commit of `committed` as the first node after the root. */
    commitTx: ({ header, headerHash }: Committed): SimTx => ({
      inputs: [rootOutRef],
      outputs: [
        queueOutput(
          SDK.STATE_QUEUE_ROOT_ASSET_NAME,
          rootDatum(SDK.GENESIS_HEADER_HASH, 0, headerHash),
        ),
        queueOutput(
          `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
          nodeDatum(header, "Unattested", null),
        ),
      ],
      nonce: 0,
    }),
    /** A correction in its own block: spends the correction lock, locks it again. */
    correct: () => {
      const step = chain.forward([
        {
          inputs: [correctionLockOutRef],
          outputs: [correctionLock()],
          nonce: chain.nonce(),
        },
      ]);
      events.push(step.event);
      correctionLockOutRef = {
        txHash: step.encoded.txHashes[0] as Buffer,
        index: 0,
      };
    },
    forward: (txs: readonly SimTx[] = []) => {
      events.push(chain.forward(txs).event);
    },
    backward: (depth: number) => {
      events.push(chain.backward(depth));
    },
    /** Waits until the loop reported on every event served so far. */
    synced: async (
      settled: (current: FollowStatus) => boolean = (current) =>
        current.state === "following" && current.atTip,
    ): Promise<FollowStatus> => {
      const deadline = performance.now() + 10_000;
      for (;;) {
        const current = status;
        if (
          current !== null &&
          current.events >= events.length &&
          settled(current)
        )
          return current;
        if (performance.now() > deadline)
          throw new Error(
            `the follower did not settle: ${JSON.stringify(current)}`,
          );
        await pause(2);
      }
    },
    /** As `synced`, for a state the loop reaches without applying the last event. */
    reached: async (
      settled: (current: FollowStatus) => boolean,
    ): Promise<FollowStatus> => {
      const deadline = performance.now() + 10_000;
      for (;;) {
        const current = status;
        if (current !== null && settled(current)) return current;
        if (performance.now() > deadline)
          throw new Error(
            `the follower did not reach the state: ${JSON.stringify(current)}`,
          );
        await pause(2);
      }
    },
    close: async () => {
      abort.abort();
      await loop.catch(() => undefined);
      for (const store of stores) await store.close();
    },
  };
};

export type Harness = Awaited<ReturnType<typeof harness>>;

/** Commits `committed` at the tip, then buries it `atDepth` deep. */
export const commit = (h: Harness, committed: Committed, atDepth = CD) => {
  h.forward([h.commitTx(committed)]);
  for (let i = 1; i < atDepth; i += 1) h.forward();
};

/** One tick that decides: the follower is ready and nothing holds it. */
export const readyTick = async (h: Harness) => {
  const result = await h.service.tick();
  expect(result.held).toBeUndefined();
  expect(result.errors).toEqual([]);
  await expect(h.service.readinessSnapshot()).resolves.toMatchObject({
    ready: true,
  });
  return result;
};
