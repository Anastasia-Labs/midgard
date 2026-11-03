import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import { expect, it } from "vitest";

import { readWatcherObservation } from "../../src/l1-follower/observation.js";
import {
  applyTx,
  attestTx,
  commitTx,
  initTx,
  queueState,
} from "../support/l1-follower-state-queue-traffic.js";
import {
  applyAll,
  AUTHORITY,
  D,
  openStore,
  RELEASE_DEPTH,
  SOURCE_ID,
} from "../support/l1-follower-store-reset.js";
import {
  GENESIS,
  genesisInitTx,
  replaySource,
} from "../support/replay-source-fixture.js";

// Synthetic chain framing and the real follower projection; no Plutus execution.
it("reads the confirmed root from the admitted queue point and revokes it on a rewind", async () => {
  const store = openStore(":memory:");
  await store.start();
  await store.initialize(SIM_ORIGIN);
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const event = chain.forward([genesisInitTx()]).event;
  let rewindPoint = chain.tip.point;
  const events = [event];
  for (let index = 0; index < 8; index += 1) {
    events.push(chain.forward([]).event);
    if (index === 5) rewindPoint = chain.tip.point;
  }
  await applyAll(store, events);
  const input = {
    authority: AUTHORITY,
    sourceId: SOURCE_ID,
    depth: RELEASE_DEPTH,
    releaseDepth: RELEASE_DEPTH,
  };
  const observed = await readWatcherObservation(store, input);
  if (observed.kind !== "ok")
    throw new Error(`observation: ${observed.reason}`);
  const source = replaySource(store);
  try {
    const confirmed = await source.confirmed(observed.observation);
    expect(confirmed.state).toEqual(GENESIS);
    expect(confirmed.current()).toBe(true);
    expect((await store.rewind(rewindPoint)).kind).toBe("rewound");
    expect(confirmed.current()).toBe(false);
    await expect(source.confirmed(observed.observation)).rejects.toThrow(
      "checkpoint observation left the canonical chain",
    );
  } finally {
    source.close();
    await store.close();
  }
});

it("reads a header's availability status from its latest queue node", async () => {
  const store = openStore(":memory:");
  await store.start();
  await store.initialize(SIM_ORIGIN);
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const state = () => {
    const live = queueState(chain, D);
    if (live === null) throw new Error("the model queue is not initialized");
    return live;
  };
  await applyAll(store, [chain.forward([initTx(D)]).event]);
  await applyAll(store, [chain.forward([commitTx(state(), D)]).event]);
  const header = state().tailHeaderHash as string;
  const source = replaySource(store);
  try {
    expect(await source.availabilityStatus(header)).toBe("Unattested");
    await applyAll(store, [chain.forward([attestTx(state(), D)]).event]);
    const attestation = state().attestations.get(header);
    if (attestation === undefined)
      throw new Error("the header is not attested");
    await applyAll(store, [
      chain.forward([applyTx(state().tail, attestation, header, D, true)])
        .event,
    ]);
    expect(await source.availabilityStatus(header)).toMatchObject({
      Attested: { commitment_hash: expect.stringMatching(/^[0-9a-f]{64}$/) },
    });
    await expect(source.availabilityStatus("ee".repeat(28))).rejects.toThrow(
      "authenticated admission is unavailable",
    );
    source.close();
    await expect(source.availabilityStatus(header)).rejects.toThrow(
      "the follower replay source is closed",
    );
  } finally {
    source.close();
    await store.close();
  }
});
