import "./native-capture-retry.fixture.js";

import {
  LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
  LocalKupmiosCheckpointChangedError,
  localKupmiosHttpOgmiosRawSourceDetails,
  LocalKupmiosTransportUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { expect, it } from "vitest";

import { WatcherStateQueueReadRetired } from "../../src/indexers/authenticated-state-queue-observation.read-scopes.js";
import { createWatcherResolvedBlockObservationSource } from "../../src/l1/resolved-block-observation.js";
import { appendFixture } from "./authenticated-state-queue-observation.append-fixture.js";
import {
  deferred,
  io,
  nextTurn,
  setup,
} from "./native-capture-retry.fixture.js";

// Native/private query admission is controlled here. Source creation, scope,
// both capture queues, causal derivation and finalized publication are real.
it("caps checkpoint and transport retries together and retains exact native resolution", async () => {
  const h = setup();
  const transport = new LocalKupmiosTransportUnavailableError("socket lost");
  io.boundary
    .mockRejectedValueOnce(
      new LocalKupmiosCheckpointChangedError("provider head changed"),
    )
    .mockRejectedValueOnce(transport)
    .mockRejectedValueOnce(transport);
  try {
    await expect(h.source.observe(h.input)).rejects.toBe(transport);
    expect(io.boundary).toHaveBeenCalledTimes(3);
    expect(h.source.latestFinalizedObservation?.()).toBeNull();
    io.boundary.mockResolvedValue({
      kupoCheckpoint: h.initial.raw.inclusionPoint,
      ogmiosTip: h.initial.raw.inclusionPoint,
      confirmationDepth: h.initial.raw.confirmationDepth,
    });
    const result = await h.source.observe(h.input);
    expect(result?.nativePoint.blockHash).toBe(h.input.nativeBlock.blockHash);
    expect(io.block.mock.calls[0]![0].point).toEqual(
      h.initial.raw.inclusionPoint,
    );
    expect(io.block.mock.calls[0]![0].source).not.toBe(h.rawSource);
    expect(io.transaction.mock.calls[0]![0].source).toBe(
      io.block.mock.calls[0]![0].source,
    );
    expect(h.source.latestFinalizedObservation?.()).toBe(result);
  } finally {
    h.scopes.close();
  }
});

it("holds the stable native queue through an expired inner callback and prevents late finalized publication", async () => {
  const h = setup();
  const held = deferred();
  io.transaction.mockImplementationOnce(async () => {
    await held.promise;
    return h.initial.raw;
  });
  const old = h.source.observe(h.input);
  const failure = old.catch((error) => error as unknown);
  let fresh: ReturnType<typeof h.source.observe> | undefined;
  try {
    await nextTurn();
    expect(io.transaction).toHaveBeenCalledTimes(1);
    h.scopes.invalidate();
    expect(await failure).toBeInstanceOf(WatcherStateQueueReadRetired);
    fresh = h.source.observe(h.input);
    await nextTurn();
    expect(io.boundary).toHaveBeenCalledTimes(1);
    expect(h.source.latestFinalizedObservation?.()).toBeNull();
    held.resolve();
    const current = await fresh;
    expect(io.boundary).toHaveBeenCalledTimes(2);
    expect(io.transaction).toHaveBeenCalledTimes(2);
    expect(h.source.latestFinalizedObservation?.()).toBe(current);
  } finally {
    held.resolve();
    h.scopes.close();
    await failure;
    await fresh?.catch(() => undefined);
  }
});

it("refuses resolved-block admission after scope retirement while its native receipt is still live", async () => {
  const h = setup();
  const attempt = h.scopes.begin("release_finality");
  const held = deferred();
  io.block.mockImplementationOnce(async () => {
    await held.promise;
    return h.rawBlock;
  });
  const reader = createWatcherResolvedBlockObservationSource({
    deploymentIdentity: h.deploymentIdentity,
    rawSource: attempt.rawSource,
    assertCurrent: attempt.assertCurrent,
  });
  const pending = reader.observe(h.input);
  try {
    await nextTurn();
    expect(io.block).toHaveBeenCalledOnce();
    h.scopes.invalidate();
    const rejected = expect(pending).rejects.toBeInstanceOf(
      WatcherStateQueueReadRetired,
    );
    held.resolve();
    await rejected;
  } finally {
    held.resolve();
    attempt.close();
    h.scopes.close();
    await pending.catch(() => undefined);
  }
});

it("retains the prior checkpoint allowance without treating protocol faults as transport retries", async () => {
  const h = setup();
  io.boundary.mockRejectedValueOnce(
    new LocalKupmiosCheckpointChangedError("provider head changed"),
  );
  try {
    await expect(h.source.observe(h.input)).resolves.not.toBeNull();
    expect(io.boundary).toHaveBeenCalledTimes(2);
    const protocol = Object.assign(new Error("noncanonical point"), {
      name: "LocalKupmiosTransportUnavailableError",
    });
    io.boundary.mockRejectedValue(protocol);
    await expect(h.source.observe(h.input)).rejects.toBe(protocol);
    expect(io.boundary).toHaveBeenCalledTimes(3);
  } finally {
    h.scopes.close();
  }
});

it("consumes its configured operation budget while waiting for the stable queue", async () => {
  const h = setup(200);
  const held = deferred();
  io.transaction.mockImplementationOnce(async () => {
    await held.promise;
    return h.initial.raw;
  });
  const first = h.source.observe(h.input).catch((error) => error as unknown);
  let queued: Promise<unknown> | undefined;
  try {
    await nextTurn();
    expect(io.transaction).toHaveBeenCalledOnce();
    queued = h.source.observe(h.input).catch((error) => error as unknown);
    expect(await first).toBeInstanceOf(WatcherStateQueueReadRetired);
    expect(await queued).toBeInstanceOf(WatcherStateQueueReadRetired);
    expect(io.boundary).toHaveBeenCalledTimes(1);
    held.resolve();
    await nextTurn();
    expect(io.boundary).toHaveBeenCalledTimes(1);
    expect(h.source.latestFinalizedObservation?.()).toBeNull();
    await expect(h.source.observe(h.input)).resolves.not.toBeNull();
    expect(io.boundary).toHaveBeenCalledTimes(2);
  } finally {
    held.resolve();
    h.scopes.close();
    await first;
    await queued;
  }
});

it("gives included observations a fresh depth-one attempt without advancing the finalized cache", async () => {
  const h = setup();
  try {
    const previous = (await h.source.observe(h.input))!;
    const append = appendFixture({ initial: h.initial, previous });
    const localObservation = {
      ...append.localObservation,
      transportAttestations: h.input.localObservation.transportAttestations,
      block: {
        ...append.localObservation.block,
        chainPoint: { ...append.localObservation.block.chainPoint, depth: "1" },
      },
    };
    io.boundary.mockResolvedValue({
      kupoCheckpoint: append.raw.inclusionPoint,
      ogmiosTip: append.raw.inclusionPoint,
      confirmationDepth: 1,
    });
    io.block.mockResolvedValue({
      schemaVersion: LOCAL_KUPMIOS_RAW_BLOCK_AT_POINT,
      sourceId: "fixture",
      point: append.raw.inclusionPoint,
      parentBlockHash: append.nativeBlock.prevHash,
      kupoCheckpoint: {
        slot: Number(append.nativeBlock.slot),
        blockHash: append.nativeBlock.blockHash,
      },
      transactions: append.nativeBlock.transactionIds.map((txHash, index) => ({
        txHash,
        transactionCbor: append.nativeBlock.transactionCbors[index]!,
      })),
    });
    io.transaction.mockResolvedValue({ ...append.raw, confirmationDepth: 1 });
    const included = await h.source.observeIncluded!({
      nativeBlock: append.nativeBlock,
      localObservation,
      previous,
    });
    expect(included.nativePoint.finalityDepth).toBe("1");
    expect(included.finalizedHeaders).toHaveLength(1);
    const attemptSource = io.block.mock.calls[1]![0].source;
    expect(attemptSource).not.toBe(h.includedSource);
    expect(
      localKupmiosHttpOgmiosRawSourceDetails(attemptSource)?.observationDepth,
    ).toBe("inclusion");
    expect(io.transaction.mock.calls[1]![0].minimumConfirmationDepth).toBe(1);
    expect(io.transaction.mock.calls[1]![0].source).toBe(attemptSource);
    expect(h.source.latestFinalizedObservation?.()).toBe(previous);
  } finally {
    h.scopes.close();
  }
});
