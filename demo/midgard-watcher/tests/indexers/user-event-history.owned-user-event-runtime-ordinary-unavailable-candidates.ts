import "./user-event-history.header-scoped-replay-authorities.js";

import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import { h32 } from "@al-ft/midgard-test-support/hex";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createWatcherLocalUserEventPublisher,
  recoverWatcherLocalUserEventPublisher,
} from "../../src/indexers/user-event-history.js";
import { readWatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import {
  assertWatcherUserEventRuntime,
  createWatcherUserEventRuntime,
} from "../../src/runtime/user-event-runtime.js";
import {
  createWatcherDurableRuntime,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import { createInMemoryWatcherUserEventCoverageStore } from "../../src/storage/user-event-coverage-store.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
  syntheticUserEventTransaction as transaction,
  transactionInput,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

describe("local user-event same-process rollback suspension", () => {
  it("retires old capabilities synchronously and requires fresh protected-head W12 corroboration", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const { pair, input, origin, facts } = await openOrigin(fixture);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(pair.finality).policy,
      );
      const publisher = await createWatcherLocalUserEventPublisher({
        ...input,
        origin,
        runtime: durable.runtime,
        archive: durable.archive,
      });
      await publisher.publish(pair);
      await pair.close();
      const empty = await fixture.openFinalizedBlock(
        fixture.emptySuccessorBlock,
      );
      await publisher.publish(empty);
      await empty.close();
      const lifecycle = historyLifecycle(facts);
      const block = await fixture.makeBlock({
        transactions: [lifecycle.create],
        creatingBodies: [fixture.initializationBodyCbor],
      });
      const published = await fixture.openFinalizedBlock(block);
      await publisher.publish(published);
      await published.close();
      const before = await fixture.openFinalizedBlock(block);
      const event = publisher.read().snapshot.activeEvents[0]!;
      const old = await publisher.eventAuthority({
        ...before,
        eventId: event.eventId,
        kind: "deposit",
      });
      await expect(
        readWatcherLocalUserEventAuthority(old),
      ).resolves.toBeDefined();
      publisher.suspend();
      await expect(readWatcherLocalUserEventAuthority(old)).rejects.toThrow(
        /suspended/u,
      );
      await expect(publisher.publish(before)).rejects.toThrow(/suspended/u);
      await expect(publisher.resume(before)).rejects.toThrow(/freshly/u);
      const fresh = await fixture.openFinalizedBlock(block);
      await publisher.resume(fresh);
      await fresh.close();
      await before.close();
      await expect(readWatcherLocalUserEventAuthority(old)).rejects.toThrow();
      const corroborated = await fixture.openFinalizedBlock(block);
      const authority = await publisher.eventAuthority({
        ...corroborated,
        eventId: event.eventId,
        kind: "deposit",
      });
      await expect(
        readWatcherLocalUserEventAuthority(authority),
      ).resolves.toBeDefined();
      publisher.suspend();
      const rollbackPair = await fixture.openFinalizedBlock(block);
      // A changed protected head cannot be accepted as the old same-process fold.
      const otherRuntime = await createWatcherDurableRuntime(
        durable.runtimeInput,
      );
      const reopenedOrigin = await openOrigin(fixture);
      const other = await recoverWatcherLocalUserEventPublisher({
        ...reopenedOrigin.input,
        origin: reopenedOrigin.origin,
        ...reopenedOrigin.pair,
        runtime: otherRuntime,
        archive: durable.archive,
        replayBlock: async (point) =>
          fixture.openFinalizedBlock(
            point.blockHash === fixture.emptySuccessorBlock.point.blockHash
              ? fixture.emptySuccessorBlock
              : block,
          ),
      });
      other.close();
      await reopenedOrigin.pair.close();
      await expect(publisher.resume(rollbackPair)).rejects.toThrow();
      await rollbackPair.close();
      await corroborated.close();
      publisher.close();
    } finally {
      await fixture.close();
    }
  }, 120_000);
});

describe("owned user-event runtime ordinary unavailable candidates", () => {
  it("refuses an absent claim, then issues a valid header-scoped capability and keeps indexing", async () => {
    const construction = await createSyntheticUserEventOriginFixture();
    const identity = construction.deploymentIdentity;
    const ruleBundle = makeWatcherCanonicalRuleBundle({
      constructionIdentity: {
        manifestId: identity.manifestId,
        network: identity.network,
        blueprintHash: identity.blueprintHash,
        programCommitments: identity.programCommitments,
      },
      targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
    });
    await construction.close();
    const state: {
      lifecycle: ReturnType<typeof historyLifecycle> | null;
      depositAddressHex: string | null;
    } = { lifecycle: null, depositAddressHex: null };
    const fixture = await createSyntheticStateQueueObservationFixture({
      ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
      composeCommitBlock: async ({ transport, commitTransactionCbor }) => {
        const origin = await openOrigin(transport);
        state.lifecycle = historyLifecycle(origin.facts);
        state.depositAddressHex = origin.facts.scripts.deposit.addressHex;
        await origin.pair.close();
        return {
          transactions: [state.lifecycle.create, commitTransactionCbor],
          creatingBodies: [transport.initializationBodyCbor],
        };
      },
    });
    let service: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      const transport = fixture.transport;
      const signed = transport.deployment;
      const deploymentAuthority = await loadWatcherVerifiedDeploymentAuthority({
        path: "/unit/authority.json",
        ruleBundlePath: "/unit/rules.json",
        unsafeReadFileForTest: async (path) =>
          new TextEncoder().encode(
            JSON.stringify(
              path === "/unit/authority.json"
                ? {
                    signedIdentity: signed.signedIdentity,
                    policy: signed.policy,
                    trustRoots: signed.trustRoots,
                    durableMarker: signed.marker,
                  }
                : ruleBundle,
            ),
          ),
      });
      const origin = await openOrigin(transport);
      const durable = await durableFixture(
        readWatcherLocalBackfillFinality(origin.pair.finality).policy,
      );
      await origin.pair.close();
      service = await createWatcherUserEventRuntime({
        watcherConfig: transport.watcherConfig,
        deploymentAuthority,
        blueprintBytes: await readFile(
          process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
            fileURLToPath(
              new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
            ),
        ),
        l1NodeTransportBinaryPath: transport.l1NodeTransportBinaryPath,
        runtime: durable.runtime,
        archive: durable.archive,
        coverage: createInMemoryWatcherUserEventCoverageStore(),
      });
      await service.advanceThrough(fixture.commitBlock.point);
      const captured = await fixture.observeFresh();
      try {
        const request = {
          kind: "deposit" as const,
          eventId: state.lifecycle!.expectedEventId,
          throughHeader: captured.header,
        };
        const before = service.read().currentPoint;
        await expect(
          service.eventAuthority({ ...request, eventId: `${h32(0xab)}#0` }),
        ).rejects.toThrow(/event is not retained/u);
        expect(service.read()).toMatchObject({
          status: "ready",
          currentPoint: before,
        });
        expect(() => assertWatcherUserEventRuntime(service!)).not.toThrow();
        const cap = await service.eventAuthority(request);
        expect(
          (await readWatcherLocalUserEventAuthority(cap)).event.eventId,
        ).toBe(request.eventId);
        // A quiet successor is covered without a publication: the issued
        // authority stays current because no new checkpoint retired it.
        const next = await transport.makeBlock({
          transactions: [],
          parent: fixture.commitBlock,
        });
        await service.advanceThrough(next.point);
        expect(service.read()).toMatchObject({
          status: "ready",
          currentPoint: next.point,
          headCursor: fixture.commitBlock.point,
        });
        expect(
          (await readWatcherLocalUserEventAuthority(cap)).event.eventId,
        ).toBe(request.eventId);
        // A touched successor (a plain payment at the deposit credential,
        // which the fold ignores) publishes a new checkpoint and retires it.
        const touchedInputs = CML.TransactionInputList.new();
        touchedInputs.add(transactionInput(`${h32(0xc3)}#0`));
        const touchedOutputs = CML.TransactionOutputList.new();
        touchedOutputs.add(
          CML.TransactionOutput.new(
            CML.Address.from_hex(state.depositAddressHex!),
            CML.Value.new(2_000_000n, CML.MultiAsset.new()),
          ),
        );
        const touched = await transport.makeBlock({
          transactions: [
            transaction(
              CML.TransactionBody.new(touchedInputs, touchedOutputs, 200_000n),
              [],
            ),
          ],
          parent: next,
        });
        await service.advanceThrough(touched.point);
        expect(service.read()).toMatchObject({
          status: "ready",
          currentPoint: touched.point,
          headCursor: touched.point,
        });
        await expect(readWatcherLocalUserEventAuthority(cap)).rejects.toThrow();
        const protectedHead = readWatcherProtectedUserEventCheckpointReceipt(
          await readWatcherProtectedUserEventCheckpoint(durable.runtime),
        );
        durable.objects.delete(protectedHead.checkpoint!.payloadDigest);
        await expect(
          service.eventAuthority({ ...request, eventId: `${h32(0xab)}#0` }),
        ).rejects.toThrow();
        expect(service.read().status).toBe("failed");
        await expect(service.done).rejects.toThrow();
      } finally {
        await captured.close();
      }
    } finally {
      await service?.close();
      await fixture.close();
    }
  }, 120_000);
});
