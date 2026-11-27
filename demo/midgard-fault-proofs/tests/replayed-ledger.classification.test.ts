import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  createLedgerStore,
  createMemoryLedgerStorage,
} from "../src/ledger-store/index.js";
import {
  createReplayedLedger,
  type ReplayedLedgerRefusal,
} from "../src/replayed-ledger.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.reconstruct-da-payload.js";
import { classifyHeader } from "../src/workflow/header-classifier.classify-header.js";
import {
  replayBudget,
  setup,
  transport,
} from "./support/catalogue-retained-classification.setup.js";
import { entriesRoot } from "./support/fixture-ledger.js";
import { ledgerEntriesAfter } from "./support/predecessor-ledger.js";
import { buildRetainedPlutusIdentityFixture } from "./support/retained-reason-classifier.build-retained-plutus-fixture.js";

afterEach(() => {
  transport.raw = undefined;
});

/**
 * The retained fixture block with its header claiming `claimedRoot` (given
 * the predecessor's), a replayed ledger whose store holds the predecessor's
 * ledger, and the classifier's input over it. The challenged block is
 * replayed from retained bytes, never read from a UTxO section.
 */
const scene = async (claimedRoot: (predecessorRoot: string) => string) => {
  const fixture = await buildRetainedPlutusIdentityFixture({
    verdict: "accepted",
  });
  const predecessor = await reconstructDaPayload({
    payloadEnvelopeCbor: fixture.predecessor.payloadEnvelopeCbor,
  });
  const header = {
    ...fixture.block.header,
    utxosRoot: claimedRoot(predecessor.header.utxosRoot),
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const original = await reconstructDaPayload({
    payloadEnvelopeCbor: fixture.block.payloadEnvelopeCbor,
  });
  const payload = {
    ...original.payload,
    block_body: {
      ...original.payload.block_body,
      header,
      header_hash: headerHash,
    },
  };
  fixture.block = {
    ...fixture.block,
    header,
    headerHash,
    payloadEnvelopeCbor: await wrapDaPayload(SDKDa.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
  const { classifyInput } = await setup(fixture, "normal");
  const store = createLedgerStore(createMemoryLedgerStorage());
  await store.admit(
    ledgerEntriesAfter(predecessor),
    predecessor.header.utxosRoot,
  );
  const refusals: ReplayedLedgerRefusal[] = [];
  const ledger = createReplayedLedger({
    store,
    onRefusal: (refusal) => refusals.push(refusal),
  });
  const headers = new Map([
    [predecessor.headerHash, predecessor.header],
    [headerHash, header],
  ]);
  const ledgerAt = ledger.ledgerAt({
    headerAt: async (hash) => headers.get(hash),
    payloadAt: async (hash) => (hash === headerHash ? payload : undefined),
    materialFor: async (served) => ({
      blockTimeMs: Number(served.block_body.header.endTime),
      replayUserEvent: () => Effect.die("fixture has no user event"),
      verifyForcedSource: () => Effect.die("fixture has no forced order"),
    }),
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  });
  return { fixture, predecessor, classifyInput, refusals, ledgerAt };
};

describe(
  "classification over the watcher's replayed ledger",
  replayBudget,
  () => {
    it("detects a block whose transition does not open its committed roots", async () => {
      const { fixture, classifyInput, refusals, ledgerAt } = await scene(() =>
        "99".repeat(32),
      );
      const derived = await ledgerAt(fixture.block.headerHash);
      if (derived === undefined)
        throw new Error("the valid fixture replay was refused");
      expect(await entriesRoot(derived)).not.toBe(
        fixture.block.header.utxosRoot,
      );
      expect(refusals).toMatchObject([
        { headerHash: fixture.block.headerHash, kind: "root-mismatch" },
      ]);
      const decision = await classifyHeader({
        ...classifyInput,
        predecessorLedgerAt: ledgerAt,
      });
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: "transitionTrace",
        headerHash: fixture.block.headerHash,
      });
      // Valid replay derives the actual ledger, but the declared root differs.
      // The classifier identifies the inconsistent transition; the keeper
      // refuses to derive any descendant from the unchecked ledger.
      expect(refusals).toMatchObject([
        { headerHash: fixture.block.headerHash, kind: "root-mismatch" },
      ]);
    });

    it("detects a block claiming a root the store already holds while its events produce another", async () => {
      // The block claims its predecessor's root, which the store holds: the
      // lookup by root answers the block without replaying it, so only the
      // classifier's own replay can see the fault.
      const { fixture, predecessor, classifyInput, ledgerAt } = await scene(
        (predecessorRoot) => predecessorRoot,
      );
      const served = await ledgerAt(fixture.block.headerHash);
      if (served === undefined) throw new Error("the held root was not served");
      expect(await entriesRoot(served)).toBe(predecessor.header.utxosRoot);
      const decision = await classifyHeader({
        ...classifyInput,
        predecessorLedgerAt: ledgerAt,
      });
      expect(decision).toMatchObject({
        decision: "fault_detected",
        category: "transitionTrace",
        headerHash: fixture.block.headerHash,
      });
    });
  },
);
