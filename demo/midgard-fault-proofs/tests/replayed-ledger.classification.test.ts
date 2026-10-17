import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import { ledgerEntriesRoot } from "../src/ledger-at.js";
import {
  createReplayedLedger,
  type ReplayedLedgerRefusal,
} from "../src/replayed-ledger.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { classifyHeader } from "../src/workflow/header-classifier.js";
import {
  replayBudget,
  setup,
  transport,
} from "./support/catalogue-retained-classification.setup.js";
import { ledgerEntriesAfter } from "./support/predecessor-ledger.js";
import { buildRetainedPlutusIdentityFixture } from "./support/retained-reason-classifier.js";

afterEach(() => {
  transport.raw = undefined;
});

describe(
  "classification over the watcher's replayed ledger",
  replayBudget,
  () => {
    it("detects a block whose transition does not open its committed roots", async () => {
      const fixture = await buildRetainedPlutusIdentityFixture({
        verdict: "accepted",
      });
      const header = { ...fixture.block.header, utxosRoot: "99".repeat(32) };
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
        payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
          mode: "identity",
        }),
      };
      const { classifyInput } = await setup(fixture, "normal");
      // The watcher's checkpoint holds the predecessor's ledger; the challenged
      // block is replayed from retained bytes, never read from a UTxO section.
      const predecessor = await reconstructDaPayload({
        payloadEnvelopeCbor: fixture.predecessor.payloadEnvelopeCbor,
      });
      const refusals: ReplayedLedgerRefusal[] = [];
      const ledger = createReplayedLedger({
        checkpoints: {
          read: async () => ({
            headerHash: predecessor.headerHash,
            entries: ledgerEntriesAfter(predecessor),
          }),
          write: async () => {
            throw new Error("the checkpoint does not move in this case");
          },
        },
        onRefusal: (refusal) => refusals.push(refusal),
      });
      expect(
        await ledger.restore({
          headerHash: predecessor.headerHash,
          utxosRoot: predecessor.header.utxosRoot,
        }),
      ).toBe("restored");
      const ledgerAt = ledger.ledgerAt({
        payloadAt: async (headerHash) =>
          headerHash === fixture.block.headerHash
            ? (
                await reconstructDaPayload({
                  payloadEnvelopeCbor: fixture.block.payloadEnvelopeCbor,
                })
              ).payload
            : undefined,
        materialFor: async (payload) => ({
          blockTimeMs: Number(payload.block_body.header.endTime),
          replayUserEvent: () => Effect.die("fixture has no user event"),
          verifyForcedSource: () => Effect.die("fixture has no forced order"),
        }),
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      });
      const derived = await ledgerAt(fixture.block.headerHash);
      if (derived === undefined)
        throw new Error("the valid fixture replay was refused");
      expect(await ledgerEntriesRoot(derived)).not.toBe(
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
  },
);
