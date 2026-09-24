import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as canonical,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { captureStagedHistoryEvent } from "./history-events.js";
import { verifyDuplicateEventSource } from "./history-settlement.js";

it.each([false, true])(
  "binds settlement source content and original funds to its captured deposit and unchanged seven-day maturity (raw=%s)",
  async (raw) => {
    const policyId = "ab".repeat(28);
    const address = credentialToAddress("Custom", {
      type: "Script",
      hash: policyId,
    });
    const owner = {
      paymentCredential: { PublicKeyCredential: ["12".repeat(28)] as [string] },
      stakeCredential: null,
    };
    const event: SDK.DepositEvent = {
      id: { transactionId: "34".repeat(32), outputIndex: 0n },
      info: {
        l2_address: owner,
        l2_network_id: 0n,
        l2_datum: raw ? "fa".repeat(40) : new Map<Data, Data>([[1n, "abcd"]]),
      },
    };
    const key = await Effect.runPromise(SDK.eventHistoryKey(event.id));
    const root: UTxO = {
      txHash: "01".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 3_000_000n, [policyId]: 1n },
      datum: Data.to(
        {
          position: "Root",
          next: key,
          protected_until: 0n,
          payload: "RootContent",
        },
        SDK.EventHistoryNode,
      ),
    };
    const output: UTxO = {
      txHash: "02".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 10_000_000n, [policyId + key]: 1n },
      datum: Data.to(
        {
          position: { Key: [key] },
          next: null,
          protected_until: 0n,
          payload: {
            Order: {
              facts: {
                event_id: event.id,
                inclusion_time: 150n,
                location: {
                  Inline: { payload: { DepositPayload: { event } } },
                },
                structural_lovelace: 2_000_000n,
                structural_refund_key: "12".repeat(28),
              },
            },
          },
        },
        SDK.EventHistoryNode,
      ),
    };
    const rawDatum = "a302a20102010301000102";
    if (raw)
      output.datum = output.datum!.replace(Data.to("fa".repeat(40)), rawDatum);
    const [order] = await Effect.runPromise(
      SDK.utxosToDepositUTxOs([root, output], [], {
        policyId,
        address,
        retentionAddress: "unused-retention",
        inlineLimitBytes: 512n,
      }),
    );
    const predecessor = await buildCanonicalBlockFixture({
      transactions: [],
      startTime: 0n,
      endTime: 100n,
    });
    const block = await depositEventsRetainedBlock({
      operatorVkey: "56".repeat(28),
      startTime: 100n,
      endTime: 200n,
      blockSlot: 0n,
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: predecessor.header.utxosRoot,
      priorLedger: predecessor.payload.block_body.utxos,
      events: [
        {
          eventCbor: canonical(
            plutusConstrFieldCbor(order!.history.payloadCbor, [0]),
          ),
          originalAssets: order!.originalAssets,
          honest: true,
        },
      ],
    });
    // Pure retained content: no submission, merge or new L1 observation is implied.
    const source = { ...block, commitTxHash: "78".repeat(32) };
    const captured = JSON.parse(
      JSON.stringify(captureStagedHistoryEvent({ order: order!, policyId })),
    );
    expect(await verifyDuplicateEventSource(source, captured)).toBe(
      block.header.endTime + 604_800_000n,
    );
    if (raw) {
      expect(captured.openingCbor).toContain(rawDatum);
      const replaced = captured.openingCbor.replace(
        rawDatum,
        "a302a20102010901000102",
      );
      expect(
        Data.to(
          Data.from(replaced, SDK.EventHistoryOpening),
          SDK.EventHistoryOpening,
        ),
      ).toBe(
        Data.to(
          Data.from(captured.openingCbor, SDK.EventHistoryOpening),
          SDK.EventHistoryOpening,
        ),
      );
      await expect(
        verifyDuplicateEventSource(source, {
          ...captured,
          openingCbor: replaced,
        }),
      ).rejects.toThrow("exact genuine deposit");
    }
    const changedOpening = Data.from(
      captured.openingCbor,
      SDK.EventHistoryOpening,
    );
    if (!("DepositPayload" in changedOpening.payload))
      throw new Error("Missing deposit payload");
    changedOpening.payload.DepositPayload.event.id.outputIndex += 1n;
    await expect(
      verifyDuplicateEventSource(source, {
        ...captured,
        openingCbor: Data.to(changedOpening, SDK.EventHistoryOpening),
      }),
    ).rejects.toThrow("exact genuine deposit");
    const changedFunds = Data.from(
      captured.openingCbor,
      SDK.EventHistoryOpening,
    );
    changedFunds.original_assets = SDK.assetsToValue({ lovelace: 10_000_000n });
    await expect(
      verifyDuplicateEventSource(source, {
        ...captured,
        openingCbor: Data.to(changedFunds, SDK.EventHistoryOpening),
      }),
    ).rejects.toThrow("exact genuine deposit");
    await expect(
      verifyDuplicateEventSource(
        { ...source, headerHash: "00".repeat(28) },
        captured,
      ),
    ).rejects.toThrow("header hash changed");
  },
);
