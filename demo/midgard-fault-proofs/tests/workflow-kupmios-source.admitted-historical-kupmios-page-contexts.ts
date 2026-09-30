import {
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  createLocalKupmiosHttpOgmiosRawSource,
  type FraudProofRawL1Point,
  LocalKupmiosExactPointNotCanonicalError,
  readAdmittedLocalKupmiosAddressUtxosAtPoint,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
} from "../src/workflow/index.js";
import {
  ANCESTOR,
  chainPoint,
  hash,
  OgmiosBoundarySocket,
  releaseFinality,
  response,
} from "./workflow-kupmios-source.ogmios-boundary-socket.js";
import { sourceFixture } from "./workflow-kupmios-source.source-fixture.js";

describe("admitted historical Kupmios page contexts", () => {
  const unit = "31".repeat(28);
  const address = credentialToAddress("Preprod", scriptHashToCredential(unit));

  it.each(["history", "address"] as const)(
    "authenticates historical %s reads without changing the live boundary",
    async (kind) => {
      const fixture = sourceFixture();
      const boundary = await readAdmittedLocalKupmiosBoundary({
        source: fixture.source,
      });
      const point = chainPoint("380", ANCESTOR, "70");
      await expect(
        fixture.source.scanUnitHistoryPage({
          unit,
          fromGenesis: true,
          throughPoint: point,
          after: null,
        }),
      ).rejects.toThrow("outside its pinned boundary");
      if (kind === "history") {
        await expect(
          readAdmittedLocalKupmiosUnitHistoryAtPoint({
            source: fixture.source,
            unit,
            point,
          }),
        ).resolves.toEqual({ checkpoint: point, transactions: [] });
      } else {
        await expect(
          readAdmittedLocalKupmiosAddressUtxosAtPoint({
            source: fixture.source,
            address,
            point,
          }),
        ).resolves.toEqual([]);
      }
      await expect(
        fixture.source.scanUnitHistoryPage({
          unit,
          fromGenesis: true,
          throughPoint: boundary.kupoCheckpoint,
          after: null,
        }),
      ).resolves.toMatchObject({
        checkpoint: boundary.kupoCheckpoint,
        complete: true,
      });
      expect(
        fixture.sockets.filter((socket) => socket.originIntersection),
      ).toHaveLength(1);
    },
  );

  it("rejects an unpinned, future, substituted, or out-of-window historical point", async () => {
    const fixture = sourceFixture();
    const read = (point: FraudProofRawL1Point) =>
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit,
        point,
      });
    await expect(read(chainPoint("380", ANCESTOR, "70"))).rejects.toThrow(
      "outside its pinned boundary",
    );
    await readAdmittedLocalKupmiosBoundary({ source: fixture.source });
    await expect(read(chainPoint("420", hash(10), "72"))).rejects.toThrow(
      "outside the pinned release recovery window",
    );
    await expect(
      read(chainPoint("380", hash(10), "70")),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    const older = sourceFixture({
      tipHeight: 3000,
      parentHeight: 2970,
      socketBehavior: { childHeight: 2971 },
    });
    await readAdmittedLocalKupmiosBoundary({ source: older.source });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: older.source,
        unit,
        point: chainPoint("380", ANCESTOR, "70"),
      }),
    ).rejects.toThrow("outside the pinned release recovery window");
  });

  it("admits exactly 2160 blocks of historical distance and rejects 2161", async () => {
    const fixture = sourceFixture({
      tipHeight: 2230,
      socketBehavior: { childHeight: 2201 },
    });
    await readAdmittedLocalKupmiosBoundary({ source: fixture.source });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit,
        point: chainPoint("380", ANCESTOR, "70"),
      }),
    ).resolves.toMatchObject({ checkpoint: { blockNo: "70" } });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit,
        point: chainPoint("380", ANCESTOR, "69"),
      }),
    ).rejects.toThrow("outside the pinned release recovery window");
  });

  it("rechecks the exact historical point after scanning its page", async () => {
    let scanned = false;
    const fixture = sourceFixture({
      beforeFetch: async (url) => {
        if (url.includes("/matches/")) scanned = true;
      },
      checkpointOverride: (slot) =>
        slot === 380 && scanned
          ? { slot_no: 380, header_hash: hash(10) }
          : undefined,
    });
    await readAdmittedLocalKupmiosBoundary({ source: fixture.source });
    await expect(
      readAdmittedLocalKupmiosUnitHistoryAtPoint({
        source: fixture.source,
        unit,
        point: chainPoint("380", ANCESTOR, "70"),
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
  });
});

describe("release-final boundary selection", () => {
  it.each([
    [1, 30],
    [20, 30],
    [1, 1],
    [20, 1],
  ])(
    "selects the most recent eligible block with %i slots per block at depth %i",
    async (spacing, depth) => {
      const tipSlot = 1000;
      const pointHash = (slot: number) => slot.toString(16).padStart(64, "0");
      const tip = {
        slot: tipSlot,
        id: pointHash(tipSlot),
        height: tipSlot / spacing,
      };
      const source = createLocalKupmiosHttpOgmiosRawSource({
        sourceId: `boundary-spacing-${spacing}`,
        kupoHttpUrl: "http://127.0.0.1:1442",
        ogmiosUrl: "http://127.0.0.1:1337",
        releaseFinality,
        fetchImpl: async (url) => {
          const requestedSlot = Number(new URL(url).pathname.split("/").at(-1));
          const slot = Math.floor(requestedSlot / spacing) * spacing;
          return response(
            { slot_no: slot, header_hash: pointHash(slot) },
            true,
          );
        },
        webSocketFactory: () => {
          const socket = new OgmiosBoundarySocket();
          let intersection: "origin" | { slot: number; id: string } = "origin";
          let acknowledge = true;
          socket.send = (data) => {
            const request = JSON.parse(data);
            let result: unknown;
            if (request.method === "findIntersection") {
              intersection = request.params.points[0];
              acknowledge = true;
              result = { intersection, tip };
            } else if (acknowledge) {
              acknowledge = false;
              result = { direction: "backward", point: intersection };
            } else {
              if (intersection === "origin")
                throw new Error("Expected exact intersection");
              const slot = intersection.slot + spacing;
              result = {
                direction: "forward",
                block: {
                  slot,
                  id: pointHash(slot),
                  height: slot / spacing,
                  ancestor: intersection.id,
                  transactions: [],
                },
              };
            }
            queueMicrotask(() =>
              socket.emit("message", {
                data: JSON.stringify({
                  jsonrpc: "2.0",
                  id: request.id,
                  result,
                }),
              }),
            );
          };
          return socket;
        },
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({
        source,
        ...(depth === 1 ? { observationDepth: "inclusion" as const } : {}),
      });
      expect(boundary.kupoCheckpoint.blockNo).toBe(
        String(tip.height - depth + 1),
      );
      expect(boundary.kupoCheckpoint.slot).toBe(
        String(tipSlot - (depth - 1) * spacing),
      );
      expect(boundary.confirmationDepth).toBe(depth);
    },
  );
});
