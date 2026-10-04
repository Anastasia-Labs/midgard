import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { LocalKupmiosTransportUnavailableError } from "../src/workflow/local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import {
  createLocalKupmiosFraudProofRawL1SnapshotAuthority,
  LocalKupmiosCheckpointChangedError,
} from "../src/workflow/local-kupmios-raw-l1-authority.js";
import {
  admit,
  fixture,
  releaseFinality,
} from "./workflow-raw-l1-snapshot.fixture.js";
import { localSource } from "./workflow-raw-l1-snapshot.raw-l1-snapshot-v1-admission.js";

describe("scoped local snapshot authority retries", () => {
  it("repins and discards partial address pages before retrying a transport failure", async () => {
    const value = fixture();
    const original = localSource(value);
    let boundaries = 0;
    const source = localSource(value, {
      readBoundary: async () => {
        boundaries++;
        return original.readBoundary();
      },
      scanAddressPage: async (input) => {
        if (boundaries === 1) {
          if (input.after !== null)
            throw new LocalKupmiosTransportUnavailableError(
              "socket dropped after the first page",
            );
          return {
            checkpoint: value.snapshot.cursor.point,
            utxos: value.snapshot.scopes[0]!.utxos,
            nextCursor: "old-page-2",
            complete: false,
          };
        }
        return original.scanAddressPage(input);
      },
    });
    const scope = createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    try {
      const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
        source,
        releaseFinality,
      });
      await expect(
        authority
          .capture(value.request, { scope })
          .then((result) => admit(result, value.request)),
      ).resolves.toEqual(value.snapshot);
      expect(boundaries).toBe(2);
    } finally {
      scope.close();
    }
  });

  it("shares the existing checkpoint allowance across transport retries", async () => {
    const value = fixture();
    const original = localSource(value);
    const errors = [
      new LocalKupmiosCheckpointChangedError("first head change"),
      new LocalKupmiosTransportUnavailableError("first drop"),
      new LocalKupmiosCheckpointChangedError("second head change"),
      new LocalKupmiosTransportUnavailableError("second drop"),
      new LocalKupmiosCheckpointChangedError("checkpoint allowance exhausted"),
    ];
    let boundaries = 0;
    const source = localSource(value, {
      readBoundary: async () => {
        const error = errors[boundaries++];
        if (error) throw error;
        return original.readBoundary();
      },
    });
    const scope = createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    try {
      const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
        source,
        releaseFinality,
      });
      await expect(authority.capture(value.request, { scope })).rejects.toBe(
        errors[4],
      );
      expect(boundaries).toBe(5);
    } finally {
      scope.close();
    }
  });

  it("does not retry a structurally invalid admitted boundary", async () => {
    const value = fixture();
    const readBoundary = vi.fn(async () => ({
      kupoCheckpoint: {
        ...value.snapshot.cursor.point,
        pointId: "ff".repeat(32),
      },
      ogmiosTip: value.snapshot.cursor.tip,
    }));
    const source = localSource(value, { readBoundary });
    const scope = createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    try {
      const authority = createLocalKupmiosFraudProofRawL1SnapshotAuthority({
        source,
        releaseFinality,
      });
      await expect(authority.capture(value.request, { scope })).rejects.toThrow(
        /pointId/u,
      );
      expect(readBoundary).toHaveBeenCalledTimes(1);
    } finally {
      scope.close();
    }
  });
});
