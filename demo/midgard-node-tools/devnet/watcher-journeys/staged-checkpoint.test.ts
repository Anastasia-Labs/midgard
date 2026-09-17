import { describe, expect, it } from "vitest";

import {
  classifyStagedCheckpoint,
  supersededCheckpointArchivePath,
} from "./staged-checkpoint.js";

describe("staged checkpoint disposition", () => {
  it("supersedes an unpublished build whose predecessor lost the tail", () => {
    expect(
      classifyStagedCheckpoint({
        checkpoint: {},
        faultHeaderOnQueue: false,
        predecessorIsTail: false,
      }),
    ).toBe("superseded");
  });

  it("resumes an unpublished build while its predecessor is still the tail", () => {
    expect(
      classifyStagedCheckpoint({
        checkpoint: {},
        faultHeaderOnQueue: false,
        predecessorIsTail: true,
      }),
    ).toBe("resume");
  });

  it("resumes every checkpoint that reached the chain or signed its commit", () => {
    const stale = { faultHeaderOnQueue: false, predecessorIsTail: false };
    expect(
      classifyStagedCheckpoint({
        checkpoint: { commitTxHash: "ab" },
        ...stale,
      }),
    ).toBe("resume");
    expect(
      classifyStagedCheckpoint({
        checkpoint: { signedCommit: { txHash: "ab", signedCbor: "cd" } },
        ...stale,
      }),
    ).toBe("resume");
    expect(
      classifyStagedCheckpoint({
        checkpoint: {},
        faultHeaderOnQueue: true,
        predecessorIsTail: false,
      }),
    ).toBe("resume");
  });

  it("archives under the journey directory with a sortable timestamp", () => {
    expect(
      supersededCheckpointArchivePath(
        "/run/work/journeys/invalidRange",
        new Date("2026-09-12T15:32:06.123Z"),
      ),
    ).toBe(
      "/run/work/journeys/invalidRange/archive/staged-superseded-2026-09-12T15-32-06-123Z.json",
    );
  });
});
