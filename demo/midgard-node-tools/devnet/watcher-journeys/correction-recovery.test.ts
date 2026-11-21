import { existsSync } from "node:fs";
import { mkdir, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk/core";
import { expect, it, vi } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import {
  finalizePendingJourneyEvidence,
  type JourneyChainCorrection,
  verifyJourneyComputationThreadAbsent,
} from "./correction.js";

const computationThreadFixture = () => {
  const policy = "ab".repeat(28);
  const category = "scriptIntegrityHashMismatch" as const;
  const headerHash = "cd".repeat(28);
  const assetName = (
    category: SDK.FraudProofCatalogueCategoryName,
    hash: string,
  ) => SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category] + hash;
  const target = assetName(category, headerHash);
  const check = () =>
    verifyJourneyComputationThreadAbsent({
      kupoUrl: "http://kupo.invalid",
      computationThreadPolicyId: policy,
      category,
      headerHash,
    });
  const respondWith = (unspentAssetNames: readonly string[]) =>
    vi.spyOn(globalThis, "fetch").mockImplementation(async (input) => {
      const url = new URL(String(input));
      expect(url.search).toBe("?unspent");
      const pattern = url.pathname.slice("/matches/".length);
      return Response.json(
        unspentAssetNames
          .filter(
            (name) =>
              pattern === `${policy}.*` || pattern === `${policy}.${name}`,
          )
          .map((name) => ({
            transaction_id: "ef".repeat(32),
            output_index: 0,
            value: { assets: { [`${policy}.${name}`]: 1 } },
          })),
      );
    });
  return {
    policy,
    category,
    headerHash,
    assetName,
    target,
    check,
    respondWith,
  };
};

it.each(["other family", "same family, other header"])(
  "allows an unspent computation thread for %s when the corrected target thread is absent",
  async (scenario) => {
    const f = computationThreadFixture();
    const unrelated =
      scenario === "other family"
        ? f.assetName("distinctAssetAccumulationLimit", f.headerHash)
        : f.assetName(f.category, "12".repeat(28));
    const fetch = f.respondWith([unrelated]);
    try {
      await expect(f.check()).resolves.toBeUndefined();
      expect(fetch).toHaveBeenCalledExactlyOnceWith(
        `http://kupo.invalid/matches/${f.policy}.${f.target}?unspent`,
      );
    } finally {
      fetch.mockRestore();
    }
  },
);

it("rejects an unspent computation thread for the exact corrected category and header", async () => {
  const f = computationThreadFixture();
  const fetch = f.respondWith([
    f.assetName("distinctAssetAccumulationLimit", "12".repeat(28)),
    f.target,
  ]);
  try {
    await expect(f.check()).rejects.toThrow();
  } finally {
    fetch.mockRestore();
  }
});

it("rejects a failed computation-thread observation even if its body is empty", async () => {
  const f = computationThreadFixture();
  const fetch = vi
    .spyOn(globalThis, "fetch")
    .mockResolvedValue(Response.json([], { status: 503 }));
  try {
    await expect(f.check()).rejects.toThrow();
  } finally {
    fetch.mockRestore();
  }
});

const target = {
  category: "transitionTrace",
  headerHash: "34".repeat(28),
} as const;

/** A correction as the chain holds it at the release depth. */
const correction: JourneyChainCorrection = {
  ...target,
  proofToken: {
    unit: `${"ab".repeat(28)}0100${target.headerHash}`,
    outRef: `${"11".repeat(32)}#0`,
    createdByTxHash: "11".repeat(32),
  },
  correction: {
    removalTxHash: "22".repeat(32),
    removedStateQueueOutRef: `${"33".repeat(32)}#0`,
  },
  economics: {
    operatorBondInputOutRef: `${"44".repeat(32)}#0`,
    operatorBondInputLovelace: "5000000",
    slashTxHash: "55".repeat(32),
    slashedLovelace: "2000000",
    proverRewardOutputOutRef: `${"55".repeat(32)}#0`,
    proverRewardLovelace: "3000000",
  },
  observedAt: { slot: 1234, blockHash: "66".repeat(32), confirmationDepth: 30 },
};

/** One family's durable pending stamp over a correction each test sets. */
const pendingStampFixture = async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-anchor-"));
  const familyDirectory = join(directory, "transition-trace");
  await mkdir(familyDirectory);
  const request = {
    ...target,
    deploymentFingerprint: "12".repeat(32),
    releaseFinalityPolicyDigest: "78".repeat(32),
    finalityDepth: 30,
    completedAtConfirmationDepth: 1,
    terminalObservedAt: "2026-09-13T00:00:00Z",
    successorTxHash: "90".repeat(32),
  };
  await writeJourneyArtifact(
    join(familyDirectory, "pending-evidence-stamp.json"),
    request,
  );
  let answer: () => JourneyChainCorrection | undefined = () => undefined;
  return {
    request,
    stampPath: join(familyDirectory, "finalized-evidence-stamp.json"),
    input: {
      ...request,
      journeysDirectory: directory,
      readCorrection: vi.fn(async () => answer()),
      nativeEvidencePath: join(directory, "native-chain.ndjson"),
      authenticate: vi.fn().mockResolvedValue(false),
    },
    serve: (value: () => JourneyChainCorrection | undefined) => {
      answer = value;
    },
    cleanup: () => rm(directory, { recursive: true, force: true }),
  };
};

it("resumes pending stamps across family boundaries and accepts a reauthenticated replacement point", async () => {
  const { request, stampPath, input, serve, cleanup } =
    await pendingStampFixture();
  try {
    expect(await finalizePendingJourneyEvidence(input)).toBe(1);
    expect(input.readCorrection).toHaveBeenCalledWith(request);
    // A rollback re-included the removal at a new point.
    const replacement = {
      ...correction,
      observedAt: { ...correction.observedAt, slot: 9999 },
    };
    serve(() => replacement);
    expect(await finalizePendingJourneyEvidence(input)).toBe(1);
    // A resumed batch has its own complete raw capture; pending stamps may
    // use it, while a completed stamp keeps its original evidence path.
    const directory = input.journeysDirectory;
    input.nativeEvidencePath = join(
      directory,
      "session-2",
      "native-chain.ndjson",
    );
    input.authenticate.mockResolvedValue(true);
    expect(await finalizePendingJourneyEvidence(input)).toBe(0);
    const stamp = await readJourneyArtifact(stampPath);
    expect(stamp).toMatchObject({
      ...request,
      terminal: replacement,
      nativeEvidencePath: input.nativeEvidencePath,
    });
    input.nativeEvidencePath = join(
      directory,
      "session-3",
      "native-chain.ndjson",
    );
    serve(() => {
      throw new Error("already stamped corrections must not be read");
    });
    expect(await finalizePendingJourneyEvidence(input)).toBe(0);
    expect(await readJourneyArtifact(stampPath)).toEqual(stamp);
  } finally {
    await cleanup();
  }
});

it.each([true, false])(
  "stamps a shallow correction only once the native recorder authenticates its release depth (%s)",
  async (authenticated) => {
    const { request, stampPath, input, serve, cleanup } =
      await pendingStampFixture();
    const shallow = {
      ...correction,
      observedAt: { ...correction.observedAt, confirmationDepth: 1 },
    };
    serve(() => shallow);
    input.authenticate.mockResolvedValue(authenticated);
    try {
      expect(await finalizePendingJourneyEvidence(input)).toBe(
        authenticated ? 0 : 1,
      );
      expect(input.authenticate).toHaveBeenCalledExactlyOnceWith(
        request,
        shallow,
      );
      if (!authenticated) {
        expect(existsSync(stampPath)).toBe(false);
        return;
      }
      expect(await readJourneyArtifact(stampPath)).toMatchObject({
        ...request,
        terminal: shallow,
      });
    } finally {
      await cleanup();
    }
  },
);

it("refuses a correction the chain read names for another header", async () => {
  const { stampPath, input, serve, cleanup } = await pendingStampFixture();
  serve(() => ({ ...correction, headerHash: "35".repeat(28) }));
  input.authenticate.mockResolvedValue(true);
  try {
    await expect(finalizePendingJourneyEvidence(input)).rejects.toThrow();
    expect(existsSync(stampPath)).toBe(false);
  } finally {
    await cleanup();
  }
});
