import { mkdir, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { expect, it, vi } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import {
  finalizePendingJourneyEvidence,
  JOURNEY_WORKFLOW_STALL_ALLOWANCE_MS,
  journeyAnchoredEvidence,
  journeyWorkflowProgressCount,
  journeyWorkflowUpdates,
  verifyJourneyComputationThreadAbsent,
  verifyJourneyCorrection,
  verifyJourneyWorkflowTransactions,
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

const identity = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "12".repeat(32),
  category: "transitionTrace",
  target: { kind: "state_queue_header", headerHash: "34".repeat(28) },
} as const;

const entry = (
  sequence: number,
  event: FraudProofWorkflowJournalEntry["event"],
): FraudProofWorkflowJournalEntry => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  workflowId: computeFraudProofWorkflowId(identity),
  identity,
  sequence,
  recordedAt: "2026-09-11T05:00:00.000Z",
  event,
});

const baseline = [entry(0, { kind: "stalled", reason: "previous attempt" })];

it("resumes after an immutable historical failure without hiding a new failure", () => {
  expect(journeyWorkflowUpdates(baseline, baseline)).toEqual([]);
  const pending = entry(1, {
    kind: "reconciled",
    actionId: "step_01",
    txHash: "56".repeat(32),
    outcome: "pending",
  });
  expect(journeyWorkflowUpdates([...baseline, pending], baseline)).toEqual([
    pending,
  ]);
  expect(() =>
    journeyWorkflowUpdates(
      [
        ...baseline,
        pending,
        entry(2, { kind: "stalled", reason: "new failure" }),
      ],
      baseline,
      1,
    ),
  ).toThrow("Workflow stalled: new failure");
});

it("tolerates a stall the workflow is still retrying inside the allowance", () => {
  const at = (sequence: number, recordedAt: string, reason: string) => ({
    ...entry(sequence, { kind: "stalled", reason }),
    recordedAt,
  });
  const prepared = entry(1, {
    kind: "reconciled",
    actionId: "init:ab#0",
    txHash: "78".repeat(32),
    outcome: "pending",
  });
  const stalls = [
    at(2, "2026-09-12T15:45:25.000Z", "preflight failed for init:ab#0"),
    at(3, "2026-09-12T15:45:26.000Z", "preflight failed for init:ab#0"),
  ];
  const now = Date.parse("2026-09-12T15:47:00.000Z");
  const stall = { now, allowanceMs: 600_000 };
  expect(
    journeyWorkflowUpdates(
      [...baseline, prepared, ...stalls],
      baseline,
      1,
      stall,
    ),
  ).toEqual(stalls);
  expect(() =>
    journeyWorkflowUpdates([...baseline, prepared, ...stalls], baseline, 1, {
      now: now + 600_000,
      allowanceMs: 600_000,
    }),
  ).toThrow("Workflow stalled: preflight failed for init:ab#0");
  const moved = entry(4, {
    kind: "reconciled",
    actionId: "init:cd#0",
    txHash: "56".repeat(32),
    outcome: "pending",
  });
  expect(
    journeyWorkflowUpdates(
      [...baseline, prepared, ...stalls, moved],
      baseline,
      1,
      { now: now + 3_600_000, allowanceMs: 600_000 },
    ),
  ).toEqual([...stalls, moved]);
});

it("rejects a changed or truncated pre-launch journal prefix", () => {
  expect(() => journeyWorkflowUpdates([], baseline)).toThrow();
  expect(() =>
    journeyWorkflowUpdates(
      [entry(0, { kind: "stalled", reason: "rewritten" })],
      baseline,
    ),
  ).toThrow();
});

it("counts durable workflow progress without the per-block observations", () => {
  const observation = entry(5, {
    kind: "reconciled",
    actionId: "init:cd#0",
    txHash: "56".repeat(32),
    outcome: "pending",
  });
  const stall = entry(6, { kind: "stalled", reason: "preflight failed" });
  expect(journeyWorkflowProgressCount([])).toBe(0);
  expect(journeyWorkflowProgressCount([observation, observation])).toBe(0);
  expect(
    journeyWorkflowProgressCount([
      entry(7, {
        kind: "reconciled",
        actionId: "unknown-intent",
        txHash: "78".repeat(32),
        outcome: "not_found",
      }),
    ]),
  ).toBe(0);
  expect(journeyWorkflowProgressCount([observation, stall, observation])).toBe(
    1,
  );
});

it("allows the watcher's whole preflight retry budget before failing a stall", () => {
  expect(JOURNEY_WORKFLOW_STALL_ALLOWANCE_MS).toBe(31 * 60_000);
});

const pollCorrectionProgress = async (
  retained: readonly FraudProofWorkflowJournalEntry[],
  observations: readonly {
    at: number;
    records: readonly FraudProofWorkflowJournalEntry[];
  }[],
  reconciliationAllowanceMs?: number,
) => {
  const directory = await mkdtemp(join(tmpdir(), "journey-progress-"));
  await mkdir(
    join(
      directory,
      "fault-proofs",
      identity.category,
      identity.target.headerHash,
      computeFraudProofWorkflowId(identity),
    ),
    { recursive: true },
  );
  const load = vi.spyOn(
    DirectoryFraudProofWorkflowJournalStore.prototype,
    "load",
  );
  const now = vi.spyOn(Date, "now").mockReturnValue(0);
  const hardTimeout = new Error("whole correction timeout");
  type Input = Parameters<typeof verifyJourneyCorrection>[0];
  try {
    await verifyJourneyCorrection({
      context: {
        deployment: {},
        provider: {},
        accounts: {},
      } as Input["context"],
      native: {} as Input["native"],
      workflowJournalDirectory: directory,
      directory,
      category: identity.category,
      headerHash: identity.target.headerHash,
      predecessorHeaderHash: "78".repeat(28),
      workflowBaseline: retained,
      actionDepth: 1,
      correctionTimeoutMs: 4_560_000,
      progressAllowanceMs: 190_000,
      reconciliationAllowanceMs,
      operatorVkey: "90".repeat(28),
      requireLive: () => undefined,
      stage: async (_name, action) => await action(),
      poll: async (_name, action, timeoutMs) => {
        expect(timeoutMs).toBe(4_560_000);
        for (const observation of observations) {
          now.mockReturnValue(observation.at);
          load.mockResolvedValue(observation.records);
          expect(await action()).toBeUndefined();
        }
        throw hardTimeout;
      },
    });
  } finally {
    const observationsRead = load.mock.calls.length;
    now.mockRestore();
    load.mockRestore();
    await rm(directory, { recursive: true, force: true });
    expect(observationsRead).toBe(observations.length);
  }
};

it.each(["started", "submission_intent"] as const)(
  "allows retained %s catch-up, then enforces the transaction progress clock",
  async (kind) => {
    const retained = [
      entry(
        0,
        kind === "started"
          ? { kind }
          : {
              kind,
              actionId: "init",
              actionInput: {},
              attempt: 1,
              txHash: "ab".repeat(32),
            },
      ),
    ];
    const observed = [
      ...retained,
      entry(1, {
        kind: "reconciled",
        actionId: "init",
        txHash: "ab".repeat(32),
        outcome: "pending",
      }),
    ];
    const progressed = [
      ...observed,
      entry(2, {
        kind: "submitted",
        actionId: "init",
        attempt: 1,
        txHash: "ab".repeat(32),
      }),
    ];
    await expect(
      pollCorrectionProgress(retained, [
        { at: 190_001, records: retained },
        { at: 900_000, records: observed },
        { at: 900_001, records: progressed },
        { at: 1_090_001, records: progressed },
        { at: 1_090_002, records: progressed },
      ]),
    ).rejects.toThrow("no durable progress for 190000 ms");
  },
);

it("bounds retained catch-up at fifteen minutes despite reconciled observations", async () => {
  const retained = [entry(0, { kind: "started" })];
  const observed = [
    ...retained,
    entry(1, {
      kind: "reconciled",
      actionId: "init",
      txHash: "ab".repeat(32),
      outcome: "pending",
    }),
  ];
  await expect(
    pollCorrectionProgress(retained, [
      { at: 190_001, records: observed },
      { at: 900_001, records: observed },
    ]),
  ).rejects.toThrow("no durable progress for 900000 ms");
});

it("keeps the initial transaction progress allowance for a fresh workflow", async () => {
  await expect(
    pollCorrectionProgress([], [{ at: 190_001, records: [] }]),
  ).rejects.toThrow("no durable progress for 190000 ms");
});

it("retains the whole correction hard cap while waiting for resumed progress", async () => {
  const retained = [entry(0, { kind: "started" })];
  await expect(
    pollCorrectionProgress(retained, [{ at: 190_001, records: retained }]),
  ).rejects.toThrow("whole correction timeout");
  const pending = [
    ...retained,
    entry(1, {
      kind: "submission_intent",
      actionId: "init",
      actionInput: {},
      attempt: 1,
      txHash: "ab".repeat(32),
    }),
  ];
  await expect(
    pollCorrectionProgress(
      pending,
      [{ at: 1_000_000, records: pending }],
      1_230_000,
    ),
  ).rejects.toThrow("whole correction timeout");
});

it("bounds pending signed-intent reconciliation without resetting on repeated observations", async () => {
  const txHash = "ab".repeat(32);
  const submitted = [
    entry(0, { kind: "started" }),
    entry(1, {
      kind: "submission_intent",
      actionId: "init:old#0",
      actionInput: {},
      attempt: 1,
      txHash,
    }),
    entry(2, { kind: "submitted", actionId: "init:old#0", attempt: 1, txHash }),
  ];
  const observed = [
    ...submitted,
    entry(3, {
      kind: "reconciled",
      actionId: "init:old#0",
      txHash,
      outcome: "pending",
    }),
  ];
  const observedAgain = [
    ...observed,
    entry(4, {
      kind: "reconciled",
      actionId: "init:old#0",
      txHash,
      outcome: "pending",
    }),
  ];
  await expect(
    pollCorrectionProgress(
      [],
      [
        { at: 1, records: submitted },
        { at: 190_002, records: observed },
        { at: 1_230_001, records: observedAgain },
        { at: 1_230_002, records: observedAgain },
      ],
      1_230_000,
    ),
  ).rejects.toThrow("no durable progress for 1230000 ms");
});

it("grants one ordinary replacement budget after abandonment without extending it for duplicate resolutions", async () => {
  const txHash = "ab".repeat(32);
  const submitted = [
    entry(0, { kind: "started" }),
    entry(1, {
      kind: "submission_intent",
      actionId: "init:old#0",
      actionInput: {},
      attempt: 1,
      txHash,
    }),
    entry(2, { kind: "submitted", actionId: "init:old#0", attempt: 1, txHash }),
  ];
  const abandoned = [
    ...submitted,
    entry(3, {
      kind: "reconciled",
      actionId: "init:old#0",
      txHash,
      outcome: "not_found",
    }),
  ];
  const duplicate = [
    ...abandoned,
    entry(4, {
      kind: "reconciled",
      actionId: "init:old#0",
      txHash,
      outcome: "not_found",
    }),
  ];
  expect(journeyWorkflowProgressCount(abandoned)).toBe(4);
  expect(journeyWorkflowProgressCount(duplicate)).toBe(4);
  await expect(
    pollCorrectionProgress(
      [],
      [
        { at: 1, records: submitted },
        { at: 600_000, records: abandoned },
        { at: 790_000, records: duplicate },
        { at: 790_001, records: duplicate },
      ],
      1_230_000,
    ),
  ).rejects.toThrow("no durable progress for 190000 ms");
});

it("keeps the reconciliation allowance through inclusion until the funding confirmation handoff ends", async () => {
  const txHash = "ab".repeat(32);
  const submitted = [
    entry(0, { kind: "started" }),
    entry(1, {
      kind: "submission_intent",
      actionId: "init",
      actionInput: {},
      attempt: 1,
      txHash,
    }),
    entry(2, { kind: "submitted", actionId: "init", attempt: 1, txHash }),
  ];
  const included = [
    ...submitted,
    entry(3, {
      kind: "reconciled",
      actionId: "init",
      txHash,
      outcome: "confirmed",
    }),
  ];
  const confirmed = [
    ...included,
    entry(4, { kind: "confirmed", actionId: "init", txHash }),
  ];
  await expect(
    pollCorrectionProgress(
      [],
      [
        { at: 1, records: submitted },
        { at: 600_000, records: included },
        { at: 600_001, records: confirmed },
        { at: 790_001, records: confirmed },
        { at: 790_002, records: confirmed },
      ],
      1_230_000,
    ),
  ).rejects.toThrow("no durable progress for 190000 ms");
});

const terminal: FraudProofWorkflowTerminal = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  category: identity.category,
  headerHash: identity.target.headerHash,
  proofToken: {
    unit: "ab".repeat(28),
    outRef: "cd".repeat(32) + "#0",
    createdByTxHash: "cd".repeat(32),
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash: "ef".repeat(32),
    removedStateQueueOutRef: "12".repeat(32) + "#0",
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: "cd".repeat(32) + "#0",
  },
  economics: {
    operatorCredential: "12".repeat(28),
    proverCredential: "34".repeat(28),
    operatorBondInputOutRef: null,
    operatorBondInputLovelace: "0",
    slashedLovelace: "0",
    proverRewardOutputOutRef: null,
    proverRewardLovelace: "0",
    removalFeeLovelace: "200000",
    duplicateRewardAbsent: true,
  },
  observedAt: {
    slot: "1234",
    blockHash: "56".repeat(32),
    confirmationDepth: 30,
  },
};
const completedEntry = (value = terminal) =>
  entry(9, {
    kind: "completed",
    terminal: value,
    terminalDigest: journalJsonDigest(value),
  });

it("never promotes terminal inclusion into a finalized evidence stamp", () => {
  const included = entry(8, {
    kind: "terminal_included",
    terminal,
    terminalDigest: journalJsonDigest(terminal),
  });
  expect(journeyAnchoredEvidence([...baseline, included])).toBeUndefined();
  expect(
    journeyAnchoredEvidence([...baseline, included, completedEntry()]),
  ).toEqual(terminal);
  expect(() =>
    journeyAnchoredEvidence([completedEntry(), completedEntry()]),
  ).toThrow();
});

it("resumes pending stamps across family boundaries and accepts a reauthenticated replacement point", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-anchor-"));
  const familyDirectory = join(directory, "transition-trace");
  const workflowJournalDirectory = join(directory, "workflows");
  await mkdir(familyDirectory);
  await mkdir(
    join(
      workflowJournalDirectory,
      "fault-proofs",
      identity.category,
      identity.target.headerHash,
      computeFraudProofWorkflowId(identity),
    ),
    { recursive: true },
  );
  const request = {
    category: identity.category,
    headerHash: identity.target.headerHash,
    deploymentFingerprint: identity.deploymentFingerprint,
    releaseFinalityPolicyDigest: "78".repeat(32),
    finalityDepth: 30,
    completedAtConfirmationDepth: 1,
    terminalObservedAt: "2026-09-13T00:00:00Z",
    successorTxHash: "90".repeat(32),
  };
  const input = {
    ...request,
    journeysDirectory: directory,
    workflowJournalDirectory,
    nativeEvidencePath: join(directory, "native-chain.ndjson"),
    authenticate: vi.fn().mockResolvedValue(false),
  };
  const load = vi.spyOn(
    DirectoryFraudProofWorkflowJournalStore.prototype,
    "load",
  );
  try {
    await writeJourneyArtifact(
      join(familyDirectory, "pending-evidence-stamp.json"),
      request,
    );
    load.mockResolvedValue([]);
    expect(await finalizePendingJourneyEvidence(input)).toBe(1);
    const replacement = {
      ...terminal,
      observedAt: { ...terminal.observedAt, slot: "9999" },
    };
    load.mockResolvedValue([completedEntry(replacement)]);
    expect(await finalizePendingJourneyEvidence(input)).toBe(1);
    // A resumed batch has its own complete raw capture; pending stamps may
    // use it, while a completed stamp keeps its original evidence path.
    input.nativeEvidencePath = join(
      directory,
      "session-2",
      "native-chain.ndjson",
    );
    input.authenticate.mockResolvedValue(true);
    expect(await finalizePendingJourneyEvidence(input)).toBe(0);
    const stampPath = join(familyDirectory, "finalized-evidence-stamp.json");
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
    load.mockRejectedValue(
      new Error("already stamped journals must not be read"),
    );
    expect(await finalizePendingJourneyEvidence(input)).toBe(0);
    expect(await readJourneyArtifact(stampPath)).toEqual(stamp);
  } finally {
    load.mockRestore();
    await rm(directory, { recursive: true, force: true });
  }
});

it("refuses a completed terminal below the bound release depth", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-anchor-depth-"));
  const familyDirectory = join(directory, "transition-trace");
  const workflowJournalDirectory = join(directory, "workflows");
  await mkdir(familyDirectory);
  await mkdir(
    join(
      workflowJournalDirectory,
      "fault-proofs",
      identity.category,
      identity.target.headerHash,
      computeFraudProofWorkflowId(identity),
    ),
    { recursive: true },
  );
  const request = {
    category: identity.category,
    headerHash: identity.target.headerHash,
    deploymentFingerprint: identity.deploymentFingerprint,
    releaseFinalityPolicyDigest: "78".repeat(32),
    finalityDepth: 30,
    completedAtConfirmationDepth: 1,
    terminalObservedAt: "2026-09-13T00:00:00Z",
    successorTxHash: "90".repeat(32),
  };
  const load = vi
    .spyOn(DirectoryFraudProofWorkflowJournalStore.prototype, "load")
    .mockResolvedValue([
      completedEntry({
        ...terminal,
        observedAt: { ...terminal.observedAt, confirmationDepth: 1 },
      }),
    ]);
  try {
    await writeJourneyArtifact(
      join(familyDirectory, "pending-evidence-stamp.json"),
      request,
    );
    await expect(
      finalizePendingJourneyEvidence({
        ...request,
        journeysDirectory: directory,
        workflowJournalDirectory,
        nativeEvidencePath: join(directory, "native-chain.ndjson"),
        authenticate: async () => true,
      }),
    ).rejects.toThrow();
  } finally {
    load.mockRestore();
    await rm(directory, { recursive: true, force: true });
  }
});

it("authenticates recovered submitted actions even when a confirmed event is absent", async () => {
  const txHash = "ab".repeat(32);
  const records = [
    entry(1, {
      kind: "submission_intent",
      actionId: "init",
      actionInput: {},
      attempt: 1,
      txHash,
    }),
  ];
  const authenticate = vi.fn().mockResolvedValue({ txHash });
  await verifyJourneyWorkflowTransactions(records, authenticate);
  expect(authenticate).toHaveBeenCalledWith(txHash);
  authenticate.mockRejectedValue(
    new Error("transaction absent from canonical chain"),
  );
  await expect(
    verifyJourneyWorkflowTransactions(records, authenticate),
  ).rejects.toThrow("transaction absent from canonical chain");
});

it.each([false, true])(
  "authenticates the replacement after exact abandonment, same action identity=%s",
  async (sameAction) => {
    const abandonedHash = "ab".repeat(32);
    const includedHash = "cd".repeat(32);
    const oldAction = "init:old-reference#0";
    const nextAction = sameAction ? oldAction : "init:new-reference#0";
    const records = [
      entry(0, {
        kind: "submission_intent",
        actionId: oldAction,
        actionInput: {},
        attempt: 1,
        txHash: abandonedHash,
      }),
      entry(1, {
        kind: "reconciled",
        actionId: oldAction,
        txHash: abandonedHash,
        outcome: "not_found",
      }),
      entry(2, {
        kind: "submission_intent",
        actionId: nextAction,
        actionInput: {},
        attempt: 2,
        txHash: includedHash,
      }),
      entry(3, {
        kind: "confirmed",
        actionId: nextAction,
        txHash: includedHash,
      }),
      // A late observation about an older attempt must not discard its replacement.
      entry(4, {
        kind: "reconciled",
        actionId: oldAction,
        txHash: abandonedHash,
        outcome: "not_found",
      }),
    ];
    const authenticate = vi.fn(async (txHash: string) => {
      if (txHash !== includedHash)
        throw new Error("old intent never reached the canonical chain");
    });
    await verifyJourneyWorkflowTransactions(records, authenticate);
    expect(authenticate).toHaveBeenCalledExactlyOnceWith(includedHash);
    await expect(
      verifyJourneyWorkflowTransactions(
        [
          ...records,
          entry(5, {
            kind: "confirmed",
            actionId: "unsubmitted",
            txHash: "ef".repeat(32),
          }),
        ],
        authenticate,
      ),
    ).rejects.toThrow();
  },
);
