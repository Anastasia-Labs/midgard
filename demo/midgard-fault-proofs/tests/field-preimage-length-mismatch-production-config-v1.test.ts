import { describe, expect, test, vi } from "vitest";

vi.mock(
  "../src/field-preimage-length-mismatch/production-evidence-v1.js",
  async (importOriginal) => {
    const original =
      await importOriginal<
        typeof import("../src/field-preimage-length-mismatch/production-evidence-v1.js")
      >();
    return {
      ...original,
      detectAuthenticatedFieldPreimageLengthEvidence: vi.fn(),
    };
  },
);

import {
  createFieldPreimageLengthLucidSubmission,
  FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS,
  type FieldPreimageLengthLucidBuilders,
  type ManifestBoundFieldPreimageLengthConfig,
  runManifestBoundFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/production-config-v1.js";
import { detectAuthenticatedFieldPreimageLengthEvidence } from "../src/field-preimage-length-mismatch/production-evidence-v1.js";
import {
  type ManifestBoundFieldPreimageLengthWorkflow,
  runOrResumeManifestBoundFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/production-workflow-v1.js";
import type {
  FieldPreimageLengthJournal,
  PreparedFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/workflow-v1.js";

const hash = (byte: string, bytes: number): string => byte.repeat(bytes * 2);
const transactionId = hash("a", 32);
const headerHash = hash("b", 28);

const prepared = (
  direction: PreparedFieldPreimageLengthWorkflow["direction"],
): PreparedFieldPreimageLengthWorkflow => ({
  schemaVersion: "midgard-field-preimage-length-mismatch-workflow-v1",
  headerHash,
  transactionId: hash("c", 32),
  direction,
  fieldIndex: 3,
  declaredLength: direction === "wrongfulAcceptance" ? 4 : 3,
  actualLength: 3,
  preimageHex: "010203",
  carriage: "Inline",
  evidenceDigest: hash("d", 32),
});

const config = {
  binding: { definition: { headerHash } },
} as unknown as ManifestBoundFieldPreimageLengthConfig;

const fieldMaterial = {
  nativeTxCompactCbor: "80",
  witnessSetCompactCbor: "80",
  itemCbors: [] as readonly string[],
};

const builders = (): {
  readonly value: FieldPreimageLengthLucidBuilders;
  readonly calls: Readonly<
    Record<keyof FieldPreimageLengthLucidBuilders, ReturnType<typeof vi.fn>>
  >;
} => {
  const calls = {
    init: vi.fn(async () => transactionId),
    dispatchAccepted: vi.fn(async () => transactionId),
    dispatchForced: vi.fn(async () => transactionId),
    authenticateAccepted: vi.fn(async () => transactionId),
    authenticateForced: vi.fn(async () => transactionId),
    finalize: vi.fn(async () => transactionId),
    remove: vi.fn(async () => transactionId),
    cancelDispatch: vi.fn(async () => transactionId),
    cancelAuthentication: vi.fn(async () => transactionId),
    cancelTerminal: vi.fn(async () => transactionId),
  };
  return { value: calls, calls };
};

describe("field-preimage-length production wiring", () => {
  test("names all four distinct physical manifest identities", () => {
    expect(
      Object.values(FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS).slice(0, 4),
    ).toEqual([
      "fraudProofFieldPreimageLengthMismatch",
      "fraudProofFieldPreimageLengthMismatchStep02Accepted",
      "fraudProofFieldPreimageLengthMismatchStep02Forced",
      "fraudProofFieldPreimageLengthMismatchStep03",
    ]);
    expect(
      new Set(
        Object.values(FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS).slice(0, 4),
      ).size,
    ).toBe(4);
  });

  test.each([
    ["wrongfulAcceptance", "dispatch", "dispatchAccepted"],
    ["wrongfulRejection", "dispatch", "dispatchForced"],
    ["wrongfulAcceptance", "authenticate", "authenticateAccepted"],
    ["wrongfulRejection", "authenticate", "authenticateForced"],
  ] as const)("routes %s %s to %s", async (direction, action, selected) => {
    const fixture = builders();
    const submission = createFieldPreimageLengthLucidSubmission({
      config,
      builders: fixture.value,
    });
    await expect(submission.submit(action, prepared(direction))).resolves.toBe(
      transactionId,
    );
    expect(fixture.calls[selected]).toHaveBeenCalledOnce();
  });

  test.each([
    ["init", "init"],
    ["finalize", "finalize"],
    ["remove", "remove"],
    ["cancelDispatch", "cancelDispatch"],
    ["cancelAuthentication", "cancelAuthentication"],
    ["cancelTerminal", "cancelTerminal"],
  ] as const)(
    "exposes %s through its concrete builder slot",
    async (action, selected) => {
      const fixture = builders();
      const submission = createFieldPreimageLengthLucidSubmission({
        config,
        builders: fixture.value,
      });
      await submission.submit(action, prepared("wrongfulAcceptance"));
      expect(fixture.calls[selected]).toHaveBeenCalledOnce();
    },
  );

  test("rejects cross-header replay before a builder sees it", async () => {
    const fixture = builders();
    const submission = createFieldPreimageLengthLucidSubmission({
      config,
      builders: fixture.value,
    });
    await expect(
      submission.submit("init", {
        ...prepared("wrongfulAcceptance"),
        headerHash: hash("e", 28),
      }),
    ).rejects.toThrow("different manifest-bound header");
    expect(fixture.calls.init).not.toHaveBeenCalled();
  });

  test("rejects a provider identity that is not canonical transaction hex", async () => {
    const fixture = builders();
    fixture.calls.finalize.mockResolvedValueOnce("not-a-tx-id");
    const submission = createFieldPreimageLengthLucidSubmission({
      config,
      builders: fixture.value,
    });
    await expect(
      submission.submit("finalize", prepared("wrongfulAcceptance")),
    ).rejects.toThrow("non-canonical transaction id");
  });

  test("reconciles a captured identity on restart before invoking concrete builders", async () => {
    const fixture = builders();
    let journal: FieldPreimageLengthJournal = {
      prepared: prepared("wrongfulAcceptance"),
      confirmed: [] as readonly (
        | "init"
        | "dispatch"
        | "authenticate"
        | "finalize"
        | "remove"
      )[],
      transactionIds: { init: transactionId },
    };
    journal = await runManifestBoundFieldPreimageLengthWorkflow({
      config,
      builders: fixture.value,
      load: async () => journal,
      save: async (next) => {
        journal = next;
      },
      observeConfirmed: async () => true,
    });
    expect(fixture.calls.init).not.toHaveBeenCalled();
    expect(fixture.calls.dispatchAccepted).toHaveBeenCalledOnce();
    expect(fixture.calls.authenticateAccepted).toHaveBeenCalledOnce();
    expect(fixture.calls.finalize).toHaveBeenCalledOnce();
    expect(fixture.calls.remove).toHaveBeenCalledOnce();
    expect(journal.confirmed).toEqual([
      "init",
      "dispatch",
      "authenticate",
      "finalize",
      "remove",
    ]);
  });

  test("watcher-facing resume re-derives retained-DA evidence and refuses digest substitution", async () => {
    const canonical = prepared("wrongfulAcceptance");
    const deriveAuthenticatedEvidence = vi.mocked(
      detectAuthenticatedFieldPreimageLengthEvidence,
    );
    deriveAuthenticatedEvidence.mockResolvedValueOnce({
      prepared: canonical,
      fieldMaterial,
      stageEvidence: {},
    });
    const workflow = {
      workflowVersion:
        "midgard-field-preimage-length-mismatch-production-workflow-v1",
      config,
      l1: { observeHeader: vi.fn(async () => ({})) },
      resolveChainStage: vi.fn(),
      remove: vi.fn(),
    } as unknown as ManifestBoundFieldPreimageLengthWorkflow;
    const complete: FieldPreimageLengthJournal = {
      prepared: canonical,
      confirmed: ["init", "dispatch", "authenticate", "finalize", "remove"],
      transactionIds: {},
    };
    const journal = {
      load: vi.fn(async () => complete),
      save: vi.fn(),
      observeConfirmed: vi.fn(),
    };
    await expect(
      runOrResumeManifestBoundFieldPreimageLengthWorkflow({
        workflow,
        sources: [{} as never],
        journal,
      }),
    ).resolves.toEqual(complete);
    expect(deriveAuthenticatedEvidence).toHaveBeenCalledOnce();
    deriveAuthenticatedEvidence.mockResolvedValueOnce({
      prepared: { ...canonical, evidenceDigest: hash("e", 32) },
      fieldMaterial,
      stageEvidence: {},
    });
    await expect(
      runOrResumeManifestBoundFieldPreimageLengthWorkflow({
        workflow,
        sources: [{} as never],
        journal,
      }),
    ).rejects.toThrow(/digest differs/u);
  });

  test("watcher-facing execution refuses caller evidence and alternate authority fields", async () => {
    const workflow = {
      workflowVersion:
        "midgard-field-preimage-length-mismatch-production-workflow-v1",
      config,
      l1: { observeHeader: vi.fn() },
      resolveChainStage: vi.fn(),
      remove: vi.fn(),
    } as unknown as ManifestBoundFieldPreimageLengthWorkflow;
    await expect(
      runOrResumeManifestBoundFieldPreimageLengthWorkflow({
        workflow,
        sources: [{} as never],
        journal: {} as never,
        prepared: prepared("wrongfulAcceptance"),
        deriveAuthenticatedEvidence: vi.fn(),
      } as never),
    ).rejects.toThrow(/rejects caller-authored evidence/u);
    expect(workflow.l1.observeHeader).not.toHaveBeenCalled();
  });
});
