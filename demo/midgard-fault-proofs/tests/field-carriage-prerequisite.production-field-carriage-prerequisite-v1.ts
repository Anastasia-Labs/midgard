import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  FIELD_CARRIAGE_RECOVERY,
  withFieldCarriagePrerequisite,
} from "../src/workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.js";
import { createRawDatumPreimageRequirement } from "../src/workflow/raw-datum-preimage.js";
import { withRawDatumPreimagePrerequisite } from "../src/workflow/raw-datum-preimage-prerequisite.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  base,
  baseAction,
  certificateAction,
  confirmedCarriage,
  context,
  headerHash,
  outRefReader,
  prerequisite,
  publicationAction,
  txHash,
} from "./field-carriage-prerequisite.prerequisite.js";

describe("production field-carriage prerequisite V1", () => {
  it("reads each journaled chunk back by its out-ref and reauthenticates it on every call", async () => {
    const requirement = createRawDatumPreimageRequirement({
      preimage: Buffer.concat([
        Buffer.alloc(15_000, 1),
        Buffer.alloc(15_000, 2),
        Buffer.alloc(1, 3),
      ]),
    });
    const signer = {
      source: "test",
      address: "addr_test1_field_publication",
      paymentKeyHash: "12".repeat(28),
      selectWallet: () => undefined,
    };
    const publicationTxHash = (index: number) =>
      (index + 1).toString(16).padStart(2, "0").repeat(32);
    let candidates: UTxO[] = requirement.publicationDatums.map(
      (datum, index) => ({
        txHash: publicationTxHash(index),
        outputIndex: 0,
        address: signer.address,
        datum,
        assets: { lovelace: 2_000_000n },
      }),
    );
    const utxosByOutRef = vi.fn(
      async (refs: readonly { txHash: string; outputIndex: number }[]) =>
        candidates.filter((candidate) =>
          refs.some(
            (ref) =>
              ref.txHash === candidate.txHash &&
              ref.outputIndex === candidate.outputIndex,
          ),
        ),
    );
    const observeExact = vi.fn(
      async ({ expectedOutRef }: { expectedOutRef: string }) => ({
        kind: "confirmed" as const,
        outRef: expectedOutRef,
      }),
    );
    const port = createAuthenticatedFieldCarriagePrerequisitePort({
      category: "nonExistentInput",
      lucid: { utxosByOutRef } as unknown as LucidEvolution,
      network: "Preview",
      signer,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact,
      },
      requirementForAction: () => requirement,
      transactionConfirmed: async () => true,
    });
    // Each chunk is published by its own action, in plan order, and the
    // journal keeps the out-ref its signed body gave the chunk.
    let entries: readonly FraudProofWorkflowJournalEntry[] = [];
    for (const [index, datumCbor] of requirement.publicationDatums.entries()) {
      const required = await port.inspect({
        headerHash,
        baseAction,
        artifact: context.artifact,
        entries,
      });
      if (required.kind !== "required")
        throw new Error("missing publication requirement");
      expect(required.action.input.publicationIndex).toBe(index);
      entries = confirmedCarriage(entries, {
        action: required.action,
        txHash: publicationTxHash(index),
        durableRecovery: {
          fieldCarriage: {
            schemaVersion: FIELD_CARRIAGE_RECOVERY,
            kind: "publication",
            requirementSha256: required.action.input
              .requirementSha256 as string,
            outRef: `${publicationTxHash(index)}#0`,
            datumCbor,
            unit: null,
          },
        },
      });
    }
    const journaled = candidates.map(
      ({ txHash: hash, outputIndex }) => `${hash}#${outputIndex.toString()}`,
    );
    const input = {
      headerHash,
      artifact: context.artifact,
      baseAction,
      entries,
    };
    utxosByOutRef.mockClear();
    observeExact.mockClear();
    await expect(port.inspect(input)).resolves.toEqual({ kind: "satisfied" });
    expect(observeExact).toHaveBeenCalledTimes(3);
    await expect(
      port.resolveAuthenticated({ ...input, action: baseAction }),
    ).resolves.toMatchObject({ publications: candidates });
    expect(observeExact).toHaveBeenCalledTimes(6);
    expect(
      utxosByOutRef.mock.calls.flatMap(([refs]) =>
        refs.map(
          ({ txHash: hash, outputIndex }) =>
            `${hash}#${outputIndex.toString()}`,
        ),
      ),
    ).toEqual([...journaled, ...journaled]);
    // The last chunk leaves the provider's view; an output elsewhere with the
    // same datum does not stand in for it.
    candidates = [
      ...candidates.slice(0, 2),
      { ...candidates[2]!, txHash: "ee".repeat(32) },
    ];
    await expect(port.inspect(input)).resolves.toMatchObject({
      kind: "pending",
    });
    await expect(
      port.resolveAuthenticated({ ...input, action: baseAction }),
    ).rejects.toThrow("unauthenticated field publication");
    // Stale provider candidates still cannot bypass fresh raw-L1 admission.
    observeExact.mockRejectedValueOnce(
      new Error("publication left the canonical chain"),
    );
    await expect(port.inspect(input)).rejects.toThrow(
      "publication left the canonical chain",
    );
  });

  it("waits for publication finality while refusing publication and proof preflight", async () => {
    const underlying = base();
    const port = prerequisite();
    vi.mocked(port.inspect).mockResolvedValue({
      kind: "pending",
      reason: "field publication is not release-final",
    });
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: port,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "pending",
      reason: "field publication is not release-final",
    });
    await expect(
      adapter.preflight({ ...context, action: baseAction }),
    ).rejects.toThrow("cannot bypass authenticated field carriage");
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("differs from current requirement");
    expect(port.capture).not.toHaveBeenCalled();
    expect(underlying.preflight).not.toHaveBeenCalled();
    expect(underlying.submit).not.toHaveBeenCalled();
    vi.mocked(port.inspect).mockResolvedValue({ kind: "satisfied" });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
  });

  it("re-exposes a journaled raw publication after its confirmed output rolls back", async () => {
    const requirement = createRawDatumPreimageRequirement({
      preimage: Buffer.from("8101", "hex"),
    });
    const signer = {
      source: "test",
      address: "addr_test1_field_publication",
      paymentKeyHash: "12".repeat(28),
      selectWallet: () => undefined,
    };
    let candidates: UTxO[] = [];
    let outputConfirmed = false;
    let included = false;
    const port = createAuthenticatedFieldCarriagePrerequisitePort({
      category: "nonExistentInput",
      lucid: outRefReader(() => candidates),
      network: "Preview",
      signer,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: async ({ expectedOutRef }) =>
          outputConfirmed
            ? { kind: "confirmed", outRef: expectedOutRef }
            : { kind: "not_found" },
      },
      requirementForAction: () => requirement,
      transactionConfirmed: async () => included,
    });
    const required = await port.inspect({
      headerHash,
      baseAction,
      artifact: context.artifact,
      entries: [],
    });
    if (required.kind !== "required")
      throw new Error("missing publication requirement");
    const identitySha256 = required.action.input.requirementSha256;
    if (typeof identitySha256 !== "string")
      throw new Error("missing requirement identity");
    const datumCbor = requirement.publicationDatums[0]!;
    const input = {
      headerHash,
      action: required.action,
      artifact: context.artifact,
      txHash,
      durableRecovery: {
        fieldCarriage: {
          schemaVersion: FIELD_CARRIAGE_RECOVERY,
          kind: "publication",
          requirementSha256: identitySha256,
          outRef: `${txHash}#0`,
          datumCbor,
          unit: null,
        },
      },
    } as const;
    // The workflow as it resumes with the publication journaled.
    const resumed = {
      ...context,
      entries: confirmedCarriage([], {
        action: required.action,
        txHash,
        durableRecovery: input.durableRecovery,
      }),
    };
    await expect(port.reconcile(input)).resolves.toEqual({
      kind: "pending",
      txHash,
    });
    candidates = [
      {
        txHash,
        outputIndex: 0,
        address: signer.address,
        assets: { lovelace: 2_000_000n },
        datum: datumCbor,
      },
    ];
    const underlying = base();
    const wrapped = withRawDatumPreimagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: port,
    });
    await expect(wrapped.observe(resumed)).resolves.toEqual({
      kind: "pending",
      reason:
        "nonExistentInput field publication 0 is not authenticated on the current chain",
    });
    await expect(
      wrapped.preflight({ ...resumed, action: baseAction }),
    ).rejects.toThrow("cannot bypass authenticated field carriage");
    included = true;
    await expect(port.reconcile(input)).resolves.toMatchObject({
      kind: "conflict",
      reason: expect.stringContaining("omitted its journaled output"),
    });
    outputConfirmed = true;
    await expect(port.reconcile(input)).resolves.toEqual({
      kind: "confirmed",
      txHash,
    });
    await expect(wrapped.observe(resumed)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    const previousCandidates = candidates;
    candidates = [];
    outputConfirmed = false;
    included = false;
    await expect(wrapped.observe(resumed)).resolves.toEqual({
      kind: "action_required",
      action: required.action,
    });
    await expect(
      wrapped.preflight({ ...resumed, action: baseAction }),
    ).rejects.toThrow("cannot bypass authenticated field carriage");
    await expect(port.reconcile(input)).resolves.toEqual({
      kind: "pending",
      txHash,
    });
    // A stale wallet candidate cannot revive the journal's old confirmation.
    candidates = previousCandidates;
    await expect(wrapped.observe(resumed)).resolves.toMatchObject({
      kind: "pending",
    });
    expect(underlying.preflight).not.toHaveBeenCalled();
    await expect(
      port.reconcile({
        ...input,
        durableRecovery: {
          fieldCarriage: {
            ...input.durableRecovery.fieldCarriage,
            datumCbor: "00",
          },
        },
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("keeps nested raw and field publication namespaces distinct", async () => {
    const fieldPort = prerequisite();
    const rawPort = prerequisite({ phase: "satisfied" });
    const adapter = withRawDatumPreimagePrerequisite({
      category: "nonExistentInput",
      base: withFieldCarriagePrerequisite({
        category: "nonExistentInput",
        base: base(),
        prerequisite: fieldPort,
      }),
      prerequisite: rawPort,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: publicationAction,
    });
    const preflight = await adapter.preflight({
      ...context,
      action: publicationAction,
    });
    expect(fieldPort.capture).toHaveBeenCalledOnce();
    expect(rawPort.capture).not.toHaveBeenCalled();
    await expect(
      adapter.submit({ ...context, action: publicationAction, preflight }),
    ).resolves.toEqual({ kind: "submitted", txHash });
    const reconcile = vi.fn(async () => ({
      kind: "confirmed" as const,
      txHash,
    }));
    const restarted = withRawDatumPreimagePrerequisite({
      category: "nonExistentInput",
      base: withFieldCarriagePrerequisite({
        category: "nonExistentInput",
        base: base(),
        prerequisite: prerequisite({ reconcile }),
      }),
      prerequisite: prerequisite({ phase: "satisfied" }),
    });
    await expect(
      restarted.reconcile({
        ...context,
        action: publicationAction,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    expect(reconcile).toHaveBeenCalledOnce();
  });

  it("journals the first raw publication and forbids direct step bypass", async () => {
    const underlying = base();
    const port = prerequisite();
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: port,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: publicationAction,
    });
    await expect(
      adapter.preflight({ ...context, action: baseAction }),
    ).rejects.toThrow("cannot bypass authenticated field carriage");
    expect(underlying.preflight).not.toHaveBeenCalled();
    const preflight = await adapter.preflight({
      ...context,
      action: publicationAction,
    });
    expect(preflight).toMatchObject({
      actionId: publicationAction.actionId,
      txHash,
      scriptExecution: "none",
      localUplcEvaluation: { status: "passed" },
    });
    await expect(
      adapter.submit({
        ...context,
        action: publicationAction,
        preflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash });
  });

  it("makes tier-3 certification another distinct action", async () => {
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: base(),
      prerequisite: prerequisite({ phase: "certificate" }),
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: certificateAction,
    });
    await expect(
      adapter.preflight({ ...context, action: certificateAction }),
    ).resolves.toMatchObject({ actionId: certificateAction.actionId, txHash });
  });

  it("delegates the proof step only after all field carriage is authenticated", async () => {
    const underlying = base();
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: prerequisite({ phase: "satisfied" }),
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    await adapter.preflight({ ...context, action: baseAction });
    expect(underlying.preflight).toHaveBeenCalledOnce();
  });

  it("reconciles after restart and rejects duplicate or substituted captures", async () => {
    const reconcile = vi.fn(async () => ({
      kind: "confirmed" as const,
      txHash,
    }));
    const port = prerequisite({ reconcile });
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: base(),
      prerequisite: port,
    });
    await expect(
      adapter.reconcile({
        ...context,
        action: publicationAction,
        txHash,
        durableRecovery: { persisted: true },
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    expect(reconcile).toHaveBeenCalledOnce();
    await adapter.preflight({ ...context, action: publicationAction });
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("already captured this action");
    await expect(
      adapter.preflight({
        ...context,
        action: {
          ...publicationAction,
          input: {
            ...publicationAction.input,
            requirementSha256: "ff".repeat(32),
          },
        },
      }),
    ).rejects.toThrow("differs from current requirement");
    expect(port.capture).toHaveBeenCalledOnce();
  });
});
