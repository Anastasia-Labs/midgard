import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { createAuthenticatedFieldCarriagePrerequisitePort } from "../src/workflow/field-carriage-prerequisite.create-authenticated-field-carriage-prerequisite-port.js";
import { withFieldCarriagePrerequisite } from "../src/workflow/field-carriage-prerequisite.with-field-carriage-prerequisite.js";
import { createRawDatumPreimageRequirement } from "../src/workflow/raw-datum-preimage.js";
import { withRawDatumPreimagePrerequisite } from "../src/workflow/raw-datum-preimage-prerequisite.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  base,
  baseAction,
  certificateAction,
  context,
  headerHash,
  outRefReader,
  prerequisite,
  publicationAction,
  txHash,
} from "./field-carriage-prerequisite.prerequisite.js";
import { TEST_PROOF_MATERIALS_HOLDER } from "./support/proof-materials-holder.js";

describe("production field-carriage prerequisite V1", () => {
  it("finds each published chunk by content and reauthenticates it on every call", async () => {
    const requirement = createRawDatumPreimageRequirement({
      preimage: Buffer.concat([
        Buffer.alloc(15_000, 1),
        Buffer.alloc(15_000, 2),
        Buffer.alloc(1, 3),
      ]),
    });
    const signer = {
      source: "test",
      address: "addr_test1_field_funding",
      proofMaterialsAddress: "addr_test1_field_publication",
      paymentKeyHash: "12".repeat(28),
      selectWallet: () => undefined,
    };
    const publicationTxHash = (index: number) =>
      (index + 1).toString(16).padStart(2, "0").repeat(32);
    let candidates: UTxO[] = requirement.publicationDatums.map(
      (datum, index) => ({
        txHash: publicationTxHash(index),
        outputIndex: 0,
        address: signer.proofMaterialsAddress,
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
    let landed = 0;
    const observeExact = vi.fn(
      async ({ expectedOutRef }: { expectedOutRef: string }) => ({
        kind: "confirmed" as const,
        outRef: expectedOutRef,
      }),
    );
    const port = createAuthenticatedFieldCarriagePrerequisitePort({
      category: "nonExistentInput",
      lucid: {
        // The address shows each chunk once its publication has landed.
        utxosAt: async (at: string) =>
          candidates.slice(0, landed).filter(({ address }) => address === at),
        utxosByOutRef,
      } as unknown as LucidEvolution,
      network: "Preview",
      signer,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact,
      },
      requirementForAction: () => requirement,
    });
    // Each chunk is published by its own action, in plan order, and found at
    // the proof-materials address by its bytes once it has landed.
    const publicationActions: unknown[] = [];
    for (const index of requirement.publicationDatums.keys()) {
      const required = await port.inspect({
        holder: TEST_PROOF_MATERIALS_HOLDER,
        headerHash,
        baseAction,
        artifact: context.artifact,
      });
      if (required.kind !== "required")
        throw new Error("missing publication requirement");
      expect(required.action.input.publicationIndex).toBe(index);
      publicationActions.push(required.action);
      landed = index + 1;
    }
    const published = candidates.map(
      ({ txHash: hash, outputIndex }) => `${hash}#${outputIndex.toString()}`,
    );
    const input = {
      headerHash,
      holder: TEST_PROOF_MATERIALS_HOLDER,
      artifact: context.artifact,
      baseAction,
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
    ).toEqual([...published, ...published]);
    // The last chunk leaves the provider's view; an output elsewhere with the
    // same datum does not stand in for it: its publication is required again.
    const last = candidates[2]!;
    candidates = [
      ...candidates.slice(0, 2),
      { ...last, txHash: "ee".repeat(32), address: "addr_test1_elsewhere" },
    ];
    await expect(port.inspect(input)).resolves.toEqual({
      kind: "required",
      action: publicationActions[2],
    });
    await expect(
      port.resolveAuthenticated({ ...input, action: baseAction }),
    ).rejects.toThrow("unauthenticated field publication");
    // A plain copy at the proof-materials address does, whoever paid it.
    const copy = { ...last, txHash: "ef".repeat(32) };
    candidates = [...candidates.slice(0, 2), copy];
    await expect(port.inspect(input)).resolves.toEqual({ kind: "satisfied" });
    await expect(
      port.resolveAuthenticated({ ...input, action: baseAction }),
    ).resolves.toMatchObject({ publications: candidates });
    candidates = [...candidates.slice(0, 2), last];
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

  it("re-exposes a raw publication after its confirmed output rolls back", async () => {
    const requirement = createRawDatumPreimageRequirement({
      preimage: Buffer.from("8101", "hex"),
    });
    const signer = {
      source: "test",
      address: "addr_test1_field_funding",
      proofMaterialsAddress: "addr_test1_field_publication",
      paymentKeyHash: "12".repeat(28),
      selectWallet: () => undefined,
    };
    let candidates: UTxO[] = [];
    let outputConfirmed = false;
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
    });
    const required = await port.inspect({
      holder: TEST_PROOF_MATERIALS_HOLDER,
      headerHash,
      baseAction,
      artifact: context.artifact,
    });
    if (required.kind !== "required")
      throw new Error("missing publication requirement");
    const datumCbor = requirement.publicationDatums[0]!;
    // The workflow as it resumes: the publication is found by content.
    const resumed = context;
    candidates = [
      {
        txHash,
        outputIndex: 0,
        address: signer.proofMaterialsAddress,
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
    outputConfirmed = true;
    await expect(wrapped.observe(resumed)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    const previousCandidates = candidates;
    candidates = [];
    outputConfirmed = false;
    await expect(wrapped.observe(resumed)).resolves.toEqual({
      kind: "action_required",
      action: required.action,
    });
    await expect(
      wrapped.preflight({ ...resumed, action: baseAction }),
    ).rejects.toThrow("cannot bypass authenticated field carriage");
    // A stale wallet candidate cannot revive the old confirmation.
    candidates = previousCandidates;
    await expect(wrapped.observe(resumed)).resolves.toMatchObject({
      kind: "pending",
    });
    expect(underlying.preflight).not.toHaveBeenCalled();
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
  });

  it("captures the first raw publication and forbids direct step bypass", async () => {
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

  it("rejects duplicate or substituted captures", async () => {
    const port = prerequisite();
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: base(),
      prerequisite: port,
    });
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
