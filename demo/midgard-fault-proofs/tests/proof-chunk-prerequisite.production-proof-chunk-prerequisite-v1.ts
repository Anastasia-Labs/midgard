import { createHash } from "node:crypto";

import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { MAXIMUM_CHUNK_PROOF_STEP_COUNT, Proof } from "@al-ft/midgard-sdk/core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  type PublishedProofChunk,
  splitProofIntoChunkDatums,
} from "../src/publish-proof-chunks.js";
import { normalizeJournalJson } from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowAction,
} from "../src/workflow/orchestrator.js";
import { createAuthenticatedProofChunkPrerequisitePort } from "../src/workflow/proof-chunk-prerequisite.create-authenticated-proof-chunk-prerequisite-port.js";
import {
  PROOF_CHUNK_PREREQUISITE,
  type ProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
} from "../src/workflow/proof-chunk-prerequisite.route-action-identity.js";
import { withProofChunkPrerequisite } from "../src/workflow/proof-chunk-prerequisite.with-proof-chunk-prerequisite.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  base,
  baseAction,
  context,
  headerHash,
  identity,
  prerequisite,
  proofSigner,
  publicationAction,
  transaction,
  txHash,
} from "./proof-chunk-prerequisite.prerequisite.js";

describe("production proof-chunk prerequisite V1", () => {
  it("does not reconstruct proof carriage after read-only intent reconciliation", async () => {
    const underlying = base();
    const port = prerequisite();
    vi.mocked(port.inspect).mockRejectedValue(
      new Error("fresh typed material unavailable"),
    );
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: underlying,
      prerequisite: port,
    });
    await expect(
      adapter.observe({ ...context, reconciliationOnly: true }),
    ).resolves.toEqual({ kind: "action_required", action: baseAction });
    expect(port.inspect).not.toHaveBeenCalled();
    expect(port.capture).not.toHaveBeenCalled();
    expect(underlying.preflight).not.toHaveBeenCalled();
  });

  it("waits for proof chunk finality without allowing another capture or proof preflight", async () => {
    const underlying = base();
    const port = prerequisite();
    vi.mocked(port.inspect).mockResolvedValue({
      kind: "pending",
      reason: "proof chunks are not release-final",
    });
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: underlying,
      prerequisite: port,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "pending",
      reason: "proof chunks are not release-final",
    });
    await expect(
      adapter.preflight({ ...context, action: baseAction }),
    ).rejects.toThrow("cannot bypass its direct-first carriage decision");
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("differs from the current requirement");
    expect(port.capture).not.toHaveBeenCalled();
    expect(underlying.preflight).not.toHaveBeenCalled();
    expect(underlying.submit).not.toHaveBeenCalled();
    vi.mocked(port.inspect).mockResolvedValue({
      kind: "satisfied",
      chunks: [],
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
  });

  it("binds the publication identity to the canonical proof encoding", async () => {
    // The MPF library and lucid both emit indefinite-length lists, so the
    // artifact proof is never byte-canonical; the requirement must still be
    // the same publication as for the canonical bytes.
    const encodedProofCbor = Data.to(
      Array.from({ length: MAXIMUM_CHUNK_PROOF_STEP_COUNT + 1 }, () => ({
        Branch: { skip: 0n, neighbors: "" },
      })),
      Proof,
    );
    const canonicalProofCbor = canonicalPlutusDataCbor(encodedProofCbor);
    expect(encodedProofCbor).not.toBe(canonicalProofCbor);
    const inspect = async (proofCbor: string) => {
      const port = createAuthenticatedProofChunkPrerequisitePort({
        category: "invalidRange",
        lucid: { utxosAt: async () => [] } as unknown as LucidEvolution,
        signer: proofSigner,
        publications: {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async () => ({ kind: "not_found" }),
        },
        proofCborForAction: () => proofCbor,
      });
      return await port.inspect({
        headerHash,
        workflowId: context.workflowId,
        baseAction,
        artifact: context.artifact,
      });
    };
    const encoded = await inspect(encodedProofCbor);
    const canonical = await inspect(canonicalProofCbor);
    expect(encoded.kind).toBe("required");
    expect(encoded).toEqual(canonical);
    if (encoded.kind !== "required")
      throw new Error("missing proof publication requirement");
    expect(encoded.action.input.chunkDatumSha256s).toEqual(
      splitProofIntoChunkDatums(canonicalProofCbor).map((datum) =>
        createHash("sha256").update(datum, "utf8").digest("hex"),
      ),
    );
    await expect(inspect("9f01")).rejects.toThrow(
      "is not a PlutusData MPF proof",
    );
  });

  it("requires the publication again once its chunks are gone, and holds on incompletely authenticated chunks", async () => {
    const proofCbor = canonicalPlutusDataCbor(
      Data.to(
        Array.from({ length: MAXIMUM_CHUNK_PROOF_STEP_COUNT + 1 }, () => ({
          Branch: { skip: 0n, neighbors: "" },
        })),
        Proof,
      ),
    );
    const datums = splitProofIntoChunkDatums(proofCbor);
    expect(datums).toHaveLength(2);
    let chain: UTxO[] = [];
    const confirmedOutputs = new Set<string>();
    const port = createAuthenticatedProofChunkPrerequisitePort({
      category: "invalidRange",
      maximumTransactionBytes: 16_384,
      lucid: { utxosAt: async () => chain } as unknown as LucidEvolution,
      signer: proofSigner,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: async ({ expectedOutRef }) =>
          confirmedOutputs.has(expectedOutRef)
            ? { kind: "confirmed", outRef: expectedOutRef }
            : { kind: "not_found" },
      },
      proofCborForAction: () => proofCbor,
    });
    const step = {
      headerHash,
      workflowId: context.workflowId,
      baseAction,
      artifact: context.artifact,
    };
    const required = await port.inspect(step);
    if (required.kind !== "required")
      throw new Error("missing proof publication requirement");
    const wrapper = withProofChunkPrerequisite({
      category: "invalidRange",
      base: base({
        preflightFailure: new Error(
          "Max transaction size of 16384 exceeded. Found: 16385",
        ),
      }),
      prerequisite: {
        ...port,
        capture: async () => ({ transaction: transaction() }),
      },
    });
    await expect(
      wrapper.preflight({ ...context, action: required.action }),
    ).resolves.toMatchObject({
      actionId: required.action.actionId,
      txHash,
      scriptExecution: "none",
    });
    // The publication's outputs exist by content, but only one is
    // authenticated on the current chain: the step waits.
    const outputs: UTxO[] = datums.map((datum, outputIndex) => ({
      txHash,
      outputIndex,
      address: proofSigner.proofMaterialsAddress,
      assets: { lovelace: 2_000_000n },
      datum,
    }));
    const outRefs = outputs.map(
      ({ txHash: hash, outputIndex }) => `${hash}#${outputIndex.toString()}`,
    );
    chain = outputs;
    confirmedOutputs.add(outRefs[0]!);
    await expect(port.inspect(step)).resolves.toMatchObject({
      kind: "pending",
      reason: expect.stringContaining("not authenticated"),
    });
    confirmedOutputs.add(outRefs[1]!);
    const satisfied = await port.inspect(step);
    expect(satisfied).toMatchObject({ kind: "satisfied" });
    if (satisfied.kind !== "satisfied") throw new Error("chunks not found");
    expect(satisfied.chunks.map(({ outRef }) => outRef)).toEqual(outRefs);
    // A previously confirmed publication disappears. The next observation must
    // expose its exact old action so the workflow publishes it again.
    chain = [];
    confirmedOutputs.clear();
    await expect(port.inspect(step)).resolves.toEqual({
      kind: "required",
      action: required.action,
    });
  });

  it("makes the publication a distinct action and forbids step bypass", async () => {
    const underlying = base({
      preflightFailure: new Error(
        "Max transaction size of 16384 exceeded. Found: 16385",
      ),
    });
    const publication = prerequisite();
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: underlying,
      prerequisite: publication,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: publicationAction,
    });
    await expect(
      adapter.preflight({ ...context, action: baseAction }),
    ).rejects.toThrow("cannot bypass its direct-first carriage decision");
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
      referenceScripts: [],
    });
    expect(publication.classifyDirectCapacityFailure).toHaveBeenCalledOnce();
    expect(publication.capture).toHaveBeenCalledOnce();
    await expect(
      adapter.submit({
        ...context,
        action: publicationAction,
        preflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash });
  });

  it("keeps a fitting proof on the exact direct transaction without publishing", async () => {
    const underlying = base();
    const publication = prerequisite();
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: underlying,
      prerequisite: publication,
    });
    const preflight = await adapter.preflight({
      ...context,
      action: publicationAction,
    });
    expect(preflight).toMatchObject({
      actionId: publicationAction.actionId,
      txHash: "99".repeat(32),
      scriptExecution: "reference_scripts",
    });
    expect(underlying.preflight).toHaveBeenCalledWith(
      expect.objectContaining({ action: baseAction }),
    );
    expect(publication.capture).not.toHaveBeenCalled();
    await expect(
      adapter.submit({
        ...context,
        action: publicationAction,
        preflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash: "99".repeat(32) });
    expect(underlying.submit).toHaveBeenCalledOnce();
  });

  it("accepts a JSON-normalized copy of the route action", async () => {
    const underlying = base();
    const publication = prerequisite();
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: underlying,
      prerequisite: publication,
    });
    const normalized = normalizeJournalJson(
      publicationAction,
      "action",
    ) as unknown as FraudProofWorkflowAction;
    expect(JSON.stringify(normalized)).not.toBe(
      JSON.stringify(publicationAction),
    );
    const preflight = await adapter.preflight({
      ...context,
      action: normalized,
    });
    expect(preflight).toMatchObject({
      actionId: publicationAction.actionId,
      txHash: "99".repeat(32),
    });
    await expect(
      adapter.submit({ ...context, action: normalized, preflight }),
    ).resolves.toEqual({ kind: "submitted", txHash: "99".repeat(32) });
  });

  it("fails closed on a non-capacity direct preflight error", async () => {
    const underlying = base({
      preflightFailure: new Error("validator identity changed"),
    });
    const publication = prerequisite();
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: underlying,
      prerequisite: publication,
    });
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("validator identity changed");
    expect(publication.capture).not.toHaveBeenCalled();
  });

  it("allows the exact base step only after authenticated publication", async () => {
    const underlying = base();
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: underlying,
      prerequisite: prerequisite({ satisfied: true }),
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    await adapter.preflight({ ...context, action: baseAction });
    expect(underlying.preflight).toHaveBeenCalledOnce();
  });

  it("rejects duplicate or substituted publication captures", async () => {
    const publication = prerequisite();
    const adapter = withProofChunkPrerequisite({
      category: "invalidRange",
      base: base({
        preflightFailure: new Error(
          "Max transaction size of 16384 exceeded. Found: 16385",
        ),
      }),
      prerequisite: publication,
    });
    await adapter.preflight({ ...context, action: publicationAction });
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("outstanding captured body");
    await expect(
      adapter.preflight({
        ...context,
        action: {
          ...publicationAction,
          input: {
            ...publicationAction.input,
            proofCborSha256: "ff".repeat(32),
          },
        },
      }),
    ).rejects.toThrow("differs from the current requirement");
    expect(publication.capture).toHaveBeenCalledOnce();
  });

  it("keeps noReferenceInput direct unless its exact body exceeds the release max", async () => {
    const proofCbor = Data.to([{ Branch: { skip: 0n, neighbors: "" } }], Proof);
    const [datumCbor] = splitProofIntoChunkDatums(proofCbor);
    if (datumCbor === undefined) throw new Error("fixture proof did not chunk");
    const preexistingChunk = {
      txHash: "cd".repeat(32),
      outputIndex: 0,
      address: "addr_test1_no_reference_input",
      assets: { lovelace: 2_000_000n },
      datum: datumCbor,
    } as UTxO;
    const directAction: FraudProofWorkflowAction = Object.freeze({
      actionId: `step_03:${"ab".repeat(32)}#0`,
      input: Object.freeze({
        schemaVersion: "midgard-production-linear-family-action-v1",
        category: "noReferenceInput",
        stage: "step_03",
        ordinal: 3,
        threadOutRef: `${"ab".repeat(32)}#0`,
      }),
    });
    const routeAction: FraudProofWorkflowAction = Object.freeze({
      actionId: `publish-proof-chunks:${directAction.actionId}:${"55".repeat(32)}`,
      input: Object.freeze({
        schemaVersion: PROOF_CHUNK_PREREQUISITE,
        category: "noReferenceInput",
        stage: "direct_or_publish_proof",
        forAction: directAction,
        proofCborSha256: "55".repeat(32),
        chunkDatumSha256s: ["66".repeat(32)],
      }),
    });
    const noReferenceContext = {
      ...context,
      identity: { ...identity, category: "noReferenceInput" as const },
    };
    const observedChunkCounts: number[] = [];
    const makeBase = (failure?: Error): FraudProofFamilyWorkflowAdapter => ({
      ...base(),
      category: "noReferenceInput",
      observe: vi.fn(async () => ({
        kind: "action_required" as const,
        action: directAction,
      })),
      preflight: vi.fn(async ({ action }) => {
        const chunks = await resolveDirectFirstProofChunks({
          action,
          address: preexistingChunk.address,
          proofCbor,
        });
        observedChunkCounts.push(chunks.length);
        if (failure !== undefined) throw failure;
        return {
          actionId: directAction.actionId,
          txHash: "99".repeat(32),
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: "no-reference-input-local-uplc",
          },
          referenceScripts: [],
        };
      }),
    });
    const makePrerequisite = (
      satisfied?: readonly PublishedProofChunk[],
    ): ProofChunkPrerequisitePort<"noReferenceInput"> => ({
      ...prerequisite(),
      category: "noReferenceInput",
      inspect: vi.fn(async () =>
        satisfied !== undefined
          ? { kind: "satisfied" as const, chunks: satisfied }
          : { kind: "required" as const, action: routeAction },
      ),
    });

    const fittingPort = makePrerequisite();
    const fitting = withProofChunkPrerequisite({
      category: "noReferenceInput",
      base: makeBase(),
      prerequisite: fittingPort,
    });
    await expect(
      fitting.preflight({
        ...noReferenceContext,
        action: routeAction,
      }),
    ).resolves.toMatchObject({
      txHash: "99".repeat(32),
      scriptExecution: "reference_scripts",
    });
    expect(fittingPort.capture).not.toHaveBeenCalled();
    expect(observedChunkCounts).toEqual([0]);

    const maximumPort = makePrerequisite();
    const maximum = withProofChunkPrerequisite({
      category: "noReferenceInput",
      base: makeBase(
        new Error("Max transaction size of 16384 exceeded. Found: 16385"),
      ),
      prerequisite: maximumPort,
    });
    await expect(
      maximum.preflight({
        ...noReferenceContext,
        action: routeAction,
      }),
    ).resolves.toMatchObject({
      txHash,
      scriptExecution: "none",
    });
    expect(maximumPort.classifyDirectCapacityFailure).toHaveReturnedWith(
      expect.objectContaining({
        maximumTransactionBytes: 16_384,
        actualTransactionBytes: 16_385,
      }),
    );
    expect(maximumPort.capture).toHaveBeenCalledOnce();
    expect(observedChunkCounts).toEqual([0, 0]);

    const chunk: PublishedProofChunk = {
      utxo: preexistingChunk,
      outRef: `${preexistingChunk.txHash}#0`,
      datumCbor,
      stepCount: 1,
    };
    // The step carries exactly the chunks the prerequisite found and
    // authenticated, whoever paid them; never a set of another proof.
    const otherDatum = splitProofIntoChunkDatums(
      Data.to([{ Branch: { skip: 1n, neighbors: "" } }], Proof),
    )[0]!;
    await expect(
      withProofChunkPrerequisite({
        category: "noReferenceInput",
        base: makeBase(),
        prerequisite: makePrerequisite([{ ...chunk, datumCbor: otherDatum }]),
      }).preflight({ ...noReferenceContext, action: directAction }),
    ).rejects.toThrow("not the exact complete set of this proof");
    await expect(
      withProofChunkPrerequisite({
        category: "noReferenceInput",
        base: makeBase(),
        prerequisite: makePrerequisite([chunk]),
      }).preflight({ ...noReferenceContext, action: directAction }),
    ).resolves.toMatchObject({ txHash: "99".repeat(32) });
    expect(observedChunkCounts).toEqual([0, 0, 1]);
  });
});
