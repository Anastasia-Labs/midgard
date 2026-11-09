import { afterEach, describe, expect, it, vi } from "vitest";

import { INSTALLED_FRAUD_PROOF_CATEGORY_ORDER } from "../src/workflow/installed-categories.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "../src/workflow/manifest-bound-family-assembly.js";
import * as orchestrator from "../src/workflow/orchestrator.js";
import {
  categories,
  definitionOf,
  fixture,
  HEADER,
} from "./support/manifest-bound-family-assembly-fixture.js";

afterEach(() => vi.restoreAllMocks());

describe("manifest-bound family assembly", () => {
  it("covers every migrated definition, linear and cursor", () => {
    // These six installed families still own their adapters outside this
    // shared assembly; every other installed category must have a definition.
    const separatelyAssembled = new Set([
      "doubleSpend",
      "missingSignature",
      "mintItemNonCanonical",
      "networkId",
      "validationTraceDispute",
      "valueNotPreserved",
    ]);
    expect([...categories].sort()).toEqual(
      INSTALLED_FRAUD_PROOF_CATEGORY_ORDER.filter(
        (category) => !separatelyAssembled.has(category),
      ).sort(),
    );
    expect(
      categories.filter(
        (category) => definitionOf(category).adapter.kind === "linear",
      ),
    ).toHaveLength(16);
    expect(
      categories.filter(
        (category) => definitionOf(category).adapter.kind === "cursor",
      ),
    ).toHaveLength(30);
    for (const category of categories) {
      expect(definitionOf(category).category).toBe(category);
    }
    expect(
      new Set(categories.map((c) => definitionOf(c).adapter.kind)),
    ).toEqual(new Set(["linear", "cursor"]));
  });

  describe.each(categories)("%s", (category) => {
    it("assembles a workflow whose adapter carries the category", async () => {
      const f = fixture(category);
      const workflow = await f.assemble();
      expect(workflow.definition).toBe(f.definition);
      expect(workflow.binding).toBe(f.binding);
      expect(workflow.l1).toBe(f.l1);
      expect(workflow.adapter.category).toBe(category);
      expect(workflow.transactions.category).toBe(category);
      expect(workflow.replayer.launchScope).toEqual([category]);
      expect(Object.isFrozen(workflow)).toBe(true);
    });

    it("binds every spec step and declared witness reference before provider startup", async () => {
      const f = fixture(category);
      await f.assemble();
      const bound = f.bindReference.mock.calls.map(
        ([input]) => input.contractName,
      );
      const expected = [
        ...f.stepNames,
        ...f.definition.witnessRoles,
        ...(f.definition.fieldPreimageCertificate
          ? ["fieldPreimageCertificateMint"]
          : []),
        ...(f.definition.fieldGrammarCertificate === true
          ? ["fieldGrammarCertificateSpend"]
          : []),
        ...Object.values(f.definition.auxiliaryReferenceScripts ?? {}),
      ];
      expect(bound).toEqual(expected);
      for (const name of expected) {
        expect(f.bindReference).toHaveBeenCalledWith({
          binding: f.binding,
          contractName: name,
          utxo: f.references.get(name),
        });
      }
      const [referenceOrder] = f.bindReference.mock.invocationCallOrder;
      const [observeOrder] = f.observe.mock.invocationCallOrder;
      expect(referenceOrder).toBeLessThan(observeOrder!);
    });

    describe.each(definitionOf(category).witnessRoles)("witness %s", (role) => {
      it("refuses a missing reference before provider startup", async () => {
        const f = fixture(category);
        Reflect.deleteProperty(f.config.referenceScripts.witnesses, role);
        await expect(f.assemble()).rejects.toThrow(
          `${category} workflow config omitted witness reference script ${role}`,
        );
        expect(f.observe).not.toHaveBeenCalled();
      });

      it("refuses a reference from a different output", async () => {
        const f = fixture(category);
        f.config.referenceScripts.witnesses[role] = {
          ...f.references.get(role)!,
          outputIndex: 99,
        };
        await expect(f.assemble()).rejects.toThrow(
          `${role} reference UTxO differs from finalized manifest identity`,
        );
        expect(f.observe).not.toHaveBeenCalled();
      });

      it("refuses a different script at the published output", async () => {
        const f = fixture(category);
        f.config.referenceScripts.witnesses[role] = {
          ...f.references.get(role)!,
          scriptRef: { type: "Native", script: `8200581c${"44".repeat(28)}` },
        };
        await expect(f.assemble()).rejects.toThrow(
          `${role} reference UTxO script differs from finalized manifest identity`,
        );
        expect(f.observe).not.toHaveBeenCalled();
      });
    });

    it("binds the field-preimage certificate mint only when the definition declares it", async () => {
      const f = fixture(category);
      if (f.definition.fieldPreimageCertificate) {
        // The assembly refuses a manifest without the certificate policy
        // before any provider observation.
        f.binding.fieldPreimageCertificate = null as never;
        await expect(f.assemble()).rejects.toThrow(
          `${category} manifest omitted the field-preimage certificate policy`,
        );
        expect(f.observe).not.toHaveBeenCalled();
        return;
      }
      // A definition that does not declare the certificate never binds the
      // minting reference script. Its transaction port may still read the
      // certificate policy id from the manifest (committed-field-shape and
      // zero-input bind it into a step verdict), so the manifest stays whole.
      const workflow = await f.assemble();
      expect(workflow.adapter.category).toBe(category);
      expect(f.bindReference).not.toHaveBeenCalledWith(
        expect.objectContaining({
          contractName: "fieldPreimageCertificateMint",
        }),
      );
    });

    it("refuses an L1 port without raw-L1 and publication authorities", async () => {
      const f = fixture(category);
      Reflect.deleteProperty(f.l1, "rawL1");
      await expect(f.assemble()).rejects.toThrow(
        `${category} requires authenticated raw L1 and publication authorities`,
      );
    });

    it("applies exactly the declared prerequisites, field carriage before proof chunks", async () => {
      const f = fixture(category);
      const workflow = await f.assemble();
      const fieldEntries = f.definition.fieldCarriage ?? [];
      expect(f.withField).toHaveBeenCalledTimes(fieldEntries.length);
      expect(f.withChunks).toHaveBeenCalledTimes(
        f.definition.proofChunk === undefined ? 0 : 1,
      );
      const decorations = [
        ...f.withField.mock.results.map((result, index) => ({
          kind: "field" as const,
          base: f.withField.mock.calls[index]![0].base,
          rawDatum: f.withField.mock.calls[index]![0].rawDatum,
          result: result.value as unknown,
        })),
        ...f.withChunks.mock.results.map((result, index) => ({
          kind: "chunk" as const,
          base: f.withChunks.mock.calls[index]![0].base,
          rawDatum: undefined,
          result: result.value as unknown,
        })),
      ];
      // Each decorator wraps the previous one's result; the last is the
      // workflow's adapter. With no prerequisites the adapter is undecorated.
      let previous: unknown = undefined;
      decorations.forEach((decoration, index) => {
        if (index > 0) expect(decoration.base).toBe(previous);
        expect(decoration.base).toMatchObject({ category });
        previous = decoration.result;
      });
      if (decorations.length === 0) {
        expect(workflow.adapter).toMatchObject({ category });
      } else {
        expect(workflow.adapter).toBe(previous);
      }
      fieldEntries.forEach((entry, index) => {
        expect(decorations[index]!.rawDatum).toBe(entry.rawDatum);
      });
    });

    it("runs through the retained-DA runner with its replayer and single-category scope", async () => {
      const f = fixture(category);
      const workflow = await f.assemble();
      const result = { fake: "run-result" };
      const run = vi
        .spyOn(orchestrator, "runFraudProofWorkflowFromRetainedDa")
        .mockResolvedValue(result as never);
      const registry = vi.spyOn(
        orchestrator,
        "createFraudProofWorkflowRegistry",
      );
      const sources = [{ fake: "source" }] as never;
      const journal = { fake: "journal" } as never;
      await expect(
        runOrResumeManifestBoundFamilyWorkflow({ workflow, sources, journal }),
      ).resolves.toBe(result);
      expect(f.l1.observeHeader).toHaveBeenCalledWith({ headerHash: HEADER });
      expect(run).toHaveBeenCalledTimes(1);
      const [input] = run.mock.calls[0]!;
      expect(input).toMatchObject({
        deploymentFingerprint: f.binding.deploymentFingerprint,
        observation: f.observation,
        sources,
        journal,
      });
      expect(input.replayer).toBe(workflow.replayer);
      expect(input.terminalVerifier).toBe(workflow.terminalVerifier);
      expect(input.releaseFinalityAuthority).toBe(
        workflow.releaseFinalityAuthority,
      );
      // The registry freezes each adapter behind a wrapper, so the adapter
      // identity is checked at the registry's input.
      expect(registry).toHaveBeenCalledTimes(1);
      expect(registry.mock.calls[0]![0]).toEqual({
        adapters: [workflow.adapter],
        launchScope: [category],
      });
      expect(registry.mock.calls[0]![0].adapters[0]).toBe(workflow.adapter);
      expect(input.registry).toBe(registry.mock.results[0]!.value);
      expect([...input.registry.keys()]).toEqual([category]);
      expect("replayContext" in input).toBe(false);
    });

    it("forwards a supplied replay context to the runner", async () => {
      const f = fixture(category);
      const replayContext = { fake: "replay-context" };
      const workflow = await assembleManifestBoundFamilyWorkflow(
        f.definition as never,
        { ...f.config, replayContext } as never,
        f.runtime,
      );
      expect(workflow.replayContext).toBe(replayContext);
      const run = vi
        .spyOn(orchestrator, "runFraudProofWorkflowFromRetainedDa")
        .mockResolvedValue({} as never);
      await runOrResumeManifestBoundFamilyWorkflow({
        workflow,
        sources: [],
        journal: {} as never,
      });
      expect(run.mock.calls[0]![0].replayContext).toBe(replayContext);
    });
  });

  it("refuses a definition that is not a frozen product of defineFamily", async () => {
    const [category] = categories;
    const f = fixture(category!);
    for (const definition of [
      { ...f.definition },
      Object.freeze({ ...f.definition, definitionVersion: "other" }),
    ]) {
      await expect(
        assembleManifestBoundFamilyWorkflow(
          definition as never,
          f.config as never,
        ),
      ).rejects.toThrow(`${category!} family definition changed identity`);
    }
    expect(f.observe).not.toHaveBeenCalled();
  });

  it("refuses a definition whose step datum schemas disagree with its spec", async () => {
    const [category] = categories;
    const f = fixture(category!);
    const definition = Object.freeze({
      ...f.definition,
      stepDatumSchemas: f.definition.stepDatumSchemas.slice(1),
    });
    await expect(
      assembleManifestBoundFamilyWorkflow(
        definition as never,
        f.config as never,
      ),
    ).rejects.toThrow(
      `${category!} definition declares ${(f.definition.stepDatumSchemas.length - 1).toString()} step datum schemas for a ${f.stepNames.length.toString()}-step spec`,
    );
    expect(f.observe).not.toHaveBeenCalled();
  });
});
