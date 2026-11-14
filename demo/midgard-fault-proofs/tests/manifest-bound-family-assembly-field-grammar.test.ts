import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { bindManifestBoundFamilyWorkflow } from "../src/workflow/manifest-bound-family-assembly.js";
import { fieldGrammarCertificateValidatorFixture } from "./support/field-grammar-certificate-deployment-fixture.js";
import {
  categories,
  CERTIFICATE_POLICY,
  fixture,
  SCRIPT,
} from "./support/manifest-bound-family-assembly-fixture.js";

afterEach(() => vi.restoreAllMocks());

describe("manifest-bound family assembly field grammar certificate", () => {
  describe.each(categories)("%s", (category) => {
    it("resolves the field grammar certificate at startup exactly when a step reads through it", async () => {
      const f = fixture(category);
      const bind = () =>
        bindManifestBoundFamilyWorkflow(
          f.definition as never,
          f.config as never,
        );
      if (f.definition.fieldGrammarCertificate !== true) {
        expect((await bind()).fieldGrammar).toBeUndefined();
        expect(f.bindReference).not.toHaveBeenCalledWith(
          expect.objectContaining({
            contractName: "fieldGrammarCertificateSpend",
          }),
        );
        return;
      }
      const { fieldGrammar } = await bind();
      const grammar = fieldGrammarCertificateValidatorFixture({
        network: "Preprod",
        fieldPreimageCertificatePolicyId: CERTIFICATE_POLICY,
      });
      expect(fieldGrammar?.certificateValidator.spendingScriptHash).toBe(
        grammar.spendingScriptHash,
      );
      expect(fieldGrammar?.certificateReferenceScriptUtxo).toBe(
        f.references.get("fieldGrammarCertificateSpend"),
      );

      // A deployment missing either certificate member fails at startup,
      // naming the members, before any provider observation.
      for (const member of [
        "fieldGrammarCertificateSpend",
        "fieldGrammarCertificateMint",
      ]) {
        const missing = fixture(category);
        Reflect.deleteProperty(
          missing.binding.referenceScriptsByContract,
          member,
        );
        // The spies are shared across fixtures; count only this startup.
        missing.observe.mockClear();
        await expect(missing.assemble()).rejects.toThrow(
          `${category} deployment lacks the field grammar certificate members fieldGrammarCertificateSpend and fieldGrammarCertificateMint (missing ${member})`,
        );
        expect(missing.observe).not.toHaveBeenCalled();
      }

      // A published script that is not the blueprint's certificate over the
      // manifest's field-preimage policy is refused even when the manifest
      // names it.
      const foreign = fixture(category);
      const published = foreign.references.get("fieldGrammarCertificateSpend")!;
      foreign.binding.referenceScriptsByContract.fieldGrammarCertificateSpend =
        {
          outRef: `${published.txHash}#${published.outputIndex.toString()}`,
          scriptHash: validatorToScriptHash(SCRIPT),
        };
      Object.assign(foreign.config.referenceScripts, {
        fieldGrammarCertificateSpend: { ...published, scriptRef: SCRIPT },
      });
      foreign.observe.mockClear();
      await expect(foreign.assemble()).rejects.toThrow(
        `${category} field grammar certificate reference script differs from the blueprint's`,
      );
      expect(foreign.observe).not.toHaveBeenCalled();
    });
  });
});
