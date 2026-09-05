import {
  buildValidationTraceDisputeFaultProofContracts,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX,
  validationSemanticResolverGlobalIndex,
} from "../src/index.js";
import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
} from "./support/submit-init-emulator-shared.js";

const SPECIALIZED_BRANCHES = [
  [2, "replay-input"],
  [4, "replay-finish"],
  [5, "output-descriptor"],
  [7, "output-finish"],
  [9, "mint-finish"],
] as const;

describe("ValueAndMint specialized branch publication", () => {
  it("publishes all five extracted branches with the release reserve", async () => {
    const harness = await makeFaultProofEmulatorHarness();
    const family = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(readBlueprint(realBlueprintPath)),
        network,
        hubOraclePolicyId: harness.contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId:
          harness.contracts.fraudProofCatalogue.policyId,
        referenceScriptAuthPolicyId:
          harness.contracts.referenceScriptAuth.policyId,
      }),
    );
    for (const [index, name] of SPECIALIZED_BRANCHES) {
      const contract =
        family.validationTraceDispute.semanticResolvers[
          validationSemanticResolverGlobalIndex(
            VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX,
            index,
          )
        ];
      expect(contract, name).toBeDefined();
      const publication = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: contract!.spendingScript,
        label: `ValueAndMint ${name}`,
      });
      expect(publication.utxo.scriptRef, name).toBeDefined();
      expect(
        publication.publicationMeasurement.l1ByteMargin,
        name,
      ).toBeGreaterThanOrEqual(512);
      console.info(
        `[value-and-mint-publication] ${JSON.stringify({
          name,
          scriptHash: contract!.spendingScriptHash,
          bytes: publication.publicationMeasurement.completeSignedBytes,
        })}`,
      );
    }
  }, 600_000);
});
