import { MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS } from "@al-ft/midgard-core/validation-dispute";
import { describe, expect, it } from "vitest";

import {
  applyBlueprintParams,
  applyThreadHolderParams,
  buildValidationThreadExpiry,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
} from "../src/index.js";
import { applyExpectedScriptParams } from "./fault-proof.expected-parameter-application.js";
import {
  compiledScript,
  loadBlueprint,
} from "./fault-proof.publication-tx-overhead-bytes.js";

const threadPolicyId = "5a".repeat(28);

describe("validation-case thread expiry", () => {
  it("is built only with the case duration the case validator applies", () => {
    const blueprint = loadBlueprint();

    for (const caseDurationMs of [
      MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS - 1,
      MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS + 1,
      0,
    ]) {
      expect(() =>
        buildValidationThreadExpiry(blueprint, threadPolicyId, caseDurationMs),
      ).toThrow(/differs from the case's D_case/);
    }

    const expiry = buildValidationThreadExpiry(
      blueprint,
      threadPolicyId,
      MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
    );
    expect(expiry.withdrawalScriptCBOR).toBe(
      applyExpectedScriptParams(
        compiledScript(
          blueprint,
          VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.threadExpiry,
        ),
        [threadPolicyId, BigInt(MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS)],
      ),
    );
  });
});

describe("validation-trace thread holder parameters", () => {
  const resolverHashes = ["c1".repeat(28)];
  const awardHash = "a1".repeat(28);
  const expiryHash = "e1".repeat(28);
  const boundaryTitle = VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.boundary;

  it("appends the expiry hash last where the holder declares it", () => {
    const blueprint = loadBlueprint();
    expect(
      applyThreadHolderParams(
        blueprint,
        boundaryTitle,
        [resolverHashes, awardHash, threadPolicyId],
        expiryHash,
      ),
    ).toBe(
      applyBlueprintParams(blueprint, boundaryTitle, [
        resolverHashes,
        awardHash,
        threadPolicyId,
        expiryHash,
      ]),
    );
  });

  it("leaves a validator that does not declare it unchanged", () => {
    const blueprint = loadBlueprint();
    const title = VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.disputeBond;
    expect(
      applyThreadHolderParams(blueprint, title, [threadPolicyId], expiryHash),
    ).toBe(applyBlueprintParams(blueprint, title, [threadPolicyId]));
  });

  it("refuses a holder that declares it anywhere but last", () => {
    const blueprint = loadBlueprint();
    const reordered = {
      ...blueprint,
      validators: blueprint.validators.map((validator) =>
        validator.title === boundaryTitle
          ? {
              ...validator,
              parameters: [
                validator.parameters.at(-1)!,
                ...validator.parameters.slice(0, -1),
              ],
            }
          : validator,
      ),
    };
    expect(() =>
      applyThreadHolderParams(
        reordered,
        boundaryTitle,
        [resolverHashes, awardHash, threadPolicyId],
        expiryHash,
      ),
    ).toThrow(/thread_expiry_script_hash at position 0, not last/);
  });
});
