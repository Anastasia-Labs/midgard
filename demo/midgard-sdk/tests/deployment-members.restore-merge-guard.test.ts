/** Restoring a family's script slots never lets one member's validator silently
 * replace another's: every slot a member occupies must agree on each key. */
import {
  DEPLOYMENT_MEMBERS,
  type DeploymentContract,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  type FamilyMemberValidator,
  restoreFaultProofFamilyScripts,
  restoreValidationTraceDisputeScripts,
} from "../src/index.js";

const script = (contract: string) => ({
  type: "PlutusV3" as const,
  script: `cbor-of-${contract}`,
});

/** A distinct, well-formed validator of the member's own purpose. */
const ownValidator = (contract: DeploymentContract): FamilyMemberValidator => {
  switch (DEPLOYMENT_MEMBERS[contract].purpose) {
    case "spend":
      return {
        spendingScriptCBOR: `cbor-of-${contract}`,
        spendingScript: script(contract),
        spendingScriptHash: `hash-of-${contract}`,
        spendingScriptAddress: `address-of-${contract}`,
      };
    case "mint":
      return {
        mintingScriptCBOR: `cbor-of-${contract}`,
        mintingScript: script(contract),
        policyId: `hash-of-${contract}`,
      };
    case "withdraw":
      return {
        withdrawalScriptCBOR: `cbor-of-${contract}`,
        withdrawalScript: script(contract),
        withdrawalScriptHash: `hash-of-${contract}`,
      };
  }
};

const restoreTrace = (
  resolve: (contract: DeploymentContract) => FamilyMemberValidator,
) =>
  restoreValidationTraceDisputeScripts({
    resolve,
    missingMember: (member) => {
      throw new Error(`Unrecorded member: ${member}`);
    },
  });

describe("deployment member slot restoration", () => {
  it("restores a co-located spend and mint pair as one validator at each of its slots", () => {
    const restored = restoreTrace(ownValidator);
    expect(restored.award).toEqual({
      ...ownValidator("validationTraceDisputeAward"),
      ...ownValidator("validationTraceDisputeAwardMint"),
    });
    expect(restored.steps[5]).toBe(restored.award);
  });

  it("restores a multi-path member outside the validation trace", () => {
    const multiPath = (
      Object.keys(DEPLOYMENT_MEMBERS) as DeploymentContract[]
    ).find(
      (contract) =>
        DEPLOYMENT_MEMBERS[contract].family === "doubleSpend" &&
        DEPLOYMENT_MEMBERS[contract].familyPaths.length > 1,
    );
    expect(multiPath).toBeDefined();
    expect(() =>
      restoreFaultProofFamilyScripts("doubleSpend", ownValidator),
    ).not.toThrow();
  });

  it("refuses a co-located member that disagrees with a multi-path member's other slot", () => {
    // The award mint shares only the award slot; carrying a spending hash of
    // its own makes that slot disagree with the award's step slot.
    expect(() =>
      restoreTrace((contract) =>
        contract === "validationTraceDisputeAwardMint"
          ? {
              ...ownValidator(contract),
              spendingScriptHash: "hash-of-another-script",
            }
          : ownValidator(contract),
      ),
    ).toThrow(/validationTraceDisputeAward.*spendingScriptHash/u);
  });

  it("refuses a co-located member that disagrees on a key of the slot it shares", () => {
    expect(() =>
      restoreTrace((contract) =>
        contract === "validationTraceDisputeCaseMint"
          ? {
              ...ownValidator(contract),
              spendingScript: script("another-script"),
            }
          : ownValidator(contract),
      ),
    ).toThrow(/validationTraceDisputeCase.*spendingScript/u);
  });
});
