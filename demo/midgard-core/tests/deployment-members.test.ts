import { describe, expect, it } from "vitest";

import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_MEMBERS,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MEMBERS,
  DEPLOYMENT_PUBLICATION_MEMBERS,
  deploymentFamilyStepNames,
  type DeploymentMember,
  verifyDeploymentMembers,
} from "../src/deployment-manifest-identity.js";

describe("deployment member admission", () => {
  it("requires the min-Ada yield even when every derived view omits it", () => {
    // Completeness comes from the independent contract-name list, not from
    // any view derived from the rows.
    const { fraudProofMinAdaStep02TxWithdraw: _omitted, ...members } =
      DEPLOYMENT_MEMBERS;
    expect(Object.keys(members)).not.toContain(
      "fraudProofMinAdaStep02TxWithdraw",
    );
    expect(() => verifyDeploymentMembers(members)).toThrow(
      "Missing deployment members: fraudProofMinAdaStep02TxWithdraw",
    );
  });

  it("refuses a duplicated reference role", () => {
    expect(() =>
      verifyDeploymentMembers({
        ...DEPLOYMENT_MEMBERS,
        fraudProofMinAdaStep02TxWithdraw: {
          ...DEPLOYMENT_MEMBERS.fraudProofMinAdaStep02TxWithdraw,
          referenceRole: DEPLOYMENT_MEMBERS.fraudProofMinAda.referenceRole,
        },
      }),
    ).toThrow("duplicated deployment role");
  });

  it("derives every roster order from the contract-name list", () => {
    expect(DEPLOYMENT_MANIFEST_MEMBERS).toEqual(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
    );
    // Publication keeps the members that carry a role, in list order.
    expect(DEPLOYMENT_PUBLICATION_MEMBERS).toEqual(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.filter(
        (contract) => DEPLOYMENT_MEMBERS[contract].referenceRole !== null,
      ),
    );
    expect(
      Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
    ).toEqual(
      DEPLOYMENT_PUBLICATION_MEMBERS.map((contract) => [
        DEPLOYMENT_MEMBERS[contract].referenceRole,
        contract,
      ]),
    );
  });

  it("rejects a member outside the protocol ABI", () => {
    const members: Record<string, DeploymentMember> = {
      ...DEPLOYMENT_MEMBERS,
      retiredMinAdaYield: DEPLOYMENT_MEMBERS.fraudProofMinAdaStep02TxWithdraw,
    };
    expect(() => verifyDeploymentMembers(members)).toThrow(
      "Unexpected deployment member: retiredMinAdaYield",
    );
  });

  it("keeps min-Ada's yield outside its four spending steps", () => {
    expect(deploymentFamilyStepNames("minAda")).toEqual([
      "fraudProofMinAda",
      "fraudProofMinAdaStep02",
      "fraudProofMinAdaStep03",
      "fraudProofMinAdaStep04",
    ]);
    expect(DEPLOYMENT_MANIFEST_MEMBERS).toContain(
      "fraudProofMinAdaStep02TxWithdraw",
    );
    expect(DEPLOYMENT_PUBLICATION_MEMBERS).toContain(
      "fraudProofMinAdaStep02TxWithdraw",
    );
  });
});
