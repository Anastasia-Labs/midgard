import { describe, expect, it } from "vitest";

import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_MEMBERS,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
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

  const withYieldReference = (reference: DeploymentMember["reference"]) => ({
    ...DEPLOYMENT_MEMBERS,
    fraudProofMinAdaStep02TxWithdraw: {
      ...DEPLOYMENT_MEMBERS.fraudProofMinAdaStep02TxWithdraw,
      reference,
    },
  });
  const minAdaReference = DEPLOYMENT_MEMBERS.fraudProofMinAda.reference;
  const yieldReference =
    DEPLOYMENT_MEMBERS.fraudProofMinAdaStep02TxWithdraw.reference;

  it("refuses a duplicated reference role", () => {
    expect(() =>
      verifyDeploymentMembers(
        withYieldReference({ ...yieldReference, role: minAdaReference.role }),
      ),
    ).toThrow("duplicated deployment role");
  });

  it("refuses a reference without an auth-token name", () => {
    expect(() =>
      verifyDeploymentMembers(
        withYieldReference({ ...yieldReference, token: "" }),
      ),
    ).toThrow("Invalid or duplicated reference-script token name");
  });

  it("refuses two roles sharing an auth-token name", () => {
    // Both would mint the same asset name under the auth policy.
    expect(() =>
      verifyDeploymentMembers(
        withYieldReference({ ...yieldReference, token: minAdaReference.token }),
      ),
    ).toThrow("Invalid or duplicated reference-script token name");
  });

  it("derives every roster order and token name from the member rows", () => {
    expect(DEPLOYMENT_MANIFEST_MEMBERS).toEqual(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
    );
    // Publication keeps the members that carry a role, in list order.
    expect(DEPLOYMENT_PUBLICATION_MEMBERS).toEqual(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.filter(
        (contract) => DEPLOYMENT_MEMBERS[contract].reference !== null,
      ),
    );
    const references = DEPLOYMENT_PUBLICATION_MEMBERS.map(
      (contract) =>
        [contract, DEPLOYMENT_MEMBERS[contract].reference!] as const,
    );
    expect(
      Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
    ).toEqual(references.map(([contract, { role }]) => [role, contract]));
    expect(
      Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES),
    ).toEqual(references.map(([, { role, token }]) => [role, token]));
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
