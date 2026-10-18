import { describe, expect, it } from "vitest";

import {
  DEPLOYMENT_MANIFEST_MEMBERS,
  DEPLOYMENT_MEMBERS,
  DEPLOYMENT_PUBLICATION_MEMBERS,
  deploymentFamilyStepNames,
  type DeploymentMember,
  verifyDeploymentMembers,
} from "../src/deployment-manifest-identity.js";

describe("deployment member admission", () => {
  it("requires the min-Ada yield even when every derived view omits it", () => {
    const { fraudProofMinAdaStep02TxWithdraw: omitted, ...remaining } =
      DEPLOYMENT_MEMBERS;
    // Close all position gaps as every derived projection would. Completeness
    // must come from the protocol ABI, not a missing numeric position.
    const members = Object.fromEntries(
      Object.entries(remaining).map(([contract, member]) => [
        contract,
        {
          ...member,
          manifestPosition:
            member.manifestPosition > omitted.manifestPosition
              ? member.manifestPosition - 1
              : member.manifestPosition,
          publicationPosition:
            member.publicationPosition !== null &&
            member.publicationPosition > omitted.publicationPosition
              ? member.publicationPosition - 1
              : member.publicationPosition,
          referenceRolePosition:
            member.referenceRolePosition !== null &&
            member.referenceRolePosition > omitted.referenceRolePosition
              ? member.referenceRolePosition - 1
              : member.referenceRolePosition,
        },
      ]),
    );
    expect(Object.keys(members)).not.toContain(
      "fraudProofMinAdaStep02TxWithdraw",
    );
    expect(() => verifyDeploymentMembers(members)).toThrow(
      "Missing deployment members: fraudProofMinAdaStep02TxWithdraw",
    );
  });

  it("refuses duplicate publication positions and roles", () => {
    const members = {
      ...DEPLOYMENT_MEMBERS,
      fraudProofMinAdaStep02TxWithdraw: {
        ...DEPLOYMENT_MEMBERS.fraudProofMinAdaStep02TxWithdraw,
        publicationPosition:
          DEPLOYMENT_MEMBERS.fraudProofMinAda.publicationPosition,
      },
    };
    expect(() => verifyDeploymentMembers(members)).toThrow(
      "duplicated publication position",
    );
    expect(() =>
      verifyDeploymentMembers({
        ...members,
        fraudProofMinAdaStep02TxWithdraw: {
          ...DEPLOYMENT_MEMBERS.fraudProofMinAdaStep02TxWithdraw,
          referenceRole: DEPLOYMENT_MEMBERS.fraudProofMinAda.referenceRole,
        },
      }),
    ).toThrow("duplicated deployment role");
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
