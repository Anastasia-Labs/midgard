import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "./catalogue-roles.deployment-manifest-contract-names.js";
import type { DeploymentManifestFraudProofCatalogueCategory } from "./catalogue-roles.protocol-categories.js";

/** The positional protocol ABI is independent of the member projections. */
export type DeploymentContract =
  (typeof DEPLOYMENT_MANIFEST_CONTRACT_NAMES)[number];
export type DeploymentFamily =
  | DeploymentManifestFraudProofCatalogueCategory
  | "protocol";
export type DeploymentScriptPurpose = "spend" | "mint" | "withdraw";

export const REFERENCE_SCRIPT_COMMAND_NAMES = [
  "node-runtime",
  "protocol-init",
  "reference-script-auth",
  "hub-oracle",
  "da",
  "state-queue",
  "scheduler",
  "registered-operators",
  "active-operators",
  "retired-operators",
  "deposit",
  "withdrawal",
  "settlement",
  "phas-membership",
  "reserve",
  "payout",
] as const;
export type ReferenceScriptCommandName =
  (typeof REFERENCE_SCRIPT_COMMAND_NAMES)[number];
export type DeploymentScriptCommandName = Exclude<
  ReferenceScriptCommandName,
  "node-runtime"
>;

export type DeploymentMember = Readonly<{
  family: DeploymentFamily;
  purpose: DeploymentScriptPurpose;
  referenceRole: string | null;
  manifestPosition: number;
  publicationPosition: number | null;
  referenceRolePosition: number | null;
  commands: readonly DeploymentScriptCommandName[];
  /** Null only for the native reference-script authorization policy. */
  validatorTitle: string | null;
  /**
   * Public SDK family aliases, relative to fraudProofContracts[family]. The
   * first path is the one the SDK deployment recipe reads the script from.
   */
  familyPaths: readonly (readonly string[])[];
}>;

export const defineDeploymentFamily = <
  const Family extends DeploymentFamily,
  const Members extends Readonly<
    Record<string, Omit<DeploymentMember, "family">>
  >,
>(
  family: Family,
  members: Members,
): {
  readonly [Contract in keyof Members]: Readonly<
    Members[Contract] & { readonly family: Family }
  >;
} =>
  Object.freeze(
    Object.fromEntries(
      Object.entries(members).map(([contract, member]) => [
        contract,
        Object.freeze({
          ...member,
          family,
          commands: Object.freeze(member.commands),
          familyPaths: Object.freeze(
            member.familyPaths.map((path) => Object.freeze(path)),
          ),
        }),
      ]),
    ),
  ) as {
    readonly [Contract in keyof Members]: Readonly<
      Members[Contract] & { readonly family: Family }
    >;
  };
