import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  type WorkflowActuationPermit,
  workflowActuationPermitIsReconciliationOnly,
} from "./actuation-permit.js";
import {
  assertManifestBoundWorkflowIdentity,
  FAMILY_APPLICATION_RECORD,
  type FamilyApplicationRecord,
  type FamilyApplicationWorkflowIdentity,
  type FamilyCommonInfrastructure,
  type FamilyResolvedReferenceScripts,
  outRefLabel,
} from "./family-application.assert-family-definition-roster.js";

export type FamilyApplicationInvocation = Readonly<{
  deploymentFingerprint: string;
  category: FraudProofCatalogueCategoryName;
  headerHash: string;
  /**
   * Proof that this invocation only reads back an already-submitted chain.
   * Omitted by every invocation that may build, and a family's optional
   * requirements are then in force. Supplying it exempts them, because a
   * reconciliation-only resume reconciles through the recovered adapter and
   * never reaches the evidence pipeline — exactly as it does today.
   *
   * It is a permit rather than a flag because the exemption is the unsafe
   * state: the permit is module-private authority, so a caller that has not
   * been restricted to reconciliation cannot claim it, and a structural
   * lookalike is refused outright.
   */
  reconciliationAuthority?: WorkflowActuationPermit;
}>;

/** How the host resolves one roster entry to its published reference UTxO. */
export type FamilyReferenceScriptResolver = (input: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly role: string;
  readonly contractName: string;
}) => Promise<UTxO>;

export type AppliedFamilyApplication<Config, Workflow> = Readonly<{
  config: Config;
  workflow: Workflow;
  references: FamilyResolvedReferenceScripts;
  /** One `txHash#index` per roster role, derived from the resolved map. */
  referenceScriptOutRefs: Readonly<Record<string, string>>;
}>;

/** A record only carries authority if it was minted through the definer. */
const assertFamilyApplicationRecordVersion = (
  record: Readonly<{ recordVersion: string; category: string }>,
): void => {
  if (record.recordVersion !== FAMILY_APPLICATION_RECORD) {
    throw new Error(
      `family application record for ${record.category} has an unsupported schema`,
    );
  }
};

/**
 * The part of a record readiness reads: its minted version, its category and
 * its roster. It names no config or workflow, so a registry entry, whose
 * config is erased, is accepted as it is.
 */
export type FamilyApplicationRosterRecord = Readonly<{
  recordVersion: typeof FAMILY_APPLICATION_RECORD;
  category: FraudProofCatalogueCategoryName;
  roster: Readonly<Record<string, string>>;
}>;

/** What a resolved roster yields, before any family config is bound. */
export type ResolvedFamilyApplicationReferences = Readonly<{
  references: FamilyResolvedReferenceScripts;
  /** One `txHash#index` per roster role, derived from the resolved map. */
  referenceScriptOutRefs: Readonly<Record<string, string>>;
}>;

/**
 * Resolves a family's whole roster and reports the out-refs it will spend.
 *
 * This is what startup readiness calls. It deliberately stops here: it binds
 * no config and constructs no workflow, so readiness cannot act and has no
 * need of the optional infrastructure an acting invocation must hold. A
 * family's requirements are therefore not exempted for readiness — readiness
 * never reaches the point where they would apply.
 */
export const resolveFamilyApplicationReferences = async ({
  record,
  resolveReferenceScript,
}: {
  readonly record: FamilyApplicationRosterRecord;
  readonly resolveReferenceScript: FamilyReferenceScriptResolver;
}): Promise<ResolvedFamilyApplicationReferences> => {
  assertFamilyApplicationRecordVersion(record);
  const references: FamilyResolvedReferenceScripts = Object.freeze(
    Object.fromEntries(
      await Promise.all(
        Object.entries(record.roster).map(
          async ([role, contractName]): Promise<readonly [string, UTxO]> => [
            role,
            await resolveReferenceScript({
              category: record.category,
              role,
              contractName,
            }),
          ],
        ),
      ),
    ),
  );
  return Object.freeze({
    references,
    referenceScriptOutRefs: Object.freeze(
      Object.fromEntries(
        Object.keys(record.roster).map((role) => [
          role,
          outRefLabel(references[role]!),
        ]),
      ),
    ),
  });
};

/**
 * The shared application loop: refuse a missing required part, resolve the
 * roster, bind the family's config, construct its workflow, check the
 * constructed identity against the invocation, and report the out-refs the
 * family will spend. No per-family branch, here or in any caller.
 */
export const applyFamilyApplicationRecord = async <
  Category extends FraudProofCatalogueCategoryName,
  Config,
  Workflow extends FamilyApplicationWorkflowIdentity<Category>,
>({
  record,
  infrastructure,
  resolveReferenceScript,
  invocation,
}: {
  readonly record: FamilyApplicationRecord<Category, Config, Workflow>;
  readonly infrastructure: FamilyCommonInfrastructure;
  readonly resolveReferenceScript: FamilyReferenceScriptResolver;
  readonly invocation: FamilyApplicationInvocation;
}): Promise<AppliedFamilyApplication<Config, Workflow>> => {
  assertFamilyApplicationRecordVersion(record);
  if (invocation.category !== record.category) {
    throw new Error(
      `family application refuses category ${invocation.category}; expected ${record.category}`,
    );
  }
  if (invocation.reconciliationAuthority === undefined) {
    for (const requirement of record.requires) {
      if (infrastructure[requirement] === undefined) {
        throw new Error(
          `${record.category} application requires ${requirement}, which the host did not supply`,
        );
      }
    }
  } else if (
    !workflowActuationPermitIsReconciliationOnly(
      invocation.reconciliationAuthority,
    )
  ) {
    throw new Error(
      `${record.category} application claimed the reconciliation exemption under a permit that still admits actuation`,
    );
  }
  const { references, referenceScriptOutRefs } =
    await resolveFamilyApplicationReferences({
      record,
      resolveReferenceScript,
    });
  const config = await record.bindConfig({ infrastructure, references });
  const workflow = await record.constructWorkflow(config);
  assertManifestBoundWorkflowIdentity({
    workflow,
    category: record.category,
    deploymentFingerprint: invocation.deploymentFingerprint,
    headerHash: invocation.headerHash,
    bindsDecisionDigest: record.bindsDecisionDigest,
    decisionDigest: infrastructure.decisionDigest,
  });
  return Object.freeze({
    config,
    workflow,
    references,
    referenceScriptOutRefs,
  });
};
