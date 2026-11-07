import {
  applyDoubleCborEncoding,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  declaredParameters,
  type FaultProofBlueprint,
  getBlueprintValidator,
} from "./blueprint.parse-fault-proof-blueprint.js";

export type BlueprintApplicationRecord = Readonly<{
  validatorTitle: string;
  parameters: readonly Readonly<{
    title: string;
    schemaRef?: string;
    cborHex: string;
  }>[];
  appliedScriptCborHex: string;
}>;
// Lifetime follows the parsed blueprint. This records inputs at their real
// application boundary; it never tries to recover parameters from a script hash.
const applications = new WeakMap<
  FaultProofBlueprint,
  Map<string, BlueprintApplicationRecord>
>();

export const recordBlueprintApplication = (
  blueprint: FaultProofBlueprint,
  validatorTitle: string,
  parameterCbor: readonly string[],
  script: string,
): void => {
  const validator = getBlueprintValidator(blueprint, validatorTitle);
  const declared = declaredParameters(validator);
  if (declared.length !== parameterCbor.length)
    throw new Error("Cannot record incomplete blueprint parameters");
  const appliedScriptCborHex = applyDoubleCborEncoding(script);
  const record: BlueprintApplicationRecord = Object.freeze({
    validatorTitle,
    parameters: Object.freeze(
      declared.map((parameter, index) =>
        Object.freeze({ ...parameter, cborHex: parameterCbor[index]! }),
      ),
    ),
    appliedScriptCborHex,
  });
  let records = applications.get(blueprint);
  if (records === undefined) {
    records = new Map();
    applications.set(blueprint, records);
  }
  records.set(
    JSON.stringify([validatorTitle, parameterCbor, appliedScriptCborHex]),
    record,
  );
};

export const blueprintApplicationRecords = (
  blueprint: FaultProofBlueprint,
): readonly BlueprintApplicationRecord[] =>
  Object.freeze([...(applications.get(blueprint)?.values() ?? [])]);

export type BlueprintDeploymentScript = Readonly<{
  contract: string;
  purpose: "spend" | "mint" | "withdraw";
  script: Readonly<{ type: "PlutusV3" | "Native"; script: string }>;
  scriptHash: string;
  referenceScriptRole: string | null;
}>;

/** Complete raw-blueprint census; unknown validators have no default bucket. */
export const buildBlueprintDeploymentRegistry = (
  blueprint: FaultProofBlueprint,
  scripts: readonly BlueprintDeploymentScript[],
  nonDeployedValidators: Readonly<Record<string, string>>,
) => {
  const titles = blueprint.validators.map((validator) => validator.title);
  if (titles.length === 0 || new Set(titles).size !== titles.length)
    throw new Error("Blueprint validator census is empty or duplicated");
  const known = new Set(titles);
  for (const [title, reason] of Object.entries(nonDeployedValidators)) {
    if (
      !known.has(title) ||
      typeof reason !== "string" ||
      reason.trim().length === 0
    )
      throw new Error(`Invalid nondeployed classification: ${title}`);
  }
  const records = blueprintApplicationRecords(blueprint);
  const used = new Map<string, string[]>();
  const contracts = scripts.map((entry) => {
    if (entry.scriptHash !== validatorToScriptHash(entry.script))
      throw new Error(
        `Deployment script hash differs from actual bytes: ${entry.contract}`,
      );
    if (entry.script.type === "Native") {
      if (entry.contract !== "referenceScriptAuthMint")
        throw new Error(
          `Unclassified native deployment contract: ${entry.contract}`,
        );
      return Object.freeze({ ...entry, application: null });
    }
    const script = applyDoubleCborEncoding(entry.script.script);
    const sameBytes = records.filter(
      (record) => record.appliedScriptCborHex === script,
    );
    const samePurpose = sameBytes.filter((record) =>
      record.validatorTitle.endsWith(`.${entry.purpose}`),
    );
    // These existing constructors use a mint-only credential, an else-titled
    // program, the history list's shared program, or a known byte-identical
    // family-step alias. No other contract may borrow
    // a program with a different declared purpose.
    const correspondence = [
      ["hubOracleSpend", "spend", "hub_oracle.mint.mint"],
      ["fraudProofCatalogueSpend", "spend", "fraud_proof_catalogue.spend.else"],
      ["fraudProofSpend", "spend", "fraud_proof.spend.else"],
      ["reserveWithdraw", "withdraw", "reserve.withdraw.else"],
      ["depositMint", "mint", "user_events/history.history.spend"],
      ["withdrawalMint", "mint", "user_events/history.history.spend"],
      [
        "validationTraceDisputeProofItem",
        "spend",
        "fraud_proofs/validation_trace/proof_item_v1.main.else",
      ],
      [
        "fraudProofWithdrawnReferenceInputStep03",
        "spend",
        "fraud_proofs/withdrawn_reference_input/step_03.main.spend",
      ],
      [
        "fraudProofWithdrawnInputStep03",
        "spend",
        "fraud_proofs/withdrawn_input/step_03.main.spend",
      ],
      [
        "fraudProofMintDeclaredAssetLimitStep04",
        "spend",
        "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
      ],
      [
        "fraudProofObserverOrderInvalidStep04",
        "spend",
        "fraud_proofs/observer_order_invalid/step_04.main.spend",
      ],
      [
        "fraudProofNonExistentInputStep03",
        "spend",
        "fraud_proofs/no_input/step_03.main.spend",
      ],
      [
        "fraudProofNonExistentInputStep04",
        "spend",
        "fraud_proofs/no_input/step_04.main.spend",
      ],
      [
        "fraudProofNonExistentInputNoIndexStep04",
        "spend",
        "fraud_proofs/input_no_idx/step_04.main.spend",
      ],
      [
        "fraudProofNoReferenceInputStep03",
        "spend",
        "fraud_proofs/no_reference_input/step_03.main.spend",
      ],
      [
        "fraudProofNoReferenceInputStep04",
        "spend",
        "fraud_proofs/no_reference_input/step_04.main.spend",
      ],
      [
        "fraudProofReferenceInputNoIdxStep04",
        "spend",
        "fraud_proofs/reference_input_no_idx/step_04.main.spend",
      ],
    ].find(
      ([contract, purpose]) =>
        contract === entry.contract && purpose === entry.purpose,
    );
    // Multi-handler aliases can have byte-identical programs. Retain the
    // constructor's requested purpose rather than picking the first bytes.
    const matched =
      correspondence === undefined
        ? samePurpose
        : sameBytes.filter(
            (record) => record.validatorTitle === correspondence[2],
          );
    if (matched.length !== 1)
      throw new Error(
        `Deployment contract needs one complete blueprint parameter record: ${entry.contract}`,
      );
    const application = matched[0]!;
    if (
      Object.prototype.hasOwnProperty.call(
        nonDeployedValidators,
        application.validatorTitle,
      )
    )
      throw new Error(
        `Deployed validator is classified nondeployed: ${application.validatorTitle}`,
      );
    const names = used.get(application.validatorTitle) ?? [];
    names.push(entry.contract);
    used.set(application.validatorTitle, names);
    return Object.freeze({ ...entry, application });
  });
  if (
    new Set(contracts.map((entry) => entry.contract)).size !== contracts.length
  )
    throw new Error("Deployment contracts are duplicated");
  const validators = titles.map((title) => {
    const names = used.get(title);
    if (names !== undefined)
      return Object.freeze({
        title,
        disposition: "deployed" as const,
        contracts: Object.freeze(names),
      });
    const reason = Object.prototype.hasOwnProperty.call(
      nonDeployedValidators,
      title,
    )
      ? nonDeployedValidators[title]
      : undefined;
    if (reason === undefined)
      throw new Error(`Unclassified raw blueprint validator: ${title}`);
    return Object.freeze({
      title,
      disposition: "not_deployed" as const,
      reason,
    });
  });
  return Object.freeze({
    schemaVersion: "midgard-blueprint-deployment-registry-v1" as const,
    validators: Object.freeze(validators),
    contracts: Object.freeze(contracts),
  });
};
