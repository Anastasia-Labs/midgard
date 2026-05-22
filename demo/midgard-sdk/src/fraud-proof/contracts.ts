import {
  Address,
  applyParamsToScript,
  Data,
  MintingPolicy,
  mintingPolicyToId,
  Network,
  SpendingValidator as LucidSpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  AuthenticatedValidator,
  MintingValidator,
  SpendingValidator,
} from "@/common.js";

export type FaultProofBlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

export type FaultProofBlueprint = {
  readonly validators: readonly FaultProofBlueprintValidator[];
};

export const DOUBLE_SPEND_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/double_spend/step_01.main.spend",
  step02: "fraud_proofs/double_spend/step_02.main.spend",
  step03: "fraud_proofs/double_spend/step_03.main.spend",
  step04: "fraud_proofs/double_spend/step_04.main.spend",
} as const;

export const FAULT_PROOF_SHARED_TITLES = {
  computationThreadMint: "computation_thread.mint.mint",
  fraudProofMint: "fraud_proof.mint.mint",
  fraudProofSpend: "fraud_proof.spend.else",
} as const;

export type FraudProofChain = {
  readonly firstStep: SpendingValidator;
  readonly steps: readonly [SpendingValidator, ...SpendingValidator[]];
};

export type DoubleSpendFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleSpend: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type BuildDoubleSpendFaultProofContractsParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
};

export const parseFaultProofBlueprint = (
  value: unknown,
): FaultProofBlueprint => {
  if (typeof value !== "object" || value === null) {
    throw new Error("Fault proof blueprint must be a JSON object");
  }

  const validators = (value as { readonly validators?: unknown }).validators;
  if (!Array.isArray(validators)) {
    throw new Error("Fault proof blueprint must contain validators[]");
  }

  return {
    validators: validators.map((validator, index) => {
      if (typeof validator !== "object" || validator === null) {
        throw new Error(`validators[${index}] must be an object`);
      }
      const candidate = validator as {
        readonly title?: unknown;
        readonly compiledCode?: unknown;
      };
      if (typeof candidate.title !== "string") {
        throw new Error(`validators[${index}].title must be a string`);
      }
      if (typeof candidate.compiledCode !== "string") {
        throw new Error(`validators[${index}].compiledCode must be a string`);
      }
      return {
        title: candidate.title,
        compiledCode: candidate.compiledCode,
      };
    }),
  };
};

const getCompiledScript = (
  blueprint: FaultProofBlueprint,
  title: string,
): string => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found in blueprint`);
  }
  return found.compiledCode;
};

const makeMintingPolicy = (mintingScriptCBOR: string): MintingValidator => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: mintingPolicyToId(mintingScript),
  };
};

const makeSpendingValidator = (
  network: Network,
  spendingScriptCBOR: string,
): SpendingValidator => {
  const spendingScript: LucidSpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
  };
};

const makeAuthenticatedValidator = (
  network: Network,
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeSpendingValidator(network, spendingScriptCBOR),
  ...makeMintingPolicy(mintingScriptCBOR),
});

const asAddressDataParam = (address: Address): Effect.Effect<Data, Error> =>
  addressDataFromBech32(address).pipe(
    Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    Effect.mapError(
      (cause) =>
        new Error(
          `Failed to encode fraud proof token address parameter: ${cause.message}`,
        ),
    ),
  );

const tryBuild = <A>(
  description: string,
  build: () => A,
): Effect.Effect<A, Error> =>
  Effect.try({
    try: build,
    catch: (cause) =>
      new Error(
        `${description}: ${cause instanceof Error ? cause.message : String(cause)}`,
      ),
  });

export const buildDoubleSpendFaultProofContracts = ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
}: BuildDoubleSpendFaultProofContractsParams): Effect.Effect<
  DoubleSpendFaultProofContracts,
  Error
> =>
  Effect.gen(function* () {
    const computationThread = yield* tryBuild(
      "Failed to build computation-thread minting policy",
      () =>
        makeMintingPolicy(
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              FAULT_PROOF_SHARED_TITLES.computationThreadMint,
            ),
            [fraudProofCataloguePolicyId, hubOraclePolicyId],
          ),
        ),
    );

    const fraudProof = yield* tryBuild(
      "Failed to build fraud-proof token validator",
      () =>
        makeAuthenticatedValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              FAULT_PROOF_SHARED_TITLES.fraudProofMint,
            ),
            [computationThread.policyId],
          ),
          getCompiledScript(
            blueprint,
            FAULT_PROOF_SHARED_TITLES.fraudProofSpend,
          ),
        ),
    );

    const fraudProofTokenAddressData = yield* asAddressDataParam(
      fraudProof.spendingScriptAddress,
    );

    const step04 = yield* tryBuild("Failed to build double-spend step 04", () =>
      makeSpendingValidator(
        network,
        applyParamsToScript(
          getCompiledScript(blueprint, DOUBLE_SPEND_FAULT_PROOF_TITLES.step04),
          [
            computationThread.policyId,
            fraudProof.policyId,
            fraudProofTokenAddressData,
          ],
        ),
      ),
    );

    const step03 = yield* tryBuild("Failed to build double-spend step 03", () =>
      makeSpendingValidator(
        network,
        applyParamsToScript(
          getCompiledScript(blueprint, DOUBLE_SPEND_FAULT_PROOF_TITLES.step03),
          [step04.spendingScriptHash, computationThread.policyId],
        ),
      ),
    );

    const step02 = yield* tryBuild("Failed to build double-spend step 02", () =>
      makeSpendingValidator(
        network,
        applyParamsToScript(
          getCompiledScript(blueprint, DOUBLE_SPEND_FAULT_PROOF_TITLES.step02),
          [
            step03.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    const step01 = yield* tryBuild("Failed to build double-spend step 01", () =>
      makeSpendingValidator(
        network,
        applyParamsToScript(
          getCompiledScript(blueprint, DOUBLE_SPEND_FAULT_PROOF_TITLES.step01),
          [
            step02.spendingScriptHash,
            computationThread.policyId,
            hubOraclePolicyId,
          ],
        ),
      ),
    );

    return {
      computationThread,
      fraudProof,
      doubleSpend: {
        firstStep: step01,
        steps: [step01, step02, step03, step04],
      },
    };
  });
