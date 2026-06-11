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
  FaultProofChain,
  FaultProofs,
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

export const FAULT_PROOF_STEP_TITLES = {
  doubleSpend: [
    "fraud_proofs/double_spend/step_01.main.spend",
    "fraud_proofs/double_spend/step_02.main.spend",
    "fraud_proofs/double_spend/step_03.main.spend",
    "fraud_proofs/double_spend/step_04.main.spend",
  ],
  nonExistentInput: [
    "fraud_proofs/no_input/step_01.main.spend",
    "fraud_proofs/no_input/step_02.main.spend",
    "fraud_proofs/no_input/step_03.main.spend",
    "fraud_proofs/no_input/step_04.main.spend",
  ],
  nonExistentInputNoIndex: [
    "fraud_proofs/input_no_idx/step_01.main.spend",
    "fraud_proofs/input_no_idx/step_02.main.spend",
    "fraud_proofs/input_no_idx/step_03.main.spend",
    "fraud_proofs/input_no_idx/step_04.main.spend",
  ],
  invalidRange: [
    "fraud_proofs/invalid_range/step_01.main.spend",
    "fraud_proofs/invalid_range/step_02.main.spend",
  ],
} as const;

export const FAULT_PROOF_SHARED_TITLES = {
  computationThreadMint: "computation_thread.mint.mint",
  fraudProofMint: "fraud_proof.mint.mint",
  fraudProofSpend: "fraud_proof.spend.else",
} as const;

type FourStepFaultProofChain = FaultProofChain & {
  readonly steps: readonly [
    SpendingValidator,
    SpendingValidator,
    SpendingValidator,
    SpendingValidator,
  ];
};

type TwoStepFaultProofChain = FaultProofChain & {
  readonly steps: readonly [SpendingValidator, SpendingValidator];
};

export type ImplementedFaultProofs = FaultProofs & {
  readonly doubleSpend: FourStepFaultProofChain;
  readonly nonExistentInput: FourStepFaultProofChain;
  readonly nonExistentInputNoIndex: FourStepFaultProofChain;
  readonly invalidRange: TwoStepFaultProofChain;
};

export type FaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly faultProofs: ImplementedFaultProofs;
};

export type DoubleSpendFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleSpend: FourStepFaultProofChain;
};

export type BuildFaultProofContractsParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
};

export type BuildDoubleSpendFaultProofContractsParams =
  BuildFaultProofContractsParams;

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
          `Failed to encode fault-proof token address parameter: ${cause.message}`,
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

type ScriptParams = Data[];

type FaultProofBuildContext = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofTokenAddressData: Data;
};

const faultProofChain = <
  Steps extends readonly [SpendingValidator, ...SpendingValidator[]],
>(
  steps: Steps,
): FaultProofChain & { readonly steps: Steps } => ({
  firstStep: steps[0],
  steps,
});

const buildFaultProofStep = (
  context: FaultProofBuildContext,
  label: string,
  title: string,
  params: ScriptParams,
): Effect.Effect<SpendingValidator, Error> =>
  tryBuild(`Failed to build ${label}`, () =>
    makeSpendingValidator(
      context.network,
      applyParamsToScript(getCompiledScript(context.blueprint, title), params),
    ),
  );

const buildDoubleSpendFaultProofChain = (
  context: FaultProofBuildContext,
): Effect.Effect<FourStepFaultProofChain, Error> =>
  Effect.gen(function* () {
    const step04 = yield* buildFaultProofStep(
      context,
      "double-spend step 04",
      FAULT_PROOF_STEP_TITLES.doubleSpend[3],
      [
        context.computationThreadPolicyId,
        context.fraudProofPolicyId,
        context.fraudProofTokenAddressData,
      ],
    );
    const step03 = yield* buildFaultProofStep(
      context,
      "double-spend step 03",
      FAULT_PROOF_STEP_TITLES.doubleSpend[2],
      [step04.spendingScriptHash, context.computationThreadPolicyId],
    );
    const step02 = yield* buildFaultProofStep(
      context,
      "double-spend step 02",
      FAULT_PROOF_STEP_TITLES.doubleSpend[1],
      [
        step03.spendingScriptHash,
        context.computationThreadPolicyId,
        context.hubOraclePolicyId,
      ],
    );
    const step01 = yield* buildFaultProofStep(
      context,
      "double-spend step 01",
      FAULT_PROOF_STEP_TITLES.doubleSpend[0],
      [
        step02.spendingScriptHash,
        context.computationThreadPolicyId,
        context.hubOraclePolicyId,
      ],
    );

    return faultProofChain([step01, step02, step03, step04]);
  });

const buildNonExistentInputFaultProofChain = (
  context: FaultProofBuildContext,
): Effect.Effect<FourStepFaultProofChain, Error> =>
  Effect.gen(function* () {
    const step04 = yield* buildFaultProofStep(
      context,
      "non-existent-input step 04",
      FAULT_PROOF_STEP_TITLES.nonExistentInput[3],
      [
        context.fraudProofPolicyId,
        context.fraudProofTokenAddressData,
        context.computationThreadPolicyId,
      ],
    );
    const step03 = yield* buildFaultProofStep(
      context,
      "non-existent-input step 03",
      FAULT_PROOF_STEP_TITLES.nonExistentInput[2],
      [step04.spendingScriptHash, context.computationThreadPolicyId],
    );
    const step02 = yield* buildFaultProofStep(
      context,
      "non-existent-input step 02",
      FAULT_PROOF_STEP_TITLES.nonExistentInput[1],
      [step03.spendingScriptHash, context.computationThreadPolicyId],
    );
    const step01 = yield* buildFaultProofStep(
      context,
      "non-existent-input step 01",
      FAULT_PROOF_STEP_TITLES.nonExistentInput[0],
      [
        step02.spendingScriptHash,
        context.computationThreadPolicyId,
        context.hubOraclePolicyId,
      ],
    );

    return faultProofChain([step01, step02, step03, step04]);
  });

const buildNonExistentInputNoIndexFaultProofChain = (
  context: FaultProofBuildContext,
): Effect.Effect<FourStepFaultProofChain, Error> =>
  Effect.gen(function* () {
    const step04 = yield* buildFaultProofStep(
      context,
      "non-existent-input-no-index step 04",
      FAULT_PROOF_STEP_TITLES.nonExistentInputNoIndex[3],
      [
        context.computationThreadPolicyId,
        context.fraudProofPolicyId,
        context.fraudProofTokenAddressData,
      ],
    );
    const step03 = yield* buildFaultProofStep(
      context,
      "non-existent-input-no-index step 03",
      FAULT_PROOF_STEP_TITLES.nonExistentInputNoIndex[2],
      [
        step04.spendingScriptHash,
        context.computationThreadPolicyId,
        context.hubOraclePolicyId,
      ],
    );
    const step02 = yield* buildFaultProofStep(
      context,
      "non-existent-input-no-index step 02",
      FAULT_PROOF_STEP_TITLES.nonExistentInputNoIndex[1],
      [step03.spendingScriptHash, context.computationThreadPolicyId],
    );
    const step01 = yield* buildFaultProofStep(
      context,
      "non-existent-input-no-index step 01",
      FAULT_PROOF_STEP_TITLES.nonExistentInputNoIndex[0],
      [
        step02.spendingScriptHash,
        context.computationThreadPolicyId,
        context.hubOraclePolicyId,
      ],
    );

    return faultProofChain([step01, step02, step03, step04]);
  });

const buildInvalidRangeFaultProofChain = (
  context: FaultProofBuildContext,
): Effect.Effect<TwoStepFaultProofChain, Error> =>
  Effect.gen(function* () {
    const step02 = yield* buildFaultProofStep(
      context,
      "invalid-range step 02",
      FAULT_PROOF_STEP_TITLES.invalidRange[1],
      [
        context.fraudProofPolicyId,
        context.fraudProofTokenAddressData,
        context.computationThreadPolicyId,
      ],
    );
    const step01 = yield* buildFaultProofStep(
      context,
      "invalid-range step 01",
      FAULT_PROOF_STEP_TITLES.invalidRange[0],
      [
        step02.spendingScriptHash,
        context.computationThreadPolicyId,
        context.hubOraclePolicyId,
      ],
    );

    return faultProofChain([step01, step02]);
  });

export const buildFaultProofContracts = ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
}: BuildFaultProofContractsParams): Effect.Effect<FaultProofContracts, Error> =>
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
      "Failed to build fault-proof token validator",
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
    const context: FaultProofBuildContext = {
      blueprint,
      network,
      hubOraclePolicyId,
      computationThreadPolicyId: computationThread.policyId,
      fraudProofPolicyId: fraudProof.policyId,
      fraudProofTokenAddressData,
    };
    const doubleSpend = yield* buildDoubleSpendFaultProofChain(context);
    const nonExistentInput =
      yield* buildNonExistentInputFaultProofChain(context);
    const nonExistentInputNoIndex =
      yield* buildNonExistentInputNoIndexFaultProofChain(context);
    const invalidRange = yield* buildInvalidRangeFaultProofChain(context);

    return {
      computationThread,
      fraudProof,
      faultProofs: {
        doubleSpend,
        nonExistentInput,
        nonExistentInputNoIndex,
        invalidRange,
      },
    };
  });

export const buildDoubleSpendFaultProofContracts = (
  params: BuildDoubleSpendFaultProofContractsParams,
): Effect.Effect<DoubleSpendFaultProofContracts, Error> =>
  buildFaultProofContracts(params).pipe(
    Effect.map(({ computationThread, fraudProof, faultProofs }) => ({
      computationThread,
      fraudProof,
      doubleSpend: faultProofs.doubleSpend,
    })),
  );
