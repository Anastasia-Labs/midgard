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

export const NON_EXISTENT_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/no_input/step_01.main.spend",
  step02: "fraud_proofs/no_input/step_02.main.spend",
  step03: "fraud_proofs/no_input/step_03.main.spend",
  step04: "fraud_proofs/no_input/step_04.main.spend",
} as const;

export const NO_REFERENCE_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/no_reference_input/step_01.main.spend",
  step02: "fraud_proofs/no_reference_input/step_02.main.spend",
  step03: "fraud_proofs/no_reference_input/step_03.main.spend",
  step04: "fraud_proofs/no_reference_input/step_04.main.spend",
} as const;

export const INPUT_NO_IDX_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/input_no_idx/step_01.main.spend",
  step02: "fraud_proofs/input_no_idx/step_02.main.spend",
  step03: "fraud_proofs/input_no_idx/step_03.main.spend",
  step04: "fraud_proofs/input_no_idx/step_04.main.spend",
} as const;

export const INVALID_RANGE_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/invalid_range/step_01.main.spend",
  step02: "fraud_proofs/invalid_range/step_02.main.spend",
} as const;

export const ZERO_INPUT_FAULT_PROOF_TITLES = {
  step01: "fraud_proofs/zero_input/step_01.main.spend",
  step02: "fraud_proofs/zero_input/step_02.main.spend",
} as const;

export const TRANSITION_TRACE_FAULT_PROOF_TITLES = {
  proof: "fraud_proofs/transition_trace/proof.main.spend",
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

export type NonExistentInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly nonExistentInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type NoReferenceInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly noReferenceInput: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type InputNoIdxFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly inputNoIdx: FraudProofChain & {
    readonly steps: readonly [
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
      SpendingValidator,
    ];
  };
};

export type InvalidRangeFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly invalidRange: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type ZeroInputFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly zeroInput: FraudProofChain & {
    readonly steps: readonly [SpendingValidator, SpendingValidator];
  };
};

export type TransitionTraceFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly transitionTrace: FraudProofChain & {
    readonly steps: readonly [SpendingValidator];
  };
};

export type FaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly doubleSpend: DoubleSpendFaultProofContracts["doubleSpend"];
  readonly nonExistentInput: NonExistentInputFaultProofContracts["nonExistentInput"];
  readonly noReferenceInput: NoReferenceInputFaultProofContracts["noReferenceInput"];
  readonly inputNoIdx: InputNoIdxFaultProofContracts["inputNoIdx"];
  readonly invalidRange: InvalidRangeFaultProofContracts["invalidRange"];
  readonly zeroInput: ZeroInputFaultProofContracts["zeroInput"];
  readonly transitionTrace: TransitionTraceFaultProofContracts["transitionTrace"];
};

type SharedFaultProofContracts = {
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
};

export type BuildFaultProofContractsParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofCataloguePolicyId: string;
};

export type BuildDoubleSpendFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildNonExistentInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildNoReferenceInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildInputNoIdxFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildInvalidRangeFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildZeroInputFaultProofContractsParams =
  BuildFaultProofContractsParams;

export type BuildTransitionTraceFaultProofContractsParams =
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

const buildSharedFaultProofContracts = ({
  blueprint,
  network,
  hubOraclePolicyId,
  fraudProofCataloguePolicyId,
}: BuildFaultProofContractsParams): Effect.Effect<
  SharedFaultProofContracts,
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

    return {
      computationThread,
      fraudProof,
      fraudProofTokenAddressData,
    };
  });

const buildDoubleSpendChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<DoubleSpendFaultProofContracts["doubleSpend"], Error> =>
  Effect.gen(function* () {
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
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildNonExistentInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  NonExistentInputFaultProofContracts["nonExistentInput"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build non-existent-input step 04",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step04,
            ),
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build non-existent-input step 03",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step03,
            ),
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build non-existent-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step02,
            ),
            [step03.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build non-existent-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NON_EXISTENT_INPUT_FAULT_PROOF_TITLES.step01,
            ),
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildInputNoIdxChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<InputNoIdxFaultProofContracts["inputNoIdx"], Error> =>
  Effect.gen(function* () {
    // step-04 params: (ct_policy, fraud_proof_policy, fraud_proof_address) —
    // note the order differs from no-input's step-04.
    const step04 = yield* tryBuild(
      "Failed to build input-no-idx step 04",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(blueprint, INPUT_NO_IDX_FAULT_PROOF_TITLES.step04),
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
            ],
          ),
        ),
    );

    // step-03 binds the producing native tx, so — unlike no-input's step-03 —
    // it takes the hub-oracle policy id as a third parameter.
    const step03 = yield* tryBuild(
      "Failed to build input-no-idx step 03",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(blueprint, INPUT_NO_IDX_FAULT_PROOF_TITLES.step03),
            [
              step04.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build input-no-idx step 02",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(blueprint, INPUT_NO_IDX_FAULT_PROOF_TITLES.step02),
            [step03.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build input-no-idx step 01",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(blueprint, INPUT_NO_IDX_FAULT_PROOF_TITLES.step01),
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildNoReferenceInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  NoReferenceInputFaultProofContracts["noReferenceInput"],
  Error
> =>
  Effect.gen(function* () {
    const step04 = yield* tryBuild(
      "Failed to build no-reference-input step 04",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step04,
            ),
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const step03 = yield* tryBuild(
      "Failed to build no-reference-input step 03",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step03,
            ),
            [step04.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step02 = yield* tryBuild(
      "Failed to build no-reference-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step02,
            ),
            [step03.spendingScriptHash, computationThread.policyId],
          ),
        ),
    );

    const step01 = yield* tryBuild(
      "Failed to build no-reference-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              NO_REFERENCE_INPUT_FAULT_PROOF_TITLES.step01,
            ),
            [
              step02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: step01,
      steps: [step01, step02, step03, step04],
    };
  });

const buildInvalidRangeChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<InvalidRangeFaultProofContracts["invalidRange"], Error> =>
  Effect.gen(function* () {
    const invalidRangeStep02 = yield* tryBuild(
      "Failed to build invalid-range step 02",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              INVALID_RANGE_FAULT_PROOF_TITLES.step02,
            ),
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const invalidRangeStep01 = yield* tryBuild(
      "Failed to build invalid-range step 01",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              INVALID_RANGE_FAULT_PROOF_TITLES.step01,
            ),
            [
              invalidRangeStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: invalidRangeStep01,
      steps: [invalidRangeStep01, invalidRangeStep02],
    };
  });

const buildZeroInputChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<ZeroInputFaultProofContracts["zeroInput"], Error> =>
  Effect.gen(function* () {
    const zeroInputStep02 = yield* tryBuild(
      "Failed to build zero-input step 02",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(blueprint, ZERO_INPUT_FAULT_PROOF_TITLES.step02),
            [
              fraudProof.policyId,
              fraudProofTokenAddressData,
              computationThread.policyId,
            ],
          ),
        ),
    );

    const zeroInputStep01 = yield* tryBuild(
      "Failed to build zero-input step 01",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(blueprint, ZERO_INPUT_FAULT_PROOF_TITLES.step01),
            [
              zeroInputStep02.spendingScriptHash,
              computationThread.policyId,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: zeroInputStep01,
      steps: [zeroInputStep01, zeroInputStep02],
    };
  });

const buildTransitionTraceChain = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThread,
  fraudProof,
  fraudProofTokenAddressData,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThread: MintingValidator;
  readonly fraudProof: AuthenticatedValidator;
  readonly fraudProofTokenAddressData: Data;
}): Effect.Effect<
  TransitionTraceFaultProofContracts["transitionTrace"],
  Error
> =>
  Effect.gen(function* () {
    const proof = yield* tryBuild(
      "Failed to build transition-trace proof validator",
      () =>
        makeSpendingValidator(
          network,
          applyParamsToScript(
            getCompiledScript(
              blueprint,
              TRANSITION_TRACE_FAULT_PROOF_TITLES.proof,
            ),
            [
              computationThread.policyId,
              fraudProof.policyId,
              fraudProofTokenAddressData,
              hubOraclePolicyId,
            ],
          ),
        ),
    );

    return {
      firstStep: proof,
      steps: [proof],
    };
  });

export const buildFaultProofContracts = (
  params: BuildFaultProofContractsParams,
): Effect.Effect<FaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleSpend = yield* buildDoubleSpendChain({
      ...params,
      ...shared,
    });
    const nonExistentInput = yield* buildNonExistentInputChain({
      ...params,
      ...shared,
    });
    const noReferenceInput = yield* buildNoReferenceInputChain({
      ...params,
      ...shared,
    });
    const inputNoIdx = yield* buildInputNoIdxChain({
      ...params,
      ...shared,
    });
    const invalidRange = yield* buildInvalidRangeChain({
      ...params,
      ...shared,
    });
    const zeroInput = yield* buildZeroInputChain({
      ...params,
      ...shared,
    });
    const transitionTrace = yield* buildTransitionTraceChain({
      ...params,
      ...shared,
    });

    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      doubleSpend,
      nonExistentInput,
      noReferenceInput,
      inputNoIdx,
      invalidRange,
      zeroInput,
      transitionTrace,
    };
  });

export const buildDoubleSpendFaultProofContracts = (
  params: BuildDoubleSpendFaultProofContractsParams,
): Effect.Effect<DoubleSpendFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleSpend = yield* buildDoubleSpendChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      doubleSpend,
    };
  });

export const buildNonExistentInputFaultProofContracts = (
  params: BuildNonExistentInputFaultProofContractsParams,
): Effect.Effect<NonExistentInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const nonExistentInput = yield* buildNonExistentInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      nonExistentInput,
    };
  });

export const buildNoReferenceInputFaultProofContracts = (
  params: BuildNoReferenceInputFaultProofContractsParams,
): Effect.Effect<NoReferenceInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const noReferenceInput = yield* buildNoReferenceInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      noReferenceInput,
    };
  });

export const buildInputNoIdxFaultProofContracts = (
  params: BuildInputNoIdxFaultProofContractsParams,
): Effect.Effect<InputNoIdxFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const inputNoIdx = yield* buildInputNoIdxChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      inputNoIdx,
    };
  });

export const buildInvalidRangeFaultProofContracts = (
  params: BuildInvalidRangeFaultProofContractsParams,
): Effect.Effect<InvalidRangeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const invalidRange = yield* buildInvalidRangeChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      invalidRange,
    };
  });

export const buildZeroInputFaultProofContracts = (
  params: BuildZeroInputFaultProofContractsParams,
): Effect.Effect<ZeroInputFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const zeroInput = yield* buildZeroInputChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      zeroInput,
    };
  });

export const buildTransitionTraceFaultProofContracts = (
  params: BuildTransitionTraceFaultProofContractsParams,
): Effect.Effect<TransitionTraceFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const transitionTrace = yield* buildTransitionTraceChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      transitionTrace,
    };
  });
